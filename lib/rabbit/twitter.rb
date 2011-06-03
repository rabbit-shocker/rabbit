require 'shellwords'
require 'pathname'

module Rabbit
  class Twitter
    CONSUMER_KEY = "wT9WSC0afRw94fxUw0iIKw"
    CONSUMER_SECRET = "mwY35vfQfmWde9lZbyNNB15QzCq3k2VwGj3X1IAkQ8"

    def initialize(logger)
      @logger = logger
      @oauth_parameters = nil
      @config_file_path = Pathname.new("~/.rabbit/twitter-oauth.yaml")
      @config_file_path = @config_file_path.expand_path
      @listeners = []
      @connection = nil
    end

    def register_listener(&block)
      @listeners << block
    end

    def setup
      return unless @oauth_parameters.nil?
      require 'twitter_oauth'
      setup_access_token unless @config_file_path.exist?
      oauth_access_parameters = YAML.load(@config_file_path.read)
      @oauth_parameters = {
        :consumer_key => CONSUMER_KEY,
        :consumer_secret => CONSUMER_SECRET,
        :access_key => oauth_access_parameters[:access_token],
        :access_secret => oauth_access_parameters[:access_secret],
      }
    end

    def close
      return if @connection.nil?
      @connection.close
      @connection = nil
    end

    def start_stream(*filters)
      close
      setup if @oauth_parameters.nil?
      require 'socket'
      require 'twitter/json_stream'
      stream_options = {
        :oauth => @oauth_parameters,
        :host => "stream.twitter.com",
        :path => "/1/statuses/filter.json",
        :method => "POST",
        :filters => filters,
      }
      @stream = ::Twitter::JSONStream.new(:signature, stream_options)
      @connection = GLibConnection.new(stream_options, @stream)

      @stream.each_item do |item|
        item = JSON.parse(item)
        p item
        @listeners.each do |listener|
          listener.call(item)
        end
      end

      @stream.on_error do |message|
        @logger.error("[twitter] #{message}")
      end

      @stream.on_reconnect do |timeout, retries|
        @logger.info("[twitter][reconnect] #{timeout} seconds (#{retries})")
      end

      @stream.on_max_reconnects do |timeout, retries|
        @logger.info("[twitter][max-reconnects] " +
                     "Failed after #{retries} failed reconnects")
      end

      @connection.connect
    end

    private
    def setup_access_token
      FileUtils.mkdir_p(@config_file_path.dirname)
      oauth_options = {
        :consumer_key => CONSUMER_KEY,
        :consumer_secret => CONSUMER_SECRET,
        :proxy => ENV["http_proxy"],
      }
      client = TwitterOAuth::Client.new(oauth_options)
      request_token = client.request_token
      puts "1) Access this URL: #{request_token.authorize_url}"
      print "2) Enter the PIN: "
      pin = STDIN.gets.strip
      access_token = request_token.get_access_token(:oauth_verifier => pin)
      oauth_parameters = {
        :access_token => access_token.token,
        :access_secret => access_token.secret,
      }
      @config_file_path.open("w") do |config_file|
        config_file.chmod(0600)
        config_file.puts(YAML.dump(oauth_parameters))
      end
    end

    class GLibConnection
      def initialize(options, handler)
        @options = options
        @handler = handler
        @socket = nil
        @channel = nil
        @source_ids = []
      end

      def connect
        close
        @socket = TCPSocket.new(@options[:host], "http")
        @channel = GLib::IOChannel.new(@socket.fileno)
        @channel.flags = GLib::IOChannel::FLAG_NONBLOCK
        reader_id = @channel.add_watch(GLib::IOChannel::IN) do |io, condition|
          data = io.read(4096)
          if data.empty?
            @source_ids.delete!(reader_id)
            false
          else
            @handler.receive_data(data)
            true
          end
        end
        @source_ids << reader_id
        error_id = @channel.add_watch(GLib::IOChannel::ERR) do |io, condition|
          p condition
          @handler.receive_error(condition)
          true
        end
        @source_ids << error_id
        @handler.extend(GLibAdapter)
        @handler.connection = self
        @handler.connection_completed
      end

      def send_data(data)
        rest = data.bytesize
        flushed = false
        @channel.add_watch(GLib::IOChannel::OUT) do |io, condition|
          if rest.zero?
            @channel.flush
            false
          else
            written_size = @channel.write(data)
            rest -= written_size
            data[0, written_size] = ""
            true
          end
        end
      end

      def close
        return if @socket.nil?
        @source_ids.reject! do |id|
          GLib::Source.remove(id)
          true
        end
        @channel = nil
        @socket.close
      end

      def reconnect(options={})
        close
        after = options[:after] || 0
        if after.zero?
          connect
        else
          id = GLib::Timeout.add(after) do
            connect
            false
          end
          @source_ids << id
        end
      end
    end

    module GLibAdapter
      attr_accessor :connection
      def send_data(data)
        @connection.send_data(data)
      end

      def reconnect_after(timeout)
        @reconnect_callback.call(timeout, @reconnect_retries) if @reconnect_callback
        @connection.reconnect(:after => timeout)
      end

      def reconnect(server, port)
        @connection.reconnect
      end

      def close_connection
        @connection.close
      end
    end
  end
end
