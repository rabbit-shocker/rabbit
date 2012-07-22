require "shellwords"
require "pathname"
require "gio2"

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
      require 'yaml'
      begin
        require 'twitter_oauth'
      rescue LoadError
        @logger.warn(_("twitter_oauth gem is missing. " \
                       "Install it by 'gem install twitter_oauth'."))
        return
      end
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

    def start(*filters, &block)
      register_listener(&block) if block_given?
      setup if @oauth_parameters.nil?
      return if @oauth_parameters.nil?
      begin
        require 'twitter/json_stream'
      rescue LoadError
        @logger.warn(_("twitter-stream gem is missing. " \
                       "Install it by 'gem install twitter-stream'."))
        return
      end

      stream_options = {
        :oauth => @oauth_parameters,
        :user_agent => "Rabitter #{Rabbit::VERSION}",
        :method => "POST",
        :filters => filters,
      }
      @stream = ::Twitter::JSONStream.allocate
      @stream.send(:initialize, stream_options)
      @stream.send(:reset_state)
      @connection = GLibConnection.new(@logger, @stream)

      @stream.each_item do |item|
        status = JSON.parse(item)
        @listeners.each do |listener|
          listener.call(status)
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
      def initialize(logger, handler)
        @logger = logger
        @handler = handler
        @options = @handler.instance_variable_get("@options")
        @client = nil
        @connection = nil
        @socket = nil
        @source_ids = []
      end

      def connect
        close
        @client = Gio::SocketClient.new
        @client.tls = @options[:ssl]
        @client.tls_validation_flags = :validate_all
        if Utils.windows?
          @client.tls_validation_flags -= :unknown_ca
        end
        @connection = @client.connect_to_host(@options[:host], @options[:port])
        @socket = @connection.socket
        @socket.blocking = false
        @input = @connection.input_stream
        @output = @connection.output_stream

        reader_source = @socket.create_source(:in) do |socket, condition|
          @logger.debug("[twitter][read][start]")
          data = @input.read(8192) || ""
          @logger.debug("[twitter][read][done] #{data.bytesize}")
          if data.empty?
            @source_ids.reject! {|id| id == reader_source.id}
            @logger.debug("[twitter][read][eof]")
            false
          else
            @handler.receive_data(data)
            true
          end
        end
        @source_ids << reader_source.attach

        error_source = @socket.create_source(:err) do |socket, condition|
          @handler.send(:receive_error, condition)
          true
        end
        @source_ids << error_source.attach

        @handler.extend(GLibAdapter)
        @handler.connection = self
        @handler.connection_completed
      end

      def send_data(data)
        rest = data.bytesize
        writer_source = @socket.create_source(:out) do |socket, condition|
          if rest.zero?
            @logger.debug("[twitter][flush][start]")
            @output.flush
            @logger.debug("[twitter][flush][done]")
            @source_ids.reject! {|id| id == writer_source.id}
            false
          else
            @logger.debug("[twitter][write][start]")
            written_size = @output.write(data)
            @logger.debug("[twitter][write][done] #{written_size}")
            rest -= written_size
            data[0, written_size] = ""
            true
          end
        end
        @source_ids << writer_source.attach
      end

      def close
        return if @client.nil?
        @source_ids.reject! do |id|
          GLib::Source.remove(id)
          true
        end
        @socket.close
        @socket = nil
        @input = nil
        @output = nil
        @connection = nil
        @client = nil
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
      def start_tls
      end

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
