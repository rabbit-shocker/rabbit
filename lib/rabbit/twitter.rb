require 'shellwords'
require 'pathname'

module Rabbit
  class Twitter
    CONSUMER_KEY = "wT9WSC0afRw94fxUw0iIKw"
    CONSUMER_SECRET = "mwY35vfQfmWde9lZbyNNB15QzCq3k2VwGj3X1IAkQ8"

    def initialize
      @oauth_parameters = nil
      @config_file_path = Pathname.new("~/.rabbit/twitter-oauth.yaml")
      @config_file_path = @config_file_path.expand_path
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
        :proxy => ENV['http_proxy'],
      }
    end

    def start_stream
      setup if @oauth_parameters.nil?
      require 'twitter/json_stream'
      Thread.start do
        EventMachine.run do
          stream_options = {
            :oauth => @oauth_parameters,
          }
          @stream = ::Twitter::JSONStream.connect(stream_options)
          @stream.each_item do |item|
            p JSON.parse(item)
          end

          @stream.on_error do |message|
            $stdout.print "error: #{message}\n"
            $stdout.flush
          end

          @stream.on_reconnect do |timeout, retries|
            $stdout.print "reconnecting in: #{timeout} seconds\n"
            $stdout.flush
          end

          @stream.on_max_reconnects do |timeout, retries|
            $stdout.print "Failed after #{retries} failed reconnects\n"
            $stdout.flush
          end
        end
      end
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
  end
end
