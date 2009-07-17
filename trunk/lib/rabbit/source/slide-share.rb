require "uri"
require "open-uri"

require 'rabbit/source/base'

module Rabbit
  module Source
    class SlideShare

      include Base

      class << self
        def initial_args_description
          N_("[USER] and [TITLE]")
        end
      end

      def initialize(encoding, logger, user, title)
        @user = user
        @title = title
        @uri = ::URI.parse("#{slide_uri}/download")
        super(encoding, logger)
        @last_modified = nil
      end

      private
      def _read
        begin
          @uri.open do |f|
            @last_modified = f.last_modified
            f.read
          end
        rescue
          @logger.error($!.message)
          @last_modified = Time.now
          ""
        end
      end

      def init_base
        set_base(slide_uri)
      end

      def last_modified
        @last_modified ||= Time.now
      end

      def slide_uri
        "http://www.slideshare.net/#{@user}/#{@title}"
      end
    end
  end
end
