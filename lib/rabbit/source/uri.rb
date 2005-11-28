require "uri"
require "open-uri"

require 'rabbit/source/base'

module Rabbit
  module Source
    class URI

      include Base
      include LimitAccessInterval

      class << self
        def new(encoding, logger, uri)
          parsed_uri = ::URI.parse(uri)
          case parsed_uri.scheme
          when nil, /file/i
            File.new(encoding, logger, parsed_uri.path)
          else
            super
          end
        end

        def initial_args_description
          N_("URI")
        end
      end
      
      def initialize(encoding, logger, uri)
        @uri = ::URI.parse(uri)
        super(encoding, logger)
        @last_modified = nil
      end

      def need_read?
        super or old?(@last_modified, :last_modified)
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
          @last_modified = Time.now + MINIMUM_ACCESS_TIME
          ""
        end
      end

      def init_base
        base = @uri.dup
        base.path = ::File.dirname(base.path)
        set_base(base.to_s)
      end

      def last_modified
        begin
          @uri.open do |f|
            f.last_modified
          end
        rescue
          @logger.error($!.message)
          Time.now
        end
      end

    end

  end
end
