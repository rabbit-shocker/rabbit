require "uri"
require "open-uri"

require 'rabbit/source/base'

module Rabbit
  module Source
    class URI

      include Base
      include LimitAccessInterval

      class << self
        def new(encoding, uri)
          parsed_uri = ::URI.parse(uri)
          case parsed_uri.scheme
          when nil, /file/i
            File.new(encoding, parsed_uri.path)
          else
            super
          end
        end

        def initial_args_description
          "URI"
        end
      end
      
      def initialize(encoding, uri)
        @uri = ::URI.parse(uri)
        @last_modified = nil
        super(encoding)
      end

      def need_read?
        super or old?(@last_modified, :last_modified)
      end

      private
      def _read
        @uri.open do |f|
          @last_modified = f.last_modified
          f.read
        end
      end

      def init_base
        base = @uri.dup
        base.path = ::File.dirname(base.path)
        set_base(base.to_s)
      end

      def last_modified
        @uri.open do |f|
          f.last_modified
        end
      end

    end

  end
end
