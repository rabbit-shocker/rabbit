require "uri"
require "open-uri"

require 'rabbit/source/base'

module Rabbit
  module Source
    class URI

      include Base
      include LimitAccessInterval

      def self.initial_args_description
        "URI"
      end

      def initialize(encoding, uri)
        @uri = ::URI.parse(uri)
        @last_modified = nil
        super(encoding)
      end

      def full_path(path)
        new_path = ::URI.parse(@base)
        new_path.path = [new_path.path, path].join("/")
        new_path.to_s
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
