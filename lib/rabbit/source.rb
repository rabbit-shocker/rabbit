require "uri"
require "open-uri"

module Rabbit

  module SourceBase
    
    attr_reader :encoding, :base
    attr_accessor :force_modified

    def initialize(encoding)
      @encoding = encoding
      @source = nil
      @base = "."
      @force_modified = false
    end

    def read
      if need_read?
         @source = _read
        unless /\Autf-?8\z/i =~ @encoding
          require "iconv"
          @source = Iconv.iconv("UTF-8", @encoding, @source)
        end
      end
      @source
    end

    def modified?
      @force_modified or need_read?
    end

    def need_read?
      @source.nil?
    end

  end

  module Source
    
    def self.types
      constants.collect {|x| const_get(x)}
    end

    class ARGF

      include SourceBase

      def self.initial_args_description
        "ARGF"
      end

      def initialize(encoding, argf)
        super(encoding)
        @argf = argf
      end

      private
      def _read
        @argf.read
      end
    end

    class File

      include SourceBase

      def self.initial_args_description
        "FILENAME"
      end

      def initialize(encoding, name)
        super(encoding)
        @name = name
        @base = ::File.dirname(@name)
      end

      def _read
        ::File.open(@name) do |f|
          @mtime = f.mtime
          f.read
        end
      end

      def need_read?
        super or @mtime.nil? or (@mtime and mtime > @mtime)
      end

      private
      def mtime
        ::File.mtime(@name)
      end

    end

    class RWiki

      include SourceBase

      def self.initial_args_description
        "RWIKI_SOAP_IF_URI and PAGE_NAME"
      end

      def initialize(encoding, soap_if_uri, name)
        super(encoding)
        @soap_if_uri = soap_if_uri
        @name = name
        @uri = URI.parse("#{@soap_if_uri}?cmd=view;name=#{@name}")
        @base = ::File.dirname(@uri.to_s)
      end

      def _read
        @uri.open do |f|
          f.read
        end
      end
    end

    class Hiki

      include SourceBase

      def self.initial_args_description
        "HIKI_CGI_URI and PAGE_NAME"
      end

      def initialize(encoding, cgi_uri, name)
        super(encoding)
        @cgi_uri = cgi_uri
        @name = name
        @uri = URI.parse("#{@cgi_uri}?c=s;p=#{@name}")
        @base = ::File.dirname(@uri.to_s)
      end

      private
      def _read
        @uri.open do |f|
          f.read
        end
      end
    end

    class URI

      include SourceBase

      def self.initial_args_description
        "URI"
      end

      def initialize(encoding, uri)
        super(encoding)
        @uri = URI.parse(uri)
        @base = ::File.dirname(@uri.to_s)
      end

      private
      def _read
        @uri.open do |f|
          f.read
        end
      end

    end
  end
end
