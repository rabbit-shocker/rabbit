require "uri"
require "open-uri"

module Rabbit

  module SourceBase
    
    attr_reader :encoding, :base, :tmp_base
    attr_accessor :force_modified

    def initialize(encoding)
      @encoding = encoding
      @source = nil
      @force_modified = false
      init_base
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

    def full_path(path)
      path
    end

    def open_full_path(path, mode="rb")
      open(full_path(path), mode) do |f|
        yield f
      end
    end

    def old?(current, get_latest_method_name)
      current.nil? or
        (current and __send__(get_latest_method_name) > current)
    end
  
    def base=(new_value)
      if new_value.nil?
        init_base
      else
        set_base(new_value)
      end
    end

    private
    def init_base
      set_base(".")
    end

    def set_base(new_value)
      @base = new_value
      if URI.parse(@base).scheme.nil?
        @tmp_base = @base
      else
        @tmp_base = "."
      end
    end
    
  end

  module Source
    
    def self.types
      constants.collect {|x| const_get(x)}
    end

    class ARGF

      include SourceBase

      def self.initial_args_description
        "none (get from STDIN) or FILE_NAMES"
      end

      def initialize(encoding, argf)
        super(encoding)
        @argf = argf
      end

      def full_path(path)
        ::File.join(@base, path)
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
        @name = name
        super(encoding)
      end

      def _read
        ::File.open(@name) do |f|
          @mtime = f.mtime
          f.read
        end
      end

      def need_read?
        super or old?(@mtime, :mtime)
      end

      def full_path(path)
        ::File.join(@base, path)
      end

      private
      def mtime
        ::File.mtime(@name)
      end

      def init_base
        set_base(::File.dirname(@name))
      end
      
    end

    class RWiki

      include SourceBase

      def self.initial_args_description
        "RWIKI_SOAP_IF_URI and PAGE_NAME"
      end

      def initialize(encoding, soap_if_uri, name)
        @soap_if_uri = soap_if_uri
        @name = name
        @uri = URI.parse("#{@soap_if_uri}?cmd=view;name=#{@name}")
        super(encoding)
      end

      private
      def _read
        @uri.open do |f|
          f.read
        end
      end

      def init_base
        set_base(::File.dirname(@uri.to_s))
      end
    end

    class Hiki

      include SourceBase

      def self.initial_args_description
        "HIKI_CGI_URI and PAGE_NAME"
      end

      def initialize(encoding, cgi_uri, name)
        @cgi_uri = cgi_uri
        @name = name
        @uri = URI.parse("#{@cgi_uri}?c=s;p=#{@name}")
        super(encoding)
      end

      private
      def _read
        @uri.open do |f|
          f.read
        end
      end

      def init_base
        set_base(::File.dirname(@uri.to_s))
      end
    end

    class URI

      include SourceBase

      MINIMUM_ACCESS_TIME = 60
      
      def self.initial_args_description
        "URI"
      end

      def initialize(encoding, uri)
        @uri = ::URI.parse(uri)
        @last_modified = nil
        @last_access_time = Time.now
        super(encoding)
      end

      def full_path(path)
        new_path = ::URI.parse(@base)
        new_path.path = [new_path.path, path].join("/")
        new_path.to_s
      end

      def need_read?
        super or
          (can_access? and old?(@last_modified, :last_modified))
      end

      private
      def _read
        @last_access_time = Time.now
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

      def can_access?
        Time.now - @last_access_time > MINIMUM_ACCESS_TIME
      end
      
    end
  end
end
