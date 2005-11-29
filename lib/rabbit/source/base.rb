require 'fileutils'

require 'rabbit/rabbit'

module Rabbit
  module Source

    module Base

      def self.append_features(klass)
        super
        klass.extend(GetText)
      end
      
      attr_reader :base, :tmp_base
      attr_accessor :encoding, :force_modified

      def initialize(encoding, logger)
        @encoding = encoding
        @logger = logger
        @source = nil
        @force_modified = false
        init_base
      end

      def source=(new_source)
        source_type = self.class.name.split("::").last.downcase
        raise ImmutableSourceTypeError.new(source_type)
      end

      def reset
      end
      
      def read
        if need_read?
          @source = _read
          if @encoding.nil?
            enc = guess_encoding(@source)
          else
            enc = @encoding
          end
 
          unless /\Autf-?8\z/i =~ enc
            require "iconv"
            @source = Iconv.conv("UTF-8", enc, @source)
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
        if @base_uri.nil? or @base_uri.relative?
          ::File.join(@base, path)
        else
          uri = @base_uri.dup
          uri.path = @base_uri.path + "/" unless /\/$/ =~ @base_uri.path
          (uri + path).to_s
        end
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
      
      def tmp_dir_name
        dir = ::File.join(@tmp_base, TMP_DIR_NAME)  
        FileUtils.mkdir_p(dir) unless ::File.exist?(dir)
        dir
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
        @base_uri = parse_uri(@base)
        if @base_uri.nil? or @base_uri.scheme.nil?
          @tmp_base = @base
        else
          @tmp_base = "."
        end
      end

      def parse_uri(str)
        begin
          ::URI.parse(str)
        rescue ::URI::InvalidURIError
          nil
        end
      end

      def guess_encoding(str)
        require 'nkf'
        case NKF.guess(str)
        when NKF::JIS
          "ISO-2022-JP"
        when NKF::EUC
          "eucJP"
        when NKF::SJIS
          "CP932"
        when NKF::UTF16
          "UTF-16"
        when NKF::UTF32
          "UTF-32"
        else
          "UTF-8"
        end
      end
    end

    module LimitAccessInterval
      MINIMUM_ACCESS_TIME = 60

      def initialize(*args, &block)
        update_last_access_time
        super
      end

      def old?(current, get_latest_method_name)
        result = (can_access? and super)
        update_last_access_time if result
        result
      end
      
      private
      def update_last_access_time
        @last_access_time = Time.now
      end

      def can_access?
        Time.now - @last_access_time > MINIMUM_ACCESS_TIME
      end
    end
    
  end
end
