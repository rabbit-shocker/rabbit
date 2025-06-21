# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require 'fileutils'
require 'tmpdir'

require 'rabbit/rabbit'

module Rabbit
  module Source
    module Base
      def self.append_features(klass)
        super
        klass.send(:include, GetText)
      end

      attr_reader :base, :tmp_base
      attr_accessor :encoding, :force_modified

      def initialize(encoding)
        @encoding = encoding
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
          case @encoding
          when nil
            enc = guess_encoding(@source) || Encoding::ASCII_8BIT
          when Encoding
            enc = @encoding
          else
            enc = Encoding.find(@encoding)
          end

          case enc
          when Encoding::UTF_8, Encoding::ASCII_8BIT
            @source.force_encoding(enc)
          else
            @source = @source.encode(Encoding::UTF_8, enc)
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

      def extension
        nil
      end

      private
      def init_base
        set_base(".")
      end

      def set_base(new_value)
        if ::File::ALT_SEPARATOR
          new_value = new_value.gsub(::File::ALT_SEPARATOR, ::File::SEPARATOR)
        end
        @base = new_value
        @base_uri = parse_uri(@base)
        if @base_uri.nil? or @base_uri.scheme.nil?
          @tmp_base = @base
        else
          @tmp_base = "."
        end
        unless ::File.writable?(@tmp_base)
          @tmp_base = Dir.tmpdir
        end
      end

      def parse_uri(str)
        begin
          ::URI.parse(str)
        rescue ::URI::InvalidURIError
          nil
        end
      end

      def guess_encoding(string)
        string = string.dup
        candidates = [
          Encoding::UTF_8,
          Encoding::EUCJP_MS,
          Encoding::EUCJP,
          Encoding::WINDOWS_31J,
          Encoding::Shift_JIS,
          Encoding::CP51932,
          Encoding::CP50221,
          Encoding::ISO2022_JP,
          Encoding::UTF_16BE,
          Encoding::UTF_16LE,
        ]
        candidates.find do |candidate|
          next if candidate.dummy?
          string.force_encoding(candidate).valid_encoding?
        end
      end

      def extract_extension(path)
        components = ::File.basename(path).split(/\./)
        return nil if components.size < 2
        components.last
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
