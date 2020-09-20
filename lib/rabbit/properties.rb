# Copyright (C) 2020  Sutou Kouhei <kou@cozmixng.org>
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

module Rabbit
  class Properties
    include Enumerable

    def initialize(data)
      @data = normalize(data)
    end

    def [](key)
      @data[normalize_key(key)]
    end

    def []=(key, value)
      @data[normalize_key(key)] = value
    end

    def each(&block)
      @data.each(&block)
    end

    def get_boolean(key, default=nil)
      value = self[key]
      return default if value.nil?
      true_value?(value)
    end

    def get_integer(key, default=nil)
      integer_value(self[key] || default)
    end

    def get_float(key, default=nil)
      float_value(self[key] || default)
    end

    def get_size(key, filename, default=nil)
      size_value(self[key] || default, filename, key)
    end

    end

    def respond_to_missing?(name, include_private)
      @data.key?(normalize_key(name.to_s.gsub(/\?\z/, "")))
    end

    def method_missing(name, *args, &block)
      case args.size
      when 0
        key = name.to_s
        if key.end_with?("?")
          key = key[0..-2]
          is_predict = true
        else
          is_predict = false
        end
        key = normalize_key(key)
        return super unless @data.key?(key)
        value = @data[key]
        if is_predict
          true_value?(value)
        else
          value
        end
      when 1
        key = name.to_s
        return super unless key.end_with?("?")
        @data[normalize_key(key)] = args[0]
      else
        super
      end
    end

    private
    def normalize(data)
      normalized_data = {}
      (data || {}).each do |key, value|
        normalized_data[normalize_key(key)] = value
      end
      normalized_data
    end

    def normalize_key(key)
      key.to_s.gsub(/-/, "_")
    end

    def true_value?(value)
      value == true or value == "true"
    end

    def integer_value(value)
      return nil if value.nil?
      Integer(value, 10)
    end

    def float_value(value)
      return nil if value.nil?
      Float(value)
    end

    def size_value(value, filename, name)
      return nil if value.nil?
      begin
        Integer(value, 10)
      rescue ArgumentError
        raise InvalidSizeError.new(filename, name, value)
      end
    end
  end
end
