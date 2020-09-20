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

    def get_boolean(key)
      true_value?(self[key])
    end

    def respond_to_missing?(name, include_private)
      @data.key?(normalize_key(name.to_s.gsub(/\?\z/, "")))
    end

    def method_missing(name, *args, &block)
      case args.size
      when 0
        if name.end_with?("?")
          name = name[0..-2]
          is_predict = true
        else
          is_predict = false
        end
        key = normalize_key(name)
        return super unless @data.key?(key)
        value = @data[key]
        if is_predict
          true_value?(value)
        else
          value
        end
      when 1
        return super unless name.end_with?("?")
        @data[normalize_key(name)] = args[0]
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
  end
end
