# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "base"

module Rabbit
  module Source
    class Memory
      include Base

      def self.initial_args_description
        N_("[FILENAME_OR_NOT]")
      end

      attr_accessor :extension

      def initialize(encoding, name=nil)
        super(encoding)
        if name
          file_source = File.new(encoding, name)
          @original_source = file_source.read
          set_base(file_source.base)
          @extension = extract_extension(name)
        else
          @original_source = ""
          @extension = nil
        end
        reset
      end

      def source=(new_source)
        @current_source = new_source
      end

      def _read
        @current_source
      end

      def need_read?
        super or @current_source != @source
      end

      def reset
        @current_source = @original_source.dup
      end
    end
  end
end
