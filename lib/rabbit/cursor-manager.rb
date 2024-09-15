# Copyright (C) 2006-2024  Sutou Kouhei <kou@cozmixng.org>
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
  class CursorManager
    @@cursors = nil

    class << self
      def cursors
        @@cursors ||= {
          :blank  => Gdk::Cursor.new(:blank_cursor),
          :pencil => Gdk::Cursor.new(:pencil),
          :hand   => Gdk::Cursor.new(:hand1),
        }
      end
    end

    attr_accessor :current
    def initialize
      @stocks = {}
      @current = nil
    end

    def keep(name)
      @stocks[name] ||= []
      @stocks[name].push(@current)
    end

    def restore(surface, name)
      if name.nil?
        type = @current
      else
        type = @stocks[name].pop
      end
      surface.cursor = type_to_cursor(type)
    end

    def update(surface, type)
      surface.cursor = type_to_cursor(type)
    end

    private
    def type_to_cursor(type)
      if type.nil?
        nil
      else
        cursor = self.class.cursors[type]
        if cursor.nil?
          raise UnknownCursorTypeError.new(type)
        end
        cursor
      end
    end
  end
end

