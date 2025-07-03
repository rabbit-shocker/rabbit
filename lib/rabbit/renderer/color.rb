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

require_relative "../gtk"

require_relative "../rabbit"

module Rabbit
  module Renderer
    class Color < Struct.new(:red, :green, :blue, :alpha)
      class << self
        def parse(*args)
          new(*args)
        end
      end

      def initialize(*args)
        case args.collect(&:class)
        when [Gdk::RGBA]
          rgba = args[0]
          super(*rgba.to_a)
        when [String]
          rgba = parse(args[0])
          super(*rgba)
        else
          super(*args)
        end
      end

      def to_s
        values = to_a.collect {|x| (x * 65535).ceil}
        "#%04X%04X%04X%04X" % values
      end

      def to_a
        [red, green, blue, alpha]
      end

      def to_gdk_rgba
        Gdk::RGBA.new(*to_a)
      end

      def to_gdk_format
        to_s
      end

      def to_css_rgba
        red_percent = (red * 100).ceil
        green_percent = (green * 100).ceil
        blue_percent = (blue * 100).ceil
        a = alpha || 1.0
        "rgba(#{red_percent}%, #{green_percent}%, #{blue_percent}%, #{a})"
      end

      private
      HEX = "(?i:[a-z0-9])"
      def parse(text)
        case text
        when /\A\#(#{HEX})(#{HEX})(#{HEX})(#{HEX})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 1 - 1)
        when /\A\#(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})(#{HEX}{2})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 2 - 1)
        when /\A\#(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})(#{HEX}{4})?\z/
          normalize_rgba($1, $2, $3, $4, 16 ** 4 - 1)
        else
          Gdk::RGBA.parse(text).to_a
        end
      end

      def normalize_rgba(r, g, b, a, max)
        red = r.hex / max.to_f
        green = g.hex / max.to_f
        blue = b.hex / max.to_f
        if a
          alpha = a.hex / max.to_f
        else
          alpha = 1.0
        end
        [red, green, blue, alpha]
      end
    end
  end
end
