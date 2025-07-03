# Copyright (C) 2008-2025  Sutou Kouhei <kou@cozmixng.org>
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
require_relative "text-container-element"

module Rabbit
  module Element
    class WaitTag
      include Base

      def have_wait_tag?
        true
      end

      def text
        ""
      end

      def markuped_text
        ""
      end

      def to_html(generator)
        ""
      end

      def draw_element(canvas, x, y, w, h, simulation)
        [x, y, w, h]
      end
    end

    class CustomTag
      include TextContainerElement

      attr_reader :name
      def initialize(name, elements=[])
        super(elements)
        @name = name
      end

      def have_tag?(name)
        @name == name
      end
    end
  end
end
