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

require_relative "text-renderer"

module Rabbit
  module Element
    module TextElement
      include TextRenderer

      attr_reader :text

      def initialize(text)
        super()
        @text = text
      end

      def text=(new_text)
        old_text = @text
        @text = new_text
        dirty! if old_text != @text
        @text
      end

      def substitute_text
        result = yield(@text.dup)
        return false if result == @text
        case result
        when Array
          new_elements = result.collect do |element|
            if element.is_a?(Base)
              element
            else
              new_element = clone
              new_element.text = element
              new_element
            end
          end
          parent.replace_element(self, *new_elements)
        else
          self.text = result
        end
        true
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          draw_layout(canvas, x, y)
        end
        [x + width, y, w - width, h]
      end

      def to_html(generator)
        text_to_html(generator)
      end

      def empty?
        @text.nil? or /\A\s*\z/ =~ @text
      end

      def inspect(verbose=false)
        if verbose
          super()
        else
          "<#{self.class.name} #{@text.inspect}>"
        end
      end
    end
  end
end
