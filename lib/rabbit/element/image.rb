# Copyright (C) 2004-2020  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/image"
require "rabbit/element/block-element"

module Rabbit
  module Element
    class Image
      include Base
      include BlockElement
      include BlockHorizontalCentering

      alias element_draw draw
      include ImageManipulable
      alias image_draw draw
      alias draw element_draw

      def initialize(filename, props)
        super(filename, props)
        setup_draw_parameters
        resize(properties.get_size("width", @filename),
               properties.get_size("height", @filename))
      end

      def draw_element(canvas, x, y, w, h, simulation)
        draw_image(canvas, x, y, w, h, simulation)
      end

      def caption
        self["caption"]
      end

      def text
        caption.to_s
      end

      def to_rd
        text
      end

      def to_html(generator)
        src = generator.save_pixbuf(pixbuf, File.basename(@filename))
        html = "<img "
        if @caption
          alt = generator.h(@caption)
          html << "title=\"#{alt}\" alt=\"#{alt}\" "
        end
        html << "src=\"#{src}\" />"
        html
      end

      def relative_width
        properties.get_size("relative_width", @filename)
      end

      def relative_height
        properties.get_size("relative_height", @filename)
      end

      def relative_margin_top
        properties.get_size("relative_margin_top", @filename)
      end

      def relative_margin_bottom
        properties.get_size("relative_margin_bottom", @filename)
      end

      def relative_margin_left
        properties.get_size("relative_margin_left", @filename)
      end

      def relative_margin_right
        properties.get_size("relative_margin_right", @filename)
      end

      def relative_padding_top
        properties.get_size("relative_padding_top", @filename)
      end

      def relative_padding_bottom
        properties.get_size("relative_padding_bottom", @filename)
      end

      def relative_padding_left
        properties.get_size("relative_padding_left", @filename)
      end

      def relative_padding_right
        properties.get_size("relative_padding_right", @filename)
      end

      alias _compile compile
      def compile_for_horizontal_centering(canvas, x, y, w, h)
        _compile(canvas, x, y, w, h)
      end

      def compile(canvas, x, y, w, h)
        super
        adjust_size(canvas, @x, @y, @w, @h)
      end

      def width
        super + @padding_left + @padding_right
      end

      def height
        super + @padding_top + @padding_bottom
      end

      def as_large_as_possible?
        properties.get_boolean("as_large_as_possible")
      end

      private
      def setup_draw_parameters
        @draw_parameters = {}

        @draw_parameters[:reflect] = {} if properties.get_boolean("reflect")
        [:ratio, :alpha].each do |key|
          name = "reflect_#{key}"
          value = self[name]
          next unless value
          @draw_parameters[:reflect] ||= {}
          @draw_parameters[:reflect][key] = Float(value)
        end

        alpha = self["alpha"]
        @draw_parameters[:alpha] = Float(alpha) if alpha
      end

      def draw_image(canvas, x, y, w, h, simulation)
        unless simulation
          image_draw(canvas, x, y, @draw_parameters)
        end
        [x, y + height, w, h - height]
      end

      def adjust_margin(w, h)
        @margin_top =
          make_relative_size(relative_margin_top, h) || @margin_top
        @margin_bottom =
          make_relative_size(relative_margin_bottom, h) || @margin_bottom
        @margin_left =
          make_relative_size(relative_margin_left, w) || @margin_left
        @margin_right =
          make_relative_size(relative_margin_right, w) || @margin_right
      end

      def adjust_padding(w, h)
        @padding_top =
          make_relative_size(relative_padding_top, h) || @padding_top
        @padding_bottom =
          make_relative_size(relative_padding_bottom, h) || @padding_bottom
        @padding_left =
          make_relative_size(relative_padding_left, w) || @padding_left
        @padding_right =
          make_relative_size(relative_padding_right, w) || @padding_right
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = @oh || h
        adjust_margin(base_w, base_h)
        adjust_padding(base_w, base_h)
        base_h = base_h - @padding_top - @padding_bottom
        if as_large_as_possible?
          iw = base_w - x
          ih = base_h - y
          if iw.to_f / original_width > ih.to_f / original_height
            iw = nil
          else
            ih = nil
          end
        else
          iw = make_relative_size(relative_width, base_w)
          ih = make_relative_size(relative_height, base_h)
        end
        resize(iw, ih)
      end

      def make_relative_size(size, parent_size)
        size && parent_size && ((size / 100.0) * parent_size).ceil
      end
    end
  end
end
