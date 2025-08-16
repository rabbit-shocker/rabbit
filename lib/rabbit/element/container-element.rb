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

require "forwardable"

require_relative "block-element"

module Rabbit
  module Element
    module ContainerElement
      extend Forwardable

      include Enumerable

      include Base

      attr_reader :elements

      def_delegators(:@elements, :empty?, :each, :first, :last)
      def_delegators(:@elements, :size, :length)

      def initialize(elems=[])
        @elements = []
        elems = [elems] unless elems.is_a?(Array)
        elems.each do |elem|
          add_element(elem)
        end
        super()
      end

      def <<(element)
        @elements << element
        element.parent = self
        dirty!
      end
      alias add_element <<

      def unshift(element)
        @elements.unshift(element)
        element.parent = self
        dirty!
      end

      def delete(element)
        return if @elements.delete(element).nil?
        element.parent = nil
        dirty!
        element
      end

      def text
        @elements.collect do |element|
          element.text
        end.join("\n")
      end

      def to_rd
        @elements.collect do |element|
          element.to_rd
        end.join("\n")
      end

      def substitute_text(&block)
        substituted = false
        e = @elements.dup
        e.each_with_index do |element, i|
          element_substituted = element.substitute_text(&block)
          substituted ||= element_substituted
        end
        dirty! if substituted
        substituted
      end

      def replace_element(child, *targets)
        index = @elements.index(child)
        @elements[index, 1] = targets
      end

      def draw_element(canvas, x, y, w, h, simulation)
        draw_elements(canvas, x, y, w, h, simulation)
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        @centering_adjusted_height = 0

        args = [x, y, w, h]
        if do_vertical_centering?
          adjust_height = ((h - height - @padding_bottom) / 2.0).ceil
          if y + adjust_height > 0
            args = [x, y + adjust_height, w, h - adjust_height]
          else
            adjust_height = 0
          end
          @centering_adjusted_height = adjust_height
          compile_elements(canvas, *args)
        end

        base_x, base_w = x, w
        @elements.each do |element|
          x, y, w, h = element.draw(simulation)
        end
        last_element = @elements.last
        if last_element and last_element.inline_element?
          container_height = height
          x = base_x
          y += container_height
          w = base_w
          h -= container_height
        end
        [x, y, w, h]
      end

      def compile(canvas, x, y, w, h)
        super
        if_dirty do
          x, y, w, h = setup_padding(@x, @y, @w, @h)
          compile_elements(canvas, x, y, w, h)
          x, w = restore_x_padding(x, w)
          x, w = restore_x_centering(x, w)
          y, h = adjust_y_padding(y, h)
        end
      end

      def compile_elements(canvas, x, y, w, h)
        prev_is_inline = false
        @elements.each do |element|
          element.compile(canvas, x, y, w, h)
          compile_horizontal_element(element, canvas, x, y, w, h)
          x, y, w, h = element.draw(true)
        end
      end

      def compile_horizontal(canvas, x, y, w, h)
        @elements.each do |element|
          compile_horizontal_element(element, canvas, x, y, w, h)
        end
      end

      def compile_horizontal_element(element, canvas, x, y, w, h)
        if do_horizontal_centering? or element.do_horizontal_centering?
          element.do_horizontal_centering(canvas, x, y, w, h)
        else
          element.reset_horizontal_centering(canvas, x, y, w, h)
        end
      end

      def setup_scene_element(canvas, scene_widget, x, y, w, h)
        x, y, w, h = super

        @centering_adjusted_height = 0
        if do_vertical_centering?
          adjust_height = ((h - height - @padding_bottom) / 2.0).ceil
          if y + adjust_height > 0
            y += adjust_height
            h -= adjust_height
          else
            adjust_height = 0
          end
          @centering_adjusted_height = adjust_height
          compile_elements(canvas, x, y, w, h)
        end

        base_x, base_w = x, w
        @elements.each do |element|
          x, y, w, h = element.setup_scene(canvas, scene_widget, x, y, w, h)
        end
        # For table
        last_element = @elements.last
        if last_element and last_element.inline_element?
          container_height = height
          y += container_height
          h -= container_height
        end
        x = base_x
        w = base_w

        [x, y, w, h]
      end

      def scene_snapshot_element(widget, snapshot, canvas, x, y, w, h)
        # TODO: height may be wrong here. We need to check
        # post_draw_hook usage.
        [x, y + height, w, h - height]
      end

      def to_html(generator)
        collect do |element|
          element.to_html(generator)
        end.join("\n")
      end

      def prop_set(*args)
        each do |elem|
          elem.prop_set(*args)
        end
      end

      def prop_get(name)
        collect do |elem|
          elem.prop_get(name)
        end
      end

      def prop_delete(name)
        collect do |elem|
          elem.prop_delete(name)
        end
      end

      def width
        @elements.collect do |elem|
          w = elem.w
          if w
            w += elem.margin_left + elem.margin_right
          end
          w
        end.compact.max.to_i + @padding_left + @padding_right
      end

      # SLOW!!
      def _width
        block_widths = []
        block_widths << @elements.inject(0) do |result, elem|
          if elem.width
            if elem.inline_element?
              result + elem.width
            else
              block_widths << elem.width
              elem.width
            end
          else
            result
          end
        end
        block_widths.max.to_i
      end

      # perhaps SLOW!!
      def height
        inline_heights = []
        @elements.inject(0) do |result, elem|
          elem_height = elem.height
          if elem_height
            if elem.inline_element?
              inline_heights << elem_height
              result
            else
              prev_inlines_max_height = inline_heights.max.to_i
              inline_heights.clear
              h = elem_height + elem.margin_top + elem.margin_bottom
              result + h + prev_inlines_max_height
            end
          else
            result
          end
        end + inline_heights.max.to_i + @padding_top + @padding_bottom
      end

      def clear_theme
        @elements.each do |element|
          element.clear_theme
        end
        super
      end
      alias container_clear_theme clear_theme

      def dirty?
        super or @elements.any?{|x| x.dirty?}
      end

      def inspect(verbose=false)
        elem_info = @elements.collect do |x|
          _indent(x.inspect(verbose))
        end.join("\n")
        if verbose
          self_info = super(verbose)
        else
          self_info = "<#{self.class.name}>"
        end
        self_info + (elem_info.empty? ? "" : "\n") + elem_info
      end

      def have_tag?(name)
        @elements.any? {|element| element.have_tag?(name)}
      end

      def have_wait_tag?
        @elements.any? {|element| element.have_wait_tag?}
      end

      def [](name_or_index)
        if name_or_index.is_a?(Integer)
          @elements[name_or_index]
        else
          super
        end
      end

      def []=(name_or_index, value)
        if name_or_index.is_a?(Integer)
          @elements[name_or_index] = value
        else
          super
        end
      end
    end
  end
end
