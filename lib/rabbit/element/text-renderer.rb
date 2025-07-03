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

require_relative "base"

module Rabbit
  module Element
    module TextRenderer
      include Base

      attr_reader :layout
      attr_reader :original_width, :original_height
      attr_reader :first_line_width, :first_line_height

      attr_writer :wrap_mode, :indent

      def initialize(*args, &block)
        super
        text_clear_theme
      end

      def text_renderer?
        true
      end

      def align=(new_value)
        if new_value.is_a?(String)
          new_value = Pango::Alignment.const_get(new_value.to_s.upcase)
        end
        dirty! if @align != new_value
        @align = new_value
      end

      def justify=(new_value)
        new_value = true if new_value
        dirty! if @justify != new_value
        @justify = new_value
      end

      def spacing=(new_spacing)
        dirty! if @spacing != new_spacing
        @spacing = new_spacing
      end

      def width
        if @width
          @width + @padding_left + @padding_right
        else
          nil
        end
      end

      def height
        if @height
          @height + @padding_top + @padding_bottom
        else
          nil
        end
      end

      def compile(canvas, x, y, w, h)
        super
        text_compile(canvas, x, y, w, h)
      end

      def text_compile(canvas, x, y, w, h)
        if (@width and @width > @w) or (@height and @height > @h)
          dirty!
        end
        if_dirty do
          setup_draw_info(markuped_text, canvas, w)
        end
      end

      def text_to_html(generator)
        markup_as_html(text)
      end

      def do_horizontal_centering?
        @horizontal_centering
      end

      def do_horizontal_centering(canvas, x, y, w, h)
        self.align = Pango::Alignment::CENTER
      end

      def reset_horizontal_centering(canvas, x, y, w, h)
        self.align = default_align if @align == Pango::Alignment::CENTER
      end

      def markuped_text
        markup(text)
      end

      def clear_theme
        super
        text_clear_theme
      end

      def text_clear_theme
        @layout = nil
        @indent = 0
        @spacing = 0
        @wrap_mode = default_wrap_mode
        @align = default_align
        @justify = default_justify
      end

      def dirty!
        super
        @layout = nil
      end

      def dirty?
        super or text_dirty?
      end

      def text_dirty?
        @layout.nil?
      end

      def text_props
        props = {}
        @prop.each do |name, formatter|
          props[name] = formatter.value
        end
        props
      end

      def font_size
        text_props["size"]
      end

      def pixel_font_size
        _font_size = font_size
        if _font_size
          _font_size / Pango::SCALE
        else
          original_height
        end
      end

      def have_numerical_font_size?
        font_size.is_a?(Numeric)
      end

      def keep_in_size(proc_name=nil, &compute_max_size)
        proc_name ||= "keep-in-size"
        make_params = Proc.new do |canvas, x, y, w, h,
                                   initial_width, initial_height,
                                   max_width, max_height|
          if (max_width and initial_width > max_width) or
              (max_height and initial_height > max_height)
            scale = lambda do |_width, _height|
              candidates = [0.95]
              candidates << (max_width.to_f) / _width if max_width
              candidates << (max_height.to_f) / _height if max_height
              candidates.min
            end
            compare = Proc.new do |_width, _height|
              (max_width.nil? or _width < max_width) and
                (max_height.nil? or _height < max_height)
            end
            [scale, compare]
          end
        end
        dynamic_font_size_computation(proc_name, compute_max_size, &make_params)
      end

      def as_large_as_possible(proc_name=nil, &compute_max_size)
        proc_name ||= "as-large-as-possible"
        make_params = Proc.new do |canvas, x, y, w, h,
                                   initial_width, initial_height,
                                   max_width, max_height|
          if (max_width and initial_width > max_width) or
              (max_height and initial_height > max_height)
            scale = 0.95
            compare = Proc.new do |_width, _height|
              (max_width.nil? or _width < max_width) and
                (max_height.nil? or _height < max_height)
            end
          else
            scale = 1.05
            compare = Proc.new do |_width, _height|
              (max_width and _width > max_width) or
                (max_height and _height > max_height)
            end
          end
          [scale, compare]
        end
        dynamic_font_size_computation(proc_name, compute_max_size, &make_params)
      end

      private
      def setup_draw_info(str, canvas, w)
        layout = canvas.make_layout(str)
        @original_width, @original_height = layout.pixel_size
        @first_line_width = @original_width / layout.line_count
        @first_line_height = @original_height / layout.line_count
        setup_layout(layout, w)
        width, height = layout.pixel_size
        if layout.width != -1 and
            (layout.alignment == Pango::Alignment::CENTER or
             layout.alignment == Pango::Alignment::RIGHT)
          width = layout.width / Pango::SCALE
        end
        @width, @height = width, height
        @layout = layout
      end

      def setup_layout(layout, w)
        layout.set_wrap(@wrap_mode) if @wrap_mode
        layout.set_alignment(@align)
        if @wrap_mode or layout.alignment != Pango::Alignment::LEFT
          layout.set_width((w * Pango::SCALE).ceil)
        else
          layout.set_width(-1)
        end
        indent = @indent
        indent = indent.value if indent.respond_to?(:value)
        layout.set_indent(indent)
        layout.set_spacing(@spacing * Pango::SCALE)
        layout.justify = @justify
        layout.context_changed
      end

      def markup(str)
        t = str
        @prop.each do |name, formatter|
          if formatter.text_formatter?
            t = formatter.format(t)
          end
        end
        t
      end

      def markup_as_html(str)
        t = str
        @prop.each do |name, formatter|
          if formatter.html_formatter?
            t = formatter.html_format(t)
          end
        end
        t
      end

      def default_wrap_mode
        Pango::WrapMode::WORD_CHAR
      end

      def default_align
        Pango::Alignment::LEFT
      end

      def default_justify
        false
      end

      def draw_layout(canvas, x, y)
        shadow_color = prop_value("shadow-color")
        if shadow_color
          shadow_foreground = Format::Foreground.new(shadow_color)
          shadow_text = markup(shadow_foreground.format(text))
          shadow_layout = canvas.make_layout(shadow_text)
          setup_layout(shadow_layout, @layout.width / Pango::SCALE)
          line_height = shadow_layout.pixel_size[1] / shadow_layout.line_count
          shadow_x = prop_value("shadow-x") || (line_height * 0.03)
          shadow_y = prop_value("shadow-y") || (line_height * 0.02)
          canvas.draw_layout(shadow_layout, x + shadow_x, y + shadow_y,
                             shadow_color)
        end

        color = prop_get("foreground")
        color = color.value if color
        canvas.draw_layout(@layout, x, y, color)
      end

      def dynamic_font_size_computation(proc_name, compute_max_size,
                                        &make_params)
        return if text.empty?
        computed = false
        min_width = nil
        add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
          if simulation and !computed and have_numerical_font_size?
            max_width = max_height = nil
            if compute_max_size
              max_width, max_height = compute_max_size.call(self, canvas,
                                                            x, y, w, h)
            else
              max_width = (pw || w) - padding_left - padding_right
              max_height = (ph || h) - padding_top - padding_bottom
              max_width = nil if max_width <= 0
              max_height = nil if max_height <= 0
            end

            computed = true

            if max_width and max_height
              compute_font_size(canvas, x, y, w, h, max_width, max_height,
                                make_params)
            end
          end
          [x, y, w, h]
        end
      end

      def compute_font_size(canvas, x, y, w, h, max_width, max_height,
                            make_params)
        compile(canvas, x, y, w, h) if dirty?
        initial_width, initial_height = @layout.pixel_size
        scale, compare = make_params.call(canvas, x, y, w, h,
                                          initial_width, initial_height,
                                          max_width, max_height)

        return if scale.nil? or compare.nil?

        if scale.respond_to?(:call)
          compute_scale = scale
          scale = nil
        end

        size = new_size = initial_font_size_for_compute_font_size
        current_layout_size = @layout.pixel_size
        unless compare.call(*@layout.pixel_size)
          loop do
            scale = compute_scale.call(*@layout.pixel_size) if compute_scale
            new_size = compute_next_font_size(size, scale)
            break if new_size == size
            set_computed_font_size(new_size)
            compile(canvas, x, y, w, h)
            break if !@wrap_mode and @layout.wrapped?
            break if compare.call(*@layout.pixel_size)
            size = new_size
          end
        end
        if scale > 1
          set_computed_font_size(size)
        else
          set_computed_font_size(new_size)
        end
      end

      protected
      def initial_font_size_for_compute_font_size
        font_size
      end

      def compute_next_font_size(previous_size, scale)
        if previous_size
          (previous_size * scale).ceil
        else
          nil
        end
      end

      def set_computed_font_size(new_size)
        if new_size
          font :size => new_size
          dirty!
        end
        dirty?
      end
    end
  end
end
