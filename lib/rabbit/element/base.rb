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

require_relative "../formatter"
require_relative "../renderer/scene-node-widget"
require_relative "../utils"
require_relative "base/draw-hook"

module Rabbit
  module Element
    module Base
      include GetText

      include Utils

      include DrawHook
      def_draw_hooks :pre, :post, :around

      attr_reader :x, :y, :w, :h
      attr_reader :px, :py, :pw, :ph

      attr_reader :base_x, :base_y, :base_w, :base_h
      attr_reader :horizontal_centering, :vertical_centering

      attr_reader :user_property

      attr_accessor :margin_left, :margin_right
      attr_accessor :margin_top, :margin_bottom

      attr_accessor :padding_left, :padding_right
      attr_accessor :padding_top, :padding_bottom

      attr_accessor :default_margin_left, :default_margin_right
      attr_accessor :default_margin_top, :default_margin_bottom

      attr_accessor :default_padding_left, :default_padding_right
      attr_accessor :default_padding_top, :default_padding_bottom

      attr_accessor :default_visible

      attr_reader :parent, :real_simulation

      def initialize
        @x = @y = @w = @h = nil
        @parent = nil
        @user_property = {}
        @default_prop = {}
        init_default_padding
        init_default_margin
        init_default_visible
        clear_theme
      end

      def parent=(parent)
        @slide = nil
        @parent = parent
      end

      def visible?
        @visible
      end

      def scene_visible?
        if @parent
          return false unless @parent.scene_visible?
        end
        return true if @visible
        return true if slide&.visible_waited_target?(self)
        false
      end

      def slide
        @slide ||= _slide
      end

      def draw(simulation=false)
        x, y, w, h = setup_padding(@x, @y, @w, @h)
        x, y, w, h = _draw(@canvas, x, y, w, h, simulation)
        x, w = restore_x_padding(x, w)
        x, w = restore_x_margin(x, w)
        x, w = restore_x_centering(x, w)
        y, h = adjust_y_padding(y, h)
        y, h = adjust_y_margin(y, h)
        [x, y, w, h]
      end

      def dirty!
        @dirty = true
      end

      def dirty?
        @dirty
      end

      def wait(*args, &block)
        slide.register_wait_proc(self, *args, &block) if slide
      end

      def have_tag?(name)
        false
      end

      def have_wait_tag?
        false
      end

      def compile(canvas, x, y, w, h)
        compile_element(canvas, x, y, w, h)
      end

      def compile_element(canvas, x, y, w, h)
        @base_x, @base_y, @base_w, @base_h = x, y, w, h
        @px, @py, @pw, @ph = @x, @y, @w, @h
        x, y, w, h = setup_margin(x, y, w, h)
        @canvas, @x, @y, @w, @h = canvas, x, y, w, h
        if [@px, @py, @pw, @ph] != [@x, @y, @w, @h]
          dirty!
        end
      end

      def compile_for_horizontal_centering(canvas, x, y, w, h)
        compile(canvas, x, y, w, h)
      end

      def compile_horizontal(canvas, x, y, w, h)
        if do_horizontal_centering?
          do_horizontal_centering(canvas, x, y, w, h)
        else
          reset_horizontal_centering(canvas, x, y, w, h)
        end
      end

      def setup_scene(canvas, fixed, x, y, w, h)
        compile(canvas, x, y, w, h)

        x, y, w, h = setup_margin(x, y, w, h)
        x, w = setup_x_centering(x, w)
        x, y, w, h = setup_padding(x, y, w, h)

        @pre_draw_procs.each do |proc, _name|
          x, y, w, h = proc.call(canvas, x, y, w, h, true)
        end

        x, y, w, h = setup_scene_element(canvas, fixed, x, y, w, h)

        @post_draw_procs.each do |proc, _name|
          x, y, w, h = proc.call(canvas, x, y, w, h, true)
        end

        x, w = restore_x_padding(x, w)
        x, w = restore_x_margin(x, w)
        x, w = restore_x_centering(x, w)
        y, h = adjust_y_padding(y, h)
        y, h = adjust_y_margin(y, h)

        [x, y, w, h]
      end

      def setup_scene_element(canvas, scene_widget, x, y, w, h)
        widget = Renderer::SceneNodeWidget.new(canvas, self, x, y, w, h)
        scene_widget.put(widget, x, y, w, h)
        [x, y, w, h]
      end

      def scene_snapshot(widget, snapshot, canvas, width, height)
        return unless scene_visible?

        x, y, w, h = widget.x, widget.y, width, height
        # Dirty... We need to reconsider when we compute horizontal
        # centering after we remove DrawingArea based rendering
        # engine.
        original_x = @x
        @x = widget.x - @padding_left
        # pre_draw_proc may be deleted while calling. For example, the
        # image-timer theme's init proc is deleted when it's called.
        @pre_draw_procs.dup.each do |proc, _name|
          x, y, w, h = proc.call(canvas, x, y, w, h, false)
        end
        # DrawingArea based renderer set padding after pre_draw_procs.
        x, y, w, h = setup_padding(x, y, w, h)
        x, y, w, h = scene_snapshot_element(widget, snapshot, canvas, x, y, w, h)
        # post_draw_proc may be deleted while calling.
        @post_draw_procs.dup.each do |proc, _name|
          x, y, w, h = proc.call(canvas, x, y, w, h, false)
        end
        @x = original_x
      end

      def prop_set(name, *values)
        name = normalize_property_name(name)
        @prop[name] = make_prop_value(name, *values)
        dirty!
      end
      alias __prop_set__ prop_set

      def prop_get(name)
        name = normalize_property_name(name)
        @prop[name]
      end
      alias __prop_get__ prop_get

      def prop_value(name)
        name = normalize_property_name(name)
        value = @prop[name]
        value = value.value if value.respond_to?(:value)
        value
      end

      def prop_delete(name)
        name = normalize_property_name(name)
        @prop.delete(name)
        dirty!
      end
      alias __prop_delete__ prop_delete

      def add_default_prop(name, value)
        name = normalize_property_name(name)
        @default_prop[name] = make_prop_value(name, value)
      end

      def font(props)
        props.each do |key, value|
          key, value = normalize_font_property(key, value)
          if value
            prop_set(key, value)
          else
            prop_delete(key)
          end
        end
      end

      def inline_element?
        true
      end

      def text_renderer?
        false
      end

      def substitute_text
        false
      end

      def substitute_newline
        substitute_text do |text|
          text.gsub(/(\\)?\\n/) {$1 ? "\\n" : "\n"}
        end
      end

      def clear_theme
        @slide = nil
        @visible = @default_visible
        @real_simulation = true
        @width = @height = nil
        @centering_adjusted_width = nil
        @centering_adjusted_height = nil
        @horizontal_centering = @vertical_centering = false
        @prop = default_prop
        clear_margin
        clear_padding
        clear_draw_procs
        dirty!
      end

      def setup_padding(x, y, w, h)
        x += @padding_left
        y += @padding_top
        w -= @padding_left + @padding_right
        h -= @padding_top + @padding_bottom
        [x, y, w, h]
      end

      def restore_x_padding(x, w)
        x -= @padding_left
        w += @padding_left + @padding_right
        [x, w]
      end

      def restore_x_margin(x, w)
        x -= @margin_left
        w += @margin_left + @margin_right
        [x, w]
      end

      def setup_x_centering(x, w)
        x += centering_adjusted_width
        w -= centering_adjusted_width
        [x, w]
      end

      def restore_x_centering(x, w)
        x -= centering_adjusted_width
        w += centering_adjusted_width
        [x, w]
      end

      def adjust_y_padding(y, h)
        y -= @padding_top
        h += @padding_top + @padding_bottom
        [y, h]
      end

      def setup_margin(x, y, w, h)
        x += @margin_left
        y += @margin_top
        w -= @margin_left + @margin_right
        h -= @margin_top + @margin_bottom
        [x, y, w, h]
      end

      def adjust_y_margin(y, h)
        y += @margin_bottom
        h -= @margin_bottom
        [y, h]
      end

      def init_default_padding
        @default_padding_left = 0
        @default_padding_right = 0
        @default_padding_top = 0
        @default_padding_bottom = 0
      end

      def init_default_margin
        @default_margin_left = 0
        @default_margin_right = 0
        @default_margin_top = 0
        @default_margin_bottom = 0
      end

      def init_default_visible
        @default_visible = !have_wait_tag?
      end

      def clear_padding
        @padding_left = @default_padding_left
        @padding_right = @default_padding_right
        @padding_top = @default_padding_top
        @padding_bottom = @default_padding_bottom
      end

      def clear_margin
        @margin_left = @default_margin_left
        @margin_right = @default_margin_right
        @margin_top = @default_margin_top
        @margin_bottom = @default_margin_bottom
      end

      def if_dirty
        if dirty?
          yield
          @dirty = false
        end
      end

      def do_horizontal_centering?
        @horizontal_centering or
          (parent and parent.do_horizontal_centering?)
      end

      def do_vertical_centering?
        @vertical_centering or
          (parent and parent.do_horizontal_centering?)
      end

      def horizontal_centering=(new_value)
        if @horizontal_centering != new_value
          dirty!
        end
        @horizontal_centering = new_value
      end

      def vertical_centering=(new_value)
        dirty! if @vertical_centering != new_value
        @vertical_centering = new_value
      end

      def do_horizontal_centering(canvas, x, y, w, h)
      end

      def reset_horizontal_centering(canvas, x, y, w, h)
      end

      def previous_element
        sibling_element(-1)
      end

      def next_element
        sibling_element(1)
      end

      def available_w
        @w - @padding_left - @padding_right
      end

      def width
        @width + @padding_left + @padding_right
      end

      def height
        @height + @padding_top + @padding_bottom
      end

      def centering_adjusted_width
        @centering_adjusted_width || 0
      end

      def centering_adjusted_height
        @centering_adjusted_height || 0
      end

      def inspect(verbose=false)
        if verbose
          super()
        else
          "<#{self.class.name}>"
        end
      end

      def clone
        obj = super
        obj.user_property = @user_property.clone
        obj.prop = @prop.clone
        obj
      end

      def default_prop
        @default_prop.dup
      end

      def match?(pattern)
        pattern === text
      end

      def margin_set(*values)
        top, right, bottom, left = parse_four_way(*values)
        @margin_top = top if top
        @margin_right = right if right
        @margin_bottom = bottom if bottom
        @margin_left = left if left
      end

      def margin_with(params)
        margin_set(params)
      end

      def padding_set(*values)
        top, right, bottom, left = parse_four_way(*values)
        @padding_top = top if top
        @padding_right = right if right
        @padding_bottom = bottom if bottom
        @padding_left = left if left
      end

      def padding_with(params)
        padding_set(params)
      end

      def show(&block)
        change_visible(true, &block)
      end

      def hide(&block)
        change_visible(false, &block)
      end

      def [](name)
        @user_property[name]
      end

      def []=(name, value)
        @user_property[name] = value
      end

      protected
      def user_property=(prop)
        @user_property = prop
      end

      def prop=(prop)
        @prop = prop
      end

      private
      def _slide
        if @parent
          @parent.slide
        else
          nil
        end
      end

      def change_visible(value)
        visible = @visible
        @visible = value
        if block_given?
          begin
            yield
          ensure
            @visible = visible
          end
        end
      end

      def make_prop_value(name, *values)
        formatter_name = to_class_name(name)
        unless Format.const_defined?(formatter_name)
          raise UnknownPropertyError.new(name)
        end
        Format.const_get(formatter_name).new(*values)
      end

      def sibling_element(relative_index)
        if @parent
          ind = @parent.elements.index(self)
          if ind
            @parent.elements[ind + relative_index]
          else
            nil
          end
        else
          nil
        end
      end

      def normalize_property_name(name)
        name = name.to_s if name.is_a?(Symbol)
        name.gsub(/_/, "-")
      end

      def normalize_font_property(key, value)
        key = key.to_s
        case key
        when /\A(family)\z/
          ["font_#{$1}", value]
        when /\A(desc)(?:ription)?\z/
          ["font_#{$1}", value]
        when /\A(?:foreground|color|fg_color|fg)\z/
          ["foreground", value]
        when /\A(?:background|bg_color|bg)\z/
          ["background", value]
        when /\A(?:underline|ul)\z/
          ["underline", value]
        when /\A(?:underline|ul)_color\z/
          ["underline_color", value]
        when /\A(rise|superscript|subscript)\z/
          value = -value if $1 == "subscript"
          ["rise", value]
        when /\A(?:strike[_]?through)\z/
          value = value ? "true" : "false" unless value.is_a?(String)
          ["strikethrough", value]
        when /\A(?:strike[_]?through_color)\z/
          ["strikethrough_color", value]
        when /\A(?:fallback)\z/
          value = value ? "true" : "false" unless value.is_a?(String)
          ["fallback", value]
        else
          [key, value]
        end
      end

      def _draw(canvas, x, y, w, h, simulation)
        around_draw_procs = @around_draw_procs.dup
        around_draw_procs.concat(slide.waited_draw_procs(self)) if slide
        _draw_rec(canvas, x, y, w, h, simulation, around_draw_procs)
      end

      def _draw_rec(canvas, x, y, w, h, simulation, around_draw_procs)
        if around_draw_procs.empty?
          (@pre_draw_procs +
           [method(:draw_element)] +
           @post_draw_procs.reverse).each do |pro,|
            @real_simulation = simulation
            _simulation = simulation
            _simulation = true unless visible?
            x, y, w, h = pro.call(canvas, x, y, w, h, _simulation)
          end
          [x, y, w, h]
        else
          @real_simulation = simulation
          _simulation = simulation
          _simulation = true unless visible?
          pro, = around_draw_procs.pop
          next_proc = Proc.new do |*args|
            args << around_draw_procs
            _draw_rec(*args)
          end
          pro.call(canvas, x, y, w, h, _simulation, next_proc)
        end
      end

      def _indent(str, width="  ")
        result = ""
        str.each_line do |line|
          result << width + line
        end
        result
      end
    end
  end
end
