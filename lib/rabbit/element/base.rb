require 'rabbit/utils'
require 'rabbit/formatter'

require 'rabbit/element/base/draw-hook'

module Rabbit
  module Element
    module Base
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

      attr_accessor :parent

      def initialize
        @x = @y = @w = @h = nil
        @parent = nil
        @user_property = {}
        @default_prop = {}
        init_default_padding
        init_default_margin
        clear_theme
      end

      def draw(simulation=false)
        x, y, w, h = setup_padding(@x, @y, @w, @h)
        x, y, w, h = _draw(@canvas, x, y, w, h, simulation)
        x, w = restore_x_padding(x, w)
        x, w = restore_x_margin(x, w)
        x, w = adjust_x_centering(x, w)
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

      def compile(canvas, x, y, w, h)
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
        end
      end

      def prop_set(name, *values)
        @prop[name] = make_prop_value(name, *values)
        dirty!
      end
      alias __prop_set__ prop_set

      def prop_get(name)
        @prop[name]
      end
      alias __prop_get__ prop_get

      def prop_delete(name)
        @prop.delete(name)
        dirty!
      end
      alias __prop_delete__ prop_delete

      def add_default_prop(name, value)
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

      def clear_theme
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

      def adjust_x_centering(x, w)
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
        dirty! if @horizontal_centering != new_value
        @horizontal_centering = new_value
      end

      def vertical_centering=(new_value)
        dirty! if @vertical_centering != new_value
        @vertical_centering = new_value
      end

      def do_horizontal_centering(canvas, x, y, w, h)
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
          self_info = super()
        else
          self_info = "<#{self.class.name}>"
        end
        self_info
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
        top, right, bottom, left = parse_four_dimensions(*values)
        @margin_top = top if top
        @margin_right = right if right
        @margin_bottom = bottom if bottom
        @margin_left = left if left
      end

      def margin_with(params)
        margin_set(*extract_four_dimensions(params))
      end

      def padding_set(*values)
        top, right, bottom, left = parse_four_dimensions(*values)
        @padding_top = top if top
        @padding_right = right if right
        @padding_bottom = bottom if bottom
        @padding_left = left if left
      end

      def padding_with(params)
        padding_set(*extract_four_dimensions(params))
      end

      protected
      def user_property=(prop)
        @user_property = prop
      end

      def prop=(prop)
        @prop = prop
      end

      private
      def make_prop_value(name, *values)
        formatter_name = to_class_name(name)
        begin
          unless Format.const_defined?(formatter_name)
            raise NameError
          end
        rescue NameError
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
        _draw_rec(canvas, x, y, w, h, simulation, @around_draw_procs.size)
      end

      def _draw_rec(canvas, x, y, w, h, simulation, around_index)
        if around_index.zero?
          (@pre_draw_procs +
           [method(:draw_element)] +
           @post_draw_procs.reverse).each do |pro,|
            x, y, w, h = pro.call(canvas, x, y, w, h, simulation)
          end
          [x, y, w, h]
        else
          pro, = @around_draw_procs[around_index - 1]
          pro.call(canvas, x, y, w, h, simulation, Proc.new do |*args|
            args << around_index - 1
            _draw_rec(*args)
          end)
        end
      end

      def _indent(str, width="  ")
        str.collect do |x|
          width + x
        end.join("")
      end
    end
  end
end
