require 'rabbit/element/base'

module Rabbit
  module Element
    module TextRenderer
      include Base

      attr_reader :layout
      attr_reader :original_width, :original_height
      attr_reader :first_line_width, :first_line_height

      attr_writer :wrap_mode, :indent, :spacing

      def initialize(*args, &block)
        super
        text_clear_theme
      end

      def align=(new_value)
        dirty! if @align != new_value
        @align = new_value
      end

      def width
        if @width
          @width + @padding_left + @padding_right
        else
          nil
        end
      end

      def height
        @height + @padding_top + @padding_bottom
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
        self.align = Pango::Layout::ALIGN_CENTER
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

      private
      def setup_draw_info(str, canvas, w)
        layout = canvas.make_layout(str)
        @original_width, @original_height = layout.pixel_size
        @first_line_width = @original_width / layout.line_count
        @first_line_height = @original_height / layout.line_count
        if @wrap_mode
          layout.set_width(w * Pango::SCALE)
          layout.set_wrap(@wrap_mode)
        else
          layout.set_width(-1)
        end
        layout.set_alignment(@align)
        layout.set_indent(@indent)
        layout.set_spacing(@spacing)
        layout.context_changed
        width, height = layout.pixel_size
        if layout.width != -1 and
            (layout.alignment == Pango::Layout::ALIGN_CENTER or
               layout.alignment == Pango::Layout::ALIGN_RIGHT)
          width = layout.width / Pango::SCALE
        end
        @width, @height = width, height
        @layout = layout
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
        Pango::Layout::WRAP_WORD_CHAR
      end

      def default_align
        Pango::Layout::ALIGN_LEFT
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

      def draw_layout(canvas, x, y)
        color = prop_get("foreground")
        color = color.value if color
        canvas.draw_layout(@layout, x, y, color)
      end
    end
  end
end
