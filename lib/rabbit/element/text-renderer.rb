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

      def font_size
        text_props["size"]
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
            scale = 0.95
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

      def draw_layout(canvas, x, y)
        color = prop_get("foreground")
        color = color.value if color
        canvas.draw_layout(@layout, x, y, color)
      end

      def dynamic_font_size_computation(proc_name, compute_max_size,
                                        &make_params)
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

        size = new_size = initial_font_size_for_compute_font_size
        current_layout_size = @layout.pixel_size
        unless compare.call(*@layout.pixel_size)
          loop do
            new_size = compute_next_font_size(size, scale)
            set_computed_font_size(new_size)
            compile(canvas, x, y, w, h)
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
