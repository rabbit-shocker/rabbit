require 'rabbit/utils'
require 'rabbit/formatter'
require 'rabbit/image'

module Rabbit
  module Element
    module Base
      include Utils

      attr_reader :x, :y, :w, :h
      attr_reader :px, :py, :pw, :ph

      attr_reader :base_x, :base_y, :base_w, :base_h
      attr_reader :horizontal_centering, :vertical_centering
      
      attr_reader :user_property
      
      attr_accessor :margin_left, :margin_right
      attr_accessor :margin_top, :margin_bottom
      
      attr_accessor :padding_left, :padding_right
      attr_accessor :padding_top, :padding_bottom

      attr_accessor :parent

      def initialize
        @x = @y = @w = @h = nil
        @parent = nil
        @user_property = {}
        @default_prop = {}
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

      def add_pre_draw_proc(name=nil, &block)
        @pre_draw_procs << [block, name]
        block
      end
      
      def add_post_draw_proc(name=nil, &block)
        @post_draw_procs << [block, name]
        block
      end

      def delete_pre_draw_proc(block)
        @pre_draw_procs.reject! do |blk,|
          blk == block
        end
      end

      def delete_post_draw_proc(block)
        @post_draw_procs.reject! do |blk,|
          blk == block
        end
      end

      def delete_pre_draw_proc_by_name(name)
        @pre_draw_procs.reject! do |_, nm|
          name === nm
        end
      end

      def delete_post_draw_proc_by_name(name)
        @post_draw_procs.reject! do |_, nm|
          name === nm
        end
      end

      def clear_pre_draw_procs
        @pre_draw_procs = []
      end
      
      def clear_post_draw_procs
        @post_draw_procs = []
      end
      
      def pre_draw_procs(name)
        @pre_draw_procs.find_all do |_, nm|
          name === nm
        end
      end

      def post_draw_procs(name)
        @post_draw_procs.find_all do |_, nm|
          name === nm
        end
      end

      def pre_draw_proc(name)
        @pre_draw_procs.find do |_, nm|
          name === nm
        end
      end

      def post_draw_proc(name)
        @post_draw_procs.find do |_, nm|
          name === nm
        end
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

      def inline_element?
        true
      end

      def clear_theme
        @pre_draw_procs = []
        @post_draw_procs = []
        @width = @height = nil
        @centering_adjusted_width = nil
        @centering_adjusted_height = nil
        @horizontal_centering = @vertical_centering = false
        @prop = default_prop
        clear_margin
        clear_padding
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

      def clear_padding
        @padding_left = @padding_right = 0
        @padding_top = @padding_bottom = 0
      end

      def clear_margin
        @margin_left = @margin_right = 0
        @margin_top = @margin_bottom = 0
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

      def text_props
        props = {}
        @prop.each do |name, formatter|
          props[name] = formatter.value
        end
        props
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

      def _draw(canvas, x, y, w, h, simulation)
        (@pre_draw_procs +
           [method(:draw_element)] +
           @post_draw_procs).each do |pro,|
          x, y, w, h = pro.call(canvas, x, y, w, h, simulation)
        end
        [x, y, w, h]
      end
      
      def _indent(str, width="  ")
        str.collect do |x|
          width + x
        end.join("")
      end

    end

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

      def text_to_html
        markup_as_html(text)
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

    module BlockHorizontalCentering
      
      attr_reader :ox, :oy, :ow, :oh # dirty!!!!

      def do_horizontal_centering(canvas, x, y, w, h)
        @ox, @oy, @ow, @oh = @x, @y, @w, @h
        adjust_width = ((w - width) / 2.0).ceil
        x += adjust_width
        w -= adjust_width
        @centering_adjusted_width = adjust_width
        compile_for_horizontal_centering(canvas, x, @y, w, h)
        draw(true)
      end

      def clear_theme
        @ox = @oy = @ow = @oh = nil
        super
      end
    end
    
    module BlockElement
      
      include Base

      def inline_element?
        false
      end

      def adjust_y_padding(y, h)
        y += @padding_bottom
        h -= @padding_bottom
        [y, h]
      end
    end
    
    module ContainerElement
      extend Forwardable
      
      include BlockElement
      include Enumerable
      
      attr_reader :elements
      
      def_delegators(:@elements, :[], :empty?, :each, :first, :last)

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

      def text
        @elements.collect do |element|
          element.text
        end.join("\n")
      end

      def draw_element(canvas, x, y, w, h, simulation)
        draw_elements(canvas, x, y, w, h, simulation)
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        args = [x, y, w, h]
        adjust_height = 0
        if do_vertical_centering?
          adjust_height = ((h - height - @padding_bottom) / 2.0).ceil
          if y + adjust_height > 0
            args = [x, y + adjust_height, w, h - adjust_height]
          else
            adjust_height = 0
          end
        end
        @centering_adjusted_height = adjust_height
        compile_elements(canvas, *args)
        base_x, base_w = x, w
        @elements.each do |element|
          x, y, w, h = element.draw(simulation)
        end
        if @elements.last and @elements.last.inline_element?
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
          x, w = adjust_x_centering(x, w)
          y, h = adjust_y_padding(y, h)
        end
      end

      def compile_elements(canvas, x, y, w, h)
        prev_is_inline = false
        @elements.each do |element|
          element.compile(canvas, x, y, w, h)
          if do_horizontal_centering? or element.do_horizontal_centering?
            element.do_horizontal_centering(canvas, x, y, w, h)
          end
          x, y, w, h = element.draw(true)
        end
      end

      def compile_horizontal(canvas, x, y, w, h)
        @elements.each do |element|
          if do_horizontal_centering? or element.do_horizontal_centering?
            element.do_horizontal_centering(canvas, x, y, w, h)
          end
        end
      end
      
      def each(&block)
        @elements.each(&block)
      end

      def to_html
        collect do |element|
          element.to_html
        end.join("\n")
      end

      def [](*args)
        @elements[*args]
      end

      def empty?
        @elements.empty?
      end

      def first
        @elements.first
      end

      def last
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

      def empty?
        @elements.empty?
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
    end

    module TextContainerElement

      include ContainerElement
      include TextRenderer

      attr_reader :prop

      alias prop_set __prop_set__
      alias prop_get __prop_get__
      alias prop_delete __prop_delete__

      def draw_elements(canvas, x, y, w, h, simulation)
        unless simulation
          draw_layout(canvas, x, y)
        end
        [x, y + @height, w, h - @height]
      end

      def to_html
        html = @elements.collect do |elem|
          elem.to_html
        end.join("\n")
        markup_as_html(html)
      end

      def markuped_text
        mt = @elements.collect do |elem|
          elem.markuped_text
        end.join("")
        markup(mt)
      end

      def text
        @elements.collect do |elem|
          elem.text
        end.join("")
      end

      def do_horizontal_centering?
        super and not width.nil?
      end

      def compile(canvas, x, y, w, h)
        super
        text_compile(canvas, @x, @y, @w, @h)
      end

      def dirty?
        super or text_dirty?
      end

      def empty?
        /\A\s*\z/ =~ text
      end

      def clear_theme
        container_clear_theme
        super
      end
    end
    
    module TextElement
      
      include Base
      include TextRenderer
      
      attr_accessor :text
      
      def initialize(text)
        super()
        @text = text
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          draw_layout(canvas, x, y)
        end
        [x + width, y, w - width, h]
      end

      def to_html
        text_to_html
      end

      def empty?
        /\A\s*\z/ =~ @text
      end
    end

    module SlideElement
      include ContainerElement

      attr_reader :title
      def initialize(first_element)
        super()
        @title = first_element.text
        add_element(first_element)
      end

      def draw(canvas, simulation=nil)
        if simulation.nil?
          begin
            draw(canvas, true)
            draw(canvas, false)
          rescue StandardError, LoadError
            canvas.logger.warn($!)
          end
        else
          canvas.draw_slide(self, simulation) do
            compile(canvas, 0, 0, canvas.width, canvas.height)
            super(simulation)
          end
        end
      end

      def to_html
        "<div class='slide'>\n#{super}\n</div>"
      end
    end
    
    class Slide
      include SlideElement
      
      def headline
        @elements[0]
      end

      def body
        @elements[1]
      end
    end
    
    class TitleSlide
      include SlideElement

      def initialize(title)
        super(title)
        @local_prop = {}
      end

      def to_html
        "<div style='text-align: center;'>\n#{super}\n</div>"
      end
      
      def <<(element)
        if element.is_a?(DescriptionList)
          element.each do |item|
            name = item.term.collect{|x| x.text}.join("")
            name = normalize_name(name)
            klass_name = to_class_name(name)
            if Element.const_defined?(klass_name)
              meta = Element.const_get(klass_name).new
              item.content.each do |elem|
                elem.each do |e|
                  meta << e
                end
              end
              super(meta)
            else
              content = ""
              item.content.each do |x|
                content << x.text
              end
              @local_prop[name] = content.strip
            end
          end
        end
      end

      def theme
        @local_prop["theme"]
      end

      def allotted_time
        time = @local_prop["allotted-time"]
        time = parse_time(time) if time
        time
      end

      private
      def normalize_name(name)
        name.gsub(/_/, "-")
      end

      def parse_time(str)
        if /\A\s*\z/m =~ str
          nil
        else
          if /\A\s*(\d*\.?\d*)\s*(h|m|s)?\s*\z/i =~ str
            time = $1.to_f
            unit = $2
            if unit
              case unit.downcase
              when "m"
                time *= 60
              when "h"
                time *= 3600
              end
            end
            time.to_i
          else
            nil
          end
        end
      end
    end

    class Body
      include ContainerElement
    end
    
    class Title
      include TextContainerElement

      def to_html
        "<h1>#{super}</h1>"
      end
    end
    
    class Author
      include TextContainerElement

      def to_html
        "<address>#{super}</address>"
      end
    end
    
    class Subtitle
      include TextContainerElement

      def to_html
        "<h2>#{super}</h2>"
      end
    end
    
    class ContentSource
      include TextContainerElement

      def to_html
        "<p class='content-source'>#{super}</p>"
      end
    end
    
    class Institution
      include TextContainerElement

      def to_html
        "<p class='institution'>#{super}</p>"
      end
    end
    
    class Date
      include TextContainerElement

      def to_html
        "<p class='date'>#{super}</p>"
      end
    end
    
    class Place
      include TextContainerElement

      def to_html
        "<p class='place'>#{super}</p>"
      end
    end
    
    class When
      include TextContainerElement

      def to_html
        "<p class='when'>#{super}</p>"
      end
    end
    
    class Where
      include TextContainerElement

      def to_html
        "<p class='where'>#{super}</p>"
      end
    end
    
    class HeadLine
      include TextContainerElement

      def to_html
        "<h1>#{super}</h1>"
      end
    end
    
    class Text
      include TextElement
    end

    class TextContainer
      include TextContainerElement
    end
    
    class PreformattedBlock
      include TextContainerElement
      include BlockHorizontalCentering

      def to_html
        "<pre>#{super}</pre>"
      end
    end
    
    class PreformattedText
      include TextContainerElement
    end

    class Keyword
      include TextContainerElement
    end

    class Comment
      include TextContainerElement
    end

    class Emphasis
      include TextContainerElement
    end
    
    class Code
      include TextContainerElement
    end
    
    class Variable
      include TextContainerElement
    end
    
    class Keyboard
      include TextContainerElement
    end
    
    class Index
      include TextContainerElement
    end
    
    class Note
      include TextContainerElement
    end

    class FootTextBlock
      include ContainerElement
    end
    
    class FootNote
      include TextElement

      attr_reader :order

      def initialize(order)
        @order = order
        super("")
      end
    end
    
    class FootText
      include TextContainerElement

      attr_reader :order

      def initialize(order, elems=[])
        @order = order
        super(elems)
      end
    end
    
    class Verbatim
      include TextContainerElement
    end
    
    class DeletedText
      include TextContainerElement
    end
    
    class ReferText
      include TextContainerElement

      attr_accessor :to
    end
    
    class Subscript
      include TextContainerElement
    end
    
    class Superscript
      include TextContainerElement
    end
    
    class Paragraph
      include TextContainerElement

      def to_html
        "<p>\n#{super}\n</p>"
      end
    end
    
    class ItemList
      include ContainerElement

      def to_html
        "<ul>\n#{super}\n</ul>"
      end
    end
    
    class ItemListItem
      include ContainerElement

      def to_html
        "<li>\n#{super}\n</li>"
      end
    end

    class EnumList
      include ContainerElement

      def to_html
        "<ol>\n#{super}\n</ol>"
      end
    end
    
    class EnumListItem
      include ContainerElement
      attr_accessor :order

      def to_html
        "<li>\n#{super}\n</li>"
      end
    end

    class DescriptionList
      include ContainerElement

      def to_html
        "<dl>\n#{super}\n</dl>"
      end
    end
    
    class DescriptionListItem
      include ContainerElement
      
      attr_reader :term, :content

      def initialize(term, content)
        super()
        @term = term
        @content = content
        add_element(@term)
        add_element(@content)
      end
    end

    class DescriptionTerm
      include TextContainerElement

      def to_html
        "<dt>\n#{super}\n</dt>"
      end
    end

    class DescriptionContent
      include ContainerElement

      def to_html
        "<dd>\n#{super}\n</dd>"
      end
    end

    class MethodList
      include ContainerElement
    end
    
    class MethodListItem
      include ContainerElement

      attr_reader :term, :description
      
      def initialize(term, description)
        super()
        @term = term
        @description = description
        add_element(@term)
        add_element(@description)
      end

      def name
        @term.name
      end
    end

    class MethodTerm
      include TextContainerElement

      attr_accessor :name
    end

    class MethodName
      include TextContainerElement
    end

    class ClassName
      include TextContainerElement
    end

    class MethodKind
      include TextContainerElement
    end

    class MethodDescription
      include ContainerElement
    end

    class Image
      
      include BlockElement
      include BlockHorizontalCentering

      include ImageManipulable

      attr_reader :caption
      attr_reader :normalized_width, :normalized_height
      attr_reader :relative_width, :relative_height

      def initialize(filename, prop)
        super(filename)
        %w(caption dither_mode).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
        %w(keep_scale keep_ratio).each do |name|
          unless prop[name].nil?
            self.keep_ratio = (prop[name] == "true")
          end
        end
        %w(width height
           x_dither y_dither
           normalized_width normalized_height
           relative_width relative_height
          ).each do |name|
          instance_variable_set("@#{name}", prop[name] && Integer(prop[name]))
        end
        resize(@width, @height)
      end

      def draw_element(canvas, x, y, w, h, simulation)
        draw_image(canvas, x, y, w, h, simulation)
      end

      def text
        @caption.to_s
      end

      def to_html
        return 'image is not supported'
        filename = File.join(base_dir, "XXX.png")
        @loader.pixbuf.save(filename, "png")
        result = "<img "
        result << "title='#{@caption}' " if @caption
        result << "src='#{filename}' />"
        result
      end

      def dither_mode
        @dither_mode ||= "normal"
        mode_name = "DITHER_#{@dither_mode.upcase}"
        if Gdk::RGB.const_defined?(mode_name)
          Gdk::RGB.const_get(mode_name)
        else
          Gdk::RGB::DITHER_NORMAL
        end
      end

      def x_dither
        @x_dither || 0
      end
      
      def y_dither
        @y_dither || 0
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
      
      private
      def draw_image(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_pixbuf(pixbuf, x, y)
        end
        [x, y + height, w, h - height]
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = (@oh || h) - @padding_top - @padding_bottom
        nw = make_normalized_size(@normalized_width)
        nh = make_normalized_size(@normalized_height)
        rw = make_relative_size(@relative_width, base_w)
        rh = make_relative_size(@relative_height, base_h)
        iw = nw || rw
        ih = nh || rh
        resize(iw, ih)
      end

      def make_normalized_size(size)
        size && screen_size(size)
      end

      def make_relative_size(size, parent_size)
        size && parent_size && ((size / 100.0) * parent_size).ceil
      end
    end

    class Table
      include ContainerElement

      attr_reader :caption
      def initialize(prop)
        super()
        %w(caption).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
      end

      def head
        elements.find {|e| e.is_a?(TableHead)}
      end

      def body
        elements.find {|e| e.is_a?(TableBody)}
      end

      def to_html
        caption = nil
        caption = "<caption>#{@caption}</caption>\n" if @caption
        "<table>\n#{caption}#{super}\n</table>"
      end
    end

    class TableHead
      include ContainerElement

      def to_html
        "<thead>\n#{super}\n</thead>"
      end
    end

    class TableBody
      include ContainerElement

      def to_html
        "<tbody>\n#{super}\n</tbody>"
      end
    end
    
    class TableRow
      include ContainerElement

      def to_html
        "<tr>\n#{super}\n</tr>"
      end
    end

    class TableHeader
      include TextElement

      def to_html
        "<th>#{super}</th>"
      end
    end

    class TableCell
      include TextElement

      def to_html
        "<td>#{super}</td>"
      end
    end
  end
end
