require 'rabbit/utils'
require 'rabbit/formatter'
require 'rabbit/image'

module Rabbit
  
  module Element
    
    module Base

      include Utils
      
      attr_reader :width, :height

      attr_reader :x, :y, :w, :h
      attr_reader :px, :py, :pw, :ph

      attr_accessor :parent, :horizontal_centering, :vertical_centering

      attr_accessor :user_property
      
      def initialize
        @x = @y = @w = @h = nil
        @simulated_width = nil
        @simulated_height = nil
        @parent = nil
        @horizontal_centering = @vertical_centering = false
        @user_property = {}
        clear_theme
      end
      
      def draw(simulation=false)
        x, y, w, h = @x, @y, @w, @h
        (@pre_draw_procs +
                          [method(:draw_element)] +
                          @post_draw_procs).each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        if simulation
          @simulated_width = x - @x + width
          @simulated_height = y - @y
        end
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
        @px, @py, @pw, @ph = @x, @y, @w, @h
        @canvas, @x, @y, @w, @h = canvas, x, y, w, h
        if [@px, @py, @pw, @ph] != [@x, @y, @w, @h]
          dirty!
        end
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
      end
      alias __prop_delete__ prop_delete
      
      def inline_element?
        true
      end

      def clear_theme
        @pre_draw_procs = []
        @post_draw_procs = []
        @width = @height = nil
        @prop = {}
        dirty!
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
        @vertical_centering
      end
      
      def do_horizontal_centering(canvas, x, y, w, h)
        removed_width = w - simulated_width
        cx = x + (removed_width / 2.0).ceil
        cw = w - removed_width
        compile(canvas, cx, y, cw, h)
        draw(true)
      end

      def previous_element
        sibling_element(-1)
      end

      def next_element
        sibling_element(1)
      end

      def simulated_width
        @simulated_width || height
      end

      def simulated_height
        @simulated_height || height
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

    end

    module TextRenderer

      include Base

      attr_reader :width, :height, :layout
      attr_reader :original_width, :original_height

      attr_writer :wrap_mode, :indent, :spacing, :align

      def initialize(*args, &block)
        super
        @indent = 0
        @spacing = 0
        @wrap_mode = Pango::Layout::WRAP_WORD_CHAR
        @align = Pango::Layout::ALIGN_LEFT
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
          info = generate_draw_info(markuped_text, canvas, @w)
          @layout, @width, @height, @original_width, @original_height = info
        end
      end
      
      def do_horizontal_centering(canvas, x, y, w, h)
        @layout.set_alignment(Pango::Layout::ALIGN_CENTER)
      end
     
      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_layout(@layout, x, y)
        end
        [x + @width, y, w - @width, h]
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_layout(@layout, x, y)
        end
        [x, y + @height, w, h - @height]
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
      end

      def dirty?
        super or text_dirty?
      end

      def text_dirty?
        @layout.nil?
      end

      private
      def generate_draw_info(str, canvas, w)
        layout, orig_width, orig_height = canvas.make_layout(str)
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
        width, height = layout.size.collect {|x| x / Pango::SCALE}
        [layout, width, height, orig_width, orig_height]
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
    end

    module BlockHorizontalCentering
      
      attr_reader :ox, :oy, :ow, :oh # dirty!!!!

      def do_horizontal_centering(canvas, x, y, w, h)
        @ox, @oy, @ow, @oh = @x, @y, @w, @h
        adjust_width = ((w / 2.0) - (width / 2.0)).ceil
        x, w = restore_x_padding(x, w)
        cx = x + adjust_width
        compile(canvas, cx, @y, w, h)
        draw(true)
      end
    end
    
    module BlockElement
      
      include Base
      include Enumerable

      attr_accessor :left_margin, :right_margin
      attr_accessor :top_margin, :bottom_margin
      
      attr_accessor :left_padding, :right_padding
      attr_accessor :top_padding, :bottom_padding

      attr_reader :base_x, :base_y, :base_w, :base_h
      
      attr_reader :elements
      
      def initialize(elems=[])
        @elements = []
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

      def draw(simulation=false)
        x, y, w, h = setup_padding(@x, @y, @w, @h)
        (@pre_draw_procs + [method(:draw_elements)]).each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        # x, y, w, h = @x, y, @w, h
        @post_draw_procs.each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        if simulation
          @simulated_width = x - @x + width
          @simulated_height = (@vertical_centered_y || y) - @y
        end
        x, w = restore_x_padding(x, w)
        [x, y, w, h]
      end
      
      def draw_elements(canvas, x, y, w, h, simulation)
        args = [x, y, w, h]
        if do_vertical_centering?
          adjust_height = ((h - simulated_height) / 2.0).ceil
          if y + adjust_height > 0
            @vertical_centered_y = y + adjust_height
            args = [x, y + adjust_height, w, h - adjust_height]
          end
        end
        compile_elements(canvas, *args)
        elements.each do |element|
          x, y, w, h = element.draw(simulation)
        end
        [x, y, w, h]
      end
      
      def compile(canvas, x, y, w, h)
        x += @left_margin
        y += @top_margin
        w -= @left_margin + @right_margin
        h -= @top_margin + @bottom_margin
        @base_x, @base_y, @base_w, @base_h = x, y, w, h
        super(canvas, x, y, w, h)
        if_dirty do
          compile_elements(canvas, @x, @y, @w, @h)
        end
      end

      def compile_elements(canvas, x, y, w, h)
        ox, oy, ow, oh = x, y, w, h
        elements.each do |element|
          element.compile(canvas, x, y, w, h)
          x, y, w, h = element.draw(true)
        end
        compile_horizontal(canvas, ox, oy, ow, oh)
      end

      def compile_horizontal(canvas, x, y, w, h)
        elements.each do |element|
          if do_horizontal_centering? or element.do_horizontal_centering?
            element.do_horizontal_centering(canvas, x, y, w, h)
          end
        end
      end
      
      def each(&block)
        @elements.each(&block)
      end

      def [](*args)
        @elements[*args]
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
        elements.collect{|elem| elem.width}.compact.max.to_i
      end

      def height
        inline_heights = []
        inject(0) do |result, elem|
          if elem.height
            if elem.inline_element?
              inline_heights << elem.height
              result
            else
              prev_inlines_max_height = inline_heights.max.to_i
              inline_heights.clear
              result + elem.height + prev_inlines_max_height
            end
          else
            result
          end
        end + inline_heights.max.to_i
      end

      def clear_theme
        @vertical_centering = false
        @horizontal_centering = false
        @left_margin = @right_margin = 0
        @top_margin = @bottom_margin = 0
        @left_padding = @right_padding = 0
        @top_padding = @bottom_padding = 0
        @elements.each do |element|
          element.clear_theme
        end
        super
      end

      def inline_element?
        false
      end

      def dirty?
        super or @elements.any?{|x| x.dirty?}
      end

      private
      def setup_padding(x, y, w, h)
        x += @left_padding
        y += @top_padding
        w -= @left_padding + @right_padding
        h -= @top_padding
        [x, y, w, h]
      end

      def restore_x_padding(x, w)
        x -= @left_padding
        w += @left_padding
        [x, w]
      end      
    end
    
    module ContainerElement
      
      include BlockElement
      
      def __draw(simulation=false)
        x, y, w, h = _draw(@x, @y, @w, @h, simulation)
        if simulation
          @simulated_width = x - @x + width
          @simulated_height = y - @y
        end
        [x, y, w, h]
      end

      def _draw(x, y, w, h, simulation)
        (@pre_draw_procs +
                          [method(:draw_elements)] +
                          @post_draw_procs).each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        [x, y, w, h]
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        _, y, _, h = super
        [x, y, w, h]
      end
    end

    module TextContainerElement

      include ContainerElement
      include TextRenderer

      attr_reader :width, :height

      attr_reader :prop

      alias prop_set __prop_set__
      alias prop_get __prop_get__
      alias prop_delete __prop_delete__
      
      def markuped_text
        mt = elements.collect do |elem|
          elem.markuped_text
        end.join("")
        markup(mt)
      end

      def text
        elements.collect do |elem|
          elem.text
        end.join("")
      end

      def do_horizontal_centering?
        super and not width.nil?
      end

      def compile(canvas, x, y, w, h)
        super
        text_compile(canvas, x, y, w, h)
      end

      def dirty?
        super or text_dirty?
      end

      def clear_theme
        super
        text_clear_theme
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
    end
    
    class Slide
      include BlockElement

      attr_reader :title

      def initialize(head_line)
        super()
        @title = head_line.text
        add_element(head_line)
      end

      def draw(canvas, simulation=false)
        canvas.draw_slide(self) do
          compile(canvas, 0, 0, canvas.width, canvas.height)
          super(simulation)
        end
      end

    end
    
    class TitleSlide
      include BlockElement

      attr_reader :title
      
      def initialize(title)
        super()
        @local_prop = {}
        @title = title.text
        add_element(title)
      end
      
      def <<(element)
        if element.is_a?(DescriptionList)
          element.each do |item|
            name = item.term.collect{|x| x.text}.join("")
            klass_name = to_class_name(name)
            if Element.const_defined?(klass_name)
              meta = Element.const_get(klass_name).new
              item.each_without_term do |elem|
                elem.each do |e|
                  meta << e
                end
              end
              super(meta)
            else
              content = ""
              item.each_without_term do |x|
                content << x.text
              end
              @local_prop[name] = content.strip
            end
          end
        end
      end

      def draw(canvas, simulation=false)
        canvas.draw_slide(self) do
          compile(canvas, 0, 0, canvas.width, canvas.height)
          super(simulation)
        end
      end
      
      def theme
        @local_prop["theme"]
      end

    end

    class Body
      include ContainerElement
    end
    
    class Title
      include TextContainerElement
    end
    
    class Author
      include TextContainerElement
    end
    
    class Subtitle
      include TextContainerElement
    end
    
    class ContentSource
      include TextContainerElement
    end
    
    class Institution
      include TextContainerElement
    end
    
    class HeadLine
      include TextContainerElement
    end
    
    class NormalText
      include TextElement
    end

    class TextContainer
      include TextContainerElement
    end
    
    class PreformattedBlock
      include TextContainerElement
      include BlockHorizontalCentering
    end
    
    class PreformattedText
      include TextElement
    end
    
    class Emphasis
      include TextElement
    end
    
    class Code
      include TextElement
    end
    
    class Variable
      include TextElement
    end
    
    class Keyboard
      include TextElement
    end
    
    class Index
      include TextElement
    end
    
    class FoottextBlock
      include ContainerElement
    end
    
    class Footnote
      include TextElement

      attr_reader :order

      def initialize(order)
        @order = order
        super("")
      end
    end
    
    class Foottext
      include TextContainerElement

      attr_reader :order

      def initialize(order, elems=[])
        @order = order
        super(elems)
      end
    end
    
    class Verbatim
      include TextElement
    end
    
    class DeletedText
      include TextElement
    end
    
    class ReferText
      include TextElement

      attr_accessor :to
    end
    
    class Subscript
      include TextElement
    end
    
    class Superscript
      include TextElement
    end
    
    class Paragraph
      include TextContainerElement
    end
    
    class ItemList
      include ContainerElement
    end
    
    class ItemListItem
      include ContainerElement
    end

    class EnumList
      include ContainerElement
      include Enumerable
    end
    
    class EnumListItem
      include ContainerElement
      attr_accessor :order
    end

    class DescriptionList
      include ContainerElement
    end
    
    class DescriptionListItem
      include ContainerElement
      
      attr_reader :term

      def initialize(term)
        super()
        @term = term
        add_element(@term)
      end

      def each_without_term(&block)
        @elements[1..-1].each(&block)
      end

    end

    class DescriptionTerm
      include TextContainerElement
    end

    class MethodList
      include ContainerElement
    end
    
    class MethodListItem
      include ContainerElement
      
      def initialize(term)
        super()
        @term = term
        add_element(@term)
      end

      def name
        @term.name
      end

      def each_without_term(&block)
        @elements[1..-1].each(&block)
      end

    end

    class MethodTerm
      include TextContainerElement

      attr_accessor :name
    end

    class MethodName
      include TextElement
    end

    class ClassName
      include TextElement
    end

    class MethodKind
      include TextElement
    end

    class Image
      
      include Base
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
        unless simulation
          canvas.draw_pixbuf(pixbuf, x, y)
        end
        [x, y + height, w, h - height]
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

      def inline_element?
        false
      end

      def restore_x_padding(x, w)
        [x, w]
      end
    end

    class Table
      include ContainerElement
      include BlockHorizontalCentering

      attr_reader :caption
      def initialize(prop)
        super()
        %w(caption).each do |name|
          instance_variable_set("@#{name}", prop[name])
        end
      end
    end

    class TableHeaders
      include TextContainerElement
      include BlockHorizontalCentering
    end

    class TableHeader
      include TextElement
    end

    class TableBody
      include ContainerElement
    end
    
    class TableRow
      include TextContainerElement
    end

    class TableCell
      include TextElement
    end
    
  end
end
