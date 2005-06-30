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

      attr_reader :base_x, :base_y, :base_w, :base_h
      
      attr_accessor :left_margin, :right_margin
      attr_accessor :top_margin, :bottom_margin
      
      attr_accessor :left_padding, :right_padding
      attr_accessor :top_padding, :bottom_padding

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
        x, y, w, h = setup_padding(@x, @y, @w, @h)
        x, y, w, h = _draw(@canvas, x, y, w, h, simulation)
        if simulation
          @simulated_width, @simulated_height = sync_simulated_size(x, y, w, h)
        end
        x, w = restore_x_padding(x, w)
        y, h = adjust_y_padding(y, h)
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
        clear_margin
        clear_padding
        dirty!
      end

      def setup_padding(x, y, w, h)
        x += @left_padding
        y += @top_padding
        w -= @left_padding + @right_padding
        h -= @top_padding
        [x, y, w, h]
      end

      def restore_x_padding(x, w)
        x -= @left_padding
        w += @left_padding + @right_padding
        [x, w]
      end

      def adjust_y_padding(y, h)
        y += @bottom_padding
        h -= @bottom_padding
        [y, h]
      end

      def setup_margin(x, y, w, h)
        x += @left_margin
        y += @top_margin
        w -= @left_margin + @right_margin
        h -= @top_margin + @bottom_margin
        [x, y, w, h]
      end

      def adjust_y_margin(y, h)
        y += @bottom_margin
        h -= @bottom_margin
        [y, h]
      end

      def clear_padding
        @left_padding = @right_padding = 0
        @top_padding = @bottom_padding = 0
      end

      def clear_margin
        @left_margin = @right_margin = 0
        @top_margin = @bottom_margin = 0
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

      def available_w
        @w - @left_padding - @right_padding
      end
      
      def inspect(verbose=false)
        if verbose
          self_info = super()
        else
          self_info = "<#{self.class.name}>"
        end
        self_info
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

      def sync_simulated_size(x, y, w, h)
        sync_w = x - @x + width
        sync_h = y - @y
        [sync_w, sync_h]
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

      attr_writer :wrap_mode, :indent, :spacing

      def initialize(*args, &block)
        super
        @indent = 0
        @spacing = 0
        @wrap_mode = Pango::Layout::WRAP_WORD_CHAR
        @align = Pango::Layout::ALIGN_LEFT
      end

      def align=(new_value)
        dirty! if @align != new_value
        @align = new_value
      end

      def width
        if !@layout.nil? and @layout.width != -1 and
            (@layout.alignment == Pango::Layout::ALIGN_CENTER or
               @layout.alignment == Pango::Layout::ALIGN_RIGHT)
          @layout.width / Pango::SCALE
        else
          @width
        end
      end

      def height
        @layout.pixel_size[1]
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
          info = generate_draw_info(markuped_text, canvas, w)
          @layout, @width, @height, @original_width, @original_height = info
        end
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
        width, height = layout.pixel_size
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
        adjust_width = ((w / 2.0) - (width / 2.0)).ceil - @left_padding
        x += adjust_width
        w -= adjust_width
        compile(canvas, x, @y, w, h)
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

      private
      def _draw(canvas, x, y, w, h, simulation)
        draw_element(canvas, x, y, w, h, simulation)
      end
      
      def sync_simulated_size(x, y, w, h)
        sync_w = x - @x + width
        sync_h = (@vertical_centered_y || y) - @y
        [sync_w, sync_h]
      end
    end
    
    module ContainerElement
      
      include BlockElement
      include Enumerable
      
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

      def draw_element(canvas, x, y, w, h, simulation)
        (@pre_draw_procs +
           [method(:draw_elements)] +
           @post_draw_procs).each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        [x, y, w, h]
      end

      def draw_elements(canvas, x, y, w, h, simulation)
        args = [x, y, w, h]
        if do_vertical_centering? and !simulation
          adjust_height = ((h - simulated_height - @bottom_padding) / 2.0).ceil
          if y + adjust_height > 0
            @vertical_centered_y = y + adjust_height
            args = [x, y + adjust_height, w, h - adjust_height]
          end
        end
        compile_elements(canvas, *args)
        base_x, base_w = x, w
        elements.each do |element|
          x, y, w, h = element.draw(simulation)
        end
        if elements.last and elements.last.inline_element?
          x = base_x
          y += elements.last.height
          w = base_w
          h -= elements.last.height
        end
        [x, y, w, h]
      end

      def compile(canvas, x, y, w, h)
        super
        if_dirty do
          compile_elements(canvas, @x, @y, @w, @h)
        end
      end

      def compile_elements(canvas, x, y, w, h)
        ox, oy, ow, oh = x, y, w, h
        prev_is_inline = false
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
        @elements.collect{|elem| elem.width}.compact.max.to_i
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
        @elements.each do |element|
          element.clear_theme
        end
        super
      end

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
          canvas.draw_layout(@layout, x, y)
        end
        [x, y + @height, w, h - @height]
      end

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

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_layout(@layout, x, y)
        end
        [x + width, y, w - width, h]
      end
    end
    
    class Slide
      include ContainerElement

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
      include ContainerElement

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
        (@pre_draw_procs +
           [method(:draw_image)] +
           @post_draw_procs).each do |pro,|
          x, y, w, h = pro.call(@canvas, x, y, w, h, simulation)
        end
        [x, y, w, h]
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

      def compile(canvas, x, y, w, h)
        super
        adjust_size(canvas, x, y, w, h)
      end
      
      private
      def draw_image(canvas, x, y, w, h, simulation)
        unless simulation
          canvas.draw_pixbuf(pixbuf, x, y)
        end
        [@ox || x, y + height, @ow || w, h - height]
      end

      def adjust_size(canvas, x, y, w, h)
        nw = make_normalized_size(@normalized_width)
        nh = make_normalized_size(@normalized_height)
        rw = make_relative_size(@relative_width, @ow || w)
        rh = make_relative_size(@relative_height, @oh || h)
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
    end

    class TableHead
      include ContainerElement
    end

    class TableBody
      include ContainerElement
    end
    
    class TableRow
      include ContainerElement
    end

    class TableHeader
      include TextElement
    end

    class TableCell
      include TextElement
    end
    
  end
end
