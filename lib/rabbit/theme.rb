require 'delegate'
require "forwardable"

require 'rabbit/element'
require 'rabbit/image'

module Rabbit

  class Theme

    module Searcher
      SUFFIXES = ["", ".rabbit", ".rab", ".rb"]

      def initialize(*args, &blocks)
        @theme_stack = []
        super
      end

      def search_theme_file(theme_name=name)
        found_path = nil
        SUFFIXES.find do |suffix|
          theme_file_name = "#{theme_name}#{suffix}"
          begin
            in_theme(theme_name) do
              found_path = search_file(theme_file_name)
            end
          rescue LoadError
            found_path = nil
          end
          found_path
        end
        raise LoadError, "can't find theme: #{theme_name}." if found_path.nil?
        found_path
      end

      def search_file(target)
        found_path = nil
        @theme_stack.find do |theme_name|
          $LOAD_PATH.find do |path|
            base_name = File.join(path, 'rabbit', 'theme', theme_name, target)
            if File.exist?(base_name)
              found_path = base_name
              break
            end
            found_path
          end
        end
        if found_path.nil?
          raise LoadError,
                "can't find file in themes #{@theme_stack.inspect}: #{name}."
        end
        found_path
      end

      def push_theme(name)
        @theme_stack.push(name)
      end

      def pop_theme
        @theme_stack.pop
      end

      def in_theme(name)
        push_theme(name)
        yield(name)
        pop_theme
      end

    end

    extend Forwardable

    def_delegators(:@canvas, :logger)
    
    attr_reader :canvas, :name
    def initialize(canvas)
      @canvas = canvas
      @applier = Applier.new(self)
      apply("base")
    end

    def apply(name)
      @name = name
      begin
        @applier.apply_theme(name)
      rescue StandardError, LoadError
        logger.warn($!)
      end
    end
    
    def pages
      @canvas.pages
    end

    class ElementContainer < DelegateClass(Array)

      def collect(*args, &block)
        self.class.new(super)
      end

      def map(*args, &block)
        self.class.new(super)
      end

      def [](*args)
        result = super
        if result.is_a?(Array)
          self.class.new(result)
        else
          result
        end
      end

      def method_missing(meth, *args, &block)
        each do |elem|
          elem.__send__(meth, *args) do |*block_args|
            block.call(elem, *block_args)
          end
        end
      end

    end

    class Applier

      include Enumerable
      include Element
      include Searcher
      include Utils::Canvas

      NORMALIZED_WIDTH = 120.0
      NORMALIZED_HEIGHT = 90.0

      def initialize(theme)
        super()
        @theme = theme
        @match_cache = {}
        class << pages
          def elements
            self
          end
        end
      end

      def apply_theme(name)
        theme_path = search_theme_file(name)
        src = File.open(theme_path) do |f|
          f.read
        end
        in_theme(name) do
          instance_eval(src, theme_path)
        end
      end

      private
      def include_theme(name)
        apply_theme(name)
      end

      def name
        @theme.name
      end

      def pages
        @theme.pages
      end

      def canvas
        @theme.canvas
      end

      def set_foreground(color, can=canvas)
        can.set_foreground(make_color(can, color))
      end

      def set_background(color, can=canvas)
        can.set_background(make_color(can, color))
      end

      def font_families
        canvas.font_families.collect{|x| x.name}
      end

      def windows?
        # Gdk.windowing_win32? # what about this?
        /cygwin|mingw|mswin32|bccwin32/.match(RUBY_PLATFORM) ? true : false
      end

      def match(*paths, &block)
        block.call(ElementContainer.new(_match(pages, *paths)))
      end
      
      def _match(current, *paths)
        last_path_index = paths.size - 1
        paths.each_with_index do |path, i|
          current = _match_with_cache(current, path, i == last_path_index) do
            if path.nil?
              pages
            elsif path == "**"
              all_sub_elements(current)
            else
              if path == "*"
                working = current # all OK
              else
                working = current.find_all do |element|
                  path === element
                end
              end
              
              if i != last_path_index
                working.inject([]) do |result, elem|
                  if elem.respond_to?(:elements)
                    result + elem.elements
                  else
                    result << elem
                  end
                end
              else
                working
              end
              
            end
          end
        end
        current
      end

      def _match_with_cache(current, path, last_path)
        key = [current, path, last_path]
        @match_cache[key] ||= yield
        @match_cache[key]
      end

      def all_sub_elements(element)
        if element.respond_to?(:inject)
          if element.respond_to?(:elements)
            elems = element.elements
          else
            elems = element
          end
          elems.inject([]) do |result, elem|
            (result << elem) + all_sub_elements(elem)
          end
        else
          []
        end
      end

      def each(*paths_array, &block)
        paths_array.each do |paths|
          match(*paths, &block)
        end
      end

      def normalized_size(s)
        ((s / canvas.width.to_f) * NORMALIZED_WIDTH).ceil
      end

      def normalized_x(sx)
        ((sx / canvas.width.to_f) * NORMALIZED_WIDTH).ceil
      end

      def normalized_y(sy)
        ((sy / canvas.height.to_f) * NORMALIZED_HEIGHT).ceil
      end

      def screen_size(n)
        ((canvas.width * n) / NORMALIZED_WIDTH).ceil
      end

      def screen_x(nx)
        ((canvas.width * nx) / NORMALIZED_WIDTH).ceil
      end

      def screen_y(ny)
        ((canvas.height * ny) / NORMALIZED_HEIGHT).ceil
      end

      def indent(elements, size_or_proc)
        indent_with(elements, size_or_proc){}
      end

      def indent_with(elements, size_or_proc)
        elements.each do |element|
          ox, oy, ow, oh = []
          other_infos = []
          compiled = false
          element.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
            ox, oy, ow, oh = x, y, w, h
            if size_or_proc.respond_to?(:call)
              indent_size, *other_infos =
                    size_or_proc.call(element, canvas, x, y, w, h)
            else
              indent_size = size_or_proc
            end
            geometry = [x + indent_size, y, w - indent_size, h]
            if geometry[0] + element.simulated_width >
                            canvas.width - indent_size
              element.compile(canvas, x, *geometry[1..-1])
            end
            geometry
          end
          element.add_post_draw_proc do |canvas, x, y, w, h, simulation|
            unless simulation
              yield(element, canvas, ox, oy, ow, oh, x, y, w, h, *other_infos)
            end
            if compiled
              element.compile(canvas, ox, oy, ow, oh)
            end
            [ox, y, ow, h]
          end
        end
      end

      def draw_mark(items, indent_width, width_or_proc, height_or_proc)
        indent_with(items, indent_width) do
          |item, canvas, ox, oy, ow, oh, x, y, w, h|

          text_height = item.elements.first.original_height

          if width_or_proc.respond_to?(:call)
            mark_width = width_or_proc.call(item, canvas)
          else
            mark_width = width_or_proc
          end
          if height_or_proc.respond_to?(:call)
            mark_height = height_or_proc.call(item, canvas)
          else
            mark_height = height_or_proc
          end

          adjust_y = ((text_height / 2.0) - (mark_height / 2.0)).ceil

          start_x = ox + mark_width
          start_y = oy + adjust_y
          end_x = mark_width
          end_y = mark_height
          yield(item, canvas, start_x, start_y, end_x, end_y)
        end
      end

      def draw_image_mark(items, name)
        unless items.empty?
          
          loader = ImageLoader.new(search_file(name))

          width_proc = Proc.new {loader.width}
          height_proc = Proc.new {loader.height}
          indent_proc = Proc.new do |item, canvas, x, y, w, h|
            text_height = item.elements.first.original_height
            if text_height < loader.height
              loader.resize(nil, (text_height * 2.0 / 3.0).ceil)
            end
            loader.width * 2.5
          end
            
          draw_mark(items, indent_proc, width_proc, height_proc) do
            |item, canvas, start_x, start_y, end_x, end_y|
            draw_pixbuf(canvas, loader.pixbuf, start_x, start_y)
          end
        end
      end
      
      def draw_order(items, indent_width, &block)
        make_order_layout = Proc.new do |item, canvas, x, y ,w, h|
          str = block.call(item)
          layout, tw, th = make_layout(canvas, str)
          [tw + indent_width, tw, th, layout]
        end

        draw_order =
            Proc.new do |item, canvas, ox, oy, ow, oh, x, y, w, h, tw, th, layout|
          # text_height = item.elements.first.original_height
          # adjust_y = ((text_height - th) / 3.0).ceil
          adjust_y = 0

          new_x = ox + indent_width
          new_y = oy + adjust_y
          draw_layout(canvas, layout, new_x, new_y)
        end

        indent_with(items, make_order_layout, &draw_order)
      end

    end

  end

end
