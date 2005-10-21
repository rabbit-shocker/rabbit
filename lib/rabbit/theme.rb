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

      module_function
      def theme_dir(base_dir)
        File.join(base_dir, 'rabbit', 'theme')
      end
      
      def search_file(target, themes=@theme_stack)
        found_path = nil
        themes.find do |theme_name|
          $LOAD_PATH.find do |path|
            base_name = File.join(theme_dir(path), theme_name, target)
            if File.exist?(base_name)
              found_path = base_name
              break
            end
            found_path
          end
        end
        if found_path.nil?
          raise LoadError,
                "can't find file in themes #{themes.inspect}: #{name}."
        end
        found_path
      end

      def collect_theme
        themes = []
        $LOAD_PATH.each do |path|
          base_name = theme_dir(path)
          if File.directory?(base_name)
            Dir.foreach(base_name) do |theme|
              next if /\A..?\z/ =~ theme
              SUFFIXES.each do |suffix|
                name = File.join(base_name, theme, "#{theme}#{suffix}")
                if File.exist?(name)
                  themes << theme
                  break
                end
              end
            end
          end
        end
        themes.uniq.sort
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
      rescue ThemeExit
        logger.info($!.message) if $!.have_message?
      rescue StandardError, LoadError, SyntaxError
        logger.warn($!)
      end
    end
    
    def slides
      @canvas.slides
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

      NORMALIZED_WIDTH = 120.0
      NORMALIZED_HEIGHT = 90.0

      def initialize(theme)
        super()
        @theme = theme
        @match_cache = {}
        class << slides
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
        begin
          apply_theme(name)
        rescue ThemeExit
          logger.info($!.message) if $!.have_message?
        end
      end

      def to_attrs(hash)
        canvas.to_attrs(hash)
      end

      def to_element_container(obj)
        if obj.is_a?(ElementContainer)
          obj
        else
          ElementContainer.new([obj])
        end
      end
      
      def name
        @theme.name
      end

      def slides
        @theme.slides
      end

      def canvas
        @theme.canvas
      end

      def print?
        canvas.renderer.printable?
      end

      def display?
        canvas.renderer.display?
      end

      def theme_exit(message=nil)
        raise ThemeExit.new(message)
      end
      
      def slides_per_page
        canvas.slides_per_page
      end
      
      def set_foreground(color)
        canvas.foreground = canvas.make_color(color)
      end

      def set_background(color)
        canvas.background = canvas.make_color(color)
      end

      def set_background_image(filename)
        loader = ImageLoader.new(search_file(filename))
        canvas.background_image = loader.pixbuf
      end

      def set_progress_foreground(color)
        canvas.progress_foreground = canvas.make_color(color)
      end

      def set_progress_background(color)
        canvas.progress_background = canvas.make_color(color)
      end

      def font_families
        canvas.font_families.collect{|x| x.name}
      end

      def set_font_family(target, family=@font_family)
        target.prop_set("font_family", family) if family
      end
      
      def windows?
        # Gdk.windowing_win32? # what about this?
        /cygwin|mingw|mswin32|bccwin32/.match(RUBY_PLATFORM) ? true : false
      end

      def match(*paths, &block)
        block.call(ElementContainer.new(_match(slides, *paths)))
      end
      
      def _match(current, *paths)
        last_path_index = paths.size - 1
        paths.each_with_index do |path, i|
          current = _match_with_cache(current, path, i == last_path_index) do
            if path.nil?
              slides
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

      def indent(elements, size_or_proc, name=nil)
        elements.each do |element|
          element.delete_pre_draw_proc_by_name(name)
          element.delete_post_draw_proc_by_name(name)

          other_infos = []
          element.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
            if size_or_proc.respond_to?(:call)
              indent_size, *other_infos = size_or_proc.call(element, simulation)
            else
              indent_size = size_or_proc
            end
            element.margin_left = indent_size
            [x, y, w, h]
          end
          
          if block_given?
            element.add_post_draw_proc(name) do |canvas, x, y, w, h, simulation|
              unless simulation
                yield(element, canvas, x, y, w, h, *other_infos)
              end
              [x, y, w, h]
            end
          end
        end
      end

      def draw_mark(items, indent_width, width_or_proc, height_or_proc, name=nil)
        indent(items, indent_width, name) do |item, canvas, x, y, w, h|
          first_text = item.elements.first
          text_height = first_text.first_line_height
          text_height += first_text.padding_top + first_text.padding_bottom

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

          start_x = item.base_x + mark_width
          start_y = item.base_y + first_text.margin_top + adjust_y
          end_x = mark_width
          end_y = mark_height
          yield(item, canvas, start_x, start_y, end_x, end_y)
        end
      end

      def draw_image_mark(items, image_name, name=nil)
        unless items.empty?
          
          loader = ImageLoader.new(search_file(image_name))

          width_proc = Proc.new {loader.width}
          height_proc = Proc.new {loader.height}
          indent_proc = Proc.new do |item, simulation|
            text_height = item.elements.first.original_height
            if text_height < loader.height
              loader.resize(nil, (text_height * 2.0 / 3.0).ceil)
            end
            loader.width * 2.5
          end
            
          draw_mark(items, indent_proc, width_proc, height_proc, name) do
            |item, canvas, start_x, start_y, end_x, end_y|
            canvas.draw_pixbuf(loader.pixbuf, start_x, start_y)
          end
        end
      end
      
      def draw_order(items, indent_width, name=nil, &block)
        layouts = {}
        make_order_layout = Proc.new do |item, simulation|
          layout = layouts[item]
          if layout.nil?
            str = block.call(item)
            layout = canvas.make_layout(str)
            layouts[item] = layout
          end
          tw, th = layout.pixel_size
          [tw + indent_width, tw, th, layout]
        end

        draw_order = Proc.new do |item, canvas, x, y, w, h, tw, th, layout|
          first_text = item.elements.first
          text_height = first_text.original_height
          text_height += first_text.padding_top + first_text.padding_bottom
          adjust_y = ((text_height / 2.0) - (th / 2.0)).ceil

          new_x = item.base_x + indent_width
          new_y = item.base_y + first_text.margin_top + adjust_y
          canvas.draw_layout(layout, new_x, new_y)
        end

        indent(items, make_order_layout, name, &draw_order)
      end

      def draw_frame(targets, params={})
        proc_name = params[:proc_name] || "draw_frame"
        frame_color = params[:frame_color]
        fill_color = params[:fill_color]
        shadow_color = params[:shadow_color]
        shadow_offset = params[:shadow_offset] || 2
        shadow_width = params[:shadow_width] || 4
        frame_width = 1

        unless targets.is_a?(ElementContainer)
          targets = ElementContainer.new([targets])
        end
        
        targets.add_pre_draw_proc(proc_name) do |target, canvas, x, y, w, h, simulation|
          unless simulation
            if block_given?
              fx, fy, fw, fh = yield(target, canvas, x, y, w, h)
            end
            fx ||= target.x
            fy ||= target.y + target.centering_adjusted_height
            fw ||= target.width
            fh ||= target.height
            if shadow_color
              fh -= shadow_width
            end
            size = [fx, fy, fw, fh]

            if fill_color
              args = size + [fill_color]
              canvas.draw_rectangle(true, *args)
            end
            
            if frame_color
              args = size + [frame_color]
              canvas.draw_rectangle(false, *args)
            end

            if shadow_color
              # Under Shadow
              usx = fx + shadow_offset
              usy = fy + fh + frame_width
              usw = fw + shadow_width - shadow_offset
              ush = shadow_width
              canvas.draw_rectangle(true, usx, usy, usw, ush, shadow_color)
      
              # Right Shadow
              rsx = fx + fw + frame_width
              rsy = fy + shadow_offset
              rsw = shadow_width
              rsh = fh + shadow_width - shadow_offset
              canvas.draw_rectangle(true, rsx, rsy, rsw, rsh, shadow_color)
            end
          end
          [x, y, w, h]
        end
      end

      def start_auto_reload_thread(interval)
        canvas.start_auto_reload_thread(interval)
      end

      def stop_auto_reload_thread
        canvas.stop_auto_reload_thread
      end
    end
  end
end
