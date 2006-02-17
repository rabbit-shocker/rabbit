require 'delegate'
require 'forwardable'

require 'rabbit/utils'
require 'rabbit/element'
require 'rabbit/image'
require 'rabbit/theme/searcher'
require 'rabbit/renderer/color'

module Rabbit
  module Theme
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

      def indent(size_or_proc, name=nil)
        each do |element|
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

      def draw_frame(params={}, &block)
        proc_name = params[:proc_name] || "draw_frame"
        frame_color = params[:frame_color]
        fill_color = params[:fill_color]
        shadow_color = params[:shadow_color]
        shadow_offset = params[:shadow_offset] || 2
        shadow_width = params[:shadow_width] || 4
        frame_width = params[:frame_width] || 1

        add_pre_draw_proc(proc_name) do |target, canvas, x, y, w, h, simulation|
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
              args = size + [frame_color, {:line_width => frame_width}]
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

      def method_missing(meth, *args, &block)
        each do |elem|
          if block
            proxy_block = Proc.new do |*block_args|
              block.call(elem, *block_args)
            end
          else
            proxy_block = nil
          end
          elem.__send__(meth, *args, &proxy_block)
        end
      end
    end

    class Applier
      include Element
      include Searcher
      include DirtyCount

      extend Forwardable

      logger_methods = [:debug, :info, :warn, :error, :fatal, :unknown]
      def_delegators(:logger, *logger_methods)
      private *logger_methods

      NORMALIZED_WIDTH = 91.0
      NORMALIZED_HEIGHT = 67.5

      def initialize(theme, &callback)
        super()
        @theme = theme
        @callback = callback
        dirty_count_clean
        @match_cache = {}
        @current_target = nil
        class << slides
          def elements
            self
          end
        end
      end

      def apply_theme(name)
        entry = find_theme(name)
        src = File.open(entry.theme_file) do |f|
          f.read
        end
        in_theme(entry) do
          instance_eval(normalize_source(src), entry.theme_file)
        end
      end

      private
      def normalize_source(src)
        src.gsub(/(?=^|\W)@(very_)?huge_(script_)?font_size(?=$|\W)/) do |x|
          x = "x"
          x *= 2 unless $1.nil?
          "@#{x}_large_#{$2}font_size"
        end
      end
      
      def include_theme(name)
        begin
          apply_theme(name)
        rescue ThemeExit
          info($!.message) if $!.have_message?
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

      def logger
        canvas.logger
      end
      
      def print?
        canvas.printable?
      end

      def display?
        canvas.display?
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
        loader = ImageLoader.new(find_file(filename))
        canvas.background_image = loader.pixbuf
      end

      def set_progress_foreground(color)
        canvas.progress_foreground = canvas.make_color(color)
      end

      def set_progress_background(color)
        canvas.progress_background = canvas.make_color(color)
      end

      def set_graffiti_color(color)
        canvas.graffiti_color = Renderer::Color.parse(color)
      end

      def set_graffiti_line_width(line_width)
        canvas.graffiti_line_width = line_width
      end

      def add_gesture_action(sequence, action=Proc.new)
        canvas.add_gesture_action(sequence, action)
      end

      def activate(name, &block)
        canvas.activate(name, &block)
      end

      def font_families
        canvas.font_families.collect{|x| x.name}
      end

      def set_font_family(target, family=@font_family)
        target.prop_set("font_family", family) if family
      end
      
      def windows?
        Utils.windows?
      end

      def match(*paths, &block)
        dirty
        begin
          @current_target = ElementContainer.new(_match(slides, *paths))
          block.call(@current_target)
        ensure
          @current_target = nil
        end
      end

      def method_missing(meth, *args, &block)
        if @current_target
          @current_target.__send__(meth, *args, &block)
        else
          super
        end
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

      def normalized_width
        (canvas.x_dpi / 72.0) * NORMALIZED_WIDTH
      end

      def normalized_height
        (canvas.y_dpi / 72.0) * NORMALIZED_HEIGHT
      end

      def normalized_size(s)
        ((s / canvas.width.to_f) * normalized_width).ceil
      end

      def normalized_x(sx)
        ((sx / canvas.width.to_f) * normalized_width).ceil
      end

      def normalized_y(sy)
        ((sy / canvas.height.to_f) * normalized_height).ceil
      end

      def screen_size(n)
        ((canvas.width * n) / normalized_width).ceil
      end

      def screen_x(nx)
        ((canvas.width * nx) / normalized_width).ceil
      end

      def screen_y(ny)
        ((canvas.height * ny) / normalized_height).ceil
      end

      def draw_mark(items, indent_width, width_or_proc, height_or_proc, name=nil)
        items.indent(indent_width, name) do |item, canvas, x, y, w, h|
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

          indent_base_x = item.x - mark_width
          indent_base_y = item.base_y + first_text.margin_top + adjust_y
          width = mark_width
          height = mark_height
          yield(item, canvas, indent_base_x, indent_base_y, width, height)
        end
      end

      def draw_image_mark(items, image_name, name=nil)
        unless items.empty?
          
          loader = ImageLoader.new(find_file(image_name))

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
            |item, canvas, x, y, w, h|
            x -= loader.width * 0.5
            canvas.draw_pixbuf(loader.pixbuf, x, y)
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

        items.indent(make_order_layout, name, &draw_order)
      end

      def draw_frame(*args, &block)
        if args.empty? or (args.size == 1 and args.first.is_a?(Hash))
          targets = @current_target
        else
          targets, *args = args
        end

        unless targets.is_a?(ElementContainer)
          targets = ElementContainer.new([targets])
        end

        targets.draw_frame(*args, &block)
      end
      def start_auto_reload_timer(interval)
        canvas.start_auto_reload_timer(interval)
      end

      def stop_auto_reload_timer
        canvas.stop_auto_reload_timer
      end

      # For backward compatibility
      def start_auto_reload_thread(interval)
        format = _("%s is deprecated. Use %s instead.")
        message = format % ["start_auto_reload_thread",
                            "start_auto_reload_timer"]
        warn(message)
        start_auto_reload_timer(interval)
      end
      def stop_auto_reload_thread
        format = _("%s is deprecated. Use %s instead.")
        message = format % ["stop_auto_reload_thread",
                            "stop_auto_reload_timer"]
        warn(message)
        stop_auto_reload_timer
      end

      def dirtied
        @callback.call if @callback
        super
      end

      def connect_key(keyval, modifier=nil, flags=nil, &block)
        modifier ||= Gdk::Window::ModifierType.new
        flags ||= Gtk::AccelFlags::VISIBLE
        canvas.connect_key(keyval, modifier, flags, &block)
      end

      def disconnect_key(keyval, modifier=nil)
        canvas.disconnect_key(keyval, modifier)
      end
    end
  end
end
