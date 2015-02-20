require 'delegate'
require 'forwardable'

require 'rabbit/utils'
require 'rabbit/element'
require 'rabbit/image'
require 'rabbit/theme/searcher'
require 'rabbit/renderer/color'
require "rabbit/size"

module Rabbit
  module Theme
    class ElementContainer < Array
      def initialize(applier, ary)
        @applier = applier
        super(ary)
      end

      def collect(*args, &block)
        @applier.make_container(super)
      end

      def map(*args, &block)
        @applier.make_container(super)
      end

      def [](*args)
        result = super
        if result.is_a?(Array)
          @applier.make_container(result)
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
            if simulation
              if size_or_proc.respond_to?(:call)
                result = size_or_proc.call(element, simulation)
                indent_size, *other_infos = result
              else
                indent_size = size_or_proc
              end
              element.margin_left = indent_size
            end
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

      def draw_mark(indent_width, width_or_proc, height_or_proc, name=nil)
        indent(indent_width, name) do |item, canvas, x, y, w, h|
          first_text = item.elements.first
          if first_text
            text_height = first_text.first_line_height
            text_height += first_text.padding_top + first_text.padding_bottom
          else
            text_height = item.height
          end

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
          indent_base_y = item.base_y + adjust_y
          indent_base_y += first_text.margin_top if first_text
          width = mark_width
          height = mark_height
          yield(item, canvas, indent_base_x, indent_base_y, width, height)
        end
      end

      def draw_image_mark(image_name, name=nil, options={})
        return if empty?

        loader = ImageLoader.new(image_name)

        width_proc = Proc.new {loader.width}
        height_proc = Proc.new {loader.height}
        custom_indent = options[:indent]
        indent_proc = Proc.new do |item, simulation|
          text_height = item.elements.first.original_height
          if text_height < loader.height
            loader.resize(nil, (text_height * 2.0 / 3.0).ceil)
          end
          if custom_indent.nil?
            loader.width * 2.5
          elsif custom_indent.respond_to?(:call)
            custom_indent.call(item, loader)
          else
            custom_indent
          end
        end

        draw_mark(indent_proc,
                  width_proc, height_proc,
                  name) do  |item, canvas, x, y, w, h|
          x -= loader.width * 0.5
          loader.draw(canvas, x, y)
        end
      end

      def draw_order(indent_width, name=nil, &block)
        layouts = {}
        make_order_layout = Proc.new do |item, simulation|
          layout = layouts[item]
          if layout.nil?
            str = block.call(item)
            layout = @applier.make_layout(str)
            layouts[item] = layout
          end
          tw, th = layout.pixel_size
          [tw + indent_width, tw, th, layout]
        end

        draw_order = Proc.new do |item, canvas, x, y, w, h, tw, th, layout|
          first_text = item.elements.first
          text_height = first_text.first_line_height
          text_height += first_text.padding_top + first_text.padding_bottom
          adjust_y = ((text_height / 2.0) - (th / 2.0)).ceil

          new_x = item.base_x + indent_width
          new_y = item.base_y + first_text.margin_top + adjust_y
          canvas.draw_layout(layout, new_x, new_y)
        end

        indent(make_order_layout, name, &draw_order)
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
        collect do |elem|
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
      include GetText

      extend Forwardable

      logger_methods = [:debug, :info, :warning, :error, :fatal, :unknown]
      def_delegators(:logger, *logger_methods)
      def_delegators(:logger, :warn) # for backward compatibility
      private *logger_methods

      def_delegators(:canvas, :make_layout)

      # Those constants are meaningless. :p
      NORMALIZED_WIDTH = 91.0 * 96
      NORMALIZED_HEIGHT = 67.5 * 96

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
        src = File.open(entry.theme_file, "r:utf-8") do |f|
          f.read
        end
        in_theme(entry) do
          instance_eval(normalize_source(src), entry.theme_file)
        end
      end

      def make_container(ary)
        ElementContainer.new(self, ary)
      end

      def to_container(obj)
        if obj.is_a?(ElementContainer)
          obj
        else
          make_container([obj])
        end
      end

      def normalized_width
        NORMALIZED_WIDTH / Canvas::INTERNAL_DPI
      end

      def normalized_height
        NORMALIZED_HEIGHT / Canvas::INTERNAL_DPI
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

      def [](name)
        instance_variable_get("@#{name}")
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

      def set_progress_foreground(*color)
        canvas.progress_foreground = canvas.make_color(*color)
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
        canvas.font_families.collect {|family| family.name}
      end

      def find_font_family(target_name)
        families = font_families.grep(/#{Regexp.escape(target_name)}/i)
        return nil if families.empty?
        if families.include?(target_name)
          target_name
        else
          families.first
        end
      end

      def set_font_family(target, family=@font_family)
        target.prop_set("font_family", family) if family
      end

      def set_font_resolution_ratio(ratio)
        canvas.font_resolution_ratio = ratio
      end

      def windows?
        Utils.windows?
      end

      def quartz?
        Utils.quartz?
      end

      def match(*paths, &block)
        dirty
        targets = _match(slides, *paths)
        return if targets.empty?

        begin
          @current_target = make_container(targets)
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
            current = ignore_wait_block(current) unless i == last_path_index
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

      def ignore_wait_block(elements)
        elements.inject([]) do |result, element|
          if element.is_a?(WaitBlock)
            result + ignore_wait_block(element.elements)
          else
            result + [element]
          end
        end
      end

      def indent(*args, &block)
        split_targets(args) do |targets, args|
          targets.indent(*args, &block)
        end
      end

      def draw_mark(*args, &block)
        split_targets(args) do |targets, args|
          targets.draw_mark(*args, &block)
        end
      end

      def draw_image_mark(*args, &block)
        split_targets(args) do |targets, args|
          image_name, *args = args
          image_name = find_file(image_name)
          targets.draw_image_mark(image_name, *args, &block)
        end
      end

      def draw_order(*args, &block)
        split_targets(args) do |targets, args|
          targets.draw_order(*args, &block)
        end
      end

      def draw_frame(*args, &block)
        split_targets(args) do |targets, args|
          targets.draw_frame(*args, &block)
        end
      end

      def split_targets(args)
        if args.empty? or
            !(args.first.is_a?(Element::Base) or
              args.first.is_a?(ElementContainer))
          targets = @current_target
        else
          targets, *args = args
        end

        yield [to_container(targets), args]
      end

      def start_auto_redraw_timer(interval)
        canvas.start_auto_redraw_timer(interval)
      end
      def stop_auto_redraw_timer
        canvas.stop_auto_redraw_timer
      end

      # For backward compatibility
      def start_auto_reload_timer(interval)
        deprecated_method("start_auto_redraw_timer",
                          "start_auto_reload_timer")
        canvas.start_auto_redraw_timer(interval)
      end
      def stop_auto_reload_timer
        deprecated_method("stop_auto_redraw_timer",
                          "stop_auto_reload_timer")
        canvas.stop_auto_redraw_timer
      end
      def start_auto_reload_thread(interval)
        deprecated_method("start_auto_redraw_timer",
                          "start_auto_reload_thread")
        start_auto_redraw_timer(interval)
      end
      def stop_auto_reload_thread
        deprecated_method("stop_auto_redraw_timer",
                          "stop_auto_reload_thread")
        stop_auto_redraw_timer
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

      def deprecated_method(current, deprecated)
        format = _("%s is deprecated. Use %s instead.")
        message = format % [deprecated, current]
        warning(message)
      end

      def tag(name, attributes, content)
        "<#{name} #{to_attrs(attributes)}>#{content}</#{name}>"
      end

      def span(attributes, content)
        tag("span", attributes, content)
      end

      def entity(key)
        Parser::Ext::Entity::TABLE[key]
      end

      def image_element(path, properties={})
        image = Parser::Ext::Image.make_image(canvas, path, properties)
        if image.nil?
          raise ImageFileDoesNotExistError.new(path)
        end
        image
      end

      def base_directory
        canvas.full_path(".")
      end

      def theme_load_path
        super + [base_directory].compact
      end

      def image_load_path
        super + [base_directory].compact
      end
    end
  end
end
