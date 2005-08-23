require "erb"
require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"

module Rabbit
  module Renderer

    module Base
      include GetText
      include ERB::Util

      Color = Struct.new(:red, :green, :blue, :alpha, :have_alpha)

      class Color

        COLOR_NORMALIZE = 65535.0

        class << self
          def new_from_gdk_color(color, have_alpha=false)
            red = color.red / COLOR_NORMALIZE
            green = color.green / COLOR_NORMALIZE
            blue = color.blue / COLOR_NORMALIZE
            alpha = 1.0
            new(red, green, blue, alpha, have_alpha)
          end
        end

        alias have_alpha? have_alpha
        
        def to_s
          "#%02X%02X%02X" % to_a.collect{|color| (color * 255).round}
        end

        def to_a
          if have_alpha?
            [red, green, blue, alpha]
          else
            [red, green, blue]
          end
        end
      end

      attr_accessor :paper_width, :paper_height, :slides_per_page
      attr_accessor :left_margin, :right_margin
      attr_accessor :top_margin, :bottom_margin
      attr_accessor :progress_foreground
      attr_accessor :progress_background
      attr_writer :left_page_margin, :right_page_margin
      attr_writer :top_page_margin, :bottom_page_margin
      
      def initialize(canvas)
        @canvas = canvas
        @font_families = nil
        @paper_width = nil
        @paper_height = nil
        @slides_per_page = nil
        @left_margin = nil
        @right_margin = nil
        @top_margin = nil
        @bottom_margin = nil
        @left_page_margin = nil
        @right_page_margin = nil
        @top_page_margin = nil
        @bottom_page_margin = nil
        @progress_foreground = nil
        @progress_background = nil
        @list_id = 0
        init_hook_procs
      end

      def left_page_margin
        @left_page_margin || 0
      end
      
      def right_page_margin
        @right_page_margin || 0
      end
      
      def top_page_margin
        @top_page_margin || 0
      end
      
      def bottom_page_margin
        @bottom_page_margin || 0
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          @font_families = create_pango_context.list_families
        end
        @font_families
      end

      def print(&block)
        if printable?
          do_print(&block)
        else
          canvas = make_canvas_with_printable_renderer
          pre_print(canvas.slide_size)
          canvas.print do |i|
            printing(i)
          end
          post_print
        end
      end

      def cache_all_slides
        canvas = make_canvas_with_off_screen_renderer
        pre_cache_all_slides(canvas.slide_size)
        canvas.slides.each_with_index do |slide, i|
          canvas.move_to_if_can(i)
          slide.draw(canvas)
          caching_all_slides(i, canvas)
        end
        post_cache_all_slides(canvas)
      end
      
      def redraw
      end
      
      def each_slide_pixbuf
        canvas = off_screen_canvas
        previous_index = canvas.current_index
        pre_to_pixbuf(canvas.slide_size)
        canvas.slides.each_with_index do |slide, i|
          to_pixbufing(i)
          yield(canvas.to_pixbuf(i), i)
        end
        post_to_pixbuf
        canvas.move_to_if_can(previous_index)
      end

      def off_screen_canvas
        if can_create_pixbuf?
          @canvas
        else
          make_canvas_with_off_screen_renderer
        end
      end

      def create_pango_context
        Pango::Context.new
      end
      
      def printable?
        false
      end

      def quit_confirm
        @canvas.quit
      end

      def draw_flag(x, y, pole_height, params)
        if params["flag_type"] == "triangle"
          draw_triangle_flag(x, y, pole_height, params)
        else
          draw_rectangle_flag(x, y, pole_height, params)
        end
      end
      
      def draw_triangle_flag(x, y, pole_height, params)
        params = setup_flag_params(pole_height, 1.5, params)

        layout = params["layout"]
        text_width = params["text_width"]
        text_height = params["text_height"]
        pole_width = params["pole_width"]
        pole_color = params["pole_color"]
        flag_height = params["flag_height"]
        flag_width = params["flag_width"]
        flag_color = params["flag_color"]
        flag_frame_width = params["flag_frame_width"]
        flag_frame_color = params["flag_frame_color"]

        draw_rectangle(true, x, y, pole_width, pole_height, pole_color)

        base_x = x + pole_width
        draw_polygon(true,
                     [
                       [base_x, y],
                       [base_x + flag_width, y + flag_height / 2],
                       [base_x, y + flag_height],
                     ],
                     flag_frame_color)
        draw_polygon(true,
                     [
                       [base_x, y + flag_frame_width],
                       [
                         base_x + flag_width - 2 * flag_frame_width,
                         y + flag_height / 2
                       ],
                       [
                         base_x,
                         y + flag_height - flag_frame_width
                       ],
                     ],
                     flag_color)

        if layout
          args = [
            layout, x, y, pole_width, flag_width * 0.8,
            text_height, flag_height,
          ]
          draw_flag_layout(*args)
        end
      end

      def draw_rectangle_flag(x, y, pole_height, params)
        params = setup_flag_params(pole_height, 1.3, params)

        layout = params["layout"]
        text_width = params["text_width"]
        text_height = params["text_height"]
        pole_width = params["pole_width"]
        pole_color = params["pole_color"]
        flag_height = params["flag_height"]
        flag_width = params["flag_width"]
        flag_color = params["flag_color"]
        flag_frame_width = params["flag_frame_width"]
        flag_frame_color = params["flag_frame_color"]

        draw_rectangle(true, x, y, pole_width, pole_height, pole_color)

        base_x = x + pole_width
        draw_rectangle(true,
                       base_x,
                       y,
                       flag_width,
                       flag_height,
                       flag_frame_color)
        draw_rectangle(true,
                       base_x,
                       y + flag_frame_width,
                       flag_width - flag_frame_width,
                       flag_height - 2 * flag_frame_width,
                       flag_color)

        if layout
          args = [
            layout, x, y, pole_width, flag_width - 2 * flag_frame_width,
            text_height, flag_height,
          ]
          draw_flag_layout(*args)
        end
      end

      def draw_flag_layout(layout, x, y, pole_width, flag_width,
                           text_height, flag_height)
        base_x = x + pole_width
        layout.width = flag_width * Pango::SCALE
        layout.alignment = Pango::Layout::ALIGN_CENTER
        base_y = y
        if text_height < flag_height
          base_y += (flag_height - text_height) / 2
        end
        draw_layout(layout, base_x, base_y)
      end

      def flag_size(pole_height, params)
        params = setup_flag_params(pole_height, 1.5, params)
        [params["pole_width"] + params["flag_width"], pole_height]
      end
      
      def draw_cube(filled, x, y, z, size, color=nil)
        not_support_method("draw_cube")
      end
      
      def draw_sphere(filled, x, y, z, radius, slices, stacks, color=nil)
        not_support_method("draw_sphere")
      end
      
      def draw_cone(filled, x, y, z, base, height, slices, stacks, color=nil)
        not_support_method("draw_cone")
      end
      
      def draw_torus(filled, x, y, z, inner_radius, outer_radius,
                     n_sides, rings, color=nil)
        not_support_method("draw_torus")
      end
      
      def draw_tetrahedron(filled, x, y, z, color=nil)
        not_support_method("draw_tetrahedron")
      end
      
      def draw_octahedron(filled, x, y, z, color=nil)
        not_support_method("draw_octahedron")
      end
      
      def draw_dodecahedron(filled, x, y, z, color=nil)
        not_support_method("draw_dodecahedron")
      end
      
      def draw_icosahedron(filled, x, y, z, color=nil)
        not_support_method("draw_icosahedron")
      end
      
      def draw_teapot(filled, x, y, z, scale, color=nil)
        not_support_method("draw_teapot")
      end

      def gl_compile(id)
        not_support_method("gl_compile")
      end

      def gl_call_list(id, x, y, z, color=nil)
        not_support_method("gl_call_list")
      end

      def gl_supported?
        false
      end

      def setup_event
      end
      
      def z_far
        100.0
      end

      def z_view
        5.0
      end

      def new_list_id
        @list_id += 1
        @list_id
      end

      def to_attrs(hash)
        hash.collect do |key, value|
          if value
            "#{h key}='#{h value}'"
          else
            nil
          end
        end.compact.join(" ")
      end

      def add_motion_notify_hook(type, hook=Proc.new)
        @motion_notify_hook_procs[type] << hook
      end
      
      def clear_motion_notify_hook(type)
        @motion_notify_hook_procs[type].clear
      end
      
      def add_button_press_hook(type, hook=Proc.new)
        @button_press_hook_procs[type] << hook
      end
      
      def clear_button_press_hook(type)
        @button_press_hook_procs[type].clear
      end
      
      def clear_hooks
        init_hook_procs
      end
      
      private
      def can_create_pixbuf?
        false
      end
      
      def do_print(&block)
        pre_print(@canvas.slide_size)
        @canvas.slides.each_with_index do |slide, i|
          @canvas.move_to_if_can(i)
          @canvas.current_slide.draw(@canvas)
          block.call(i) if block
        end
        post_print
      end

      def make_canvas_with_renderer(renderer)
        canvas = Canvas.new(@canvas.logger, renderer)
        yield canvas
        canvas.apply_theme(@canvas.theme_name)
        @canvas.source_force_modified(true) do |source|
          canvas.parse_rd(source)
        end
        canvas.toggle_index_mode if @canvas.index_mode?
        canvas
      end
      
      def make_canvas_with_printable_renderer
        renderer = Renderer.printable_renderer(@canvas.slides_per_page)
        make_canvas_with_renderer(renderer) do |canvas|
          canvas.filename = @canvas.filename
          setup_margin(canvas)
          setup_page_margin(canvas)
          setup_paper_size(canvas)
          canvas.slides_per_page = @canvas.slides_per_page
        end
      end
      
      def make_canvas_with_off_screen_renderer
        make_canvas_with_renderer(Pixmap) do |canvas|
          canvas.width = @canvas.width
          canvas.height = @canvas.height
        end
      end

      def setup_margin(canvas)
        canvas.left_margin = @canvas.left_margin
        canvas.right_margin = @canvas.right_margin
        canvas.top_margin = @canvas.top_margin
        canvas.bottom_page_margin = @canvas.bottom_page_margin
      end

      def setup_page_margin(canvas)
        canvas.left_page_margin = @canvas.left_page_margin
        canvas.right_page_margin = @canvas.right_page_margin
        canvas.top_page_margin = @canvas.top_page_margin
        canvas.bottom_page_margin = @canvas.bottom_page_margin
      end

      def setup_paper_size(canvas)
        if @canvas.paper_width and @canvas.paper_height
          canvas.paper_width = @canvas.paper_width
          canvas.paper_height = @canvas.paper_height
        else
          canvas.paper_width = @canvas.width
          canvas.paper_height = @canvas.height
        end
      end

      def setup_flag_params(pole_height, default_flag_width_ratio, params)
        params = params.dup
        
        text = params["text"]
        text_attrs = params["text_attributes"] || {}
        if text
          markupped_text = "<span #{to_attrs(text_attrs)}>#{text}</span>"
          layout, text_width, text_height = make_layout(markupped_text)
          params["layout"] = layout
          params["text_width"] = text_width
          params["text_height"] = text_height
          flag_width_default = [
            text_width * default_flag_width_ratio,
            pole_height / 2
          ].max
          flag_height_default = [text_height, flag_width_default].max
        else
          params["layout"] = nil
          flag_width_default = flag_height_default = nil
        end
        
        params["pole_width"] = params["pole_width"] || 2
        params["pole_color"] ||= "black"
        flag_height = params["flag_height"] ||
          flag_height_default || pole_height / 2
        flag_height = [flag_height, pole_height].min
        params["flag_height"] = flag_height
        params["flag_width"] ||= flag_width_default || flag_height
        params["flag_color"] ||= "red"
        params["flag_frame_width"] ||= params["pole_width"]
        params["flag_frame_color"] ||= params["pole_color"]

        params
      end

      def not_support_method(name)
        format = _("%s does not support: %s")
        msg = format % [self.class.name, name]
        @canvas.logger.warn(msg)
      end

      def init_hook_procs
        @motion_notify_hook_procs = {
          Gdk::Window::BUTTON1_MASK => [],
          Gdk::Window::BUTTON2_MASK => [],
          Gdk::Window::BUTTON3_MASK => [],
        }
        @button_press_hook_procs = {
          Gdk::Event::Type::BUTTON_PRESS => [],
          Gdk::Event::Type::BUTTON2_PRESS => [],
          Gdk::Event::Type::BUTTON3_PRESS => [],
        }
      end
    end
    
  end
end
