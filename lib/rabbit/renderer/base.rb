require "forwardable"
require "erb"
require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"
require "rabbit/renderer/color"

module Pango
  class Context
    unless method_defined?(:families)
      alias families list_families
    end
  end
end

module Rabbit
  module Renderer
    module Base
      include GetText
      include DirtyCount
      include ERB::Util

      extend Forwardable
      
      def_delegators(:@canvas, :quit, :reload_theme, :reload_source)
      
      attr_reader :keys, :x_dpi, :y_dpi
      attr_accessor :paper_width, :paper_height, :slides_per_page
      attr_accessor :margin_left, :margin_right
      attr_accessor :margin_top, :margin_bottom
      attr_accessor :progress_foreground
      attr_accessor :progress_background
      attr_accessor :adjustment_x, :adjustment_y
      attr_writer :margin_page_left, :margin_page_right
      attr_writer :margin_page_top, :margin_page_bottom
      
      def initialize(canvas)
        super()
        @canvas = canvas
        @font_families = nil
        @paper_width = nil
        @paper_height = nil
        @slides_per_page = nil
        @margin_left = nil
        @margin_right = nil
        @margin_top = nil
        @margin_bottom = nil
        @margin_page_left = nil
        @margin_page_right = nil
        @margin_page_top = nil
        @margin_page_bottom = nil
        @white_out = false
        @black_out = false
        @list_id = 0
        @adjustment_x = 0
        @adjustment_y = 0
        clean
        clear_progress_color
        init_hook_procs
        init_dpi
      end

      def margin_page_left
        @margin_page_left || 0
      end
      
      def margin_page_right
        @margin_page_right || 0
      end
      
      def margin_page_top
        @margin_page_top || 0
      end
      
      def margin_page_bottom
        @margin_page_bottom || 0
      end
      
      def font_families
        if @font_families.nil? or @font_families.empty?
          @font_families = create_pango_context.families
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
          canvas.quit
        end
      end

      def cache_all_slides
        canvas = make_canvas_with_off_screen_renderer
        pre_cache_all_slides(canvas.slide_size)
        canceled = false
        canvas.slides.each_with_index do |slide, i|
          canvas.move_to_if_can(i)
          slide.draw(canvas)
          unless caching_all_slides(i, canvas)
            canceled = true
            break
          end
        end
        post_cache_all_slides(canvas, canceled)
        canvas.quit
      end
      
      def redraw
      end

      def reset_adjustment
        @adjustment_x = 0
        @adjustment_y = 0
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
        canvas.quit if canvas != @canvas
      end

      def off_screen_canvas
        if off_screen_renderer?
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

      def display?
        false
      end

      def confirm_quit
        quit
      end

      def draw_circle(filled, x, y, w, h, color=nil, params={})
        draw_arc(filled, x, y, w, h, 0, 360, color, params)
      end
      
      def draw_circle_by_radius(filled, x, y, r, color=nil, params={})
        draw_arc_by_radius(filled, x, y, r, 0, 360, color, params)
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

      def setup_event(area)
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

      def add_motion_notify_hook(hook=Proc.new)
        @motion_notify_hook_procs << hook
      end
      
      def clear_motion_notify_hook
        @motion_notify_hook_procs.clear
      end
      
      def add_button_press_hook(hook=Proc.new)
        @button_press_hook_procs << hook
      end
      
      def clear_button_press_hook
        @button_press_hook_procs.clear
      end
      
      def add_button_release_hook(hook=Proc.new)
        @button_release_hook_procs << hook
      end
      
      def clear_button_release_hook
        @button_release_hook_procs.clear
      end
      
      def clear_hooks
        init_hook_procs
      end

      def clean
        dirty_count_clean
      end

      def clean_if_dirty
        clean if dirty?
      end

      def clear_theme
        clear_keys
        clear_progress_color
      end

      def white_outing?
        @white_out
      end

      def black_outing?
        @black_out
      end

      def toggle_white_out
        @black_out = false
        @white_out = !@white_out
      end

      def toggle_black_out
        @black_out = !@black_out
        @white_out = false
      end

      def toggle_comment_frame
      end

      def toggle_comment_view
      end

      def showing_comment_frame?
        false
      end

      def showing_comment_view?
        false
      end

      def comment_frame_available?
        false
      end

      def comment_view_available?
        false
      end

      def update_comment(source)
      end
      
      def post_init_gui
      end

      def graffiti_mode?
        false
      end
      
      private
      def off_screen_renderer?
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
        canvas.margin_left = @canvas.margin_left
        canvas.margin_right = @canvas.margin_right
        canvas.margin_top = @canvas.margin_top
        canvas.margin_page_bottom = @canvas.margin_page_bottom
      end

      def setup_page_margin(canvas)
        canvas.margin_page_left = @canvas.margin_page_left
        canvas.margin_page_right = @canvas.margin_page_right
        canvas.margin_page_top = @canvas.margin_page_top
        canvas.margin_page_bottom = @canvas.margin_page_bottom
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
          layout = make_layout(markupped_text)
          text_width, text_height = layout.pixel_size
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
        @motion_notify_hook_procs = []
        @button_press_hook_procs = []
        @button_release_hook_procs = []
      end

      def init_dpi
        @x_dpi = 72
        @y_dpi = 72
      end

      def clear_keys
        @keys = nil
      end

      def clear_progress_color
        @progress_foreground = nil
        @progress_background = nil
      end

      def get_line_width(params, default=nil)
        params[:line_width] || default
      end

      def invert_y(y)
        height - y
      end

      def init_color
        @background_color = "white"
      end
    end
  end
end
