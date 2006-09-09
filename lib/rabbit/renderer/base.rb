require "forwardable"
require "erb"
require "gtk2"

require "rabbit/rabbit"
require "rabbit/gettext"
require "rabbit/trackball"
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
      
      def_delegators(:@canvas, :reload_source)
      
      attr_reader :x_dpi, :y_dpi
      attr_accessor :paper_width, :paper_height, :slides_per_page
      attr_accessor :margin_left, :margin_right
      attr_accessor :margin_top, :margin_bottom
      attr_accessor :progress_foreground
      attr_accessor :progress_background
      attr_accessor :adjustment_x, :adjustment_y
      attr_accessor :graffiti_color, :graffiti_line_width
      attr_accessor :gl_scale, :gl_quaternion
      attr_writer :page_margin_left, :page_margin_right
      attr_writer :page_margin_top, :page_margin_bottom

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
        @page_margin_left = nil
        @page_margin_right = nil
        @page_margin_top = nil
        @page_margin_bottom = nil
        @white_out = false
        @black_out = false
        @list_id = 0
        @adjustment_x = 0
        @adjustment_y = 0
        @progress_foreground = nil
        @progress_background = nil
        @graffiti_color = nil
        @graffiti_line_width = nil
        clean
        init_hook_procs
        init_dpi
        init_gl_parameters
      end

      def page_margin_left
        @page_margin_left || 0
      end
      
      def page_margin_right
        @page_margin_right || 0
      end
      
      def page_margin_top
        @page_margin_top || 0
      end
      
      def page_margin_bottom
        @page_margin_bottom || 0
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
          canceled = false
          canvas.print do |i|
            result = printing(i)
            canceled = !result
            result
          end
          post_print(canceled)
          canvas.activate("Quit")
        end
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
        canceled = false
        canvas.slides.each_with_index do |slide, i|
          if !to_pixbufing(i) or !yield(slide, canvas.to_pixbuf(i), i)
            canceled = true
            break
          end
        end
        post_to_pixbuf(canceled)
        canvas.move_to_if_can(previous_index)
        canvas.activate("Quit") if canvas != @canvas
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

      def confirm
        true
      end

      def setup_event(area)
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
        init_color
        clear_keys
        clear_progress_color
        clear_graffiti_config
        clear_gesture_actions
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

      def toggle_info_window
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

      def gl_available?
        @canvas.use_gl? and gl_supported?
      end

      def update_comment(source)
      end
      
      def post_init_gui
      end

      def graffiti_mode?
        false
      end
      
      def have_graffiti?
        false
      end
      
      def can_undo_graffiti?
        false
      end

      def expand_hole
      end

      def narrow_hole
      end

      def search_slide(forward=true)
      end

      def stop_slide_search
      end

      def searching?
        false
      end

      def connect_key(keyval, modifier, flags, &block)
      end

      def disconnect_key(keyval, modifier)
      end

      def change_graffiti_color
      end

      def add_gesture_action(sequence, action, &block)
      end

      private
      def off_screen_renderer?
        false
      end
      
      def do_print(&block)
        pre_print(@canvas.slide_size)
        canceled = false
        @canvas.slides.each_with_index do |slide, i|
          @canvas.move_to_if_can(i)
          @canvas.current_slide.draw(@canvas)
          if block and !block.call(i)
            canceled = true
            break
          end
        end
        post_print(canceled)
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
          setup_3d(canvas)
          canvas.slides_per_page = @canvas.slides_per_page
        end
      end
      
      def make_canvas_with_off_screen_renderer
        make_canvas_with_renderer(Pixmap) do |canvas|
          canvas.width = @canvas.width
          canvas.height = @canvas.height
          setup_3d(canvas)
        end
      end

      def setup_margin(canvas)
        canvas.margin_left = @canvas.margin_left
        canvas.margin_right = @canvas.margin_right
        canvas.margin_top = @canvas.margin_top
        canvas.page_margin_bottom = @canvas.page_margin_bottom
      end

      def setup_page_margin(canvas)
        canvas.page_margin_left = @canvas.page_margin_left
        canvas.page_margin_right = @canvas.page_margin_right
        canvas.page_margin_top = @canvas.page_margin_top
        canvas.page_margin_bottom = @canvas.page_margin_bottom
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

      def setup_3d(canvas)
        canvas.use_gl = @canvas.use_gl?
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

      def init_gl_parameters
        angle = 0.0 * (Math::PI / 180.0)
        axis_x = 1.0
        axis_y = 0.0
        axis_z = 0.0
        sine = Math.sin(0.5 * angle)
        quaternion = [
                      axis_x * sine,
                      axis_y * sine,
                      axis_z * sine,
                      Math.cos(0.5 * angle)
                     ]
        @gl_quaternion = TrackBall::Vector.new(quaternion)
        @gl_scale = 1.0
      end

      def clear_keys
      end

      def clear_graffiti_config
        @graffiti_color = nil
        @graffiti_line_width = nil
      end

      def clear_progress_color
        @progress_foreground = nil
        @progress_background = nil
      end

      def invert_y(y)
        height - y
      end

      def init_color
        @background_color = "white"
      end

      def clear_gesture_actions
        init_gesture_actions
      end

      def init_gesture_actions
      end
    end
  end
end
