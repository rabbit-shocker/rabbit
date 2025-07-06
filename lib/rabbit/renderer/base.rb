# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require "forwardable"
require "erb"

require_relative "../gtk"

require_relative "../key-handler"
require_relative "../rabbit"
require_relative "../trackball"
require_relative "color"

module Rabbit
  module Renderer
    module Base
      extend Forwardable

      include ERB::Util
      include GetText
      include DirtyCount

      def_delegators(:@canvas, :reload_source)

      attr_accessor :base_width
      attr_accessor :base_height
      attr_accessor :paper_width, :paper_height, :slides_per_page
      attr_accessor :margin_left, :margin_right
      attr_accessor :margin_top, :margin_bottom
      attr_accessor :progress_foreground
      attr_accessor :progress_background
      attr_accessor :adjustment_x, :adjustment_y
      attr_accessor :graffiti_color, :graffiti_line_width
      attr_accessor :gl_scale, :gl_quaternion
      attr_accessor :draw_scaled_image
      attr_writer :page_margin_left, :page_margin_right
      attr_writer :page_margin_top, :page_margin_bottom

      def initialize(canvas)
        super()
        @canvas = canvas
        @font_families = nil
        @base_width = nil
        @base_height = nil
        @paper_width = nil
        @paper_height = nil
        @slides_per_page = 1
        @margin_left = nil
        @margin_right = nil
        @margin_top = nil
        @margin_bottom = nil
        @page_margin_left = nil
        @page_margin_right = nil
        @page_margin_top = nil
        @page_margin_bottom = nil
        @whiteout = false
        @blackout = false
        @adjustment_x = 0
        @adjustment_y = 0
        @progress_foreground = nil
        @progress_background = nil
        @graffiti_color = nil
        @graffiti_line_width = nil
        @draw_scaled_image = true
        @key_handler = nil
        clean
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

      def clear_slide
        current = @canvas.current_slide
        current.clear_waiting if current
      end

      def reset_adjustment
        @adjustment_x = 0
        @adjustment_y = 0
      end

      def each_slide_pixbuf
        canvas = offscreen_canvas
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

      def offscreen_canvas
        if offscreen_renderer?
          @canvas
        else
          make_canvas_with_offscreen_renderer
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

      def whiteouting?
        @whiteout
      end

      def blackouting?
        @blackout
      end

      def toggle_whiteout
        @blackout = false
        @whiteout = !@whiteout
      end

      def toggle_blackout
        @blackout = !@blackout
        @whiteout = false
      end

      def hiding?
        @blackout or @whiteout
      end

      def toggle_info_window
      end

      def info_window_showing?
        false
      end

      def toggle_spotlight
      end

      def spotlighting?
        false
      end

      def toggle_magnifier
      end

      def magnifying?
        false
      end

      def gl_available?
        @canvas.use_gl? and gl_supported?
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
        @key_handler.connect_key(keyval, modifier, flags, &block) if @key_handler
      end

      def disconnect_key(keyval, modifier)
        @key_handler.disconnect_key(keyval, modifier) if @key_handler
      end

      def change_graffiti_color
      end

      def add_gesture_action(sequence, action, &block)
      end

      def pre_terminal
        @key_handler.pre_terminal if @key_handler
      end

      def post_terminal
        @key_handler.post_terminal if @key_handler
      end

      private
      def offscreen_renderer?
        false
      end

      def do_print(&block)
        pre_print(@canvas.slide_size)
        canceled = false
        @canvas.slides.each_with_index do |slide, i|
          @canvas.move_to_if_can(i)
          current_slide = @canvas.current_slide
          current_slide.flush
          current_slide.draw(@canvas)
          if block and !block.call(i)
            canceled = true
            break
          end
        end
        post_print(canceled)
      end

      def make_canvas_with_renderer(renderer)
        canvas = Canvas.new(renderer)
        yield canvas
        canvas.apply_theme(@canvas.theme_name)
        @canvas.source_force_modified(true) do |source|
          canvas.parse(source)
        end
        canvas.toggle_index_mode if @canvas.index_mode?
        canvas
      end

      def make_canvas_with_printable_renderer
        renderer = Renderer::Printer
        make_canvas_with_renderer(renderer) do |canvas|
          canvas.filename = @canvas.filename
          setup_size(canvas)
          setup_margin(canvas)
          setup_page_margin(canvas)
          setup_paper_size(canvas)
          setup_3d(canvas)
          canvas.slides_per_page = @canvas.slides_per_page
        end
      end

      def make_canvas_with_offscreen_renderer
        make_canvas_with_renderer(Offscreen) do |canvas|
          setup_size(canvas)
          setup_3d(canvas)
        end
      end

      def setup_size(canvas)
        canvas.width = @canvas.width
        canvas.height = @canvas.height
      end

      def setup_margin(canvas)
        canvas.margin_left = @canvas.margin_left
        canvas.margin_right = @canvas.margin_right
        canvas.margin_top = @canvas.margin_top
        canvas.margin_bottom = @canvas.margin_bottom
      end

      def setup_page_margin(canvas)
        canvas.page_margin_left = @canvas.page_margin_left
        canvas.page_margin_right = @canvas.page_margin_right
        canvas.page_margin_top = @canvas.page_margin_top
        canvas.page_margin_bottom = @canvas.page_margin_bottom
      end

      def setup_paper_size(canvas)
        canvas.paper_width = @canvas.paper_width
        canvas.paper_height = @canvas.paper_height
      end

      def setup_3d(canvas)
        canvas.use_gl = @canvas.use_gl?
      end

      def not_support_method(name)
        format = _("%s does not support: %s")
        msg = format % [self.class.name, name]
        Rabbit.logger.warn(msg)
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
