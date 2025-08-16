# Copyright (C) 2018-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "base"

require_relative "display/base"
require_relative "display/progress"
require_relative "display/mask"
require_relative "display/menu"
require_relative "display/search"
require_relative "display/gesture"
require_relative "display/graffiti"
require_relative "display/button-handler"
require_relative "display/motion-handler"
require_relative "display/scroll-handler"
require_relative "display/info"
require_relative "display/spotlight"
require_relative "display/magnifier"

require_relative "scene-background-widget"

module Rabbit
  module Renderer
    # This is not a real widget. This is just a wrapper to enforce
    # our conventions:
    #
    #  * We need to use real x,y not logical x,y for widget location
    #  * We need to adjust margin of rendering area (not element's margin)
    #    for GTK widget
    #  * We need to use real w,h not logical w,h for GTK widget size
    class SceneWidget
      def initialize(fixed, size)
        @fixed = fixed
        @size = size
      end

      def put(widget, x, y, w, h)
        case widget
        when SceneNodeWidget, SceneBackgroundWidget
          # Our widgets process size by themselves
        else
          x += @size.logical_margin_left
          y += @size.logical_margin_top
          widget.set_size_request(@size.resolve_logical_x(w),
                                  @size.resolve_logical_y(h))
        end
        real_x = @size.resolve_logical_x(x)
        real_y = @size.resolve_logical_y(y)
        @fixed.put(widget, real_x, real_y)
      end
    end

    class Scene < Base
      include Kernel # TODO: Remove me

      include Display::Base

      include Display::Cursor
      include Display::Menu
      # include Display::Graffiti
      # include Display::Mask
      # include Display::Progress
      # include Display::Search
      # include Display::Gesture
      include Display::ButtonHandler
      include Display::MotionHandler
      include Display::ScrollHandler
      include Display::Info
      include Display::Spotlight
      # include Display::Magnifier

      attr_accessor :filename
      def initialize(canvas)
        super
        @filename = nil
        @snapshots = []
        @base_xys = []
        init_ui
      end

      def update_size(width, height)
        super
        compile_slides
      end

      def attach_to(window, container=nil)
        super
        if container
          container.add(@stack)
        else
          @window.child = @stack
        end
        @stack.show
        set_default_size(window.default_width, window.default_height)
        update_size(window.default_width, window.default_height)
        init_menu
        attach_menu(@window)
      end

      def detach
        detach_menu(@window)
        @stack.hide
        super
      end

      def widget
        @stack
      end

      def queue_draw
        @stack.queue_draw
        queue_draw_recursive = lambda do |widget|
          widget.queue_draw
          if widget.respond_to?(:children)
            widget.children.each do |child|
              queue_draw_recursive.call(child)
            end
          end
        end
        queue_draw_recursive.call(@stack.visible_child)
      end

      def clear_slide
        super
        compile_slides
      end

      def post_fullscreen
        update_cursor(:blank, true)
        update_menu
      end

      def post_unfullscreen
        update_cursor(nil, true)
        update_menu
      end

      def post_iconify
        update_menu
      end

      def post_apply_theme
        clear_slide
        queue_draw
        update_menu
      end

      def post_move(old_index, index)
        @stack.visible_child_name = index.to_s
        update_title
        update_menu
      end

      def post_move_in_slide(old_index, index)
        queue_draw
        update_menu
      end

      def pre_parse
      end

      def post_parse
        update_title
      end

      def pre_toggle_index_mode
      end

      def post_toggle_index_mode
        queue_draw
      end

      def pre_to_pixbuf(n_slides)
      end

      def to_pixbufing(i)
        true
      end

      def post_to_pixbuf(canceled)
      end

      def index_mode_on
        keep_cursor(:index)
        update_cursor(nil, true)
      end

      def index_mode_off
        restore_cursor(:index)
      end

      def display?
        true
      end

      def push_snapshot(snapshot, base_x, base_y)
        @snapshots << snapshot
        @base_xys << [base_x, base_y]
        begin
          snapshot.save do
            yield
          end
        ensure
          @base_xys.pop
          @snapshots.pop
        end
      end

      def current_snapshot
        @snapshots.last
      end

      def current_base_xy
        @base_xys.last
      end

      def draw_pixbuf(pixbuf, x, y, params={})
        x, y = adjust_xy(x, y)
        width = (params[:width] || pixbuf.width).to_f
        height = (params[:height] || pixbuf.height).to_f
        draw_scaled_pixbuf = params[:draw_scaled_pixbuf]
        draw_scaled_pixbuf = @draw_scaled_image if draw_scaled_pixbuf.nil?

        snapshot = current_snapshot
        snapshot.save do
          # TODO: clip
          snapshot.translate([x, y])

          if width == pixbuf.width and height == pixbuf.height
            need_scale = false
          elsif draw_scaled_pixbuf
            need_scale = true
          else
            need_scale = false
          end
          if need_scale
            snapshot.append_scale_texture(Gdk::Texture.new(pixbuf),
                                          :linear,
                                          [0, 0, width, height])
          else
            snapshot.append_texture(Gdk::Texture.new(pixbuf),
                                    [0, 0, width, height])
          end
        end
      end

      def draw_layout(layout, x, y, color=nil, params={})
        return if params[:stroke] # TODO

        x, y = adjust_xy(x, y)
        snapshot = current_snapshot
        snapshot.save do
          snapshot.translate([x, y])
          rgba = make_color(color).to_gdk_rgba
          snapshot.append_layout(layout, rgba)
        end
      end

      def draw_line(x1, y1, x2, y2, color=nil, params={})
        x1, y1 = adjust_xy(x1, y1)
        x2, y2 = adjust_xy(x2, y2)
        snapshot = current_snapshot
        snapshot.save do
          rgba = make_color(color).to_gdk_rgba
          builder = Gsk::PathBuilder.new
          builder.move_to(x1, y1)
          builder.line_to(x2, y2)
          # set_stroke_options(params)
          stroke = Gsk::Stroke.new(get_line_width(params))
          snapshot.append_stroke(builder.to_path, stroke, rgba)
          # apply_cairo_action(filled, params)
        end
      end

      def draw_rectangle(filled, x, y, w, h, color=nil, params={})
        x, y = adjust_xy(x, y)
        snapshot = current_snapshot
        snapshot.save do
          rgba = make_color(color).to_gdk_rgba
          if filled
            # set_stroke_options(params)
            snapshot.append_color(rgba, [x, y, w, h])
          else
            builder = Gsk::PathBuilder.new
            builder.add_rect([x, y, w, h])
            stroke = Gsk::Stroke.new(get_line_width(params))
            snapshot.append_stroke(builder.to_path, stroke, rgba)
          end
          # apply_cairo_action(filled, params)
        end
      end

      def draw_poppler_page(page, x, y, params={})
        x, y = adjust_xy(x, y)
        w, h = page.size
        width = (params[:width] || w).to_f
        height = (params[:height] || h).to_f

        snapshot = current_snapshot
        snapshot.save do
          # TODO: clip
          snapshot.scale(width / w, height / h)
          context = snapshot.append_cairo([x, y, w, h])
          context.render_poppler_page(page)
        end
      end

      private
      def init_ui
        @stack = Gtk::Stack.new
        # if Gtk::StackTransitionType.const_defined?(:ROTATE_LEFT_RIGHT)
        #   @stack.transition_type = :rotate_left_right
        # else
        #   @stack.transition_type = :slide_left_right
        # end
        set_button_event(@stack)
        set_motion_event(@stack)
        set_scroll_event(@stack)
      end

      def compile_slides
        visible_child_name = @stack.visible_child_name
        @stack.pages.to_a.each do |page|
          @stack.remove(page.child)
        end
        @canvas.slides.each_with_index do |slide, i|
          set_size_ratio(slide.size_ratio || @default_size_ratio)
          fixed = Gtk::Fixed.new
          scene_widget = SceneWidget.new(fixed, size)
          background = SceneBackgroundWidget.new(@canvas, self, size)
          w = size.logical_width
          h = size.logical_height
          scene_widget.put(background, 0, 0, w, h)
          slide.setup_scene(@canvas, scene_widget, 0, 0, w, h)
          @stack.add_named(fixed, i.to_s)
        end
        @stack.visible_child_name = visible_child_name if visible_child_name
        redraw
      end

      # For backward compatibility. Legacy DrawingArea based
      # renderer. Legacy DrawingAare based renderer uses uses {x: 0,
      # y: 0, width: @canvas.width, height: @canvas.height} coordinate
      # for all elements. Scene based renderer uses {x: element.x, y:
      # element.y, width: element.width, height: element.height} for
      # each element.
      def adjust_xy(x, y)
        base_x, base_y = current_base_xy
        [x - base_x, y - base_y]
      end

      def depth
        @stack.window.depth
      end

      def reload_theme(&callback)
        callback ||= Utils.process_pending_events_proc
        @canvas.activate("ReloadTheme", &callback)
      end

      def grab
      end

      def ungrab
      end
    end
  end
end
