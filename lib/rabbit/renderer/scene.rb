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

      def redraw
        queue_draw
      end

      def queue_draw
        @stack.queue_draw
        visible_child = @stack.visible_child
        return if visible_child.nil?

        queue_draw_recursive = lambda do |widget|
          widget.queue_draw
          if widget.respond_to?(:children)
            widget.children.each do |child|
              queue_draw_recursive.call(child)
            end
          end
        end
        queue_draw_recursive.call(visible_child)
      end

      def cache_all_slides
        # We don't need this.
      end

      def clear_slide
        super
        compile_slides
      end

      def post_fullscreen
        update_cursor(:none, true)
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

      def create_stroke(params)
        stroke = Gsk::Stroke.new(params[:line_width] || 1)
        [:line_cap, :line_join].each do |name|
          value = params[name]
          stroke.__send__("#{name}=", value) if value
        end
        # TODO: dash
        stroke
      end

      def draw_line(x1, y1, x2, y2, color=nil, params={})
        return if params[:line_width] == 0

        x1, y1 = adjust_xy(x1, y1)
        x2, y2 = adjust_xy(x2, y2)
        snapshot = current_snapshot
        snapshot.save do
          rgba = make_color(color).to_gdk_rgba
          builder = Gsk::PathBuilder.new
          builder.move_to(x1, y1)
          builder.line_to(x2, y2)
          stroke = create_stroke(params)
          snapshot.append_stroke(builder.to_path, stroke, rgba)
        end
      end

      def draw_rectangle(filled, x, y, w, h, color=nil, params={})
        x, y = adjust_xy(x, y)
        snapshot = current_snapshot
        snapshot.save do
          rgba = make_color(color).to_gdk_rgba
          if filled
            snapshot.append_color(rgba, [x, y, w, h])
          else
            builder = Gsk::PathBuilder.new
            builder.add_rect([x, y, w, h])
            stroke = create_stroke(params)
            snapshot.append_stroke(builder.to_path, stroke, rgba)
          end
        end
      end

      def draw_rounded_rectangle(filled, x, y, w, h, radius, color=nil, params={})
        x, y = adjust_xy(x, y)
        snapshot = current_snapshot
        snapshot.save do
          rgba = make_color(color).to_gdk_rgba
          builder = Gsk::PathBuilder.new
          builder.add_rounded_rect([[x, y, w, h], radius])
          path = builder.to_path
          if filled
            snapshot.append_fill(path, :winding, rgba)
          else
            stroke = create_stroke(params)
            snapshot.append_stroke(path, stroke, rgba)
          end
        end
      end

      #            <--w-->
      #               90 degree
      #              +-+             -
      #             /   \            |
      # 180 degree |     | 0 degree  h
      #             \   /            |
      #            p +-+             -
      # p: (x,y)
      def draw_arc(filled,
                   x,
                   y,
                   w,
                   h,
                   start_degree,
                   length_degree,
                   color=nil,
                   params={})
        radius = w * 0.5
        draw_arc_by_radius(filled,
                           x + w * 0.5,
                           y + h * 0.5,
                           radius,
                           start_degree,
                           length_degree,
                           color,
                           params)
      end

      #               90 degree
      #              +-+
      #             /   \
      # 180 degree |  c  | 0 degree
      #             \   /
      #              +-+
      #
      # c: center (center_x, center_y)
      def draw_arc_by_radius(filled,
                             center_x,
                             center_y,
                             radius,
                             start_degree,
                             length_degree,
                             color=nil,
                             params={})
        center_x, center_y = adjust_xy(center_x, center_y)
        snapshot = current_snapshot
        snapshot.save do
          builder = Gsk::PathBuilder.new
          if length_degree == 360
            builder.add_circle([center_x, center_y], radius)
          else
            end_degree = start_degree + length_degree
            # Convert to the HTML arcTo command:
            # https://www.w3.org/TR/SVG11/paths.html#PathDataEllipticalArcCommands
            #
            #               90 degree
            #              +-+
            #             /   \
            # 180 degree |  c  | 0 degree
            #  end degree \   / start degree
            #              +-+
            # ->
            #              +-+
            #   end (x,y) /   \
            # 180 degree |  c  | 0 degree
            #             \   / start (x0,y0)
            #              +-+
            #               90 degree
            x_axis_rotation = 0
            large_arc = length_degree >= 180
            positive_sweep = false
            start_radian = -start_degree * (Math::PI / 180.0)
            end_radian = -end_degree * (Math::PI / 180.0)
            start_x = center_x + (radius * Math.cos(start_radian))
            start_y = center_y + (radius * Math.sin(start_radian))
            end_x = center_x + (radius * Math.cos(end_radian))
            end_y = center_y + (radius * Math.sin(end_radian))
            if filled
              builder.move_to(center_x, center_y)
              builder.line_to(start_x, start_y)
            else
              builder.move_to(start_x, start_y)
            end
            builder.svg_arc_to(radius,
                               radius,
                               x_axis_rotation,
                               large_arc,
                               positive_sweep,
                               center_x + (radius * Math.cos(end_radian)),
                               center_y + (radius * Math.sin(end_radian)))
            builder.close if filled
          end
          path = builder.to_path
          rgba = make_color(color).to_gdk_rgba
          if filled
            snapshot.append_fill(path, :winding, rgba)
          else
            snapshot.append_stroke(path, create_stroke(params), rgba)
          end
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
        if ENV["RABBIT_SLIDE_TRANSITION"] == "yes"
          @stack.transition_type = :rotate_left_right
        end
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
