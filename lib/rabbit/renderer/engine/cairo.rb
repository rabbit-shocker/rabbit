# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "cairo"
require "stringio"

require_relative "../../image-data-loader"
require_relative "../kernel"

module Rabbit
  module Renderer
    module Engine
      module Cairo
        include Kernel

        attr_writer :foreground, :background

        def background_image=(pixbuf)
          surface = ::Cairo::ImageSurface.new(::Cairo::FORMAT_A1, 1, 1)
          context = ::Cairo::Context.new(surface)
          context.set_source_pixbuf(pixbuf)
          @background_image = context.source
          @background_image.extend = ::Cairo::EXTEND_REPEAT
          pixbuf
        end

        def init_renderer(surface)
          init_context(surface.create_cairo_context)
        end

        def finish_renderer
          finish_context
        end

        def init_context(context)
          @context = context
          @contexts ||= []
          @contexts.push(@context)
          set_line_width(1)
          @context.line_cap = ::Cairo::LINE_CAP_ROUND
          @context.line_join = ::Cairo::LINE_JOIN_ROUND
        end

        def finish_context
          @contexts.pop
          @context.destroy if @context.respond_to?(:destroy)
          @context = @contexts.last
        end

        def to_gdk_rgb(color)
          make_color(color).to_gdk_rgb
        end

        def translate_context(x, y, params={})
          @context.translate(x, y)
        end

        def rotate_context(angle, params={})
          @context.rotate(convert_angle(angle, 0)[0])
        end

        def scale_context(x, y, params={})
          @context.scale(x, y)
        end

        def reflect_context(base, params={})
          case base
          when :y
            matrix = make_matrix(-1, 0, 0,
                                  0, 1, 0)
          else
            matrix = make_matrix(1,  0, 0,
                                 0, -1, 0)
          end
          @context.transform(matrix)
        end

        def shear_context(x, y, params={})
          @context.transform(make_matrix(1, x, 0,
                                         y, 1, 0))
        end

        def save_context
          @context.save
          super
        end

        def restore_context
          @context.restore
        end

        def draw_background(slide)
          super
          if @background_image
            @context.save do
              @context.set_source(@background_image)
              @context.paint
            end
          end
        end

        def draw_line(x1, y1, x2, y2, color=nil, params={})
          x1, y1 = from_screen(x1, y1)
          x2, y2 = from_screen(x2, y2)
          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            @context.new_path
            @context.move_to(x1, y1)
            @context.line_to(x2, y2)
            apply_cairo_action(false, params)
          end
        end

        def draw_rectangle(filled, x, y, w, h, color=nil, params={})
          x, y = from_screen(x, y)
          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            @context.rectangle(x, y, w, h)
            apply_cairo_action(filled, params)
          end
        end

        def draw_rounded_rectangle(filled, x, y, w, h, radius,
                                   color=nil, params={})
          x, y = from_screen(x, y)
          x_radius = params[:x_radius] || radius
          y_radius = params[:y_radius] || radius

          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            @context.new_path
            @context.rounded_rectangle(x, y, w, h, x_radius, y_radius)
            apply_cairo_action(filled, params)
          end
        end

        def draw_arc(filled, x, y, w, h, a1, a2, color=nil, params={})
          r = w * 0.5
          draw_arc_by_radius(filled, x + w * 0.5, y + h * 0.5,
                             r, a1, a2, color, params)
        end

        def draw_arc_by_radius(filled, x, y, r, a1, a2, color=nil, params={})
          x, y = from_screen(x, y)
          a1, a2 = convert_angle(a1, a2)
          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            args = [x, y, r, a1, a2]
            action, = cairo_action(filled, params)
            @context.move_to(x, y) unless action == :stroke
            @context.arc(*args)
            @context.close_path unless action == :stroke
            apply_cairo_action(filled, params)
          end
        end

        def draw_lines(points, color=nil, params={})
          draw_polygon(false, points, color, params.merge({:opened => true}))
        end

        def draw_polygon(filled, points, color=nil, params={})
          return if points.empty?
          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            @context.move_to(*from_screen(*points.first))
            points[1..-1].each do |x, y|
              @context.line_to(*from_screen(x, y))
            end
            @context.line_to(*from_screen(*points.first)) unless params[:opened]
            apply_cairo_action(filled, params)
          end
        end

        def draw_layout(layout, x, y, color=nil, params={})
          x, y = from_screen(x, y)
          @context.save do
            set_source(color, params)
            set_stroke_options(params)
            @context.move_to(x, y)
            if params[:stroke]
              @context.pango_layout_path(layout)
              apply_cairo_action(false, params)
            else
              @context.show_pango_layout(layout)
            end
          end
        end

        def draw_pixbuf(pixbuf, x, y, params={})
          x, y = from_screen(x, y)
          @context.save do
            apply_clip(x, y, pixbuf.width, pixbuf.height, params)
            @context.translate(x, y)
            set_source_pixbuf(pixbuf, params)
            @context.paint(params[:alpha])
          end

          _draw_reflected_pixbuf(pixbuf, x, y, params)
        end

        def set_source_pixbuf(pixbuf, params)
          draw_scaled_pixbuf = params[:draw_scaled_pixbuf]
          draw_scaled_pixbuf = @draw_scaled_image if draw_scaled_pixbuf.nil?
          width = (params[:width] || pixbuf.width).to_f
          height = (params[:height] || pixbuf.height).to_f

          if [width, height] == [pixbuf.width, pixbuf.height]
            @context.set_source_pixbuf(pixbuf, 0, 0)
            return
          end

          case draw_scaled_pixbuf
          when true
            scaled_pixbuf = pixbuf.scale(width, height)
            @context.set_source_pixbuf(scaled_pixbuf, 0, 0)
          when false
            @context.set_source_pixbuf(pixbuf, 0, 0)
            matrix = ::Cairo::Matrix.scale(pixbuf.width / width,
                                           pixbuf.height / height)
            @context.source.matrix = matrix
          else
            scales = [4, 3, 2]
            scales.each do |scale|
              if width * scale < pixbuf.width and height * scale < pixbuf.height
                scaled_pixbuf = pixbuf.scale(width * scale, height * scale)
                @context.set_source_pixbuf(scaled_pixbuf)
                matrix = ::Cairo::Matrix.scale(scale, scale)
                @context.source.matrix = matrix
                return
              end
            end
            @context.set_source_pixbuf(pixbuf, 0, 0)
            matrix = ::Cairo::Matrix.scale(pixbuf.width / width,
                                           pixbuf.height / height)
            @context.source.matrix = matrix
          end
        end

        def draw_rsvg_handle(handle, x, y, params={})
          x, y = from_screen(x, y)
          dim = handle.dimensions
          w = dim.width
          h = dim.height
          width = (params[:width] || w).to_f
          height = (params[:height] || h).to_f
          @context.save do
            apply_clip(x, y, w, h, params)
            @context.translate(x, y)
            @context.scale(width / w, height / h)
            @context.render_rsvg_handle(handle)
          end

          _draw_reflected_rsvg_handle(handle, x, y, width, height,
                                      params)
        end

        def draw_poppler_page(page, x, y, params={})
          x, y = from_screen(x, y)
          w, h = page.size
          width = (params[:width] || w).to_f
          height = (params[:height] || h).to_f
          @context.save do
            apply_clip(x, y, w, h, params)
            @context.translate(x, y)
            @context.scale(width / w, height / h)
            @context.render_poppler_page(page)
          end
        end

        def draw_link(uri)
          if @context.respond_to?(:tag)
            @context.tag(::Cairo::Tag::LINK, "uri='#{uri}'") do
              yield
            end
          else
            yield
          end
        end

        def make_layout(text)
          attributes, text = Pango.parse_markup(text)
          layout = @context.create_pango_layout
          layout.text = text
          layout.attributes = attributes
          set_font_resolution(layout.context)
          @context.update_pango_layout(layout)
          layout
        end

        def create_pango_context
          context = Pango::CairoFontMap.default.create_context
          set_font_resolution(context)
          context
        end

        def set_font_resolution(context)
          context.resolution = @canvas.font_resolution
        end

        private
        def init_engine_color
          @foreground = make_color("black")
          @background = make_color(@background_color)
        end

        def set_source(color, params={})
          pattern = params[:pattern]
          set = false
          if pattern
            set = set_pattern(pattern)
            color ||= Color.parse("#fff0") unless set
          end
          set_color(make_color(color)) unless set
        end

        def set_pattern(info)
          pattern = nil
          case info[:type]
          when :radial
            cx0, cy0, radius0, cx1, cy1, radius1 = info[:base]
            cx0, cy0 = from_screen(cx0, cy0)
            cx1, cy1 = from_screen(cx1, cy1)
            pattern = ::Cairo::RadialPattern.new(cx0, cy0, radius0,
                                                 cx1, cy1, radius1)
            info[:color_stops].each do |offset, r, g, b, a|
              pattern.add_color_stop_rgba(offset, r, g, b, a)
            end
          when :linear
            x, y, w, h = info[:base]
            x, y = from_screen(x, y)
            pattern = ::Cairo::LinearPattern.new(x, y, w, h)
            info[:color_stops].each do |offset, r, g, b, a|
              pattern.add_color_stop_rgba(offset, r, g, b, a)
            end
          when :pixbuf
            @context.set_source_pixbuf(info[:pixbuf])
            pattern = @context.source
          end
          if pattern
            set_pattern_common_options(pattern, info)
            @context.set_source(pattern)
          end
          !pattern.nil?
        end

        def set_pattern_common_options(pattern, info)
          extend = info[:extend]
          pattern.extend = extend if extend
          transformations = info[:transformations]
          if transformations
            matrix = ::Cairo::Matrix.identity
            transformations.each do |operation, *args|
              matrix.send("#{operation}!", *args)
            end
            pattern.matrix = matrix
          end
        end

        def set_color(color)
          @context.set_source_rgba(make_color(color).to_a)
        end

        def set_line_width(line_width)
          if line_width
            @context.set_line_width(line_width)
          end
        end

        def convert_angle(a1, a2)
          a2 += a1
          [a2, a1].collect {|a| (360 - a) * (Math::PI / 180.0)}
        end

        def from_screen(x, y)
          [x.ceil, y.ceil]
        end

        def cairo_action(filled, params={})
          if params[:clip]
            [:clip, params[:clip]]
          elsif filled
            :fill
          else
            :stroke
          end
        end

        def apply_cairo_action(filled, params={})
          action, *other_info = cairo_action(filled, params)
          @context.send(action)
          case action
          when :clip
            block, = other_info
            block.call if block
          end
        end

        def apply_clip(x, y, w, h, params)
          clip_x = params[:clip_x]
          clip_y = params[:clip_y]
          clip_width = params[:clip_width]
          clip_height = params[:clip_height]
          if clip_x or clip_y or clip_width or clip_height
            @context.rectangle(x,
                               y,
                               clip_width || w,
                               clip_height || h)
            @context.clip
            @context.translate(x - (clip_x || x),
                               y - (clip_y || y))
          end
        end

        def make_matrix(xx, xy, x0, yx, yy, y0)
          ::Cairo::Matrix.new(xx, yx, xy, yy, x0, y0)
        end

        def _draw_reflected_pixbuf(pixbuf, x, y, params)
          reflect_params = params[:reflect]
          return unless reflect_params

          reflect_params = {} if reflect_params == true
          ratio = reflect_params[:ratio] || 0.3
          start_alpha = reflect_params[:start_alpha] || 0.3

          @context.save do
            width = (params[:width] || pixbuf.width).to_f
            height = (params[:height] || pixbuf.height).to_f
            @context.translate(x, y + height * 2)
            reflect_context(:x)
            set_source_pixbuf(pixbuf, params)
            pattern = ::Cairo::LinearPattern.new(width * 0.5, 0,
                                                 width * 0.5, height)
            pattern.add_color_stop_rgba(0, 0, 0, 0, 0)
            pattern.add_color_stop_rgba(1 - ratio, 0, 0, 0, 0)
            pattern.add_color_stop_rgba(1, 0, 0, 0, start_alpha)
            @context.mask(pattern)
          end
        end

        def _draw_reflected_rsvg_handle(handle, x, y, width, height, params)
          return unless params

          dim = handle.dimensions
          ::Cairo::ImageSurface.new(:argb32, width, height) do |surface|
            ::Cairo::Context.new(surface) do |context|
              context.scale(width / dim.width, height / dim.height)
              context.render_rsvg_handle(handle)
              png = StringIO.new
              context.target.write_to_png(png)
              loader = ImageDataLoader.new(png.string)
              loader.load
              _draw_reflected_pixbuf(loader.pixbuf, x, y, params)
            end
          end
        end

        def set_stroke_options(params)
          set_line_width(get_line_width(params))
          [:line_cap, :line_join].each do |key|
            value = params[key]
            @context.send("#{key}=", value) if value
          end
          value = params[:dash]
          @context.set_dash(*value) if value
        end
      end
    end
  end
end
