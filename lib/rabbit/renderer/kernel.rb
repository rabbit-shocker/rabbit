# Copyright (C) 2006-2018  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/pango-markup"

require "rabbit/renderer/color"

module Rabbit
  module Renderer
    module Kernel
      def make_color(color)
        return color if color.is_a?(Color)
        if color.nil?
          @foreground
        else
          Color.parse(color)
        end
      end

      def translate_context(x, y, params={})
      end

      def rotate_context(angle, params={})
      end

      def scale_context(x, y, params={})
      end

      def reflect_context(base, params={})
      end

      def shear_context(x, y, params={})
      end

      def save_context
        if block_given?
          begin
            yield
          ensure
            restore_context
          end
        end
      end

      def restore_context
      end

      def draw_slide(slide, simulation)
        unless simulation
          save_context
          draw_background(slide)
        end
        yield
      ensure
        restore_context unless simulation
      end

      def draw_background(slide)
        draw_rectangle(true, 0, 0, width, height, @background)
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
        layout.alignment = Pango::Alignment::CENTER
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

      private
      def get_line_width(params, default=nil)
        params[:line_width] || default
      end

      def set_font_resolution(context)
      end

      def setup_flag_params(pole_height, default_flag_width_ratio, params)
        params = params.dup

        text = params["text"]
        text_attrs = params["text_attributes"] || {}
        if text
          markupped_text = PangoMarkup.new("span", text_attrs, text)
          layout = make_layout(markupped_text.to_s)
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
    end
  end
end
