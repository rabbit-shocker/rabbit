require "rabbit/renderer/color"

module Rabbit
  module Renderer
    module Kernel
      def rsvg_available?
        false
      end

      def poppler_available?
        false
      end

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
        context.resolution = @canvas.font_resolution
      end
    end
  end
end
