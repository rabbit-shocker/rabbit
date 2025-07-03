# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "gtkglext"

require_relative "../rabbit"
Rabbit.add_gui_init_proc do
  Gtk::GL.init
end

require_relative "../trackball"

module Rabbit
  module Renderer
    module GL
      def draw_slide(slide, simulation, &block)
        if gl_available?
          gl_drawable.gl_begin(gl_context) do
            init_gl_drawing_info
            super(slide, simulation, &block)
            gl_drawable.swap_buffers if gl_drawable.double_buffered?
          end
        else
          super(slide, simulation, &block)
        end
      end

      def gl_supported?
        (Gtk::GL::BUILD_VERSION <=> [1, 2, 0]) >= 0
      end

      private
      def init_gl_drawing_info
        gl_drawable.wait_gdk
        ::GL.ClearDepth(1.0)
        ::GL.Clear(::GL::DEPTH_BUFFER_BIT)

        ::GL.Viewport(0, 0, width, height)

        ::GL.Light(::GL::LIGHT0, ::GL::POSITION, [1.0, 1.0, 1.0, 0.0])
        ::GL.Enable(::GL::LIGHTING)
        ::GL.Enable(::GL::LIGHT0)
        ::GL.Enable(::GL::DEPTH_TEST)

        ::GL.MatrixMode(::GL::PROJECTION)
        ::GL.LoadIdentity
        GLU.Perspective(30.0, width / height, 1.0, z_far)

        ::GL.MatrixMode(::GL::MODELVIEW)
        ::GL.LoadIdentity
        GLU.LookAt(0.0, 0.0, z_view,
                   0.0, 0.0, 0.0,
                   0.0, 1.0, 0.0)
        ::GL.Translate(0.0, 0.0, -z_view)
        ::GL.Scale(gl_scale, gl_scale, gl_scale)
        ::GL.MultMatrix(gl_quaternion.build_rotmatrix)
        gl_drawable.wait_gl
      end

      def draw_gl(x, y, z, color, block)
        gl_drawable.wait_gdk
        ::GL.PushMatrix
        setup_geometry(x, y, z)
        setup_color(color)
        block.call(true) if block
        yield
        block.call(false) if block
        unless gl_drawable.double_buffered?
          ::GL.Flush
        end
        ::GL.PopMatrix
        gl_drawable.wait_gl
      end

      def setup_color(color)
        color = make_color(color)
        ::GL.Light(::GL::LIGHT0, ::GL::DIFFUSE, color.to_a)
      end

      def setup_geometry(x, y, z)
        new_x = to_gl_size(x, width)
        new_y = to_gl_size(height - y, height)
        new_z = z
        ::GL.MatrixMode(::GL::MODELVIEW)
        ::GL.Translate(new_x, new_y, new_z)
      end

      def to_gl_size(value, max)
        min = 0.0
        range = min.abs + max.abs
        gl_min = -1.0
        gl_max = 1.0
        gl_range = gl_min.abs + gl_max.abs
        (value / range.to_f) * gl_range + gl_min
      end
    end
  end
end
