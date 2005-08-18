require "gtkglext"

require "rabbit/rabbit"
Rabbit.add_gui_init_proc do
  Gtk::GL.init
end

require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    class PixmapGL < PixmapBase

      def draw_slide(slide)
        super(slide) do
          yield
        end
        drawable.gl_end
        @gl_context.destroy
        @gl_context = nil
      end

      # because GtkGLDrawable is not implement
      def draw_layout(layout, x, y, color=nil)
        gc = make_gc(color)
        @pixmap.draw_layout(gc, x, y, layout)
      end

      def draw_cube(filled, x, y, z, size, color=nil)
        draw_gl do
          Gdk::GL.draw_cube(filled, size)
        end
      end
      
      def draw_sphere(filled, x, y, z, radius, slices, stacks, color=nil)
        draw_gl do
          Gdk::GL.draw_sphere(filled, radius, slices, stacks)
        end
      end
      
      def draw_cone(filled, x, y, z, base, height, slices, stacks, color=nil)
        draw_gl do
          Gdk::GL.draw_cone(filled, base, height, slices, stacks)
        end
      end
      
      def draw_torus(filled, x, y, z, inner_radius, outer_radius,
                     n_sides, rings, color=nil)
        draw_gl do
          Gdk::GL.draw_torus(filled, inner_radius,
                             outer_radius, n_sides, rings)
        end
      end
      
      def draw_tetrahedron(filled, x, y, z, color=nil)
        draw_gl do
          Gdk::GL.draw_tetrahedron(filled)
        end
      end
      
      def draw_octahedron(filled, x, y, z, color=nil)
        draw_gl do
          Gdk::GL.draw_octahedron(filled)
        end
      end
      
      def draw_dodecahedron(filled, x, y, z, color=nil)
        draw_gl do
          Gdk::GL.draw_dodecahedron(filled)
        end
      end
      
      def draw_icosahedron(filled, x, y, z, color=nil)
        draw_gl do
          Gdk::GL.draw_icosahedron(filled)
        end
      end
      
      def draw_teapot(filled, x, y, z, scale, color=nil)
        draw_gl do
          Gdk::GL.draw_teapot(filled, scale)
        end
      end
      
      private
      def drawable
        @pixmap.gl_pixmap
      end
      
      def init_pixmap(slide)
        super
        mode = Gdk::GLConfig::MODE_RGB | Gdk::GLConfig::MODE_DEPTH
        @gl_config = Gdk::GLConfig.new(mode)
        if @pixmap.method(:set_gl_capability).arity == 2
          # bug of Ruby/GtkGLExt <= 0.13.0
          @pixmap.set_gl_capability(@gl_config, nil)
        else
          @pixmap.set_gl_capability(@gl_config)
        end
        @gl_context = Gdk::GLContext.new(drawable, nil, false, 0)
        drawable.gl_begin(@gl_context)
        init_gl
      end

      def init_gl
        GL.ClearDepth(1.0)
        GL.Clear(GL::DEPTH_BUFFER_BIT)
    
        GL.Viewport(0, 0, width, height)
        
        GL.Light(GL::LIGHT0, GL::DIFFUSE, [1.0, 0.0, 0.0, 1.0])
        GL.Light(GL::LIGHT0, GL::POSITION, [1.0, 1.0, 1.0, 0.0])
        GL.Enable(GL::LIGHTING)
        GL.Enable(GL::LIGHT0)
        GL.Enable(GL::DEPTH_TEST)
          
        GL.MatrixMode(GL::PROJECTION)
        GL.LoadIdentity
        GLU.Perspective(40.0, 1.0, 1.0, 10.0)

        GL.MatrixMode(GL::MODELVIEW)
        GL.LoadIdentity
        GLU.LookAt(0.0, 0.0, 3.0,
                   0.0, 0.0, 0.0,
                   0.0, 1.0, 0.0)
        GL.Translate(0.0, 0.0, -3.0)
        drawable.wait_gl
      end

      def draw_gl
        drawable.wait_gdk
        yield
        GL.Flush
        drawable.wait_gl
      end
      
    end
  end
end
