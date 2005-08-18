raise LoadError

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
          drawable.wait_gdk
          
          qobj = GLU.NewQuadric
          GLU.Sphere(qobj, 1.0, 20, 20)
          GL.Viewport(0, 0, width, height)
          
          GL.Flush
          drawable.wait_gl
          drawable.gl_end
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
        drawable.gl_begin(Gdk::GLContext.new(drawable, nil, false, 0))
        init_gl
      end

      def init_gl
        GL.ClearDepth(1.0)
        GL.Clear(GL::DEPTH_BUFFER_BIT)
    
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
      
    end
  end
end
