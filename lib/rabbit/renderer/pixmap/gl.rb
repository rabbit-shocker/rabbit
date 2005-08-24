require "gtkglext"

require "rabbit/rabbit"
Rabbit.add_gui_init_proc do
  Gtk::GL.init
end

require "rabbit/trackball"
require "rabbit/renderer/pixmap/base"

module Rabbit
  module Renderer
    class PixmapGL < PixmapBase
      
      def initialize(canvas, width=nil, height=nil)
        super
        @view_quat  = [0.0, 0.0, 0.0, 1.0]
        @view_scale = 1.0
        @logo_quat  = [0.0, 0.0, 0.0, 1.0]
        init_view
      end
      
      def draw_slide(slide, simulation)
        super(slide, simulation) do
          yield
        end
        drawable.gl_end
      end

      # because GtkGLDrawable is not implement
      def draw_layout(layout, x, y, color=nil)
        gc = make_gc(color)
        @pixmap.draw_layout(gc, x, y, layout)
      end

      def draw_cube(filled, x, y, z, size, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_cube(filled, size)
        end
      end
      
      def draw_sphere(filled, x, y, z, radius, slices, stacks,
                      color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_sphere(filled, radius, slices, stacks)
        end
      end
      
      def draw_cone(filled, x, y, z, base, height, slices, stacks,
                    color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_cone(filled, base, height, slices, stacks)
        end
      end
      
      def draw_torus(filled, x, y, z, inner_radius, outer_radius,
                     n_sides, rings, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_torus(filled, inner_radius,
                             outer_radius, n_sides, rings)
        end
      end
      
      def draw_tetrahedron(filled, x, y, z, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_tetrahedron(filled)
        end
      end
      
      def draw_octahedron(filled, x, y, z, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_octahedron(filled)
        end
      end
      
      def draw_dodecahedron(filled, x, y, z, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_dodecahedron(filled)
        end
      end
      
      def draw_icosahedron(filled, x, y, z, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_icosahedron(filled)
        end
      end
      
      def draw_teapot(filled, x, y, z, scale, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          Gdk::GL.draw_teapot(filled, scale)
        end
      end

      def gl_compile(id)
        GL.NewList(id, GL::COMPILE)
        yield
        GL.EndList
      end

      def gl_call_list(id, x, y, z, color=nil, &block)
        draw_gl(x, y, z, color, block) do
          GL.CallList(id)
        end
      end

      def gl_supported?
        true
      end

      def setup_event(area)
        begin_x = nil
        begin_y = nil
        start_time = nil
        handled = false
        view_scale_max = 2.0
        view_scale_min = 0.5
        
        area.add_button_press_hook do |event|
          handled = false
          start_time = event.time
          begin_x = event.x
          begin_y = event.y
          false
        end
        
        area.add_motion_notify_hook do |event|
          state = event.state
          if state.button3_mask?
            handled = true
            x = event.x.to_f
            y = event.y.to_f
            w = width.to_f
            h = height.to_f
            
            if state.control_mask?
              @view_scale = @view_scale * (1.0 + (y - begin_y) / h)
              if @view_scale > view_scale_max
                @view_scale = view_scale_max
              elsif @view_scale < view_scale_min
                @view_scale = view_scale_min
              end
            elsif state.shift_mask?
              d_quat = TrackBall.trackball((2.0 * begin_x - w) / w,
                                           (h - 2.0 * begin_y) / h,
                                           (2.0 * x - w) / w,
                                           (h - 2.0 * y) / h)
              @view_quat = TrackBall.add_quats(d_quat, @view_quat)
            end
            
            if event.time > start_time + 100
              area.redraw
              start_time = event.time
            end
          end
          false
        end
        
        area.add_button_release_hook do |event, last_button_press_event|
          area.redraw if handled
          handled
        end
      end
      
      def clear_pixmaps
        @pixmaps.values.each do |pixmap|
          pixmap.unset_gl_capability
        end
        super
        @contexts = {}
      end
      
      private
      def drawable
        @pixmap.gl_pixmap
      end

      def init_pixmap(slide, simulation)
        super
        if simulation
          mode = Gdk::GLConfig::MODE_RGBA | Gdk::GLConfig::MODE_DEPTH
          @gl_config ||= Gdk::GLConfig.new(mode)
          if @pixmap.method(:set_gl_capability).arity == 2
            # bug of Ruby/GtkGLExt <= 0.13.0
            @pixmap.set_gl_capability(@gl_config, nil)
          else
            @pixmap.set_gl_capability(@gl_config)
          end
          @contexts[slide] = Gdk::GLContext.new(drawable, nil, false, 0)
        end
        drawable.gl_begin(@contexts[slide])
        init_gl
      end

      def init_view
        angle = 0.0 * (Math::PI / 180.0)
        axis_x = 1.0
        axis_y = 0.0
        axis_z = 0.0
        sine = Math.sin(0.5 * angle)
        @view_quat  = [
          axis_x * sine, 
          axis_y * sine,
          axis_z * sine,
          Math.cos(0.5 * angle)
        ]
        @view_quat = TrackBall::Vector.new(@view_quat)
        @view_scale = 1.0
      end
      
      def init_gl
        GL.ClearDepth(1.0)
        GL.Clear(GL::DEPTH_BUFFER_BIT)
    
        GL.Viewport(0, 0, width, height)
        
        GL.Light(GL::LIGHT0, GL::POSITION, [1.0, 1.0, 1.0, 0.0])
        GL.Enable(GL::LIGHTING)
        GL.Enable(GL::LIGHT0)
        GL.Enable(GL::DEPTH_TEST)

        GL.MatrixMode(GL::PROJECTION)
        GL.LoadIdentity
        GLU.Perspective(30.0, width / height, 1.0, z_far)

        GL.MatrixMode(GL::MODELVIEW)
        GL.LoadIdentity
        GLU.LookAt(0.0, 0.0, z_view,
                   0.0, 0.0, 0.0,
                   0.0, 1.0, 0.0)
        GL.Translate(0.0, 0.0, -z_view)
        GL.Scale(@view_scale, @view_scale, @view_scale)
        GL.MultMatrix(@view_quat.build_rotmatrix)
        drawable.wait_gl
      end

      def draw_gl(x, y, z, color, block)
        drawable.wait_gdk
        GL.PushMatrix
        setup_geometry(x, y, z)
        setup_color(color)
        block.call(true) if block
        yield
        block.call(false) if block
        GL.Flush
        GL.PopMatrix
        drawable.wait_gl
      end

      def setup_color(color)
        if color.nil?
          color = @foreground.foreground
        elsif color.is_a?(String)
          color = Gdk::Color.parse(color)
        end
        color = Color.new_from_gdk_color(color, true)
        GL.Light(GL::LIGHT0, GL::DIFFUSE, color.to_a)
      end

      def setup_geometry(x, y, z)
        new_x = to_gl_size(x, width)
        new_y = to_gl_size(height - y, height)
        new_z = z
        GL.MatrixMode(GL::MODELVIEW)
        GL.Translate(new_x, new_y, new_z)
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
