begin
  require "rabbit/renderer/gl"
rescue LoadError
end

module Rabbit
  module Renderer
    module Display
      module GL
        include Renderer::GL if defined?(Renderer::GL)

        private
        def init_gl(widget)
          return unless gl_available?
          init_gl_event
          init_gl_capability(widget)
        end

        def init_gl_event
          begin_x = nil
          begin_y = nil
          start_time = nil
          handled = false
          view_scale_max = 2.0
          view_scale_min = 0.5

          add_button_press_hook do |event|
            handled = false
            start_time = event.time
            begin_x = event.x
            begin_y = event.y
            false
          end

          add_motion_notify_hook do |event|
            state = event.state
            if state.button2_mask?
              handled = true
              processed = true
              x = event.x.to_f
              y = event.y.to_f
              w = width.to_f
              h = height.to_f

              if state.control_mask?
                scale = gl_scale * (1.0 + (y - begin_y) / h)
                scale = [view_scale_min, scale].max
                self.gl_scale = [scale, view_scale_max].min
              elsif state.shift_mask?
                d_quat = TrackBall.trackball((2.0 * begin_x - w) / w,
                                             (h - 2.0 * begin_y) / h,
                                             (2.0 * x - w) / w,
                                             (h - 2.0 * y) / h)
                quat = TrackBall.add_quats(d_quat, gl_quaternion)
                self.gl_quaternion = quat
              else
                processed = false
              end

              if processed and event.time > start_time + 100
                redraw
                start_time = event.time
              end
            end
            false
          end

          add_button_release_hook do |event, last_button_press_event|
            redraw if handled
            handled
          end
        end

        def init_gl_capability(widget)
          mode = ::Gdk::GLConfig::MODE_RGBA
          mode |= ::Gdk::GLConfig::MODE_DEPTH
          # gl_config = ::Gdk::GLConfig.new(mode | ::Gdk::GLConfig::MODE_DOUBLE)
          gl_config = ::Gdk::GLConfig.new(mode)
          widget.set_gl_capability(gl_config)
          @gl_widget = widget
        end

        def finalize_gl
          @gl_widget = nil
        end

        def gl_drawable
          @gl_widget.gl_drawable
        end

        def gl_context
          @gl_widget.gl_context
        end
      end
    end
  end
end

