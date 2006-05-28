begin
  require "rabbit/renderer/gl"
rescue LoadError
end

module Rabbit
  module Renderer
    module Pixmap
      module GL
        include Renderer::GL if defined?(Renderer::GL)

        private
        def init_renderer(drawable)
          if gl_available?
            super(@gl_drawable)
          else
            super
          end
        end

        def init_gl_capability(drawable)
          return unless gl_available?
          mode = ::Gdk::GLConfig::MODE_RGBA
          mode |= ::Gdk::GLConfig::MODE_DEPTH
          # gl_config = ::Gdk::GLConfig.new(mode | ::Gdk::GLConfig::MODE_DOUBLE)
          gl_config = ::Gdk::GLConfig.new(mode)
          if drawable.method(:set_gl_capability).arity == 2
            @gl_drawable = drawable.set_gl_capability(gl_config, nil)
          else
            @gl_drawable = drawable.set_gl_capability(gl_config)
          end
          @gl_context = ::Gdk::GLContext.new(@gl_drawable, nil, false,
                                             ::Gdk::GL::RGBA_TYPE)
        end

        def gl_drawable
          @gl_drawable
        end

        def gl_context
          @gl_context
        end
      end
    end
  end
end
