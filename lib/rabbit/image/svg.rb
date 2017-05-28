require "rsvg2"

require "rabbit/image/base"

module Rabbit
  module ImageManipulable

    class SVG < Base

      unshift_loader(self)

      class << self
        def match?(filename)
          File.open(filename) do |f|
            begin
              /<svg|<!DOCTYPE\s+svg/ =~ f.read(200)
            rescue EncodingError
              false
            end
          end
        end
      end

      def draw(canvas, x, y, params={})
        if @handle
          default_params = {
            :width => width,
            :height => height,
          }
          canvas.draw_rsvg_handle(@handle, x, y, default_params.merge(params))
        else
          super
        end
      end

      def pixbuf
        @pixbuf ||= to_pixbuf
      end

      private
      def update_size
        rsvg_environment do |name|
          @handle = Rsvg::Handle.new(:path => name)
          dim = @handle.dimensions
          @width = dim.width
          @height = dim.height
        end
      end

      def filename
        File.expand_path(@filename)
      end

      def rsvg_environment
        dir = File.dirname(filename)
        name = File.basename(filename)
        Dir.chdir(dir) do
          yield(name)
        end
      end

      def to_pixbuf
        surface = Cairo::ImageSurface.new(width, height)
        context = Cairo::Context.new(surface)
        context.render_rsvg_handle(@handle)
        surface.finish
        if surface.method(:to_pixbuf).arity == -1
          surface.to_pixbuf
        else
          # TODO: Remove this branch when rsvg2 3.1.4 is released.
          surface.to_pixbuf(0, 0, width, height)
        end
      end
    end
  end
end
