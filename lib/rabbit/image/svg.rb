require "rabbit/utils"

Rabbit::Utils.require_safe "rsvg2"

require "rabbit/image/base"

module Rabbit
  module ImageManipulable

    class SVG < Base

      unshift_loader(self)

      class << self
        def match?(filename)
          File.open(filename) do |f|
            /<svg|<!DOCTYPE\s+svg/ =~ f.read(200)
          end
        end
      end

      private
      def ensure_resize(w, h)
        @pixbuf = to_pixbuf(w, h)
      end

      def update_size
        rsvg_environment do |name|
          if RSVG::Handle.respond_to?(:new_from_file)
            handle = RSVG::Handle.new_from_file(name)
            dim = handle.dimensions
            @width = dim.width
            @height = dim.height
          else
            _pixbuf = RSVG.pixbuf_from_file(name)
            @width = _pixbuf.width
            @height = _pixbuf.height
          end
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

      def to_pixbuf(w=nil, h=nil)
        rsvg_environment do |name|
          if w or h
            RSVG.pixbuf_from_file_at_size(name, w || width, h || height)
          else
            RSVG.pixbuf_from_file(name)
          end
        end
      end
    end
  end
end
