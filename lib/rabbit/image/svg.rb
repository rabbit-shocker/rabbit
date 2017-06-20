# Copyright (C) 2004-2017  Kouhei Sutou <kou@cozmixng.org>
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

require "rsvg2"

require "rabbit/image/base"

unless Object.const_defined?(:Rsvg)
  Rsvg = RSVG
end

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
        surface.to_pixbuf
      end
    end
  end
end
