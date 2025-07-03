# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "base"

module Rabbit
  module ImageManipulable
    class EPS < Base

      unshift_loader(self)

      GS_COMMANDS = %w(gs gswin32c)

      include SystemRunner

      class << self
        def match?(filename)
          File.open(filename) do |f|
            begin
              f.each_line do |line|
                case line
                when /^%!PS-Adobe-\d+.\d+ EPS/i
                  return true
                when /^%%/
                  # ignore
                else
                  return false
                end
              end
              false
            rescue EncodingError, ArgumentError
              false
            end
          end
        end
      end

      private
      def update_size
        load_image
      end

      def load_image(width=nil, height=nil)
        data = begin
                 eps_to_png(width, height)
               rescue EPSCanNotHandleError
                 eps_to_pnm(width, height)
               end
        load_data(data)
      end

      def eps_to_png(width=nil, height=nil)
        eps_to(width, height, "pngalpha")
      end

      def eps_to_pnm(width=nil, height=nil)
        eps_to(width, height, "pnm")
      end

      def eps_to(width, height, device, *gs_options)
        x, y, w, h, r = eps_size
        resolution = (r || Canvas::INTERNAL_DPI).round
        ratio = resolution.to_f / Canvas::INTERNAL_DPI
        width = ((width || w) * ratio).round
        height = ((height || h) * ratio).round

        adjust_eps_if_need(x, y) do |path|
          tmp = Tempfile.new("rabbit-image-eps")
          args = %W(-q -dBATCH -dNOPAUSE -sDEVICE=#{device}
            -sOutputFile=#{tmp.path} -dEPSFitPage
            -dGraphicsAlphaBits=4 -dTextAlphaBits=4
            -g#{width}x#{height} -r#{resolution}x#{resolution}
            #{path})
          if GS_COMMANDS.any? {|gs| run(gs, *args)}
            begin
              tmp.open
              tmp.binmode
              tmp.read
            ensure
              tmp.close
            end
          else
            raise EPSCanNotHandleError.new("gs #{args.join(' ')}", GS_COMMANDS)
          end
        end
      end

      def eps_size
        sx, sy, w, h, r = nil
        File.read(@filename).split(/(?:\r\n?|\n)/).each do |line|
          if /^%%BoundingBox:\s*/ =~ line
            next if $POSTMATCH.chomp == '(atend)'
            sx, sy, ex, ey = $POSTMATCH.scan(/-?\d+/).map{|x| Integer(x)}
            w, h = ex - sx, ey - sy
          elsif /^%%Feature:\s*\*Resolution\s*(\d+)dpi/ =~ line
            r = $1.to_i
          end
          break if r and sx and sy and w and h
        end
        [sx, sy, w, h, r]
      end

      def adjust_eps_if_need(x, y)
        if x and y and x > 0 and y > 0
          tmp = Tempfile.new("rabbit-loader-adjusted-eps")
          tmp.puts("#{x} neg #{y} neg translate")
          tmp.print(File.open(@filename) {|f| f.read})
          tmp.close
          yield tmp.path
        else
          yield @filename
        end
      end
    end
  end
end
