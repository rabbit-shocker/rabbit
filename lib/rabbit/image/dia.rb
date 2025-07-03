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
    class Dia < Base

      unshift_loader(self)

      DIA_COMMANDS = %w(dia)

      include SystemRunner

      class << self
        def match?(filename)
          return true if File.extname(filename).downcase.end_with?(".dia")

          File.open(filename) do |f|
            begin
              first_line = f.gets
              second_line = f.gets
              return false unless second_line
              return false unless first_line.start_with?("<?xml")
              return false unless second_line.start_with?("<dia:diagram")
              true
            rescue EncodingError
              false
            end
          end
        end
      end

      delegate

      def initialize(filename, props, canvas: nil)
        init_delegated_loader(filename, props, canvas)
        super
      end

      private
      def init_delegated_loader(filename, props, canvas)
        @svg_file = Tempfile.new(["rabbit-image-loader-dia", ".svg"])
        args = ["--export=#{@svg_file.path}"]
        args << "--filter=svg"
        args << filename
        if DIA_COMMANDS.any? {|dia| run(dia, *args)}
          @delegated_loader = SVG.new(@svg_file.path, props)
        else
          raise DiaCanNotHandleError.new("dia #{args.join(' ')}",
                                         DIA_COMMANDS)
        end
      end

      def load_image
        # do nothing
      end
    end
  end
end
