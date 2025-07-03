# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../utils"
require_relative "base"

module Rabbit
  module ImageManipulable
    class GIMP < Base

      unshift_loader(self)

      GIMP_COMMANDS = %w(gimp)
      HEADER = "gimp xcf file"
      HEADER_SIZE = HEADER.size

      include SystemRunner

      class << self
        def match?(filename)
          File.open(filename, "rb") do |f|
            HEADER == f.read(HEADER_SIZE)
          end
        end
      end

      private
      def update_size
        png_file = Tempfile.new(["rabbit-loader-gimp-png", ".png"])
        png_path = png_file.path
        clip_to_image = 1
        merge_type = clip_to_image
        command = <<-COMMAND
(let ((image (car (gimp-file-load RUN-NONINTERACTIVE
                                  "#{@filename}" "#{@filename}"))))
  (gimp-file-save RUN-NONINTERACTIVE image "#{png_path}")
  (gimp-image-delete image))
        COMMAND
        args = [
          "--no-interface",
          "--batch-interpreter", "plug-in-script-fu-eval",
          "--batch", command,
          "--batch", "(gimp-quit TRUE)",
        ]
        if GIMP_COMMANDS.any? {|gimp| run(gimp, *args); File.exist?(png_path)}
          png_file.open
          png_file.binmode
          load_data(png_file.read)
        else
          raise GIMPCanNotHandleError.new("gimp #{args.join(' ')}",
                                          GIMP_COMMANDS)
        end
      end
    end
  end
end
