# Copyright (C) 2022  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/image/pdf"

module Rabbit
  module ImageManipulable
    class Mermaid < Base

      unshift_loader(self)

      include SystemRunner

      class << self
        def match?(filename)
          File.extname(filename).downcase.end_with?(".mmd")
        end
      end

      delegate

      def initialize(filename, props, canvas: nil)
        init_delegated_loader(filename, props, canvas)
        super
      end

      private
      def init_delegated_loader(filename, props, canvas)
        background_color = props["background_color"] || "transparent"
        pdf_path = File.open(filename, "rb") do |source|
          cache_processed_data(canvas, [source, background_color], "pdf") do
            @pdf_file = Tempfile.new(["rabbit-image-loader-mermaid", ".pdf"])
            command_line = [
              "npx",
              "--yes",
              "--package=@mermaid-js/mermaid-cli",
              "mmdc",
              "--backgroundColor", background_color,
              "--input", filename,
              "--output", @pdf_file.path,
              "--pdfFit",
            ]
            unless SystemRunner.run(*command_line)
              format = _("tried mermaid command: %s")
              additional_info = format % command_line.inspect
              raise MermaidCanNotHandleError.new(command_line.join(' '),
                                                 additional_info)
            end
            @pdf_file.path
          end
        end
        @delegated_loader = PDF.new(pdf_path, props)
      end

      def load_image
        # do nothing
      end
    end
  end
end
