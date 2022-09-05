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

module Rabbit
  module Parser
    module Ext
      module Mermaid
        include GetText

        module_function
        def make_image(path, prop, logger, progress: nil)
          format = prop["format"] || "pdf"
          image_file = Tempfile.new(["rabbit-image-mermaid", ".#{format}"])
          background_color = prop["background_color"] || "transparent"
          command_line = [
            "npx",
            "--yes",
            "--package=@mermaid-js/mermaid-cli",
            "mmdc",
            "--backgroundColor", background_color,
            "--input", path,
            "--output", image_file.path,
            "--pdfFit",
          ]
          if SystemRunner.run(*command_line, progress: progress)
            image_file
          else
            format = _("tried mermaid command: %s")
            additional_info = format % command_line.inspect
            raise MermaidCanNotHandleError.new(command_line.join(' '),
                                               additional_info)
          end
        end
      end
    end
  end
end
