# Copyright (C) 2010-2025  Sutou Kouhei <kou@cozmixng.org>
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
      module AAFigure
        include GetText

        module_function
        AVAILABLE_OPTIONS = ["linewidth", "foreground", "fill", "background",
                             "option"]
        def make_image(path, prop)
          image_file = Tempfile.new("rabbit-image-aafigure")
          command = [
            "aafigure",
            "--type", "svg",
            "--encoding", "utf-8",
            "--output", image_file.path,
          ]
          aafigure_options = []
          AVAILABLE_OPTIONS.each do |name|
            command.concat(["--#{name}", prop[name]]) if prop.has_key?(name)
          end
          command << path
          if SystemRunner.run(*command)
            image_file
          else
            format = _("tried aafigure command: %s")
            additional_info = format % command.inspect
            raise AAFigureCanNotHandleError.new(command.join(' '),
                                                additional_info)
          end
        end
      end
    end
  end
end
