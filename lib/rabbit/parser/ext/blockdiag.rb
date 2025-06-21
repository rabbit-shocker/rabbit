# Copyright (C) 2011-2025  Sutou Kouhei <kou@cozmixng.org>
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
      module BlockDiag
        include GetText

        module_function
        AVAILABLE_FLAG_OPTIONS = ["antialias"]
        # "font" is treated as specially.
        AVAILABLE_VALUE_OPTIONS = []
        def make_image(path, prop)
          image_file = Tempfile.new(["rabbit-image-blockdiag", ".svg"])
          command_line = [
            "-T", "svg",
            "-o", image_file.path,
          ]
          font = find_font(prop)
          command_line.concat(["-f", font]) if font
          AVAILABLE_FLAG_OPTIONS.each do |name|
            command_line << "--#{name}" if /\A(?:true|yes)\z/i =~ prop[name].to_s
          end
          AVAILABLE_VALUE_OPTIONS.each do |name|
            command_line.concat(["--#{name}", prop[name]]) if prop.has_key?(name)
          end
          command_line << path
          blockdiag_commands = ["blockdiag3", "blockdiag"]
          blockdiag_commands.each do |blockdiag|
            if SystemRunner.run(blockdiag, *command_line)
              return image_file
            end
          end
          command_line.unshift(blockdiag_commands.first)
          format = _("tried blockdiag command: %s")
          additional_info = format % command_line
          raise BlockDiagCanNotHandleError.new(command_line.join(' '),
                                               additional_info)
        end

        def find_font(prop)
          font = prop["font"]
          return font if font and File.exist?(font)
          fonts = prop["fonts"]
          return nil if fonts.nil?
          fonts = fonts.split(/\s*,\s*/) if fonts.is_a?(String)
          fonts.find do |font|
            File.exist?(font)
          end
        end
      end
    end
  end
end
