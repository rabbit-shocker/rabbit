# Copyright (C) 2004-2022  Sutou Kouhei <kou@cozmixng.org>
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

require 'rabbit/element'
require 'rabbit/parser/ext/image'

module Rabbit
  module Parser
    class RD
      module Ext
        module Image
          include Parser::Ext::Image

          def img(label, content, visitor)
            label = label.to_s
            return nil unless /^img:\s*(.+)$/ =~ label
            make_image(visitor.canvas, $1, body: visitor.current_body)
          end

          def make_image_from_file(source, visitor, **options)
            src, prop = parse_source(source)
            super(visitor.canvas,
                  src,
                  body: visitor.current_body,
                  **options) do |src_file_path|
              [yield(src_file_path, prop), prop]
            end
          end
        end
      end
    end
  end
end
