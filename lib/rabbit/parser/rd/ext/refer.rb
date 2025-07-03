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
require_relative "image"

module Rabbit
  module Parser
    class RD
      module Ext
        class Refer < Base
          include Image

          def default_ext_refer(label, source, content, visitor)
            ref = ReferText.new(content)
            ref.to = label.element_label
            ref
          end

          # For backward compatibility.
#         def ext_refer_img(label, content, visitor)
#           img(label, content, visitor)
#         end

          def ext_refer_quote(label, source, content, visitor)
            return nil unless /^quote:(.*)$/ =~ label.element_label
            quoted_name = $1
            quoted_label = ::RD::Reference::RDLabel.new(quoted_name,
                                                        label.filename)
            source = quoted_label.to_s if label.to_s == source
            default_ext_refer(quoted_label, source, content)
          end
        end
      end
    end
  end
end
