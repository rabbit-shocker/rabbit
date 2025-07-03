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

require_relative "container-element"
require_relative "block-element"

module Rabbit
  module Element
    class EnumList
      include ContainerElement
      include BlockElement

      def to_html(generator)
        "<ol>\n#{super}\n</ol>"
      end
    end

    class EnumListItem
      include ContainerElement
      include BlockElement

      attr_accessor :order

      def to_rd
        prefix = "(#{order}) "
        indent = " " * prefix.length
        first, *rest = text.split(/\n/)
        rest = rest.collect do |line|
          "#{indent}#{line}"
        end.join("\n")
        "#{prefix}#{first}\n#{rest}".rstrip
      end

      def to_html(generator)
        "<li>\n#{super}\n</li>"
      end
    end
  end
end

