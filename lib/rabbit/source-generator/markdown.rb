# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../rabbit"

module Rabbit
  module SourceGenerator
    class Markdown
      include GetText
      include PathManipulatable

      def heading(level, title)
        ("#" * level) + " #{title}"
      end

      def definition_list_item(item, description)
        <<-DEFINITION_LIST_ITEM
#{item}
:   #{description}
        DEFINITION_LIST_ITEM
      end

      def unordered_list_item(item)
        "* #{item}"
      end

      def image(source, options={})
        parameters = options.collect do |key, value|
          "#{key}='#{value}'"
        end
        "![](#{source}){:#{parameters.join(' ')}}"
      end

      def preformatted_line(content)
        "    #{content}"
      end

      def comment(content)
        ""
      end
    end
  end
end
