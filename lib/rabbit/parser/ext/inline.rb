# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../../element"

module Rabbit
  module Parser
    module Ext
      module Inline
        include Element

        module_function
        def sub(text)
          Subscript.new(text)
        end

        def sup(text)
          Superscript.new(text)
        end

        def note(text)
          Note.new(text)
        end

        def lang(lang, text)
          text.add_default_prop("lang", lang)
          text
        end
      end
    end
  end
end
