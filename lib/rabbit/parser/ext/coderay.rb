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

require 'coderay'

require 'rabbit/utils'
require 'rabbit/parser/ext/escape'

module Rabbit
  module Parser
    module Ext
      module CodeRay
        include GetText

        module_function
        def highlight(lang, text)
          tokens = ::CodeRay.scan(text.strip, lang.to_sym)
          tokens.encode(RabbitEncoder.new)
        end

        class RabbitEncoder < ::CodeRay::Encoders::Encoder
          include Element

          def setup(options)
            super
            @out = SyntaxHighlightingBlock.new
            @containers = [@out]
          end

          def text_token(text, type=:plain)
            p [:text, type, text] if Utils.syntax_highlighting_debug?
            escaped_text = Escape.escape_meta_character(text)
            text_element = SyntaxHighlightingText.new(Text.new(escaped_text))
            tag_name = type.to_s.gsub(/_/, '-')
            tag = CustomTag.new("syntax-#{tag_name}", text_element)
            current_container << tag
          end

          def begin_group(kind)
            p [:begin_group, kind] if Utils.syntax_highlighting_debug?
            @containers << TextContainer.new
            tag = CustomTag.new("syntax-#{kind}")
            current_container << tag
          end

          def begin_line(kind)
            p [:begin_line, kind] if Utils.syntax_highlighting_debug?
          end

          def end_line(kind)
            p [:end_line, kind] if Utils.syntax_highlighting_debug?
          end

          def end_group(kind)
            p [:end_group, kind] if Utils.syntax_highlighting_debug?
            block = @containers.pop
            current_container << block
          end

          def finish(options)
            super
          end

          private
          def current_container
            @containers.last
          end
        end
      end
    end
  end
end
