# Copyright (C) 2017  Kouhei Sutou <kou@cozmixng.org>
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

require "rouge"

require "rabbit/utils"
require "rabbit/parser/ext/escape"

module Rabbit
  module Parser
    module Ext
      module Rouge
        include GetText

        module_function
        def highlight(lang, text, logger)
          lexer = ::Rouge::Lexer.find(lang).new
          formatter = RabbitFormatter.new
          block = Element::SyntaxHighlightingBlock.new
          formatter.format(lexer.lex(text.strip)) do |element|
            block << element
          end
          block
        end

        class RabbitFormatter < ::Rouge::Formatter
          tag "rabbit"

          include Element

          def stream(tokens)
            tokens.each do |token, value|
              escaped_text = Escape.escape_meta_character(value)
              text_element = SyntaxHighlightingText.new(Text.new(escaped_text))
              tag_name = compute_tag_name(token)
              if Utils.syntax_highlighting_debug?
                p [tag_name, token.qualname, value]
              end
              yield(CustomTag.new("syntax-#{tag_name}", text_element))
            end
          end

          private
          def compute_tag_name(token)
            group = token.token_chain.first.name
            case group
            when :Keyword
              case token.name
              when :Constant
                tag_namenize(token.name)
              else
                tag_namenize(group)
              end
            when :Name
              case token.name
              when :Namespace
                "include"
              else
                case token.parent.name
                when :Variable
                  "#{tag_namenize(token.name)}_variable"
                else
                  tag_namenize(token.name)
                end
              end
            when :Literal
              if match_token?("Literal.String", token)
                case token.name
                when :Symbol
                  tag_namenize(token.name)
                else
                  "string"
                end
              elsif match_token?("Literal.Number", token)
                if match_token?("Literal.Number.Float", token)
                  "float"
                elsif match_token?("Literal.Number.Integer", token)
                  "integer"
                else
                  tag_namenize(token.name)
                end
              else
                tag_namenize(token.name)
              end
            when :Generic
              tag_name = tag_namenize(token.name)
              case tag_name
              when "deleted"
                "delete"
              when "inserted"
                "insert"
              else
                tag_name
              end
            else
              tag_namenize(group)
            end
          end

          def tag_namenize(name)
            name.to_s.downcase
          end

          def match_token?(name, token)
            ::Rouge::Token[name].matches?(token)
          end
        end
      end
    end
  end
end
