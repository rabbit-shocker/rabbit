# Copyright (C) 2006-2023  Sutou Kouhei <kou@cozmixng.org>
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

require 'rabbit/element/text-element'
require 'rabbit/element/text-container-element'
require 'rabbit/element/text-block-element'

module Rabbit
  module Element
    class Text
      include TextElement
    end

    class FontAwesomeText
      include TextElement

      class << self
        def [](name)
          mapping[name]
        end

        private
        @mapping = nil
        def mapping
          @mapping ||= File.open(css_path) do |css|
            mapping = {}
            names = []
            css.each_line do |line|
              case line
              when /\A\.fa-(.+?):before/
                names << Regexp.last_match[1]
              when /\A\s*content:\s*\"\\(.+?)\"/
                code_point = Integer(Regexp.last_match[1], 16)
                character = [code_point].pack("U")
                names.each do |name|
                  mapping[name] = character
                end
                names.clear
              when /\A}/
                names.clear
              end
            end
            mapping
          end
        end

        def css_path
          ENV["RABBIT_FONT_AWESOME_CSS"] ||
            "/usr/share/fonts-font-awesome/css/font-awesome.css" # Debian
        end
      end

      def initialize(*args)
        super(*args)
        @character = self.class[@text]
        raise UnknownFontAwesomeNameError.new(@text) if @character.nil?
      end

      def markuped_text
        "<span font_family=\"FontAwesome\">#{@character}</span>"
      end
    end

    class TextContainer
      include TextContainerElement
    end

    class TextBlock
      include TextBlockElement
    end

    class Keyword
      include TextContainerElement
    end

    class Comment
      include TextContainerElement
    end

    class Emphasis
      include TextContainerElement
    end

    class Code
      include TextContainerElement
    end

    class Variable
      include TextContainerElement
    end

    class Keyboard
      include TextContainerElement
    end

    class Index
      include TextContainerElement
    end

    class Note
      include TextContainerElement
    end

    class Verbatim
      include TextContainerElement
    end

    class DeletedText
      include TextContainerElement
    end

    class ReferText
      include TextContainerElement

      attr_accessor :to

      # TODO: This makes paragraph instead of word link.
      def draw_sub_elements(canvas, x, y, w, h)
        canvas.draw_link(to) do
          super
        end
      end
    end

    class Subscript
      include TextContainerElement
    end

    class Superscript
      include TextContainerElement
    end
  end
end
