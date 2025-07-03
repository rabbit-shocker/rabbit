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

require_relative "slide-element"
require_relative "text-block-element"
require_relative "description-list"

module Rabbit
  module Element
    class TitleSlide
      include SlideElement

      def <<(element)
        return unless element.is_a?(DescriptionList)
        element.each do |item|
          name = item.term.collect{|x| x.text}.join("")
          name = normalize_name(name)
          klass_name = to_class_name(name)
          if Element.const_defined?(klass_name)
            meta = Element.const_get(klass_name).new
            item.content.each do |elem|
              elem.each do |e|
                meta << e
              end
            end
            super(meta)
          else
            content = ""
            item.content.each do |x|
              content << x.text
            end
            self[name] = content.strip
          end
        end
      end

      def theme
        self["theme"]
      end

      def allotted_time
        self["allotted-time"]
      end

      def start_time
        self["start-time"]
      end

      def end_time
        self["end-time"]
      end

      def to_html(generator)
        "<div class=\"title-slide\">\n#{super}\n</div>"
      end

      def title
        sub_title = find {|element| element.is_a?(Subtitle)}
        sub_title = sub_title.text if sub_title
        [super, sub_title].compact.join(" - ")
      end

      private
      def normalize_name(name)
        name.gsub(/_/, "-").strip
      end
    end

    class Title
      include TextBlockElement

      def to_rd
        "= #{text}"
      end

      def to_html(generator)
        "<h1>#{super}</h1>"
      end
    end

    class Subtitle
      include TextBlockElement

      def to_rd
        ": subtitle\n   #{text}"
      end

      def to_html(generator)
        "<h2>#{super}</h2>"
      end
    end

    class Author
      include TextBlockElement

      def to_rd
        ": author\n   #{text}"
      end

      def to_html(generator)
        "<address>#{super}</address>"
      end
    end

    class ContentSource
      include TextBlockElement

      def to_rd
        ": content-source\n   #{text}"
      end

      def to_html(generator)
        "<p class='content-source'>#{super}</p>"
      end
    end

    class Institution
      include TextBlockElement

      def to_rd
        ": institution\n   #{text}"
      end

      def to_html(generator)
        "<p class='institution'>#{super}</p>"
      end
    end

    class Date
      include TextBlockElement

      def to_rd
        ": date\n   #{text}"
      end

      def to_html(generator)
        "<p class='date'>#{super}</p>"
      end
    end

    class Place
      include TextBlockElement

      def to_rd
        ": place\n   #{text}"
      end

      def to_html(generator)
        "<p class='place'>#{super}</p>"
      end
    end

    class When
      include TextBlockElement

      def to_rd
        ": when\n   #{text}"
      end

      def to_html(generator)
        "<p class='when'>#{super}</p>"
      end
    end

    class Where
      include TextBlockElement

      def to_rd
        ": where\n   #{text}"
      end

      def to_html(generator)
        "<p class='where'>#{super}</p>"
      end
    end
  end
end
