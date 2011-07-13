require 'rabbit/element/slide-element'
require 'rabbit/element/text-block-element'
require 'rabbit/element/description-list'

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
