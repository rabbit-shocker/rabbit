require 'rabbit/element/slide-element'
require 'rabbit/element/text-container-element'
require 'rabbit/element/description-list'

module Rabbit
  module Element
    class TitleSlide
      include SlideElement

      def initialize(title)
        super(title.text)
        add_element(title)
        @local_prop = {}
      end

      def <<(element)
        if element.is_a?(DescriptionList)
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
              @local_prop[name] = content.strip
            end
          end
        end
      end

      def theme
        @local_prop["theme"]
      end

      def allotted_time
        time = @local_prop["allotted-time"]
        time = parse_time(time) if time
        time
      end

      private
      def normalize_name(name)
        name.gsub(/_/, "-")
      end

      def parse_time(str)
        if /\A\s*\z/m =~ str
          nil
        else
          if /\A\s*(\d*\.?\d*)\s*(h|m|s)?\s*\z/i =~ str
            time = $1.to_f
            unit = $2
            if unit
              case unit.downcase
              when "m"
                time *= 60
              when "h"
                time *= 3600
              end
            end
            time.to_i
          else
            nil
          end
        end
      end
    end

    class Title
      include TextContainerElement

      def to_html(generator)
        "<h1>#{super}</h1>"
      end
    end

    class Subtitle
      include TextContainerElement

      def to_html(generator)
        "<h2>#{super}</h2>"
      end
    end

    class Author
      include TextContainerElement

      def to_html(generator)
        "<address>#{super}</address>"
      end
    end

    class ContentSource
      include TextContainerElement

      def to_html(generator)
        "<p class='content-source'>#{super}</p>"
      end
    end

    class Institution
      include TextContainerElement

      def to_html(generator)
        "<p class='institution'>#{super}</p>"
      end
    end

    class Date
      include TextContainerElement

      def to_html(generator)
        "<p class='date'>#{super}</p>"
      end
    end

    class Place
      include TextContainerElement

      def to_html(generator)
        "<p class='place'>#{super}</p>"
      end
    end

    class When
      include TextContainerElement

      def to_html(generator)
        "<p class='when'>#{super}</p>"
      end
    end

    class Where
      include TextContainerElement

      def to_html(generator)
        "<p class='where'>#{super}</p>"
      end
    end
  end
end
