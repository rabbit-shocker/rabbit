require "erb"
require "rabbit/renderer/color"

module Rabbit
  module Format

    extend Utils

    module Formatter
      include ERB::Util

      def text_formatter?
        false
      end

      def html_formatter?
        false
      end

      def tagged_text(text, name, attrs)
        attrs = attrs.collect do |key, value|
          %Q[ #{h(key)}="#{h(value)}"]
        end.join("")
        "<#{name}#{attrs}>#{text}</#{name}>"
      end
    end

    module SpanTextFormatter
      include Formatter

      attr_reader :value

      def initialize(value)
        @value = value
      end

      def text_formatter?
        true
      end

      def html_formatter?
        true
      end

      def format(text)
        tagged_text(text, "span", {name => pango_value})
      end

      def html_format(text)
        tagged_text(text, "span", {'style' => "#{css_name}: #{css_value};"})
      end

      def pango_value
        @value
      end

      private
      def css_name
        name
      end

      def css_value
        value
      end
    end

    %w(font_desc font_family face size style weight variant
        stretch foreground background underline
        underline_color rise strikethrough
        strikethrough_color fallback lang).each do |name|
      module_eval(<<-EOC)
        class #{to_class_name(name)}
          include SpanTextFormatter

          def name
            #{name.dump}
          end
        end
EOC
    end

    class FontFamily
      private
      def css_name
        "font-family"
      end

      def css_value
        "'#{@value}'"
      end
    end

    class Foreground
      def pango_value
        Renderer::Color.parse(@value).to_gdk_format
      end

      private
      def css_name
        "color"
      end
    end

    class Size
      def pango_value
        if @value > Pango::SCALE # For backward compatibility
          @value.ceil
        else
          (@value * Pango::SCALE).ceil
        end
      end

      private
      def css_name
        "font-size"
      end

      def css_value
        if value > Pango::SCALE # For backward compatibility
          "#{(value / Pango::SCALE) * 2}px"
        else
          "#{value * 2}px"
        end
      end
    end

    class Style
      private
      def css_name
        "font-style"
      end
    end

    class Weigth
      private
      def css_name
        "font-weight"
      end
    end

    module ConvenienceTextFormatter
      include Formatter

      def text_formatter?
        true
      end

      def html_formatter?
        true
      end

      def format(text)
        tagged_text(text, name, {})
      end

      def html_format(text)
        tagged_text(text, name, {})
      end
    end

    %w(b big i s sub sup small tt u).each do |name|
      module_eval(<<-EOC)
        class #{to_class_name(name)}
          include ConvenienceTextFormatter

          def name
            #{name.dump}
          end
        end
      EOC
    end

    module ValueContainerFormatter
      include Formatter

      attr_reader :value
      def initialize(value)
        @value = value
      end
    end

    %w(shadow-color shadow-x shadow-y).each do |name|
      module_eval(<<-EOC)
        class #{to_class_name(name)}
          include ValueContainerFormatter

          def name
            #{name.dump}
          end
        end
      EOC
    end
  end
end
