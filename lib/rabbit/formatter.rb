require "cgi"

module Rabbit
  
  module Format

    extend Utils
    
    module Formatter

      def h(str)
        CGI.escapeHTML(str.to_s)
      end
      
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

      PANGO2CSS = {
        "font_family" => Proc.new do |name, value|
          ["font-family", "'#{value}'"]
        end,
        "foreground" => "color",
        "size" => Proc.new do |name, value|
          ["font-size", "#{(value / Pango::SCALE) * 2}px"]
        end,
        "style" => "font-style",
        "weight" => "font-weight",
      }

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
        tagged_text(text, "span", {name => @value})
      end
      
      def html_format(text)
        css_name, css_value = pango2css(name, @value)
        tagged_text(text, "span", {'style' => "#{css_name}: #{css_value};"})
      end

      private
      def pango2css(name, value)
        css_name = PANGO2CSS[name]
        if css_name.respond_to?(:call)
          css_name.call(name, value)
        else
          [css_name || name, value]
        end
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
    
    class Foreground
      def text_formatter?
        false
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

  end
end
