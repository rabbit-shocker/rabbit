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
      
      def format(text)
        tagged_text(text, "span", {name => @value})
      end
      
    end
    
    %w(font_desc font_family face size style weight variant
        stretch foreground background underline rise
        strikethrough fallback lang).each do |name|
      module_eval(<<-EOC)
        class #{to_class_name(name)}
          include SpanTextFormatter
          
          def name
            #{name.dump}
          end
        end
EOC
    end

    module ConvenienceTextFormatter

      include Formatter

      def text_formatter?
        true
      end
      
      def format(text)
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
