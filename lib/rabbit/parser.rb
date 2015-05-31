require "rabbit/utils"

dir = File.join("rabbit", "parser")
Rabbit::Utils.require_files_under_directory_in_load_path(dir)

module Rabbit
  module Parser
    include GetText

    module_function
    def parse(canvas, source)
      parser = Base.find_loader(source)
      if parser.nil?
        format = _("unsupported format. (supported: %s)")
        format_names = Base.loaders.collect do |loader|
          loader.format_name
        end
        message = format % "[#{format_names.join(', ')}]"
        raise UnsupportedFormatError.new(message)
      end
      parser.new(canvas, source).parse
    end

    def normalize_property_name(name)
      name.gsub(/_/, "-").strip
    end

    class SlidePropertySetter
      def initialize(slide)
        @slide = slide
      end

      def apply(element)
        return unless element.is_a?(Element::DescriptionList)
        element.each do |item|
          name = Parser.normalize_property_name(item.term.text)
          @slide[name] = item.content.text.strip
        end
      end
    end

    class NoteSetter
      def initialize(slide)
        @slide = slide
      end

      def apply(element)
        return unless element.is_a?(Element::Paragraph)
        @slide['note'] ||= ""
        @slide['note'] << element.text
      end
    end
  end
end
