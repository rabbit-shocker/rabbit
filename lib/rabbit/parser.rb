# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "parser/markdown"
require_relative "parser/pdf"
require_relative "parser/rd"
require_relative "parser/wiki"

# Image is fallback
require_relative "parser/image"

module Rabbit
  module Parser
    include GetText

    module_function
    def parse(canvas, source, **options)
      parser = Base.find_loader(source)
      if parser.nil?
        format = _("unsupported format. (supported: %s)")
        format_names = Base.loaders.collect do |loader|
          loader.format_name
        end
        message = format % "[#{format_names.join(', ')}]"
        raise UnsupportedFormatError.new(message)
      end
      parser.new(canvas, source, **options).parse
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
