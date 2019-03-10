# Copyright (C) 2014-2019  Kouhei Sutou <kou@cozmixng.org>
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

module Helper
  module Parser
    private
    def parse(parser_class, text)
      canvas = []
      def canvas.logger
        Rabbit::Logger.default
      end
      source = Rabbit::Source::Memory.new("UTF-8", nil)
      source.source = text
      parser = parser_class.new(canvas, source)
      parser.parse
      canvas
    end

    def inspect_canvas(canvas)
      canvas.collect do |page|
        inspect_element(page)
      end
    end

    def inspect_element(element)
      name = element.class.name.split(/::/).last
      if element.respond_to?(:elements)
        children = element.elements.collect {|child| inspect_element(child)}
      else
        children = [element.text]
      end
      [name, *children]
    end
  end
end
