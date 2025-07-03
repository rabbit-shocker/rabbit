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

require_relative "container-element"
require_relative "text-block-element"

module Rabbit
  module Element
    class MethodList
      include ContainerElement
    end

    class MethodListItem
      include ContainerElement

      attr_reader :term, :description

      def initialize(term, description)
        super()
        @term = term
        @description = description
        add_element(@term)
        add_element(@description)
      end

      def name
        @term.name
      end
    end

    class MethodTerm
      include TextBlockElement

      attr_accessor :name
    end

    class MethodName
      include TextContainerElement
    end

    class ClassName
      include TextContainerElement
    end

    class MethodKind
      include TextContainerElement
    end

    class MethodDescription
      include ContainerElement
    end
  end
end
