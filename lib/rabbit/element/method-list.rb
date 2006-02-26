require 'rabbit/element/container-element'
require 'rabbit/element/text-container-element'

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
      include TextContainerElement

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
