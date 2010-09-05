require 'rabbit/element/text-container-element'
require 'rabbit/element/block-element'

module Rabbit
  module Element
    module TextBlockElement
      include TextContainerElement
      include BlockElement
    end
  end
end
