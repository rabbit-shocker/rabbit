require "rabbit/utils"

module Rabbit
  module Parser
    class Base
      extend ModuleLoader

      def initialize(canvas, source)
        @canvas = canvas
        @source = source
      end
    end
  end
end
