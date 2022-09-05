require "rabbit/utils"

module Rabbit
  module Parser
    class Base
      extend ModuleLoader

      def initialize(canvas, source, progress: nil)
        @canvas = canvas
        @source = source
        @progress = progress
      end
    end
  end
end
