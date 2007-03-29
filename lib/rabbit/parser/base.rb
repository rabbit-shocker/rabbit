require "rabbit/utils"

module Rabbit
  module Parser
    class Base
      extend ModuleLoader

      class << self
        def name
          super.split(/::/).last
        end
      end

      def initialize(canvas, source)
        @canvas = canvas
        @source = source
      end
    end
  end
end
