require "rabbit/parser/rd/rd2rabbit-lib"

module Rabbit
  module Parser
    class RD
      def initialize(canvas, source)
        @canvas = canvas
        @source = source
      end

      def parse
        tree = ::RD::RDTree.new("=begin\n#{@source.read}\n=end\n")
        visitor = RD2RabbitVisitor.new(@canvas)
        visitor.visit(tree)
      end
    end
  end
end
