require "hikidoc"

require "rabbit/parser/base"

module Rabbit
  module Parser
    class Wiki < Base
      unshift_loader(self)
      class << self
        def match?(source)
          /^!/.match(source.read)
        end
      end

      include Element
      def parse
        HikiDoc.new(RabbitOutput.new(@canvas)).compile(@source.read)
      end
    end
  end
end

require "rabbit/parser/wiki/output"
