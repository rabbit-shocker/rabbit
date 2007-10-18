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
        parser = HikiDoc.new(RabbitOutput.new(@canvas),
                             :use_wiki_name => false)
        parser.compile(@source.read)
      end
    end
  end
end

require "rabbit/parser/wiki/output"
