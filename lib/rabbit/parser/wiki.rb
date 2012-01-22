require "hikidoc"

require "rabbit/parser/base"

module Rabbit
  module Parser
    class Wiki < Base
    end
  end
end

require "rabbit/parser/wiki/output"

module Rabbit
  module Parser
    class Wiki
      unshift_loader(self)
      class << self
        def match?(source)
          return true if /\A(?:hiki|wiki)\z/i =~ source.extension.to_s
          head = source.read[0, 500]
          head.force_encoding("ASCII-8BIT") if head.respond_to?(:force_encoding)
          /^!/.match(head)
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
