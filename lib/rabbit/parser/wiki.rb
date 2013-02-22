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
          extension = source.extension
          if extension.nil?
            head = source.read[0, 500]
            if head.respond_to?(:force_encoding)
              head.force_encoding("ASCII-8BIT")
            end
            /^!/.match(head)
          else
            /\A(?:hiki|wiki)\z/i =~ extension
          end
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
