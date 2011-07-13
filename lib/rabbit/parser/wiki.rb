begin
  require "hikidoc"
rescue LoadError
  require "rubygems"
  require "hikidoc"
end

require "rabbit/parser/base"

module Rabbit
  module Parser
    class Wiki < Base
      unshift_loader(self)
      class << self
        def match?(source)
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

require "rabbit/parser/wiki/output"
