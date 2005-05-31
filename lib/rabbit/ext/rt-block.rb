module Rabbit
  module Ext
    class BlockVerbatim
      def ext_block_verb_rt(label, content, visitor)
        return nil unless /^rt$/i =~ label
        @rt_visitor = RT2RabbitVisitor.new(visitor)
        @rt_visitor.visit(RT::RTParser.parse(content))
      end
    end
  end
end
