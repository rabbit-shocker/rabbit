require 'rabbit/parser/ext/enscript'

module Rabbit
  module Parser
    class RD
      module Ext
        module Enscript
          include GetText

          def enscript_block(label, lang, source, content, visitor)
            src, prop = parse_source(source)
            logger = visitor.logger

            result = nil
            if Parser::Ext::Enscript.check_availability(lang, logger)
              result = Parser::Ext::Enscript.highlight(lang, src, logger)
            end
            result || default_ext_block_verbatim(label, src, src, visitor)
          end
        end
      end
    end
  end
end
