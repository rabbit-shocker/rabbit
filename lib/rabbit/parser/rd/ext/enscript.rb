require 'rabbit/parser/ext/enscript'

module Rabbit
  module Parser
    class RD
      module Ext
        module Enscript
          include GetText

          def enscript_block(label, lang, source, content, visitor)
            src, prop = parse_source(source)
            default_result = default_ext_block_verbatim(label, src, src, visitor)
            logger = visitor.logger
            unless Parser::Ext::Enscript.available?(lang)
              format = _("enscript: unsupported language: %s")
              logger.warn(format % lang)
              return default_result
            end

            Parser::Ext::Enscript.highlight(src, lang, logger) || default_result
          end
        end
      end
    end
  end
end
