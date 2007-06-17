require 'rabbit/parser/ext/enscript'

module Rabbit
  module Parser
    class RD
      module Ext
        module Enscript
          include Element
          include GetText

          @@enscript_highlight = {}
          enscript_highlight = []
          begin
            enscript_highlight = `enscript --help-highlight`.scan(/^Name: (\w+)/)
          rescue Errno::ENOENT => ignored
          end
          enscript_highlight.flatten.each do |name|
            @@enscript_highlight[name.downcase] = name
          end

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
