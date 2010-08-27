require 'rabbit/utils'
require 'rabbit/parser/ext/inline'
require 'rabbit/parser/rd/ext/base'
require 'rabbit/parser/rd/ext/image'
require 'rabbit/parser/rd/ext/entity'

module Rabbit
  module Parser
    class RD
      module Ext
        class InlineVerbatim < Base
          extend Utils
          include Image
          include Entity

          Inline = Parser::Ext::Inline

          def default_ext_inline_verbatim(label, source, content, visitor)
            Text.new(source)
          end

          def apply_inline_verbatim(visitor, text)
            visitor.apply_to_Verb(::RD::Verb.new(text))
          end

#           def ext_inline_verb_img(label, content, visitor)
#             img(label, content, visitor)
#           end

          def ext_inline_verb_quote(label, source, content, visitor)
            label = label.to_s
            return nil unless /^quote:(.*)$/ =~ label
            default_ext_inline_verb($1, visitor.apply_to_String($1), $1, visitor)
          end

          def ext_inline_verb_del(label, source, content, visitor)
            label = label.to_s
            return nil unless /^del:(.*)$/ =~ label
            DeletedText.new(apply_inline_verbatim(visitor, $1))
          end

          def ext_inline_verb_sub(label, source, content, visitor)
            label = label.to_s
            return nil unless /^sub:(.*)$/ =~ label
            sub_text = apply_inline_verbatim(visitor, $1)
            Inline.sub(sub_text)
          end

          def ext_inline_verb_sup(label, source, content, visitor)
            label = label.to_s
            return nil unless /^sup:(.*)$/ =~ label
            sup_text = apply_inline_verbatim(visitor, $1)
            Inline.sup(sup_text)
          end

          def ext_inline_verb_note(label, source, content, visitor)
            label = label.to_s
            return nil unless /^note:(.*)$/ =~ label
            target = apply_inline_verbatim(visitor, $1)
            Inline.note(target)
          end

          def ext_inline_verb_lang(label, source, content, visitor)
            label = label.to_s
            return nil unless /^lang:([a-z]{2,2}):(.*)$/ =~ label
            Inline.lang($1, apply_inline_verbatim(visitor, $2))
          end

          def ext_inline_verb_wait(label, source, content, visitor)
            label = label.to_s
            return nil unless /^wait$/ =~ label
            WaitTag.new
          end

          def ext_inline_verb_tag(label, source, content, visitor)
            label = label.to_s
            return nil unless /^tag:(.+?)(?::(.+))?$/ =~ label
            name = $1
            content = $2
            if content
              CustomTag.new(name, apply_inline_verbatim(visitor, content))
            else
              CustomTag.new(name)
            end
          end
        end
      end
    end
  end
end
