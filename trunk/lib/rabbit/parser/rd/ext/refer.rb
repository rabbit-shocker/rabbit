require 'rabbit/parser/rd/ext/base'
require 'rabbit/parser/rd/ext/image'

module Rabbit
  module Parser
    class RD
      module Ext
        class Refer < Base
          include Image

          def default_ext_refer(label, source, content, visitor)
            ref = ReferText.new(content)
            ref.to = label.element_label
            ref
          end

          # For backward compatibility.
#         def ext_refer_img(label, content, visitor)
#           img(label, content, visitor)
#         end

          def ext_refer_quote(label, source, content, visitor)
            return nil unless /^quote:(.*)$/ =~ label.element_label
            quoted_name = $1
            quoted_label = ::RD::Reference::RDLabel.new(quoted_name,
                                                        label.filename)
            source = quoted_label.to_s if label.to_s == source
            default_ext_refer(quoted_label, source, content)
          end
        end
      end
    end
  end
end
