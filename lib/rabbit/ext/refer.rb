require 'rabbit/ext/base'
require 'rabbit/ext/image'

module Rabbit
  module Ext
    class Refer < Base
      include Image

      # For backward compatibility.
#       def ext_refer_img(label, content, visitor)
#         img(label, content, visitor)
#       end

      def ext_refer_quote(label, content, visitor)
        return nil unless /^quote:(.*)$/ =~ label.element_label
        quoted_name = $1
        quoted_label = RD::Reference::RDLabel.new(quoted_name, label.filename)
        content = quoted_label.to_s if label.to_s == content
        visitor.__send__(:default_ext_refer, quoted_label, content)
      end

    end
  end
end
