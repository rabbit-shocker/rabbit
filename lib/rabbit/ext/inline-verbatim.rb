require 'rabbit/utils'
require 'rabbit/ext/base'
require 'rabbit/ext/image'

module Rabbit
  module Ext
    class InlineVerbatim < Base
      extend Utils
      include Image

      $LOAD_PATH.each do |path|
        Dir.glob(File.join(path, *%w(rabbit entity *.rb))) do |x|
          lib_name = x.gsub(/\A#{path}#{File::SEPARATOR}/, '')
          require lib_name
          mod_name = to_class_name(File.basename(lib_name, ".rb"))
          include Entity.const_get(mod_name)
        end
      end

#       def ext_inline_verb_img(label, content, visitor)
#         img(label, content, visitor)
#       end
      
      def ext_inline_verb_quote(label, content, visitor)
        label = label.to_s
        return nil unless /^quote:(.*)$/ =~ label
        visitor.__send__(:default_ext_inline_verb, $1, $1)
      end
      
      def ext_inline_verb_del(label, content, visitor)
        label = label.to_s
        return nil unless /^del:(.*)$/ =~ label
        DeletedText.new(visitor.apply_to_String($1))
      end
      
      def ext_inline_verb_sub(label, content, visitor)
        label = label.to_s
        return nil unless /^sub:(.*)$/ =~ label
        sub_text = $1
        unless /\A\s*\z/ =~ sub_text
          sub_text = visitor.apply_to_Verb(RD::Verb.new(sub_text)).text
        end
        Subscript.new(sub_text)
      end
      
      def ext_inline_verb_sup(label, content, visitor)
        label = label.to_s
        return nil unless /^sup:(.*)$/ =~ label
        sub_text = $1
        unless /\A\s*\z/ =~ sub_text
          sub_text = visitor.apply_to_Verb(RD::Verb.new(sub_text)).text
        end
        Superscript.new(sub_text)
      end
      
      def ext_inline_verb_br(label, content, visitor)
        label = label.to_s
        return nil unless /^br:(.*)$/ =~ label
        NormalText.new("&#xa;")
      end
    end
  end
end
