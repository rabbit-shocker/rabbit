require 'rabbit/element'

module Rabbit
  module Entity
    module Isodia

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =acute accent 
      def ext_inline_verb_acute(label, content, visitor)
        label = label.to_s
        return nil unless /^acute:(.*)$/ =~ label
        NormalText.new("&\#x000B4;")
      end

      # =breve 
      def ext_inline_verb_breve(label, content, visitor)
        label = label.to_s
        return nil unless /^breve:(.*)$/ =~ label
        NormalText.new("&\#x002D8;")
      end

      # =caron 
      def ext_inline_verb_caron(label, content, visitor)
        label = label.to_s
        return nil unless /^caron:(.*)$/ =~ label
        NormalText.new("&\#x002C7;")
      end

      # =cedilla 
      def ext_inline_verb_cedil(label, content, visitor)
        label = label.to_s
        return nil unless /^cedil:(.*)$/ =~ label
        NormalText.new("&\#x000B8;")
      end

      # circumflex accent 
      def ext_inline_verb_circ(label, content, visitor)
        label = label.to_s
        return nil unless /^circ:(.*)$/ =~ label
        NormalText.new("&\#x002C6;")
      end

      # =double acute accent 
      def ext_inline_verb_dblac(label, content, visitor)
        label = label.to_s
        return nil unless /^dblac:(.*)$/ =~ label
        NormalText.new("&\#x002DD;")
      end

      # =dieresis 
      def ext_inline_verb_die(label, content, visitor)
        label = label.to_s
        return nil unless /^die:(.*)$/ =~ label
        NormalText.new("&\#x000A8;")
      end

      # =dot above 
      def ext_inline_verb_dot(label, content, visitor)
        label = label.to_s
        return nil unless /^dot:(.*)$/ =~ label
        NormalText.new("&\#x002D9;")
      end

      # =grave accent 
      def ext_inline_verb_grave(label, content, visitor)
        label = label.to_s
        return nil unless /^grave:(.*)$/ =~ label
        NormalText.new("&\#x00060;")
      end

      # =macron 
      def ext_inline_verb_macr(label, content, visitor)
        label = label.to_s
        return nil unless /^macr:(.*)$/ =~ label
        NormalText.new("&\#x000AF;")
      end

      # =ogonek 
      def ext_inline_verb_ogon(label, content, visitor)
        label = label.to_s
        return nil unless /^ogon:(.*)$/ =~ label
        NormalText.new("&\#x002DB;")
      end

      # =ring 
      def ext_inline_verb_ring(label, content, visitor)
        label = label.to_s
        return nil unless /^ring:(.*)$/ =~ label
        NormalText.new("&\#x002DA;")
      end

      # =tilde 
      def ext_inline_verb_tilde(label, content, visitor)
        label = label.to_s
        return nil unless /^tilde:(.*)$/ =~ label
        NormalText.new("&\#x002DC;")
      end

      # =umlaut mark 
      def ext_inline_verb_uml(label, content, visitor)
        label = label.to_s
        return nil unless /^uml:(.*)$/ =~ label
        NormalText.new("&\#x000A8;")
      end

    end
  end
end
