require 'rabbit/element'

module Rabbit
  module Entity
    module Isomopf

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /Bbb A, open face A 
      def ext_inline_verb_Aopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Aopf:(.*)$/ =~ label
        NormalText.new("&\#x1D538;")
      end

      # /Bbb B, open face B 
      def ext_inline_verb_Bopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Bopf:(.*)$/ =~ label
        NormalText.new("&\#x1D539;")
      end

      # /Bbb C, open face C 
      def ext_inline_verb_Copf(label, content, visitor)
        label = label.to_s
        return nil unless /^Copf:(.*)$/ =~ label
        NormalText.new("&\#x02102;")
      end

      # /Bbb D, open face D 
      def ext_inline_verb_Dopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Dopf:(.*)$/ =~ label
        NormalText.new("&\#x1D53B;")
      end

      # /Bbb E, open face E 
      def ext_inline_verb_Eopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Eopf:(.*)$/ =~ label
        NormalText.new("&\#x1D53C;")
      end

      # /Bbb F, open face F 
      def ext_inline_verb_Fopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Fopf:(.*)$/ =~ label
        NormalText.new("&\#x1D53D;")
      end

      # /Bbb G, open face G 
      def ext_inline_verb_Gopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Gopf:(.*)$/ =~ label
        NormalText.new("&\#x1D53E;")
      end

      # /Bbb H, open face H 
      def ext_inline_verb_Hopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Hopf:(.*)$/ =~ label
        NormalText.new("&\#x0210D;")
      end

      # /Bbb I, open face I 
      def ext_inline_verb_Iopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Iopf:(.*)$/ =~ label
        NormalText.new("&\#x1D540;")
      end

      # /Bbb J, open face J 
      def ext_inline_verb_Jopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Jopf:(.*)$/ =~ label
        NormalText.new("&\#x1D541;")
      end

      # /Bbb K, open face K  
      def ext_inline_verb_Kopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Kopf:(.*)$/ =~ label
        NormalText.new("&\#x1D542;")
      end

      # /Bbb L, open face L  
      def ext_inline_verb_Lopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Lopf:(.*)$/ =~ label
        NormalText.new("&\#x1D543;")
      end

      # /Bbb M, open face M  
      def ext_inline_verb_Mopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Mopf:(.*)$/ =~ label
        NormalText.new("&\#x1D544;")
      end

      # /Bbb N, open face N 
      def ext_inline_verb_Nopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Nopf:(.*)$/ =~ label
        NormalText.new("&\#x02115;")
      end

      # /Bbb O, open face O 
      def ext_inline_verb_Oopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Oopf:(.*)$/ =~ label
        NormalText.new("&\#x1D546;")
      end

      # /Bbb P, open face P 
      def ext_inline_verb_Popf(label, content, visitor)
        label = label.to_s
        return nil unless /^Popf:(.*)$/ =~ label
        NormalText.new("&\#x02119;")
      end

      # /Bbb Q, open face Q 
      def ext_inline_verb_Qopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Qopf:(.*)$/ =~ label
        NormalText.new("&\#x0211A;")
      end

      # /Bbb R, open face R 
      def ext_inline_verb_Ropf(label, content, visitor)
        label = label.to_s
        return nil unless /^Ropf:(.*)$/ =~ label
        NormalText.new("&\#x0211D;")
      end

      # /Bbb S, open face S 
      def ext_inline_verb_Sopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Sopf:(.*)$/ =~ label
        NormalText.new("&\#x1D54A;")
      end

      # /Bbb T, open face T 
      def ext_inline_verb_Topf(label, content, visitor)
        label = label.to_s
        return nil unless /^Topf:(.*)$/ =~ label
        NormalText.new("&\#x1D54B;")
      end

      # /Bbb U, open face U 
      def ext_inline_verb_Uopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Uopf:(.*)$/ =~ label
        NormalText.new("&\#x1D54C;")
      end

      # /Bbb V, open face V 
      def ext_inline_verb_Vopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Vopf:(.*)$/ =~ label
        NormalText.new("&\#x1D54D;")
      end

      # /Bbb W, open face W 
      def ext_inline_verb_Wopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Wopf:(.*)$/ =~ label
        NormalText.new("&\#x1D54E;")
      end

      # /Bbb X, open face X 
      def ext_inline_verb_Xopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Xopf:(.*)$/ =~ label
        NormalText.new("&\#x1D54F;")
      end

      # /Bbb Y, open face Y 
      def ext_inline_verb_Yopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Yopf:(.*)$/ =~ label
        NormalText.new("&\#x1D550;")
      end

      # /Bbb Z, open face Z 
      def ext_inline_verb_Zopf(label, content, visitor)
        label = label.to_s
        return nil unless /^Zopf:(.*)$/ =~ label
        NormalText.new("&\#x02124;")
      end

    end
  end
end
