require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamsc

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /llcorner O: lower left corner 
      def ext_inline_verb_dlcorn(label, content, visitor)
        label = label.to_s
        return nil unless /^dlcorn:(.*)$/ =~ label
        NormalText.new("&\#x0231E;")
      end

      # /lrcorner C: lower right corner 
      def ext_inline_verb_drcorn(label, content, visitor)
        label = label.to_s
        return nil unless /^drcorn:(.*)$/ =~ label
        NormalText.new("&\#x0231F;")
      end

      # dbl left parenthesis, greater 
      def ext_inline_verb_gtlPar(label, content, visitor)
        label = label.to_s
        return nil unless /^gtlPar:(.*)$/ =~ label
        NormalText.new("&\#x02995;")
      end

      # left angle, dot 
      def ext_inline_verb_langd(label, content, visitor)
        label = label.to_s
        return nil unless /^langd:(.*)$/ =~ label
        NormalText.new("&\#x02991;")
      end

      # left bracket, equal 
      def ext_inline_verb_lbrke(label, content, visitor)
        label = label.to_s
        return nil unless /^lbrke:(.*)$/ =~ label
        NormalText.new("&\#x0298B;")
      end

      # left bracket, solidus bottom corner 
      def ext_inline_verb_lbrksld(label, content, visitor)
        label = label.to_s
        return nil unless /^lbrksld:(.*)$/ =~ label
        NormalText.new("&\#x0298F;")
      end

      # left bracket, solidus top corner 
      def ext_inline_verb_lbrkslu(label, content, visitor)
        label = label.to_s
        return nil unless /^lbrkslu:(.*)$/ =~ label
        NormalText.new("&\#x0298D;")
      end

      # /lceil O: left ceiling 
      def ext_inline_verb_lceil(label, content, visitor)
        label = label.to_s
        return nil unless /^lceil:(.*)$/ =~ label
        NormalText.new("&\#x02308;")
      end

      # /lfloor O: left floor 
      def ext_inline_verb_lfloor(label, content, visitor)
        label = label.to_s
        return nil unless /^lfloor:(.*)$/ =~ label
        NormalText.new("&\#x0230A;")
      end

      # /lmoustache 
      def ext_inline_verb_lmoust(label, content, visitor)
        label = label.to_s
        return nil unless /^lmoust:(.*)$/ =~ label
        NormalText.new("&\#x023B0;")
      end

      # O: left parenthesis, lt 
      def ext_inline_verb_lparlt(label, content, visitor)
        label = label.to_s
        return nil unless /^lparlt:(.*)$/ =~ label
        NormalText.new("&\#x02993;")
      end

      # dbl right parenthesis, less 
      def ext_inline_verb_ltrPar(label, content, visitor)
        label = label.to_s
        return nil unless /^ltrPar:(.*)$/ =~ label
        NormalText.new("&\#x02996;")
      end

      # right angle, dot 
      def ext_inline_verb_rangd(label, content, visitor)
        label = label.to_s
        return nil unless /^rangd:(.*)$/ =~ label
        NormalText.new("&\#x02992;")
      end

      # right bracket, equal 
      def ext_inline_verb_rbrke(label, content, visitor)
        label = label.to_s
        return nil unless /^rbrke:(.*)$/ =~ label
        NormalText.new("&\#x0298C;")
      end

      # right bracket, solidus bottom corner 
      def ext_inline_verb_rbrksld(label, content, visitor)
        label = label.to_s
        return nil unless /^rbrksld:(.*)$/ =~ label
        NormalText.new("&\#x0298E;")
      end

      # right bracket, solidus top corner 
      def ext_inline_verb_rbrkslu(label, content, visitor)
        label = label.to_s
        return nil unless /^rbrkslu:(.*)$/ =~ label
        NormalText.new("&\#x02990;")
      end

      # /rceil C: right ceiling 
      def ext_inline_verb_rceil(label, content, visitor)
        label = label.to_s
        return nil unless /^rceil:(.*)$/ =~ label
        NormalText.new("&\#x02309;")
      end

      # /rfloor C: right floor 
      def ext_inline_verb_rfloor(label, content, visitor)
        label = label.to_s
        return nil unless /^rfloor:(.*)$/ =~ label
        NormalText.new("&\#x0230B;")
      end

      # /rmoustache 
      def ext_inline_verb_rmoust(label, content, visitor)
        label = label.to_s
        return nil unless /^rmoust:(.*)$/ =~ label
        NormalText.new("&\#x023B1;")
      end

      # C: right paren, gt 
      def ext_inline_verb_rpargt(label, content, visitor)
        label = label.to_s
        return nil unless /^rpargt:(.*)$/ =~ label
        NormalText.new("&\#x02994;")
      end

      # /ulcorner O: upper left corner 
      def ext_inline_verb_ulcorn(label, content, visitor)
        label = label.to_s
        return nil unless /^ulcorn:(.*)$/ =~ label
        NormalText.new("&\#x0231C;")
      end

      # /urcorner C: upper right corner 
      def ext_inline_verb_urcorn(label, content, visitor)
        label = label.to_s
        return nil unless /^urcorn:(.*)$/ =~ label
        NormalText.new("&\#x0231D;")
      end

    end
  end
end
