require 'rabbit/element'

module Rabbit
  module Entity
    module Isobox

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # lower left quadrant 
      def ext_inline_verb_boxDL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxDL:(.*)$/ =~ label
        NormalText.new("&\#x02557;")
      end

      # lower left quadrant 
      def ext_inline_verb_boxDl(label, content, visitor)
        label = label.to_s
        return nil unless /^boxDl:(.*)$/ =~ label
        NormalText.new("&\#x02556;")
      end

      # lower left quadrant 
      def ext_inline_verb_boxdL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxdL:(.*)$/ =~ label
        NormalText.new("&\#x02555;")
      end

      # lower left quadrant 
      def ext_inline_verb_boxdl(label, content, visitor)
        label = label.to_s
        return nil unless /^boxdl:(.*)$/ =~ label
        NormalText.new("&\#x02510;")
      end

      # lower right quadrant 
      def ext_inline_verb_boxDR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxDR:(.*)$/ =~ label
        NormalText.new("&\#x02554;")
      end

      # lower right quadrant 
      def ext_inline_verb_boxDr(label, content, visitor)
        label = label.to_s
        return nil unless /^boxDr:(.*)$/ =~ label
        NormalText.new("&\#x02553;")
      end

      # lower right quadrant 
      def ext_inline_verb_boxdR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxdR:(.*)$/ =~ label
        NormalText.new("&\#x02552;")
      end

      # lower right quadrant 
      def ext_inline_verb_boxdr(label, content, visitor)
        label = label.to_s
        return nil unless /^boxdr:(.*)$/ =~ label
        NormalText.new("&\#x0250C;")
      end

      # horizontal line 
      def ext_inline_verb_boxH(label, content, visitor)
        label = label.to_s
        return nil unless /^boxH:(.*)$/ =~ label
        NormalText.new("&\#x02550;")
      end

      # horizontal line  
      def ext_inline_verb_boxh(label, content, visitor)
        label = label.to_s
        return nil unless /^boxh:(.*)$/ =~ label
        NormalText.new("&\#x02500;")
      end

      # lower left and right quadrants 
      def ext_inline_verb_boxHD(label, content, visitor)
        label = label.to_s
        return nil unless /^boxHD:(.*)$/ =~ label
        NormalText.new("&\#x02566;")
      end

      # lower left and right quadrants 
      def ext_inline_verb_boxHd(label, content, visitor)
        label = label.to_s
        return nil unless /^boxHd:(.*)$/ =~ label
        NormalText.new("&\#x02564;")
      end

      # lower left and right quadrants 
      def ext_inline_verb_boxhD(label, content, visitor)
        label = label.to_s
        return nil unless /^boxhD:(.*)$/ =~ label
        NormalText.new("&\#x02565;")
      end

      # lower left and right quadrants 
      def ext_inline_verb_boxhd(label, content, visitor)
        label = label.to_s
        return nil unless /^boxhd:(.*)$/ =~ label
        NormalText.new("&\#x0252C;")
      end

      # upper left and right quadrants 
      def ext_inline_verb_boxHU(label, content, visitor)
        label = label.to_s
        return nil unless /^boxHU:(.*)$/ =~ label
        NormalText.new("&\#x02569;")
      end

      # upper left and right quadrants 
      def ext_inline_verb_boxHu(label, content, visitor)
        label = label.to_s
        return nil unless /^boxHu:(.*)$/ =~ label
        NormalText.new("&\#x02567;")
      end

      # upper left and right quadrants 
      def ext_inline_verb_boxhU(label, content, visitor)
        label = label.to_s
        return nil unless /^boxhU:(.*)$/ =~ label
        NormalText.new("&\#x02568;")
      end

      # upper left and right quadrants 
      def ext_inline_verb_boxhu(label, content, visitor)
        label = label.to_s
        return nil unless /^boxhu:(.*)$/ =~ label
        NormalText.new("&\#x02534;")
      end

      # upper left quadrant 
      def ext_inline_verb_boxUL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxUL:(.*)$/ =~ label
        NormalText.new("&\#x0255D;")
      end

      # upper left quadrant 
      def ext_inline_verb_boxUl(label, content, visitor)
        label = label.to_s
        return nil unless /^boxUl:(.*)$/ =~ label
        NormalText.new("&\#x0255C;")
      end

      # upper left quadrant 
      def ext_inline_verb_boxuL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxuL:(.*)$/ =~ label
        NormalText.new("&\#x0255B;")
      end

      # upper left quadrant 
      def ext_inline_verb_boxul(label, content, visitor)
        label = label.to_s
        return nil unless /^boxul:(.*)$/ =~ label
        NormalText.new("&\#x02518;")
      end

      # upper right quadrant 
      def ext_inline_verb_boxUR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxUR:(.*)$/ =~ label
        NormalText.new("&\#x0255A;")
      end

      # upper right quadrant 
      def ext_inline_verb_boxUr(label, content, visitor)
        label = label.to_s
        return nil unless /^boxUr:(.*)$/ =~ label
        NormalText.new("&\#x02559;")
      end

      # upper right quadrant 
      def ext_inline_verb_boxuR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxuR:(.*)$/ =~ label
        NormalText.new("&\#x02558;")
      end

      # upper right quadrant 
      def ext_inline_verb_boxur(label, content, visitor)
        label = label.to_s
        return nil unless /^boxur:(.*)$/ =~ label
        NormalText.new("&\#x02514;")
      end

      # vertical line 
      def ext_inline_verb_boxV(label, content, visitor)
        label = label.to_s
        return nil unless /^boxV:(.*)$/ =~ label
        NormalText.new("&\#x02551;")
      end

      # vertical line 
      def ext_inline_verb_boxv(label, content, visitor)
        label = label.to_s
        return nil unless /^boxv:(.*)$/ =~ label
        NormalText.new("&\#x02502;")
      end

      # all four quadrants 
      def ext_inline_verb_boxVH(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVH:(.*)$/ =~ label
        NormalText.new("&\#x0256C;")
      end

      # all four quadrants 
      def ext_inline_verb_boxVh(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVh:(.*)$/ =~ label
        NormalText.new("&\#x0256B;")
      end

      # all four quadrants 
      def ext_inline_verb_boxvH(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvH:(.*)$/ =~ label
        NormalText.new("&\#x0256A;")
      end

      # all four quadrants 
      def ext_inline_verb_boxvh(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvh:(.*)$/ =~ label
        NormalText.new("&\#x0253C;")
      end

      # upper and lower left quadrants 
      def ext_inline_verb_boxVL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVL:(.*)$/ =~ label
        NormalText.new("&\#x02563;")
      end

      # upper and lower left quadrants 
      def ext_inline_verb_boxVl(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVl:(.*)$/ =~ label
        NormalText.new("&\#x02562;")
      end

      # upper and lower left quadrants 
      def ext_inline_verb_boxvL(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvL:(.*)$/ =~ label
        NormalText.new("&\#x02561;")
      end

      # upper and lower left quadrants 
      def ext_inline_verb_boxvl(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvl:(.*)$/ =~ label
        NormalText.new("&\#x02524;")
      end

      # upper and lower right quadrants 
      def ext_inline_verb_boxVR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVR:(.*)$/ =~ label
        NormalText.new("&\#x02560;")
      end

      # upper and lower right quadrants 
      def ext_inline_verb_boxVr(label, content, visitor)
        label = label.to_s
        return nil unless /^boxVr:(.*)$/ =~ label
        NormalText.new("&\#x0255F;")
      end

      # upper and lower right quadrants 
      def ext_inline_verb_boxvR(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvR:(.*)$/ =~ label
        NormalText.new("&\#x0255E;")
      end

      # upper and lower right quadrants 
      def ext_inline_verb_boxvr(label, content, visitor)
        label = label.to_s
        return nil unless /^boxvr:(.*)$/ =~ label
        NormalText.new("&\#x0251C;")
      end

    end
  end
end
