require 'rabbit/element'

module Rabbit
  module Entity
    module Mmlextra

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # character showing function application in presentation tagging 
      def ext_inline_verb_af(label, content, visitor)
        label = label.to_s
        return nil unless /^af:(.*)$/ =~ label
        NormalText.new("&\#x02061;")
      end

      #  
      def ext_inline_verb_aopf(label, content, visitor)
        label = label.to_s
        return nil unless /^aopf:(.*)$/ =~ label
        NormalText.new("&\#x1D552;")
      end

      # Old ISOAMSR asymp (for HTML compatibility) 
      def ext_inline_verb_asympeq(label, content, visitor)
        label = label.to_s
        return nil unless /^asympeq:(.*)$/ =~ label
        NormalText.new("&\#x0224D;")
      end

      #  
      def ext_inline_verb_bopf(label, content, visitor)
        label = label.to_s
        return nil unless /^bopf:(.*)$/ =~ label
        NormalText.new("&\#x1D553;")
      end

      #  
      def ext_inline_verb_copf(label, content, visitor)
        label = label.to_s
        return nil unless /^copf:(.*)$/ =~ label
        NormalText.new("&\#x1D554;")
      end

      # cross or vector product 
      def ext_inline_verb_Cross(label, content, visitor)
        label = label.to_s
        return nil unless /^Cross:(.*)$/ =~ label
        NormalText.new("&\#x02A2F;")
      end

      # D for use in differentials, e.g., within integrals 
      def ext_inline_verb_DD(label, content, visitor)
        label = label.to_s
        return nil unless /^DD:(.*)$/ =~ label
        NormalText.new("&\#x02145;")
      end

      # d for use in differentials, e.g., within integrals 
      def ext_inline_verb_dd(label, content, visitor)
        label = label.to_s
        return nil unless /^dd:(.*)$/ =~ label
        NormalText.new("&\#x02146;")
      end

      #  
      def ext_inline_verb_dopf(label, content, visitor)
        label = label.to_s
        return nil unless /^dopf:(.*)$/ =~ label
        NormalText.new("&\#x1D555;")
      end

      # down arrow to bar 
      def ext_inline_verb_DownArrowBar(label, content, visitor)
        label = label.to_s
        return nil unless /^DownArrowBar:(.*)$/ =~ label
        NormalText.new("&\#x02913;")
      end

      # left-down-right-down harpoon 
      def ext_inline_verb_DownLeftRightVector(label, content, visitor)
        label = label.to_s
        return nil unless /^DownLeftRightVector:(.*)$/ =~ label
        NormalText.new("&\#x02950;")
      end

      # left-down harpoon from bar 
      def ext_inline_verb_DownLeftTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^DownLeftTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295E;")
      end

      # left-down harpoon to bar 
      def ext_inline_verb_DownLeftVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^DownLeftVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02956;")
      end

      # right-down harpoon from bar 
      def ext_inline_verb_DownRightTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^DownRightTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295F;")
      end

      # right-down harpoon to bar 
      def ext_inline_verb_DownRightVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^DownRightVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02957;")
      end

      # e use for the exponential base of the natural logarithms 
      def ext_inline_verb_ee(label, content, visitor)
        label = label.to_s
        return nil unless /^ee:(.*)$/ =~ label
        NormalText.new("&\#x02147;")
      end

      # empty small square 
      def ext_inline_verb_EmptySmallSquare(label, content, visitor)
        label = label.to_s
        return nil unless /^EmptySmallSquare:(.*)$/ =~ label
        NormalText.new("&\#x025FB;")
      end

      # empty small square 
      def ext_inline_verb_EmptyVerySmallSquare(label, content, visitor)
        label = label.to_s
        return nil unless /^EmptyVerySmallSquare:(.*)$/ =~ label
        NormalText.new("&\#x025AB;")
      end

      #  
      def ext_inline_verb_eopf(label, content, visitor)
        label = label.to_s
        return nil unless /^eopf:(.*)$/ =~ label
        NormalText.new("&\#x1D556;")
      end

      # two consecutive equal signs 
      def ext_inline_verb_Equal(label, content, visitor)
        label = label.to_s
        return nil unless /^Equal:(.*)$/ =~ label
        NormalText.new("&\#x02A75;")
      end

      # filled small square 
      def ext_inline_verb_FilledSmallSquare(label, content, visitor)
        label = label.to_s
        return nil unless /^FilledSmallSquare:(.*)$/ =~ label
        NormalText.new("&\#x025FC;")
      end

      # filled very small square 
      def ext_inline_verb_FilledVerySmallSquare(label, content, visitor)
        label = label.to_s
        return nil unless /^FilledVerySmallSquare:(.*)$/ =~ label
        NormalText.new("&\#x025AA;")
      end

      #  
      def ext_inline_verb_fopf(label, content, visitor)
        label = label.to_s
        return nil unless /^fopf:(.*)$/ =~ label
        NormalText.new("&\#x1D557;")
      end

      #  
      def ext_inline_verb_gopf(label, content, visitor)
        label = label.to_s
        return nil unless /^gopf:(.*)$/ =~ label
        NormalText.new("&\#x1D558;")
      end

      # alias for GT 
      def ext_inline_verb_GreaterGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterGreater:(.*)$/ =~ label
        NormalText.new("&\#x02AA2;")
      end

      # circumflex accent 
      def ext_inline_verb_Hat(label, content, visitor)
        label = label.to_s
        return nil unless /^Hat:(.*)$/ =~ label
        NormalText.new("&\#x0005E;")
      end

      #  
      def ext_inline_verb_hopf(label, content, visitor)
        label = label.to_s
        return nil unless /^hopf:(.*)$/ =~ label
        NormalText.new("&\#x1D559;")
      end

      # short horizontal line  
      def ext_inline_verb_HorizontalLine(label, content, visitor)
        label = label.to_s
        return nil unless /^HorizontalLine:(.*)$/ =~ label
        NormalText.new("&\#x02500;")
      end

      # short form of  &InvisibleComma; 
      def ext_inline_verb_ic(label, content, visitor)
        label = label.to_s
        return nil unless /^ic:(.*)$/ =~ label
        NormalText.new("&\#x02063;")
      end

      # i for use as a square root of -1 
      def ext_inline_verb_ii(label, content, visitor)
        label = label.to_s
        return nil unless /^ii:(.*)$/ =~ label
        NormalText.new("&\#x02148;")
      end

      #  
      def ext_inline_verb_iopf(label, content, visitor)
        label = label.to_s
        return nil unless /^iopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55A;")
      end

      # marks multiplication when it is understood without a mark 
      def ext_inline_verb_it(label, content, visitor)
        label = label.to_s
        return nil unless /^it:(.*)$/ =~ label
        NormalText.new("&\#x02062;")
      end

      #  
      def ext_inline_verb_jopf(label, content, visitor)
        label = label.to_s
        return nil unless /^jopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55B;")
      end

      #  
      def ext_inline_verb_kopf(label, content, visitor)
        label = label.to_s
        return nil unless /^kopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55C;")
      end

      # leftwards arrow to bar 
      def ext_inline_verb_larrb(label, content, visitor)
        label = label.to_s
        return nil unless /^larrb:(.*)$/ =~ label
        NormalText.new("&\#x021E4;")
      end

      # down-left harpoon from bar 
      def ext_inline_verb_LeftDownTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftDownTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x02961;")
      end

      # down-left harpoon to bar 
      def ext_inline_verb_LeftDownVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftDownVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02959;")
      end

      # left-up-right-up harpoon 
      def ext_inline_verb_LeftRightVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftRightVector:(.*)$/ =~ label
        NormalText.new("&\#x0294E;")
      end

      # left-up harpoon from bar 
      def ext_inline_verb_LeftTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295A;")
      end

      # left triangle, vertical bar 
      def ext_inline_verb_LeftTriangleBar(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTriangleBar:(.*)$/ =~ label
        NormalText.new("&\#x029CF;")
      end

      # up-left-down-left harpoon 
      def ext_inline_verb_LeftUpDownVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftUpDownVector:(.*)$/ =~ label
        NormalText.new("&\#x02951;")
      end

      # up-left harpoon from bar 
      def ext_inline_verb_LeftUpTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftUpTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x02960;")
      end

      # up-left harpoon to bar 
      def ext_inline_verb_LeftUpVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftUpVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02958;")
      end

      # left-up harpoon to bar 
      def ext_inline_verb_LeftVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02952;")
      end

      # alias for Lt 
      def ext_inline_verb_LessLess(label, content, visitor)
        label = label.to_s
        return nil unless /^LessLess:(.*)$/ =~ label
        NormalText.new("&\#x02AA1;")
      end

      #  
      def ext_inline_verb_lopf(label, content, visitor)
        label = label.to_s
        return nil unless /^lopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55D;")
      end

      # downwards arrow from bar 
      def ext_inline_verb_mapstodown(label, content, visitor)
        label = label.to_s
        return nil unless /^mapstodown:(.*)$/ =~ label
        NormalText.new("&\#x021A7;")
      end

      # leftwards arrow from bar 
      def ext_inline_verb_mapstoleft(label, content, visitor)
        label = label.to_s
        return nil unless /^mapstoleft:(.*)$/ =~ label
        NormalText.new("&\#x021A4;")
      end

      # upwards arrow from bar 
      def ext_inline_verb_mapstoup(label, content, visitor)
        label = label.to_s
        return nil unless /^mapstoup:(.*)$/ =~ label
        NormalText.new("&\#x021A5;")
      end

      # space of width 4/18 em 
      def ext_inline_verb_MediumSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^MediumSpace:(.*)$/ =~ label
        NormalText.new("&\#x0205F;")
      end

      #  
      def ext_inline_verb_mopf(label, content, visitor)
        label = label.to_s
        return nil unless /^mopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55E;")
      end

      # not bumpy equals 
      def ext_inline_verb_nbump(label, content, visitor)
        label = label.to_s
        return nil unless /^nbump:(.*)$/ =~ label
        NormalText.new("&\#x0224E;&\#x00338;")
      end

      # not bumpy single equals 
      def ext_inline_verb_nbumpe(label, content, visitor)
        label = label.to_s
        return nil unless /^nbumpe:(.*)$/ =~ label
        NormalText.new("&\#x0224F;&\#x00338;")
      end

      # not equal or similar 
      def ext_inline_verb_nesim(label, content, visitor)
        label = label.to_s
        return nil unless /^nesim:(.*)$/ =~ label
        NormalText.new("&\#x02242;&\#x00338;")
      end

      # force a line break; line feed 
      def ext_inline_verb_NewLine(label, content, visitor)
        label = label.to_s
        return nil unless /^NewLine:(.*)$/ =~ label
        NormalText.new("&\#x0000A;")
      end

      # never break line here 
      def ext_inline_verb_NoBreak(label, content, visitor)
        label = label.to_s
        return nil unless /^NoBreak:(.*)$/ =~ label
        NormalText.new("&\#x02060;")
      end

      #  
      def ext_inline_verb_nopf(label, content, visitor)
        label = label.to_s
        return nil unless /^nopf:(.*)$/ =~ label
        NormalText.new("&\#x1D55F;")
      end

      # alias for &nasymp; 
      def ext_inline_verb_NotCupCap(label, content, visitor)
        label = label.to_s
        return nil unless /^NotCupCap:(.*)$/ =~ label
        NormalText.new("&\#x0226D;")
      end

      # alias for &nbumpe; 
      def ext_inline_verb_NotHumpEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotHumpEqual:(.*)$/ =~ label
        NormalText.new("&\#x0224F;&\#x00338;")
      end

      # not left triangle, vertical bar 
      def ext_inline_verb_NotLeftTriangleBar(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLeftTriangleBar:(.*)$/ =~ label
        NormalText.new("&\#x029CF;&\#x00338;")
      end

      # not double greater-than sign 
      def ext_inline_verb_NotNestedGreaterGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^NotNestedGreaterGreater:(.*)$/ =~ label
        NormalText.new("&\#x02AA2;&\#x00338;")
      end

      # not double less-than sign 
      def ext_inline_verb_NotNestedLessLess(label, content, visitor)
        label = label.to_s
        return nil unless /^NotNestedLessLess:(.*)$/ =~ label
        NormalText.new("&\#x02AA1;&\#x00338;")
      end

      # not vertical bar, right triangle 
      def ext_inline_verb_NotRightTriangleBar(label, content, visitor)
        label = label.to_s
        return nil unless /^NotRightTriangleBar:(.*)$/ =~ label
        NormalText.new("&\#x029D0;&\#x00338;")
      end

      # square not subset 
      def ext_inline_verb_NotSquareSubset(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSquareSubset:(.*)$/ =~ label
        NormalText.new("&\#x0228F;&\#x00338;")
      end

      # negated set-like partial order operator 
      def ext_inline_verb_NotSquareSuperset(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSquareSuperset:(.*)$/ =~ label
        NormalText.new("&\#x02290;&\#x00338;")
      end

      # not succeeds or similar 
      def ext_inline_verb_NotSucceedsTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSucceedsTilde:(.*)$/ =~ label
        NormalText.new("&\#x0227F;&\#x00338;")
      end

      #  
      def ext_inline_verb_oopf(label, content, visitor)
        label = label.to_s
        return nil unless /^oopf:(.*)$/ =~ label
        NormalText.new("&\#x1D560;")
      end

      # over bar 
      def ext_inline_verb_OverBar(label, content, visitor)
        label = label.to_s
        return nil unless /^OverBar:(.*)$/ =~ label
        NormalText.new("&\#x000AF;")
      end

      # over brace  
      def ext_inline_verb_OverBrace(label, content, visitor)
        label = label.to_s
        return nil unless /^OverBrace:(.*)$/ =~ label
        NormalText.new("&\#x0FE37;")
      end

      # over bracket 
      def ext_inline_verb_OverBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^OverBracket:(.*)$/ =~ label
        NormalText.new("&\#x023B4;")
      end

      # over parenthesis 
      def ext_inline_verb_OverParenthesis(label, content, visitor)
        label = label.to_s
        return nil unless /^OverParenthesis:(.*)$/ =~ label
        NormalText.new("&\#x0FE35;")
      end

      # the ring (skew field) of quaternions 
      def ext_inline_verb_planckh(label, content, visitor)
        label = label.to_s
        return nil unless /^planckh:(.*)$/ =~ label
        NormalText.new("&\#x0210E;")
      end

      #  
      def ext_inline_verb_popf(label, content, visitor)
        label = label.to_s
        return nil unless /^popf:(.*)$/ =~ label
        NormalText.new("&\#x1D561;")
      end

      # alias for &prod; 
      def ext_inline_verb_Product(label, content, visitor)
        label = label.to_s
        return nil unless /^Product:(.*)$/ =~ label
        NormalText.new("&\#x0220F;")
      end

      #  
      def ext_inline_verb_qopf(label, content, visitor)
        label = label.to_s
        return nil unless /^qopf:(.*)$/ =~ label
        NormalText.new("&\#x1D562;")
      end

      # leftwards arrow to bar 
      def ext_inline_verb_rarrb(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrb:(.*)$/ =~ label
        NormalText.new("&\#x021E5;")
      end

      # down-right harpoon from bar 
      def ext_inline_verb_RightDownTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightDownTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295D;")
      end

      # down-right harpoon to bar 
      def ext_inline_verb_RightDownVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^RightDownVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02955;")
      end

      # right-up harpoon from bar 
      def ext_inline_verb_RightTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295B;")
      end

      # vertical bar, right triangle 
      def ext_inline_verb_RightTriangleBar(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTriangleBar:(.*)$/ =~ label
        NormalText.new("&\#x029D0;")
      end

      # up-right-down-right harpoon 
      def ext_inline_verb_RightUpDownVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightUpDownVector:(.*)$/ =~ label
        NormalText.new("&\#x0294F;")
      end

      # up-right harpoon from bar 
      def ext_inline_verb_RightUpTeeVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightUpTeeVector:(.*)$/ =~ label
        NormalText.new("&\#x0295C;")
      end

      # up-right harpoon to bar 
      def ext_inline_verb_RightUpVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^RightUpVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02954;")
      end

      # up-right harpoon to bar 
      def ext_inline_verb_RightVectorBar(label, content, visitor)
        label = label.to_s
        return nil unless /^RightVectorBar:(.*)$/ =~ label
        NormalText.new("&\#x02953;")
      end

      #  
      def ext_inline_verb_ropf(label, content, visitor)
        label = label.to_s
        return nil unless /^ropf:(.*)$/ =~ label
        NormalText.new("&\#x1D563;")
      end

      # round implies 
      def ext_inline_verb_RoundImplies(label, content, visitor)
        label = label.to_s
        return nil unless /^RoundImplies:(.*)$/ =~ label
        NormalText.new("&\#x02970;")
      end

      # rule-delayed (colon right arrow) 
      def ext_inline_verb_RuleDelayed(label, content, visitor)
        label = label.to_s
        return nil unless /^RuleDelayed:(.*)$/ =~ label
        NormalText.new("&\#x029F4;")
      end

      #  
      def ext_inline_verb_sopf(label, content, visitor)
        label = label.to_s
        return nil unless /^sopf:(.*)$/ =~ label
        NormalText.new("&\#x1D564;")
      end

      # tabulator stop; horizontal tabulation 
      def ext_inline_verb_Tab(label, content, visitor)
        label = label.to_s
        return nil unless /^Tab:(.*)$/ =~ label
        NormalText.new("&\#x00009;")
      end

      # space of width 5/18 em 
      def ext_inline_verb_ThickSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^ThickSpace:(.*)$/ =~ label
        NormalText.new("&\#x02009;&\#x0200A;&\#x0200A;")
      end

      #  
      def ext_inline_verb_topf(label, content, visitor)
        label = label.to_s
        return nil unless /^topf:(.*)$/ =~ label
        NormalText.new("&\#x1D565;")
      end

      # under brace  
      def ext_inline_verb_UnderBrace(label, content, visitor)
        label = label.to_s
        return nil unless /^UnderBrace:(.*)$/ =~ label
        NormalText.new("&\#x0FE38;")
      end

      # under bracket 
      def ext_inline_verb_UnderBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^UnderBracket:(.*)$/ =~ label
        NormalText.new("&\#x023B5;")
      end

      # under parenthesis 
      def ext_inline_verb_UnderParenthesis(label, content, visitor)
        label = label.to_s
        return nil unless /^UnderParenthesis:(.*)$/ =~ label
        NormalText.new("&\#x0FE36;")
      end

      #  
      def ext_inline_verb_uopf(label, content, visitor)
        label = label.to_s
        return nil unless /^uopf:(.*)$/ =~ label
        NormalText.new("&\#x1D566;")
      end

      # up arrow to bar 
      def ext_inline_verb_UpArrowBar(label, content, visitor)
        label = label.to_s
        return nil unless /^UpArrowBar:(.*)$/ =~ label
        NormalText.new("&\#x02912;")
      end

      # ISOGRK1 Ugr, HTML4 Upsilon 
      def ext_inline_verb_Upsilon(label, content, visitor)
        label = label.to_s
        return nil unless /^Upsilon:(.*)$/ =~ label
        NormalText.new("&\#x003A5;")
      end

      # alias ISONUM verbar 
      def ext_inline_verb_VerticalLine(label, content, visitor)
        label = label.to_s
        return nil unless /^VerticalLine:(.*)$/ =~ label
        NormalText.new("&\#x0007C;")
      end

      # vertical separating operator 
      def ext_inline_verb_VerticalSeparator(label, content, visitor)
        label = label.to_s
        return nil unless /^VerticalSeparator:(.*)$/ =~ label
        NormalText.new("&\#x02758;")
      end

      #  
      def ext_inline_verb_vopf(label, content, visitor)
        label = label.to_s
        return nil unless /^vopf:(.*)$/ =~ label
        NormalText.new("&\#x1D567;")
      end

      #  
      def ext_inline_verb_wopf(label, content, visitor)
        label = label.to_s
        return nil unless /^wopf:(.*)$/ =~ label
        NormalText.new("&\#x1D568;")
      end

      #  
      def ext_inline_verb_xopf(label, content, visitor)
        label = label.to_s
        return nil unless /^xopf:(.*)$/ =~ label
        NormalText.new("&\#x1D569;")
      end

      #  
      def ext_inline_verb_yopf(label, content, visitor)
        label = label.to_s
        return nil unless /^yopf:(.*)$/ =~ label
        NormalText.new("&\#x1D56A;")
      end

      # zero width space 
      def ext_inline_verb_ZeroWidthSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^ZeroWidthSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200B;")
      end

      #  
      def ext_inline_verb_zopf(label, content, visitor)
        label = label.to_s
        return nil unless /^zopf:(.*)$/ =~ label
        NormalText.new("&\#x1D56B;")
      end

    end
  end
end
