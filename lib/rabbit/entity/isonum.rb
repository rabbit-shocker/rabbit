require 'rabbit/element'

module Rabbit
  module Entity
    module Isonum

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =ampersand 
      def ext_inline_verb_amp(label, content, visitor)
        label = label.to_s
        return nil unless /^amp:(.*)$/ =~ label
        NormalText.new("&\#38;\#38;")
      end

      # =apostrophe 
      def ext_inline_verb_apos(label, content, visitor)
        label = label.to_s
        return nil unless /^apos:(.*)$/ =~ label
        NormalText.new("&\#x00027;")
      end

      # /ast B: =asterisk 
      def ext_inline_verb_ast(label, content, visitor)
        label = label.to_s
        return nil unless /^ast:(.*)$/ =~ label
        NormalText.new("&\#x0002A;")
      end

      # =broken (vertical) bar 
      def ext_inline_verb_brvbar(label, content, visitor)
        label = label.to_s
        return nil unless /^brvbar:(.*)$/ =~ label
        NormalText.new("&\#x000A6;")
      end

      # /backslash =reverse solidus 
      def ext_inline_verb_bsol(label, content, visitor)
        label = label.to_s
        return nil unless /^bsol:(.*)$/ =~ label
        NormalText.new("&\#x0005C;")
      end

      # =cent sign 
      def ext_inline_verb_cent(label, content, visitor)
        label = label.to_s
        return nil unless /^cent:(.*)$/ =~ label
        NormalText.new("&\#x000A2;")
      end

      # /colon P: 
      def ext_inline_verb_colon(label, content, visitor)
        label = label.to_s
        return nil unless /^colon:(.*)$/ =~ label
        NormalText.new("&\#x0003A;")
      end

      # P: =comma 
      def ext_inline_verb_comma(label, content, visitor)
        label = label.to_s
        return nil unless /^comma:(.*)$/ =~ label
        NormalText.new("&\#x0002C;")
      end

      # =commercial at 
      def ext_inline_verb_commat(label, content, visitor)
        label = label.to_s
        return nil unless /^commat:(.*)$/ =~ label
        NormalText.new("&\#x00040;")
      end

      # =copyright sign 
      def ext_inline_verb_copy(label, content, visitor)
        label = label.to_s
        return nil unless /^copy:(.*)$/ =~ label
        NormalText.new("&\#x000A9;")
      end

      # =general currency sign 
      def ext_inline_verb_curren(label, content, visitor)
        label = label.to_s
        return nil unless /^curren:(.*)$/ =~ label
        NormalText.new("&\#x000A4;")
      end

      # /downarrow A: =downward arrow 
      def ext_inline_verb_darr(label, content, visitor)
        label = label.to_s
        return nil unless /^darr:(.*)$/ =~ label
        NormalText.new("&\#x02193;")
      end

      # =degree sign 
      def ext_inline_verb_deg(label, content, visitor)
        label = label.to_s
        return nil unless /^deg:(.*)$/ =~ label
        NormalText.new("&\#x000B0;")
      end

      # /div B: =divide sign 
      def ext_inline_verb_divide(label, content, visitor)
        label = label.to_s
        return nil unless /^divide:(.*)$/ =~ label
        NormalText.new("&\#x000F7;")
      end

      # =dollar sign 
      def ext_inline_verb_dollar(label, content, visitor)
        label = label.to_s
        return nil unless /^dollar:(.*)$/ =~ label
        NormalText.new("&\#x00024;")
      end

      # =equals sign R: 
      def ext_inline_verb_equals(label, content, visitor)
        label = label.to_s
        return nil unless /^equals:(.*)$/ =~ label
        NormalText.new("&\#x0003D;")
      end

      # =exclamation mark 
      def ext_inline_verb_excl(label, content, visitor)
        label = label.to_s
        return nil unless /^excl:(.*)$/ =~ label
        NormalText.new("&\#x00021;")
      end

      # =fraction one-half 
      def ext_inline_verb_frac12(label, content, visitor)
        label = label.to_s
        return nil unless /^frac12:(.*)$/ =~ label
        NormalText.new("&\#x000BD;")
      end

      # =fraction one-quarter 
      def ext_inline_verb_frac14(label, content, visitor)
        label = label.to_s
        return nil unless /^frac14:(.*)$/ =~ label
        NormalText.new("&\#x000BC;")
      end

      # =fraction one-eighth 
      def ext_inline_verb_frac18(label, content, visitor)
        label = label.to_s
        return nil unless /^frac18:(.*)$/ =~ label
        NormalText.new("&\#x0215B;")
      end

      # =fraction three-quarters 
      def ext_inline_verb_frac34(label, content, visitor)
        label = label.to_s
        return nil unless /^frac34:(.*)$/ =~ label
        NormalText.new("&\#x000BE;")
      end

      # =fraction three-eighths 
      def ext_inline_verb_frac38(label, content, visitor)
        label = label.to_s
        return nil unless /^frac38:(.*)$/ =~ label
        NormalText.new("&\#x0215C;")
      end

      # =fraction five-eighths 
      def ext_inline_verb_frac58(label, content, visitor)
        label = label.to_s
        return nil unless /^frac58:(.*)$/ =~ label
        NormalText.new("&\#x0215D;")
      end

      # =fraction seven-eighths 
      def ext_inline_verb_frac78(label, content, visitor)
        label = label.to_s
        return nil unless /^frac78:(.*)$/ =~ label
        NormalText.new("&\#x0215E;")
      end

      # =greater-than sign R: 
      def ext_inline_verb_gt(label, content, visitor)
        label = label.to_s
        return nil unless /^gt:(.*)$/ =~ label
        NormalText.new("&\#x0003E;")
      end

      # =fraction one-half 
      def ext_inline_verb_half(label, content, visitor)
        label = label.to_s
        return nil unless /^half:(.*)$/ =~ label
        NormalText.new("&\#x000BD;")
      end

      # =horizontal bar 
      def ext_inline_verb_horbar(label, content, visitor)
        label = label.to_s
        return nil unless /^horbar:(.*)$/ =~ label
        NormalText.new("&\#x02015;")
      end

      # =hyphen 
      def ext_inline_verb_hyphen(label, content, visitor)
        label = label.to_s
        return nil unless /^hyphen:(.*)$/ =~ label
        NormalText.new("&\#x02010;")
      end

      # =inverted exclamation mark 
      def ext_inline_verb_iexcl(label, content, visitor)
        label = label.to_s
        return nil unless /^iexcl:(.*)$/ =~ label
        NormalText.new("&\#x000A1;")
      end

      # =inverted question mark 
      def ext_inline_verb_iquest(label, content, visitor)
        label = label.to_s
        return nil unless /^iquest:(.*)$/ =~ label
        NormalText.new("&\#x000BF;")
      end

      # =angle quotation mark, left 
      def ext_inline_verb_laquo(label, content, visitor)
        label = label.to_s
        return nil unless /^laquo:(.*)$/ =~ label
        NormalText.new("&\#x000AB;")
      end

      # /leftarrow /gets A: =leftward arrow 
      def ext_inline_verb_larr(label, content, visitor)
        label = label.to_s
        return nil unless /^larr:(.*)$/ =~ label
        NormalText.new("&\#x02190;")
      end

      # /lbrace O: =left curly bracket 
      def ext_inline_verb_lcub(label, content, visitor)
        label = label.to_s
        return nil unless /^lcub:(.*)$/ =~ label
        NormalText.new("&\#x0007B;")
      end

      # =double quotation mark, left 
      def ext_inline_verb_ldquo(label, content, visitor)
        label = label.to_s
        return nil unless /^ldquo:(.*)$/ =~ label
        NormalText.new("&\#x0201C;")
      end

      # =low line 
      def ext_inline_verb_lowbar(label, content, visitor)
        label = label.to_s
        return nil unless /^lowbar:(.*)$/ =~ label
        NormalText.new("&\#x0005F;")
      end

      # O: =left parenthesis 
      def ext_inline_verb_lpar(label, content, visitor)
        label = label.to_s
        return nil unless /^lpar:(.*)$/ =~ label
        NormalText.new("&\#x00028;")
      end

      # /lbrack O: =left square bracket 
      def ext_inline_verb_lsqb(label, content, visitor)
        label = label.to_s
        return nil unless /^lsqb:(.*)$/ =~ label
        NormalText.new("&\#x0005B;")
      end

      # =single quotation mark, left 
      def ext_inline_verb_lsquo(label, content, visitor)
        label = label.to_s
        return nil unless /^lsquo:(.*)$/ =~ label
        NormalText.new("&\#x02018;")
      end

      # =less-than sign R: 
      def ext_inline_verb_lt(label, content, visitor)
        label = label.to_s
        return nil unless /^lt:(.*)$/ =~ label
        NormalText.new("&\#38;\#60;")
      end

      # =micro sign 
      def ext_inline_verb_micro(label, content, visitor)
        label = label.to_s
        return nil unless /^micro:(.*)$/ =~ label
        NormalText.new("&\#x000B5;")
      end

      # /centerdot B: =middle dot 
      def ext_inline_verb_middot(label, content, visitor)
        label = label.to_s
        return nil unless /^middot:(.*)$/ =~ label
        NormalText.new("&\#x000B7;")
      end

      # =no break (required) space 
      def ext_inline_verb_nbsp(label, content, visitor)
        label = label.to_s
        return nil unless /^nbsp:(.*)$/ =~ label
        NormalText.new("&\#x000A0;")
      end

      # /neg /lnot =not sign 
      def ext_inline_verb_not(label, content, visitor)
        label = label.to_s
        return nil unless /^not:(.*)$/ =~ label
        NormalText.new("&\#x000AC;")
      end

      # =number sign 
      def ext_inline_verb_num(label, content, visitor)
        label = label.to_s
        return nil unless /^num:(.*)$/ =~ label
        NormalText.new("&\#x00023;")
      end

      # =ohm sign 
      def ext_inline_verb_ohm(label, content, visitor)
        label = label.to_s
        return nil unless /^ohm:(.*)$/ =~ label
        NormalText.new("&\#x02126;")
      end

      # =ordinal indicator, feminine 
      def ext_inline_verb_ordf(label, content, visitor)
        label = label.to_s
        return nil unless /^ordf:(.*)$/ =~ label
        NormalText.new("&\#x000AA;")
      end

      # =ordinal indicator, masculine 
      def ext_inline_verb_ordm(label, content, visitor)
        label = label.to_s
        return nil unless /^ordm:(.*)$/ =~ label
        NormalText.new("&\#x000BA;")
      end

      # =pilcrow (paragraph sign) 
      def ext_inline_verb_para(label, content, visitor)
        label = label.to_s
        return nil unless /^para:(.*)$/ =~ label
        NormalText.new("&\#x000B6;")
      end

      # =percent sign 
      def ext_inline_verb_percnt(label, content, visitor)
        label = label.to_s
        return nil unless /^percnt:(.*)$/ =~ label
        NormalText.new("&\#x00025;")
      end

      # =full stop, period 
      def ext_inline_verb_period(label, content, visitor)
        label = label.to_s
        return nil unless /^period:(.*)$/ =~ label
        NormalText.new("&\#x0002E;")
      end

      # =plus sign B: 
      def ext_inline_verb_plus(label, content, visitor)
        label = label.to_s
        return nil unless /^plus:(.*)$/ =~ label
        NormalText.new("&\#x0002B;")
      end

      # /pm B: =plus-or-minus sign 
      def ext_inline_verb_plusmn(label, content, visitor)
        label = label.to_s
        return nil unless /^plusmn:(.*)$/ =~ label
        NormalText.new("&\#x000B1;")
      end

      # =pound sign 
      def ext_inline_verb_pound(label, content, visitor)
        label = label.to_s
        return nil unless /^pound:(.*)$/ =~ label
        NormalText.new("&\#x000A3;")
      end

      # =question mark 
      def ext_inline_verb_quest(label, content, visitor)
        label = label.to_s
        return nil unless /^quest:(.*)$/ =~ label
        NormalText.new("&\#x0003F;")
      end

      # =quotation mark 
      def ext_inline_verb_quot(label, content, visitor)
        label = label.to_s
        return nil unless /^quot:(.*)$/ =~ label
        NormalText.new("&\#x00022;")
      end

      # =angle quotation mark, right 
      def ext_inline_verb_raquo(label, content, visitor)
        label = label.to_s
        return nil unless /^raquo:(.*)$/ =~ label
        NormalText.new("&\#x000BB;")
      end

      # /rightarrow /to A: =rightward arrow 
      def ext_inline_verb_rarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rarr:(.*)$/ =~ label
        NormalText.new("&\#x02192;")
      end

      # /rbrace C: =right curly bracket 
      def ext_inline_verb_rcub(label, content, visitor)
        label = label.to_s
        return nil unless /^rcub:(.*)$/ =~ label
        NormalText.new("&\#x0007D;")
      end

      # =double quotation mark, right 
      def ext_inline_verb_rdquo(label, content, visitor)
        label = label.to_s
        return nil unless /^rdquo:(.*)$/ =~ label
        NormalText.new("&\#x0201D;")
      end

      # /circledR =registered sign 
      def ext_inline_verb_reg(label, content, visitor)
        label = label.to_s
        return nil unless /^reg:(.*)$/ =~ label
        NormalText.new("&\#x000AE;")
      end

      # C: =right parenthesis 
      def ext_inline_verb_rpar(label, content, visitor)
        label = label.to_s
        return nil unless /^rpar:(.*)$/ =~ label
        NormalText.new("&\#x00029;")
      end

      # /rbrack C: =right square bracket 
      def ext_inline_verb_rsqb(label, content, visitor)
        label = label.to_s
        return nil unless /^rsqb:(.*)$/ =~ label
        NormalText.new("&\#x0005D;")
      end

      # =single quotation mark, right 
      def ext_inline_verb_rsquo(label, content, visitor)
        label = label.to_s
        return nil unless /^rsquo:(.*)$/ =~ label
        NormalText.new("&\#x02019;")
      end

      # =section sign 
      def ext_inline_verb_sect(label, content, visitor)
        label = label.to_s
        return nil unless /^sect:(.*)$/ =~ label
        NormalText.new("&\#x000A7;")
      end

      # =semicolon P: 
      def ext_inline_verb_semi(label, content, visitor)
        label = label.to_s
        return nil unless /^semi:(.*)$/ =~ label
        NormalText.new("&\#x0003B;")
      end

      # =soft hyphen 
      def ext_inline_verb_shy(label, content, visitor)
        label = label.to_s
        return nil unless /^shy:(.*)$/ =~ label
        NormalText.new("&\#x000AD;")
      end

      # =solidus 
      def ext_inline_verb_sol(label, content, visitor)
        label = label.to_s
        return nil unless /^sol:(.*)$/ =~ label
        NormalText.new("&\#x0002F;")
      end

      # =music note (sung text sign) 
      def ext_inline_verb_sung(label, content, visitor)
        label = label.to_s
        return nil unless /^sung:(.*)$/ =~ label
        NormalText.new("&\#x0266A;")
      end

      # =superscript one 
      def ext_inline_verb_sup1(label, content, visitor)
        label = label.to_s
        return nil unless /^sup1:(.*)$/ =~ label
        NormalText.new("&\#x000B9;")
      end

      # =superscript two 
      def ext_inline_verb_sup2(label, content, visitor)
        label = label.to_s
        return nil unless /^sup2:(.*)$/ =~ label
        NormalText.new("&\#x000B2;")
      end

      # =superscript three 
      def ext_inline_verb_sup3(label, content, visitor)
        label = label.to_s
        return nil unless /^sup3:(.*)$/ =~ label
        NormalText.new("&\#x000B3;")
      end

      # /times B: =multiply sign 
      def ext_inline_verb_times(label, content, visitor)
        label = label.to_s
        return nil unless /^times:(.*)$/ =~ label
        NormalText.new("&\#x000D7;")
      end

      # =trade mark sign 
      def ext_inline_verb_trade(label, content, visitor)
        label = label.to_s
        return nil unless /^trade:(.*)$/ =~ label
        NormalText.new("&\#x02122;")
      end

      # /uparrow A: =upward arrow 
      def ext_inline_verb_uarr(label, content, visitor)
        label = label.to_s
        return nil unless /^uarr:(.*)$/ =~ label
        NormalText.new("&\#x02191;")
      end

      # /vert =vertical bar 
      def ext_inline_verb_verbar(label, content, visitor)
        label = label.to_s
        return nil unless /^verbar:(.*)$/ =~ label
        NormalText.new("&\#x0007C;")
      end

      # /yen =yen sign 
      def ext_inline_verb_yen(label, content, visitor)
        label = label.to_s
        return nil unless /^yen:(.*)$/ =~ label
        NormalText.new("&\#x000A5;")
      end

    end
  end
end
