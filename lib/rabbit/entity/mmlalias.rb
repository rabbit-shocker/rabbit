require 'rabbit/element'

module Rabbit
  module Entity
    module Mmlalias

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # alias ISOAMSO ang 
      def ext_inline_verb_angle(label, content, visitor)
        label = label.to_s
        return nil unless /^angle:(.*)$/ =~ label
        NormalText.new("&\#x02220;")
      end

      # character showing function application in presentation tagging 
      def ext_inline_verb_ApplyFunction(label, content, visitor)
        label = label.to_s
        return nil unless /^ApplyFunction:(.*)$/ =~ label
        NormalText.new("&\#x02061;")
      end

      # alias ISOTECH ap 
      def ext_inline_verb_approx(label, content, visitor)
        label = label.to_s
        return nil unless /^approx:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # alias ISOAMSR ape 
      def ext_inline_verb_approxeq(label, content, visitor)
        label = label.to_s
        return nil unless /^approxeq:(.*)$/ =~ label
        NormalText.new("&\#x0224A;")
      end

      # assignment operator, alias ISOAMSR colone 
      def ext_inline_verb_Assign(label, content, visitor)
        label = label.to_s
        return nil unless /^Assign:(.*)$/ =~ label
        NormalText.new("&\#x02254;")
      end

      # alias ISOAMSR bcong 
      def ext_inline_verb_backcong(label, content, visitor)
        label = label.to_s
        return nil unless /^backcong:(.*)$/ =~ label
        NormalText.new("&\#x0224C;")
      end

      # alias ISOAMSR bepsi 
      def ext_inline_verb_backepsilon(label, content, visitor)
        label = label.to_s
        return nil unless /^backepsilon:(.*)$/ =~ label
        NormalText.new("&\#x003F6;")
      end

      # alias ISOAMSO bprime 
      def ext_inline_verb_backprime(label, content, visitor)
        label = label.to_s
        return nil unless /^backprime:(.*)$/ =~ label
        NormalText.new("&\#x02035;")
      end

      # alias ISOAMSR bsim 
      def ext_inline_verb_backsim(label, content, visitor)
        label = label.to_s
        return nil unless /^backsim:(.*)$/ =~ label
        NormalText.new("&\#x0223D;")
      end

      # alias ISOAMSR bsime 
      def ext_inline_verb_backsimeq(label, content, visitor)
        label = label.to_s
        return nil unless /^backsimeq:(.*)$/ =~ label
        NormalText.new("&\#x022CD;")
      end

      # alias ISOAMSB setmn 
      def ext_inline_verb_Backslash(label, content, visitor)
        label = label.to_s
        return nil unless /^Backslash:(.*)$/ =~ label
        NormalText.new("&\#x02216;")
      end

      # alias ISOAMSB barwed 
      def ext_inline_verb_barwedge(label, content, visitor)
        label = label.to_s
        return nil unless /^barwedge:(.*)$/ =~ label
        NormalText.new("&\#x02305;")
      end

      # alias ISOTECH becaus 
      def ext_inline_verb_Because(label, content, visitor)
        label = label.to_s
        return nil unless /^Because:(.*)$/ =~ label
        NormalText.new("&\#x02235;")
      end

      # alias ISOTECH becaus 
      def ext_inline_verb_because(label, content, visitor)
        label = label.to_s
        return nil unless /^because:(.*)$/ =~ label
        NormalText.new("&\#x02235;")
      end

      # alias ISOTECH bernou 
      def ext_inline_verb_Bernoullis(label, content, visitor)
        label = label.to_s
        return nil unless /^Bernoullis:(.*)$/ =~ label
        NormalText.new("&\#x0212C;")
      end

      # alias ISOAMSR twixt 
      def ext_inline_verb_between(label, content, visitor)
        label = label.to_s
        return nil unless /^between:(.*)$/ =~ label
        NormalText.new("&\#x0226C;")
      end

      # alias ISOAMSB xcap 
      def ext_inline_verb_bigcap(label, content, visitor)
        label = label.to_s
        return nil unless /^bigcap:(.*)$/ =~ label
        NormalText.new("&\#x022C2;")
      end

      # alias ISOAMSB xcirc 
      def ext_inline_verb_bigcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^bigcirc:(.*)$/ =~ label
        NormalText.new("&\#x025EF;")
      end

      # alias ISOAMSB xcup 
      def ext_inline_verb_bigcup(label, content, visitor)
        label = label.to_s
        return nil unless /^bigcup:(.*)$/ =~ label
        NormalText.new("&\#x022C3;")
      end

      # alias ISOAMSB xodot 
      def ext_inline_verb_bigodot(label, content, visitor)
        label = label.to_s
        return nil unless /^bigodot:(.*)$/ =~ label
        NormalText.new("&\#x02A00;")
      end

      # alias ISOAMSB xoplus 
      def ext_inline_verb_bigoplus(label, content, visitor)
        label = label.to_s
        return nil unless /^bigoplus:(.*)$/ =~ label
        NormalText.new("&\#x02A01;")
      end

      # alias ISOAMSB xotime 
      def ext_inline_verb_bigotimes(label, content, visitor)
        label = label.to_s
        return nil unless /^bigotimes:(.*)$/ =~ label
        NormalText.new("&\#x02A02;")
      end

      # alias ISOAMSB xsqcup 
      def ext_inline_verb_bigsqcup(label, content, visitor)
        label = label.to_s
        return nil unless /^bigsqcup:(.*)$/ =~ label
        NormalText.new("&\#x02A06;")
      end

      # ISOPUB    starf  
      def ext_inline_verb_bigstar(label, content, visitor)
        label = label.to_s
        return nil unless /^bigstar:(.*)$/ =~ label
        NormalText.new("&\#x02605;")
      end

      # alias ISOAMSB xdtri 
      def ext_inline_verb_bigtriangledown(label, content, visitor)
        label = label.to_s
        return nil unless /^bigtriangledown:(.*)$/ =~ label
        NormalText.new("&\#x025BD;")
      end

      # alias ISOAMSB xutri 
      def ext_inline_verb_bigtriangleup(label, content, visitor)
        label = label.to_s
        return nil unless /^bigtriangleup:(.*)$/ =~ label
        NormalText.new("&\#x025B3;")
      end

      # alias ISOAMSB xuplus 
      def ext_inline_verb_biguplus(label, content, visitor)
        label = label.to_s
        return nil unless /^biguplus:(.*)$/ =~ label
        NormalText.new("&\#x02A04;")
      end

      # alias ISOAMSB xvee 
      def ext_inline_verb_bigvee(label, content, visitor)
        label = label.to_s
        return nil unless /^bigvee:(.*)$/ =~ label
        NormalText.new("&\#x022C1;")
      end

      # alias ISOAMSB xwedge 
      def ext_inline_verb_bigwedge(label, content, visitor)
        label = label.to_s
        return nil unless /^bigwedge:(.*)$/ =~ label
        NormalText.new("&\#x022C0;")
      end

      # alias ISOAMSA rbarr 
      def ext_inline_verb_bkarow(label, content, visitor)
        label = label.to_s
        return nil unless /^bkarow:(.*)$/ =~ label
        NormalText.new("&\#x0290D;")
      end

      # alias ISOPUB lozf 
      def ext_inline_verb_blacklozenge(label, content, visitor)
        label = label.to_s
        return nil unless /^blacklozenge:(.*)$/ =~ label
        NormalText.new("&\#x029EB;")
      end

      # ISOTECH  squarf  
      def ext_inline_verb_blacksquare(label, content, visitor)
        label = label.to_s
        return nil unless /^blacksquare:(.*)$/ =~ label
        NormalText.new("&\#x025AA;")
      end

      # alias ISOPUB utrif 
      def ext_inline_verb_blacktriangle(label, content, visitor)
        label = label.to_s
        return nil unless /^blacktriangle:(.*)$/ =~ label
        NormalText.new("&\#x025B4;")
      end

      # alias ISOPUB dtrif 
      def ext_inline_verb_blacktriangledown(label, content, visitor)
        label = label.to_s
        return nil unless /^blacktriangledown:(.*)$/ =~ label
        NormalText.new("&\#x025BE;")
      end

      # alias ISOPUB ltrif 
      def ext_inline_verb_blacktriangleleft(label, content, visitor)
        label = label.to_s
        return nil unless /^blacktriangleleft:(.*)$/ =~ label
        NormalText.new("&\#x025C2;")
      end

      # alias ISOPUB rtrif 
      def ext_inline_verb_blacktriangleright(label, content, visitor)
        label = label.to_s
        return nil unless /^blacktriangleright:(.*)$/ =~ label
        NormalText.new("&\#x025B8;")
      end

      # alias ISOTECH bottom 
      def ext_inline_verb_bot(label, content, visitor)
        label = label.to_s
        return nil unless /^bot:(.*)$/ =~ label
        NormalText.new("&\#x022A5;")
      end

      # alias ISOAMSB minusb 
      def ext_inline_verb_boxminus(label, content, visitor)
        label = label.to_s
        return nil unless /^boxminus:(.*)$/ =~ label
        NormalText.new("&\#x0229F;")
      end

      # alias ISOAMSB plusb 
      def ext_inline_verb_boxplus(label, content, visitor)
        label = label.to_s
        return nil unless /^boxplus:(.*)$/ =~ label
        NormalText.new("&\#x0229E;")
      end

      # alias ISOAMSB timesb 
      def ext_inline_verb_boxtimes(label, content, visitor)
        label = label.to_s
        return nil unless /^boxtimes:(.*)$/ =~ label
        NormalText.new("&\#x022A0;")
      end

      # alias ISODIA breve 
      def ext_inline_verb_Breve(label, content, visitor)
        label = label.to_s
        return nil unless /^Breve:(.*)$/ =~ label
        NormalText.new("&\#x002D8;")
      end

      # alias ISOPUB bull 
      def ext_inline_verb_bullet(label, content, visitor)
        label = label.to_s
        return nil unless /^bullet:(.*)$/ =~ label
        NormalText.new("&\#x02022;")
      end

      # alias ISOAMSR bump 
      def ext_inline_verb_Bumpeq(label, content, visitor)
        label = label.to_s
        return nil unless /^Bumpeq:(.*)$/ =~ label
        NormalText.new("&\#x0224E;")
      end

      # alias ISOAMSR bumpe 
      def ext_inline_verb_bumpeq(label, content, visitor)
        label = label.to_s
        return nil unless /^bumpeq:(.*)$/ =~ label
        NormalText.new("&\#x0224F;")
      end

      # D for use in differentials, e.g., within integrals 
      def ext_inline_verb_CapitalDifferentialD(label, content, visitor)
        label = label.to_s
        return nil unless /^CapitalDifferentialD:(.*)$/ =~ label
        NormalText.new("&\#x02145;")
      end

      # the non-associative ring of octonions or Cayley numbers 
      def ext_inline_verb_Cayleys(label, content, visitor)
        label = label.to_s
        return nil unless /^Cayleys:(.*)$/ =~ label
        NormalText.new("&\#x0212D;")
      end

      # alias ISODIA cedil 
      def ext_inline_verb_Cedilla(label, content, visitor)
        label = label.to_s
        return nil unless /^Cedilla:(.*)$/ =~ label
        NormalText.new("&\#x000B8;")
      end

      # alias ISONUM middot 
      def ext_inline_verb_CenterDot(label, content, visitor)
        label = label.to_s
        return nil unless /^CenterDot:(.*)$/ =~ label
        NormalText.new("&\#x000B7;")
      end

      # alias ISONUM middot 
      def ext_inline_verb_centerdot(label, content, visitor)
        label = label.to_s
        return nil unless /^centerdot:(.*)$/ =~ label
        NormalText.new("&\#x000B7;")
      end

      # alias ISOPUB check 
      def ext_inline_verb_checkmark(label, content, visitor)
        label = label.to_s
        return nil unless /^checkmark:(.*)$/ =~ label
        NormalText.new("&\#x02713;")
      end

      # alias ISOAMSR cire 
      def ext_inline_verb_circeq(label, content, visitor)
        label = label.to_s
        return nil unless /^circeq:(.*)$/ =~ label
        NormalText.new("&\#x02257;")
      end

      # alias ISOAMSA olarr 
      def ext_inline_verb_circlearrowleft(label, content, visitor)
        label = label.to_s
        return nil unless /^circlearrowleft:(.*)$/ =~ label
        NormalText.new("&\#x021BA;")
      end

      # alias ISOAMSA orarr 
      def ext_inline_verb_circlearrowright(label, content, visitor)
        label = label.to_s
        return nil unless /^circlearrowright:(.*)$/ =~ label
        NormalText.new("&\#x021BB;")
      end

      # alias ISOAMSB oast 
      def ext_inline_verb_circledast(label, content, visitor)
        label = label.to_s
        return nil unless /^circledast:(.*)$/ =~ label
        NormalText.new("&\#x0229B;")
      end

      # alias ISOAMSB ocir 
      def ext_inline_verb_circledcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^circledcirc:(.*)$/ =~ label
        NormalText.new("&\#x0229A;")
      end

      # alias ISOAMSB odash 
      def ext_inline_verb_circleddash(label, content, visitor)
        label = label.to_s
        return nil unless /^circleddash:(.*)$/ =~ label
        NormalText.new("&\#x0229D;")
      end

      # alias ISOAMSB odot 
      def ext_inline_verb_CircleDot(label, content, visitor)
        label = label.to_s
        return nil unless /^CircleDot:(.*)$/ =~ label
        NormalText.new("&\#x02299;")
      end

      # alias ISONUM reg 
      def ext_inline_verb_circledR(label, content, visitor)
        label = label.to_s
        return nil unless /^circledR:(.*)$/ =~ label
        NormalText.new("&\#x000AE;")
      end

      # alias ISOAMSO oS 
      def ext_inline_verb_circledS(label, content, visitor)
        label = label.to_s
        return nil unless /^circledS:(.*)$/ =~ label
        NormalText.new("&\#x024C8;")
      end

      # alias ISOAMSB ominus 
      def ext_inline_verb_CircleMinus(label, content, visitor)
        label = label.to_s
        return nil unless /^CircleMinus:(.*)$/ =~ label
        NormalText.new("&\#x02296;")
      end

      # alias ISOAMSB oplus 
      def ext_inline_verb_CirclePlus(label, content, visitor)
        label = label.to_s
        return nil unless /^CirclePlus:(.*)$/ =~ label
        NormalText.new("&\#x02295;")
      end

      # alias ISOAMSB otimes 
      def ext_inline_verb_CircleTimes(label, content, visitor)
        label = label.to_s
        return nil unless /^CircleTimes:(.*)$/ =~ label
        NormalText.new("&\#x02297;")
      end

      # alias ISOTECH cwconint 
      def ext_inline_verb_ClockwiseContourIntegral(label, content, visitor)
        label = label.to_s
        return nil unless /^ClockwiseContourIntegral:(.*)$/ =~ label
        NormalText.new("&\#x02232;")
      end

      # alias ISONUM rdquo 
      def ext_inline_verb_CloseCurlyDoubleQuote(label, content, visitor)
        label = label.to_s
        return nil unless /^CloseCurlyDoubleQuote:(.*)$/ =~ label
        NormalText.new("&\#x0201D;")
      end

      # alias ISONUM rsquo 
      def ext_inline_verb_CloseCurlyQuote(label, content, visitor)
        label = label.to_s
        return nil unless /^CloseCurlyQuote:(.*)$/ =~ label
        NormalText.new("&\#x02019;")
      end

      # ISOPUB    clubs  
      def ext_inline_verb_clubsuit(label, content, visitor)
        label = label.to_s
        return nil unless /^clubsuit:(.*)$/ =~ label
        NormalText.new("&\#x02663;")
      end

      # alias ISOAMSR colone 
      def ext_inline_verb_coloneq(label, content, visitor)
        label = label.to_s
        return nil unless /^coloneq:(.*)$/ =~ label
        NormalText.new("&\#x02254;")
      end

      # alias ISOAMSO comp 
      def ext_inline_verb_complement(label, content, visitor)
        label = label.to_s
        return nil unless /^complement:(.*)$/ =~ label
        NormalText.new("&\#x02201;")
      end

      # the field of complex numbers 
      def ext_inline_verb_complexes(label, content, visitor)
        label = label.to_s
        return nil unless /^complexes:(.*)$/ =~ label
        NormalText.new("&\#x02102;")
      end

      # alias ISOTECH equiv 
      def ext_inline_verb_Congruent(label, content, visitor)
        label = label.to_s
        return nil unless /^Congruent:(.*)$/ =~ label
        NormalText.new("&\#x02261;")
      end

      # alias ISOTECH conint 
      def ext_inline_verb_ContourIntegral(label, content, visitor)
        label = label.to_s
        return nil unless /^ContourIntegral:(.*)$/ =~ label
        NormalText.new("&\#x0222E;")
      end

      # alias ISOAMSB coprod 
      def ext_inline_verb_Coproduct(label, content, visitor)
        label = label.to_s
        return nil unless /^Coproduct:(.*)$/ =~ label
        NormalText.new("&\#x02210;")
      end

      # alias ISOTECH awconint 
      def ext_inline_verb_CounterClockwiseContourIntegral(label, content, visitor)
        label = label.to_s
        return nil unless /^CounterClockwiseContourIntegral:(.*)$/ =~ label
        NormalText.new("&\#x02233;")
      end

      # alias asympeq 
      def ext_inline_verb_CupCap(label, content, visitor)
        label = label.to_s
        return nil unless /^CupCap:(.*)$/ =~ label
        NormalText.new("&\#x0224D;")
      end

      # alias ISOAMSR cuepr 
      def ext_inline_verb_curlyeqprec(label, content, visitor)
        label = label.to_s
        return nil unless /^curlyeqprec:(.*)$/ =~ label
        NormalText.new("&\#x022DE;")
      end

      # alias ISOAMSR cuesc 
      def ext_inline_verb_curlyeqsucc(label, content, visitor)
        label = label.to_s
        return nil unless /^curlyeqsucc:(.*)$/ =~ label
        NormalText.new("&\#x022DF;")
      end

      # alias ISOAMSB cuvee 
      def ext_inline_verb_curlyvee(label, content, visitor)
        label = label.to_s
        return nil unless /^curlyvee:(.*)$/ =~ label
        NormalText.new("&\#x022CE;")
      end

      # alias ISOAMSB cuwed 
      def ext_inline_verb_curlywedge(label, content, visitor)
        label = label.to_s
        return nil unless /^curlywedge:(.*)$/ =~ label
        NormalText.new("&\#x022CF;")
      end

      # alias ISOAMSA cularr 
      def ext_inline_verb_curvearrowleft(label, content, visitor)
        label = label.to_s
        return nil unless /^curvearrowleft:(.*)$/ =~ label
        NormalText.new("&\#x021B6;")
      end

      # alias ISOAMSA curarr 
      def ext_inline_verb_curvearrowright(label, content, visitor)
        label = label.to_s
        return nil unless /^curvearrowright:(.*)$/ =~ label
        NormalText.new("&\#x021B7;")
      end

      # alias ISOAMSA rBarr 
      def ext_inline_verb_dbkarow(label, content, visitor)
        label = label.to_s
        return nil unless /^dbkarow:(.*)$/ =~ label
        NormalText.new("&\#x0290F;")
      end

      # alias ISOPUB Dagger 
      def ext_inline_verb_ddagger(label, content, visitor)
        label = label.to_s
        return nil unless /^ddagger:(.*)$/ =~ label
        NormalText.new("&\#x02021;")
      end

      # alias ISOAMSR eDDot 
      def ext_inline_verb_ddotseq(label, content, visitor)
        label = label.to_s
        return nil unless /^ddotseq:(.*)$/ =~ label
        NormalText.new("&\#x02A77;")
      end

      # alias ISOTECH nabla 
      def ext_inline_verb_Del(label, content, visitor)
        label = label.to_s
        return nil unless /^Del:(.*)$/ =~ label
        NormalText.new("&\#x02207;")
      end

      # alias ISODIA acute 
      def ext_inline_verb_DiacriticalAcute(label, content, visitor)
        label = label.to_s
        return nil unless /^DiacriticalAcute:(.*)$/ =~ label
        NormalText.new("&\#x000B4;")
      end

      # alias ISODIA dot 
      def ext_inline_verb_DiacriticalDot(label, content, visitor)
        label = label.to_s
        return nil unless /^DiacriticalDot:(.*)$/ =~ label
        NormalText.new("&\#x002D9;")
      end

      # alias ISODIA dblac 
      def ext_inline_verb_DiacriticalDoubleAcute(label, content, visitor)
        label = label.to_s
        return nil unless /^DiacriticalDoubleAcute:(.*)$/ =~ label
        NormalText.new("&\#x002DD;")
      end

      # alias ISODIA grave 
      def ext_inline_verb_DiacriticalGrave(label, content, visitor)
        label = label.to_s
        return nil unless /^DiacriticalGrave:(.*)$/ =~ label
        NormalText.new("&\#x00060;")
      end

      # alias ISODIA tilde 
      def ext_inline_verb_DiacriticalTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^DiacriticalTilde:(.*)$/ =~ label
        NormalText.new("&\#x002DC;")
      end

      # alias ISOAMSB diam 
      def ext_inline_verb_Diamond(label, content, visitor)
        label = label.to_s
        return nil unless /^Diamond:(.*)$/ =~ label
        NormalText.new("&\#x022C4;")
      end

      # alias ISOAMSB diam 
      def ext_inline_verb_diamond(label, content, visitor)
        label = label.to_s
        return nil unless /^diamond:(.*)$/ =~ label
        NormalText.new("&\#x022C4;")
      end

      # ISOPUB    diams  
      def ext_inline_verb_diamondsuit(label, content, visitor)
        label = label.to_s
        return nil unless /^diamondsuit:(.*)$/ =~ label
        NormalText.new("&\#x02666;")
      end

      # d for use in differentials, e.g., within integrals 
      def ext_inline_verb_DifferentialD(label, content, visitor)
        label = label.to_s
        return nil unless /^DifferentialD:(.*)$/ =~ label
        NormalText.new("&\#x02146;")
      end

      # alias ISOGRK3 gammad 
      def ext_inline_verb_digamma(label, content, visitor)
        label = label.to_s
        return nil unless /^digamma:(.*)$/ =~ label
        NormalText.new("&\#x003DD;")
      end

      # alias ISONUM divide 
      def ext_inline_verb_div(label, content, visitor)
        label = label.to_s
        return nil unless /^div:(.*)$/ =~ label
        NormalText.new("&\#x000F7;")
      end

      # alias ISOAMSB divonx 
      def ext_inline_verb_divideontimes(label, content, visitor)
        label = label.to_s
        return nil unless /^divideontimes:(.*)$/ =~ label
        NormalText.new("&\#x022C7;")
      end

      # alias ISOAMSR esdot 
      def ext_inline_verb_doteq(label, content, visitor)
        label = label.to_s
        return nil unless /^doteq:(.*)$/ =~ label
        NormalText.new("&\#x02250;")
      end

      # alias ISOAMSR eDot 
      def ext_inline_verb_doteqdot(label, content, visitor)
        label = label.to_s
        return nil unless /^doteqdot:(.*)$/ =~ label
        NormalText.new("&\#x02251;")
      end

      # alias ISOAMSR esdot 
      def ext_inline_verb_DotEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^DotEqual:(.*)$/ =~ label
        NormalText.new("&\#x02250;")
      end

      # alias ISOAMSB minusd 
      def ext_inline_verb_dotminus(label, content, visitor)
        label = label.to_s
        return nil unless /^dotminus:(.*)$/ =~ label
        NormalText.new("&\#x02238;")
      end

      # alias ISOAMSB plusdo 
      def ext_inline_verb_dotplus(label, content, visitor)
        label = label.to_s
        return nil unless /^dotplus:(.*)$/ =~ label
        NormalText.new("&\#x02214;")
      end

      # alias ISOAMSB sdotb 
      def ext_inline_verb_dotsquare(label, content, visitor)
        label = label.to_s
        return nil unless /^dotsquare:(.*)$/ =~ label
        NormalText.new("&\#x022A1;")
      end

      # alias ISOAMSB Barwed 
      def ext_inline_verb_doublebarwedge(label, content, visitor)
        label = label.to_s
        return nil unless /^doublebarwedge:(.*)$/ =~ label
        NormalText.new("&\#x02306;")
      end

      # alias ISOTECH Conint 
      def ext_inline_verb_DoubleContourIntegral(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleContourIntegral:(.*)$/ =~ label
        NormalText.new("&\#x0222F;")
      end

      # alias ISODIA die 
      def ext_inline_verb_DoubleDot(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleDot:(.*)$/ =~ label
        NormalText.new("&\#x000A8;")
      end

      # alias ISOAMSA dArr 
      def ext_inline_verb_DoubleDownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleDownArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D3;")
      end

      # alias ISOTECH lArr 
      def ext_inline_verb_DoubleLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D0;")
      end

      # alias ISOAMSA hArr 
      def ext_inline_verb_DoubleLeftRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLeftRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D4;")
      end

      # alias for  &Dashv;  
      def ext_inline_verb_DoubleLeftTee(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLeftTee:(.*)$/ =~ label
        NormalText.new("&\#x02AE4;")
      end

      # alias ISOAMSA xlArr 
      def ext_inline_verb_DoubleLongLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLongLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x027F8;")
      end

      # alias ISOAMSA xhArr 
      def ext_inline_verb_DoubleLongLeftRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLongLeftRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x027FA;")
      end

      # alias ISOAMSA xrArr 
      def ext_inline_verb_DoubleLongRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleLongRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x027F9;")
      end

      # alias ISOTECH rArr 
      def ext_inline_verb_DoubleRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D2;")
      end

      # alias ISOAMSR vDash 
      def ext_inline_verb_DoubleRightTee(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleRightTee:(.*)$/ =~ label
        NormalText.new("&\#x022A8;")
      end

      # alias ISOAMSA uArr 
      def ext_inline_verb_DoubleUpArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleUpArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D1;")
      end

      # alias ISOAMSA vArr 
      def ext_inline_verb_DoubleUpDownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleUpDownArrow:(.*)$/ =~ label
        NormalText.new("&\#x021D5;")
      end

      # alias ISOTECH par 
      def ext_inline_verb_DoubleVerticalBar(label, content, visitor)
        label = label.to_s
        return nil unless /^DoubleVerticalBar:(.*)$/ =~ label
        NormalText.new("&\#x02225;")
      end

      # alias ISONUM darr 
      def ext_inline_verb_DownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DownArrow:(.*)$/ =~ label
        NormalText.new("&\#x02193;")
      end

      # alias ISOAMSA dArr 
      def ext_inline_verb_Downarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Downarrow:(.*)$/ =~ label
        NormalText.new("&\#x021D3;")
      end

      # alias ISONUM darr 
      def ext_inline_verb_downarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^downarrow:(.*)$/ =~ label
        NormalText.new("&\#x02193;")
      end

      # alias ISOAMSA duarr 
      def ext_inline_verb_DownArrowUpArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DownArrowUpArrow:(.*)$/ =~ label
        NormalText.new("&\#x021F5;")
      end

      # alias ISOAMSA ddarr 
      def ext_inline_verb_downdownarrows(label, content, visitor)
        label = label.to_s
        return nil unless /^downdownarrows:(.*)$/ =~ label
        NormalText.new("&\#x021CA;")
      end

      # alias ISOAMSA dharl 
      def ext_inline_verb_downharpoonleft(label, content, visitor)
        label = label.to_s
        return nil unless /^downharpoonleft:(.*)$/ =~ label
        NormalText.new("&\#x021C3;")
      end

      # alias ISOAMSA dharr 
      def ext_inline_verb_downharpoonright(label, content, visitor)
        label = label.to_s
        return nil unless /^downharpoonright:(.*)$/ =~ label
        NormalText.new("&\#x021C2;")
      end

      # alias ISOAMSA lhard 
      def ext_inline_verb_DownLeftVector(label, content, visitor)
        label = label.to_s
        return nil unless /^DownLeftVector:(.*)$/ =~ label
        NormalText.new("&\#x021BD;")
      end

      # alias ISOAMSA rhard 
      def ext_inline_verb_DownRightVector(label, content, visitor)
        label = label.to_s
        return nil unless /^DownRightVector:(.*)$/ =~ label
        NormalText.new("&\#x021C1;")
      end

      # alias ISOTECH top 
      def ext_inline_verb_DownTee(label, content, visitor)
        label = label.to_s
        return nil unless /^DownTee:(.*)$/ =~ label
        NormalText.new("&\#x022A4;")
      end

      # alias for mapstodown 
      def ext_inline_verb_DownTeeArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^DownTeeArrow:(.*)$/ =~ label
        NormalText.new("&\#x021A7;")
      end

      # alias ISOAMSA RBarr 
      def ext_inline_verb_drbkarow(label, content, visitor)
        label = label.to_s
        return nil unless /^drbkarow:(.*)$/ =~ label
        NormalText.new("&\#x02910;")
      end

      # alias ISOTECH isinv 
      def ext_inline_verb_Element(label, content, visitor)
        label = label.to_s
        return nil unless /^Element:(.*)$/ =~ label
        NormalText.new("&\#x02208;")
      end

      # alias ISOAMSO empty 
      def ext_inline_verb_emptyset(label, content, visitor)
        label = label.to_s
        return nil unless /^emptyset:(.*)$/ =~ label
        NormalText.new("&\#x02205;")
      end

      # alias ISOAMSR ecir 
      def ext_inline_verb_eqcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^eqcirc:(.*)$/ =~ label
        NormalText.new("&\#x02256;")
      end

      # alias ISOAMSR ecolon 
      def ext_inline_verb_eqcolon(label, content, visitor)
        label = label.to_s
        return nil unless /^eqcolon:(.*)$/ =~ label
        NormalText.new("&\#x02255;")
      end

      # alias ISOAMSR esim 
      def ext_inline_verb_eqsim(label, content, visitor)
        label = label.to_s
        return nil unless /^eqsim:(.*)$/ =~ label
        NormalText.new("&\#x02242;")
      end

      # alias ISOAMSR egs 
      def ext_inline_verb_eqslantgtr(label, content, visitor)
        label = label.to_s
        return nil unless /^eqslantgtr:(.*)$/ =~ label
        NormalText.new("&\#x02A96;")
      end

      # alias ISOAMSR els 
      def ext_inline_verb_eqslantless(label, content, visitor)
        label = label.to_s
        return nil unless /^eqslantless:(.*)$/ =~ label
        NormalText.new("&\#x02A95;")
      end

      # alias ISOAMSR esim 
      def ext_inline_verb_EqualTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^EqualTilde:(.*)$/ =~ label
        NormalText.new("&\#x02242;")
      end

      # alias ISOAMSA rlhar 
      def ext_inline_verb_Equilibrium(label, content, visitor)
        label = label.to_s
        return nil unless /^Equilibrium:(.*)$/ =~ label
        NormalText.new("&\#x021CC;")
      end

      # alias ISOTECH exist 
      def ext_inline_verb_Exists(label, content, visitor)
        label = label.to_s
        return nil unless /^Exists:(.*)$/ =~ label
        NormalText.new("&\#x02203;")
      end

      # expectation (operator) 
      def ext_inline_verb_expectation(label, content, visitor)
        label = label.to_s
        return nil unless /^expectation:(.*)$/ =~ label
        NormalText.new("&\#x02130;")
      end

      # e use for the exponential base of the natural logarithms 
      def ext_inline_verb_ExponentialE(label, content, visitor)
        label = label.to_s
        return nil unless /^ExponentialE:(.*)$/ =~ label
        NormalText.new("&\#x02147;")
      end

      # base of the Napierian logarithms 
      def ext_inline_verb_exponentiale(label, content, visitor)
        label = label.to_s
        return nil unless /^exponentiale:(.*)$/ =~ label
        NormalText.new("&\#x02147;")
      end

      # alias ISOAMSR efDot 
      def ext_inline_verb_fallingdotseq(label, content, visitor)
        label = label.to_s
        return nil unless /^fallingdotseq:(.*)$/ =~ label
        NormalText.new("&\#x02252;")
      end

      # alias ISOTECH forall 
      def ext_inline_verb_ForAll(label, content, visitor)
        label = label.to_s
        return nil unless /^ForAll:(.*)$/ =~ label
        NormalText.new("&\#x02200;")
      end

      # Fourier transform 
      def ext_inline_verb_Fouriertrf(label, content, visitor)
        label = label.to_s
        return nil unless /^Fouriertrf:(.*)$/ =~ label
        NormalText.new("&\#x02131;")
      end

      # alias ISOTECH ge 
      def ext_inline_verb_geq(label, content, visitor)
        label = label.to_s
        return nil unless /^geq:(.*)$/ =~ label
        NormalText.new("&\#x02265;")
      end

      # alias ISOAMSR gE 
      def ext_inline_verb_geqq(label, content, visitor)
        label = label.to_s
        return nil unless /^geqq:(.*)$/ =~ label
        NormalText.new("&\#x02267;")
      end

      # alias ISOAMSR ges 
      def ext_inline_verb_geqslant(label, content, visitor)
        label = label.to_s
        return nil unless /^geqslant:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;")
      end

      # alias ISOAMSR Gt 
      def ext_inline_verb_gg(label, content, visitor)
        label = label.to_s
        return nil unless /^gg:(.*)$/ =~ label
        NormalText.new("&\#x0226B;")
      end

      # alias ISOAMSR Gg 
      def ext_inline_verb_ggg(label, content, visitor)
        label = label.to_s
        return nil unless /^ggg:(.*)$/ =~ label
        NormalText.new("&\#x022D9;")
      end

      # alias ISOAMSN gnap 
      def ext_inline_verb_gnapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^gnapprox:(.*)$/ =~ label
        NormalText.new("&\#x02A8A;")
      end

      # alias ISOAMSN gne 
      def ext_inline_verb_gneq(label, content, visitor)
        label = label.to_s
        return nil unless /^gneq:(.*)$/ =~ label
        NormalText.new("&\#x02A88;")
      end

      # alias ISOAMSN gnE 
      def ext_inline_verb_gneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^gneqq:(.*)$/ =~ label
        NormalText.new("&\#x02269;")
      end

      # alias ISOTECH ge 
      def ext_inline_verb_GreaterEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterEqual:(.*)$/ =~ label
        NormalText.new("&\#x02265;")
      end

      # alias ISOAMSR gel 
      def ext_inline_verb_GreaterEqualLess(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterEqualLess:(.*)$/ =~ label
        NormalText.new("&\#x022DB;")
      end

      # alias ISOAMSR gE 
      def ext_inline_verb_GreaterFullEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterFullEqual:(.*)$/ =~ label
        NormalText.new("&\#x02267;")
      end

      # alias ISOAMSR gl 
      def ext_inline_verb_GreaterLess(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterLess:(.*)$/ =~ label
        NormalText.new("&\#x02277;")
      end

      # alias ISOAMSR ges 
      def ext_inline_verb_GreaterSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;")
      end

      # alias ISOAMSR gsim 
      def ext_inline_verb_GreaterTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^GreaterTilde:(.*)$/ =~ label
        NormalText.new("&\#x02273;")
      end

      # alias ISOAMSR gap 
      def ext_inline_verb_gtrapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^gtrapprox:(.*)$/ =~ label
        NormalText.new("&\#x02A86;")
      end

      # alias ISOAMSR gtdot 
      def ext_inline_verb_gtrdot(label, content, visitor)
        label = label.to_s
        return nil unless /^gtrdot:(.*)$/ =~ label
        NormalText.new("&\#x022D7;")
      end

      # alias ISOAMSR gel 
      def ext_inline_verb_gtreqless(label, content, visitor)
        label = label.to_s
        return nil unless /^gtreqless:(.*)$/ =~ label
        NormalText.new("&\#x022DB;")
      end

      # alias ISOAMSR gEl 
      def ext_inline_verb_gtreqqless(label, content, visitor)
        label = label.to_s
        return nil unless /^gtreqqless:(.*)$/ =~ label
        NormalText.new("&\#x02A8C;")
      end

      # alias ISOAMSR gl 
      def ext_inline_verb_gtrless(label, content, visitor)
        label = label.to_s
        return nil unless /^gtrless:(.*)$/ =~ label
        NormalText.new("&\#x02277;")
      end

      # alias ISOAMSR gsim 
      def ext_inline_verb_gtrsim(label, content, visitor)
        label = label.to_s
        return nil unless /^gtrsim:(.*)$/ =~ label
        NormalText.new("&\#x02273;")
      end

      # alias ISOAMSN gvnE 
      def ext_inline_verb_gvertneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^gvertneqq:(.*)$/ =~ label
        NormalText.new("&\#x02269;&\#x0FE00;")
      end

      # alias ISODIA caron 
      def ext_inline_verb_Hacek(label, content, visitor)
        label = label.to_s
        return nil unless /^Hacek:(.*)$/ =~ label
        NormalText.new("&\#x002C7;")
      end

      # alias ISOAMSO plank 
      def ext_inline_verb_hbar(label, content, visitor)
        label = label.to_s
        return nil unless /^hbar:(.*)$/ =~ label
        NormalText.new("&\#x0210F;")
      end

      # ISOPUB hearts 
      def ext_inline_verb_heartsuit(label, content, visitor)
        label = label.to_s
        return nil unless /^heartsuit:(.*)$/ =~ label
        NormalText.new("&\#x02665;")
      end

      # Hilbert space 
      def ext_inline_verb_HilbertSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^HilbertSpace:(.*)$/ =~ label
        NormalText.new("&\#x0210B;")
      end

      # alias ISOAMSA searhk 
      def ext_inline_verb_hksearow(label, content, visitor)
        label = label.to_s
        return nil unless /^hksearow:(.*)$/ =~ label
        NormalText.new("&\#x02925;")
      end

      # alias ISOAMSA swarhk 
      def ext_inline_verb_hkswarow(label, content, visitor)
        label = label.to_s
        return nil unless /^hkswarow:(.*)$/ =~ label
        NormalText.new("&\#x02926;")
      end

      # alias ISOAMSA larrhk 
      def ext_inline_verb_hookleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^hookleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x021A9;")
      end

      # alias ISOAMSA rarrhk 
      def ext_inline_verb_hookrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^hookrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021AA;")
      end

      # alias ISOAMSO plankv 
      def ext_inline_verb_hslash(label, content, visitor)
        label = label.to_s
        return nil unless /^hslash:(.*)$/ =~ label
        NormalText.new("&\#x0210F;")
      end

      # alias ISOAMSR bump 
      def ext_inline_verb_HumpDownHump(label, content, visitor)
        label = label.to_s
        return nil unless /^HumpDownHump:(.*)$/ =~ label
        NormalText.new("&\#x0224E;")
      end

      # alias ISOAMSR bumpe 
      def ext_inline_verb_HumpEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^HumpEqual:(.*)$/ =~ label
        NormalText.new("&\#x0224F;")
      end

      # alias ISOTECH qint 
      def ext_inline_verb_iiiint(label, content, visitor)
        label = label.to_s
        return nil unless /^iiiint:(.*)$/ =~ label
        NormalText.new("&\#x02A0C;")
      end

      # alias ISOTECH tint 
      def ext_inline_verb_iiint(label, content, visitor)
        label = label.to_s
        return nil unless /^iiint:(.*)$/ =~ label
        NormalText.new("&\#x0222D;")
      end

      # alias ISOAMSO image 
      def ext_inline_verb_Im(label, content, visitor)
        label = label.to_s
        return nil unless /^Im:(.*)$/ =~ label
        NormalText.new("&\#x02111;")
      end

      # i for use as a square root of -1 
      def ext_inline_verb_ImaginaryI(label, content, visitor)
        label = label.to_s
        return nil unless /^ImaginaryI:(.*)$/ =~ label
        NormalText.new("&\#x02148;")
      end

      # the geometric imaginary line 
      def ext_inline_verb_imagline(label, content, visitor)
        label = label.to_s
        return nil unless /^imagline:(.*)$/ =~ label
        NormalText.new("&\#x02110;")
      end

      # alias ISOAMSO image 
      def ext_inline_verb_imagpart(label, content, visitor)
        label = label.to_s
        return nil unless /^imagpart:(.*)$/ =~ label
        NormalText.new("&\#x02111;")
      end

      # alias ISOTECH rArr 
      def ext_inline_verb_Implies(label, content, visitor)
        label = label.to_s
        return nil unless /^Implies:(.*)$/ =~ label
        NormalText.new("&\#x021D2;")
      end

      # ISOTECH   isin  
      def ext_inline_verb_in(label, content, visitor)
        label = label.to_s
        return nil unless /^in:(.*)$/ =~ label
        NormalText.new("&\#x02208;")
      end

      # the ring of integers 
      def ext_inline_verb_integers(label, content, visitor)
        label = label.to_s
        return nil unless /^integers:(.*)$/ =~ label
        NormalText.new("&\#x02124;")
      end

      # alias ISOTECH int 
      def ext_inline_verb_Integral(label, content, visitor)
        label = label.to_s
        return nil unless /^Integral:(.*)$/ =~ label
        NormalText.new("&\#x0222B;")
      end

      # alias ISOAMSB intcal 
      def ext_inline_verb_intercal(label, content, visitor)
        label = label.to_s
        return nil unless /^intercal:(.*)$/ =~ label
        NormalText.new("&\#x022BA;")
      end

      # alias ISOAMSB xcap 
      def ext_inline_verb_Intersection(label, content, visitor)
        label = label.to_s
        return nil unless /^Intersection:(.*)$/ =~ label
        NormalText.new("&\#x022C2;")
      end

      # alias ISOAMSB iprod 
      def ext_inline_verb_intprod(label, content, visitor)
        label = label.to_s
        return nil unless /^intprod:(.*)$/ =~ label
        NormalText.new("&\#x02A3C;")
      end

      # used as a separator, e.g., in indices 
      def ext_inline_verb_InvisibleComma(label, content, visitor)
        label = label.to_s
        return nil unless /^InvisibleComma:(.*)$/ =~ label
        NormalText.new("&\#x02063;")
      end

      # marks multiplication when it is understood without a mark 
      def ext_inline_verb_InvisibleTimes(label, content, visitor)
        label = label.to_s
        return nil unless /^InvisibleTimes:(.*)$/ =~ label
        NormalText.new("&\#x02062;")
      end

      # alias ISOTECH lang 
      def ext_inline_verb_langle(label, content, visitor)
        label = label.to_s
        return nil unless /^langle:(.*)$/ =~ label
        NormalText.new("&\#x02329;")
      end

      # Laplace transform 
      def ext_inline_verb_Laplacetrf(label, content, visitor)
        label = label.to_s
        return nil unless /^Laplacetrf:(.*)$/ =~ label
        NormalText.new("&\#x02112;")
      end

      # alias ISONUM lcub 
      def ext_inline_verb_lbrace(label, content, visitor)
        label = label.to_s
        return nil unless /^lbrace:(.*)$/ =~ label
        NormalText.new("&\#x0007B;")
      end

      # alias ISONUM lsqb 
      def ext_inline_verb_lbrack(label, content, visitor)
        label = label.to_s
        return nil unless /^lbrack:(.*)$/ =~ label
        NormalText.new("&\#x0005B;")
      end

      # alias ISOTECH lang 
      def ext_inline_verb_LeftAngleBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftAngleBracket:(.*)$/ =~ label
        NormalText.new("&\#x02329;")
      end

      # alias ISONUM larr 
      def ext_inline_verb_LeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x02190;")
      end

      # alias ISOTECH lArr 
      def ext_inline_verb_Leftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Leftarrow:(.*)$/ =~ label
        NormalText.new("&\#x021D0;")
      end

      # alias ISONUM larr 
      def ext_inline_verb_leftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^leftarrow:(.*)$/ =~ label
        NormalText.new("&\#x02190;")
      end

      # alias for larrb 
      def ext_inline_verb_LeftArrowBar(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftArrowBar:(.*)$/ =~ label
        NormalText.new("&\#x021E4;")
      end

      # alias ISOAMSA lrarr 
      def ext_inline_verb_LeftArrowRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftArrowRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x021C6;")
      end

      # alias ISOAMSA larrtl 
      def ext_inline_verb_leftarrowtail(label, content, visitor)
        label = label.to_s
        return nil unless /^leftarrowtail:(.*)$/ =~ label
        NormalText.new("&\#x021A2;")
      end

      # alias ISOAMSC lceil 
      def ext_inline_verb_LeftCeiling(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftCeiling:(.*)$/ =~ label
        NormalText.new("&\#x02308;")
      end

      # left double bracket delimiter 
      def ext_inline_verb_LeftDoubleBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftDoubleBracket:(.*)$/ =~ label
        NormalText.new("&\#x0301A;")
      end

      # alias ISOAMSA dharl 
      def ext_inline_verb_LeftDownVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftDownVector:(.*)$/ =~ label
        NormalText.new("&\#x021C3;")
      end

      # alias ISOAMSC lfloor 
      def ext_inline_verb_LeftFloor(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftFloor:(.*)$/ =~ label
        NormalText.new("&\#x0230A;")
      end

      # alias ISOAMSA lhard 
      def ext_inline_verb_leftharpoondown(label, content, visitor)
        label = label.to_s
        return nil unless /^leftharpoondown:(.*)$/ =~ label
        NormalText.new("&\#x021BD;")
      end

      # alias ISOAMSA lharu 
      def ext_inline_verb_leftharpoonup(label, content, visitor)
        label = label.to_s
        return nil unless /^leftharpoonup:(.*)$/ =~ label
        NormalText.new("&\#x021BC;")
      end

      # alias ISOAMSA llarr 
      def ext_inline_verb_leftleftarrows(label, content, visitor)
        label = label.to_s
        return nil unless /^leftleftarrows:(.*)$/ =~ label
        NormalText.new("&\#x021C7;")
      end

      # alias ISOAMSA harr 
      def ext_inline_verb_LeftRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x02194;")
      end

      # alias ISOAMSA hArr 
      def ext_inline_verb_Leftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Leftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021D4;")
      end

      # alias ISOAMSA harr 
      def ext_inline_verb_leftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^leftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x02194;")
      end

      # alias ISOAMSA lrarr 
      def ext_inline_verb_leftrightarrows(label, content, visitor)
        label = label.to_s
        return nil unless /^leftrightarrows:(.*)$/ =~ label
        NormalText.new("&\#x021C6;")
      end

      # alias ISOAMSA lrhar 
      def ext_inline_verb_leftrightharpoons(label, content, visitor)
        label = label.to_s
        return nil unless /^leftrightharpoons:(.*)$/ =~ label
        NormalText.new("&\#x021CB;")
      end

      # alias ISOAMSA harrw 
      def ext_inline_verb_leftrightsquigarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^leftrightsquigarrow:(.*)$/ =~ label
        NormalText.new("&\#x021AD;")
      end

      # alias ISOAMSR dashv 
      def ext_inline_verb_LeftTee(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTee:(.*)$/ =~ label
        NormalText.new("&\#x022A3;")
      end

      # alias for mapstoleft 
      def ext_inline_verb_LeftTeeArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTeeArrow:(.*)$/ =~ label
        NormalText.new("&\#x021A4;")
      end

      # alias ISOAMSB lthree 
      def ext_inline_verb_leftthreetimes(label, content, visitor)
        label = label.to_s
        return nil unless /^leftthreetimes:(.*)$/ =~ label
        NormalText.new("&\#x022CB;")
      end

      # alias ISOAMSR vltri 
      def ext_inline_verb_LeftTriangle(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTriangle:(.*)$/ =~ label
        NormalText.new("&\#x022B2;")
      end

      # alias ISOAMSR ltrie 
      def ext_inline_verb_LeftTriangleEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftTriangleEqual:(.*)$/ =~ label
        NormalText.new("&\#x022B4;")
      end

      # alias ISOAMSA uharl 
      def ext_inline_verb_LeftUpVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftUpVector:(.*)$/ =~ label
        NormalText.new("&\#x021BF;")
      end

      # alias ISOAMSA lharu 
      def ext_inline_verb_LeftVector(label, content, visitor)
        label = label.to_s
        return nil unless /^LeftVector:(.*)$/ =~ label
        NormalText.new("&\#x021BC;")
      end

      # alias ISOTECH le 
      def ext_inline_verb_leq(label, content, visitor)
        label = label.to_s
        return nil unless /^leq:(.*)$/ =~ label
        NormalText.new("&\#x02264;")
      end

      # alias ISOAMSR lE 
      def ext_inline_verb_leqq(label, content, visitor)
        label = label.to_s
        return nil unless /^leqq:(.*)$/ =~ label
        NormalText.new("&\#x02266;")
      end

      # alias ISOAMSR les 
      def ext_inline_verb_leqslant(label, content, visitor)
        label = label.to_s
        return nil unless /^leqslant:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;")
      end

      # alias ISOAMSR lap 
      def ext_inline_verb_lessapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^lessapprox:(.*)$/ =~ label
        NormalText.new("&\#x02A85;")
      end

      # alias ISOAMSR ltdot 
      def ext_inline_verb_lessdot(label, content, visitor)
        label = label.to_s
        return nil unless /^lessdot:(.*)$/ =~ label
        NormalText.new("&\#x022D6;")
      end

      # alias ISOAMSR leg 
      def ext_inline_verb_lesseqgtr(label, content, visitor)
        label = label.to_s
        return nil unless /^lesseqgtr:(.*)$/ =~ label
        NormalText.new("&\#x022DA;")
      end

      # alias ISOAMSR lEg 
      def ext_inline_verb_lesseqqgtr(label, content, visitor)
        label = label.to_s
        return nil unless /^lesseqqgtr:(.*)$/ =~ label
        NormalText.new("&\#x02A8B;")
      end

      # alias ISOAMSR leg 
      def ext_inline_verb_LessEqualGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^LessEqualGreater:(.*)$/ =~ label
        NormalText.new("&\#x022DA;")
      end

      # alias ISOAMSR lE 
      def ext_inline_verb_LessFullEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^LessFullEqual:(.*)$/ =~ label
        NormalText.new("&\#x02266;")
      end

      # alias ISOAMSR lg 
      def ext_inline_verb_LessGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^LessGreater:(.*)$/ =~ label
        NormalText.new("&\#x02276;")
      end

      # alias ISOAMSR lg 
      def ext_inline_verb_lessgtr(label, content, visitor)
        label = label.to_s
        return nil unless /^lessgtr:(.*)$/ =~ label
        NormalText.new("&\#x02276;")
      end

      # alias ISOAMSR lsim 
      def ext_inline_verb_lesssim(label, content, visitor)
        label = label.to_s
        return nil unless /^lesssim:(.*)$/ =~ label
        NormalText.new("&\#x02272;")
      end

      # alias ISOAMSR les 
      def ext_inline_verb_LessSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^LessSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;")
      end

      # alias ISOAMSR lsim 
      def ext_inline_verb_LessTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^LessTilde:(.*)$/ =~ label
        NormalText.new("&\#x02272;")
      end

      # alias ISOAMSR Lt 
      def ext_inline_verb_ll(label, content, visitor)
        label = label.to_s
        return nil unless /^ll:(.*)$/ =~ label
        NormalText.new("&\#x0226A;")
      end

      # alias ISOAMSC dlcorn 
      def ext_inline_verb_llcorner(label, content, visitor)
        label = label.to_s
        return nil unless /^llcorner:(.*)$/ =~ label
        NormalText.new("&\#x0231E;")
      end

      # alias ISOAMSA lAarr 
      def ext_inline_verb_Lleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Lleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x021DA;")
      end

      # alias ISOAMSC lmoust 
      def ext_inline_verb_lmoustache(label, content, visitor)
        label = label.to_s
        return nil unless /^lmoustache:(.*)$/ =~ label
        NormalText.new("&\#x023B0;")
      end

      # alias ISOAMSN lnap 
      def ext_inline_verb_lnapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^lnapprox:(.*)$/ =~ label
        NormalText.new("&\#x02A89;")
      end

      # alias ISOAMSN lne 
      def ext_inline_verb_lneq(label, content, visitor)
        label = label.to_s
        return nil unless /^lneq:(.*)$/ =~ label
        NormalText.new("&\#x02A87;")
      end

      # alias ISOAMSN lnE 
      def ext_inline_verb_lneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^lneqq:(.*)$/ =~ label
        NormalText.new("&\#x02268;")
      end

      # alias ISOAMSA xlarr 
      def ext_inline_verb_LongLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LongLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x027F5;")
      end

      # alias ISOAMSA xlArr 
      def ext_inline_verb_Longleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Longleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x027F8;")
      end

      # alias ISOAMSA xlarr 
      def ext_inline_verb_longleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^longleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x027F5;")
      end

      # alias ISOAMSA xharr 
      def ext_inline_verb_LongLeftRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LongLeftRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x027F7;")
      end

      # alias ISOAMSA xhArr 
      def ext_inline_verb_Longleftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Longleftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x027FA;")
      end

      # alias ISOAMSA xharr 
      def ext_inline_verb_longleftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^longleftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x027F7;")
      end

      # alias ISOAMSA xmap 
      def ext_inline_verb_longmapsto(label, content, visitor)
        label = label.to_s
        return nil unless /^longmapsto:(.*)$/ =~ label
        NormalText.new("&\#x027FC;")
      end

      # alias ISOAMSA xrarr 
      def ext_inline_verb_LongRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LongRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x027F6;")
      end

      # alias ISOAMSA xrArr 
      def ext_inline_verb_Longrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Longrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x027F9;")
      end

      # alias ISOAMSA xrarr 
      def ext_inline_verb_longrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^longrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x027F6;")
      end

      # alias ISOAMSA larrlp 
      def ext_inline_verb_looparrowleft(label, content, visitor)
        label = label.to_s
        return nil unless /^looparrowleft:(.*)$/ =~ label
        NormalText.new("&\#x021AB;")
      end

      # alias ISOAMSA rarrlp 
      def ext_inline_verb_looparrowright(label, content, visitor)
        label = label.to_s
        return nil unless /^looparrowright:(.*)$/ =~ label
        NormalText.new("&\#x021AC;")
      end

      # alias ISOAMSA swarr 
      def ext_inline_verb_LowerLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LowerLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x02199;")
      end

      # alias ISOAMSA searr 
      def ext_inline_verb_LowerRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^LowerRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x02198;")
      end

      # alias ISOPUB loz 
      def ext_inline_verb_lozenge(label, content, visitor)
        label = label.to_s
        return nil unless /^lozenge:(.*)$/ =~ label
        NormalText.new("&\#x025CA;")
      end

      # alias ISOAMSC drcorn 
      def ext_inline_verb_lrcorner(label, content, visitor)
        label = label.to_s
        return nil unless /^lrcorner:(.*)$/ =~ label
        NormalText.new("&\#x0231F;")
      end

      # alias ISOAMSA lsh 
      def ext_inline_verb_Lsh(label, content, visitor)
        label = label.to_s
        return nil unless /^Lsh:(.*)$/ =~ label
        NormalText.new("&\#x021B0;")
      end

      # alias ISOAMSN lvnE 
      def ext_inline_verb_lvertneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^lvertneqq:(.*)$/ =~ label
        NormalText.new("&\#x02268;&\#x0FE00;")
      end

      # alias ISOPUB malt 
      def ext_inline_verb_maltese(label, content, visitor)
        label = label.to_s
        return nil unless /^maltese:(.*)$/ =~ label
        NormalText.new("&\#x02720;")
      end

      # alias ISOAMSA map 
      def ext_inline_verb_mapsto(label, content, visitor)
        label = label.to_s
        return nil unless /^mapsto:(.*)$/ =~ label
        NormalText.new("&\#x021A6;")
      end

      # alias ISOAMSO angmsd 
      def ext_inline_verb_measuredangle(label, content, visitor)
        label = label.to_s
        return nil unless /^measuredangle:(.*)$/ =~ label
        NormalText.new("&\#x02221;")
      end

      # Mellin transform 
      def ext_inline_verb_Mellintrf(label, content, visitor)
        label = label.to_s
        return nil unless /^Mellintrf:(.*)$/ =~ label
        NormalText.new("&\#x02133;")
      end

      # alias ISOTECH mnplus 
      def ext_inline_verb_MinusPlus(label, content, visitor)
        label = label.to_s
        return nil unless /^MinusPlus:(.*)$/ =~ label
        NormalText.new("&\#x02213;")
      end

      # alias ISOTECH mnplus 
      def ext_inline_verb_mp(label, content, visitor)
        label = label.to_s
        return nil unless /^mp:(.*)$/ =~ label
        NormalText.new("&\#x02213;")
      end

      # alias ISOAMSA mumap 
      def ext_inline_verb_multimap(label, content, visitor)
        label = label.to_s
        return nil unless /^multimap:(.*)$/ =~ label
        NormalText.new("&\#x022B8;")
      end

      # alias ISOAMSN nap 
      def ext_inline_verb_napprox(label, content, visitor)
        label = label.to_s
        return nil unless /^napprox:(.*)$/ =~ label
        NormalText.new("&\#x02249;")
      end

      # alias ISOPUB natur 
      def ext_inline_verb_natural(label, content, visitor)
        label = label.to_s
        return nil unless /^natural:(.*)$/ =~ label
        NormalText.new("&\#x0266E;")
      end

      # the semi-ring of natural numbers 
      def ext_inline_verb_naturals(label, content, visitor)
        label = label.to_s
        return nil unless /^naturals:(.*)$/ =~ label
        NormalText.new("&\#x02115;")
      end

      # alias ISOAMSA nearr 
      def ext_inline_verb_nearrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nearrow:(.*)$/ =~ label
        NormalText.new("&\#x02197;")
      end

      # space of width -4/18 em 
      def ext_inline_verb_NegativeMediumSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^NegativeMediumSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200B;")
      end

      # space of width -5/18 em 
      def ext_inline_verb_NegativeThickSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^NegativeThickSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200B;")
      end

      # space of width -3/18 em 
      def ext_inline_verb_NegativeThinSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^NegativeThinSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200B;")
      end

      # space of width -1/18 em 
      def ext_inline_verb_NegativeVeryThinSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^NegativeVeryThinSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200B;")
      end

      # alias ISOAMSR Gt 
      def ext_inline_verb_NestedGreaterGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^NestedGreaterGreater:(.*)$/ =~ label
        NormalText.new("&\#x0226B;")
      end

      # alias ISOAMSR Lt 
      def ext_inline_verb_NestedLessLess(label, content, visitor)
        label = label.to_s
        return nil unless /^NestedLessLess:(.*)$/ =~ label
        NormalText.new("&\#x0226A;")
      end

      # alias ISOAMSO nexist 
      def ext_inline_verb_nexists(label, content, visitor)
        label = label.to_s
        return nil unless /^nexists:(.*)$/ =~ label
        NormalText.new("&\#x02204;")
      end

      # alias ISOAMSN nge 
      def ext_inline_verb_ngeq(label, content, visitor)
        label = label.to_s
        return nil unless /^ngeq:(.*)$/ =~ label
        NormalText.new("&\#x02271;")
      end

      # alias ISOAMSN ngE 
      def ext_inline_verb_ngeqq(label, content, visitor)
        label = label.to_s
        return nil unless /^ngeqq:(.*)$/ =~ label
        NormalText.new("&\#x02267;&\#x00338;")
      end

      # alias ISOAMSN nges 
      def ext_inline_verb_ngeqslant(label, content, visitor)
        label = label.to_s
        return nil unless /^ngeqslant:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;&\#x00338;")
      end

      # alias ISOAMSN ngt 
      def ext_inline_verb_ngtr(label, content, visitor)
        label = label.to_s
        return nil unless /^ngtr:(.*)$/ =~ label
        NormalText.new("&\#x0226F;")
      end

      # alias ISOAMSA nlArr 
      def ext_inline_verb_nLeftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nLeftarrow:(.*)$/ =~ label
        NormalText.new("&\#x021CD;")
      end

      # alias ISOAMSA nlarr 
      def ext_inline_verb_nleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x0219A;")
      end

      # alias ISOAMSA nhArr 
      def ext_inline_verb_nLeftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nLeftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021CE;")
      end

      # alias ISOAMSA nharr 
      def ext_inline_verb_nleftrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nleftrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021AE;")
      end

      # alias ISOAMSN nle 
      def ext_inline_verb_nleq(label, content, visitor)
        label = label.to_s
        return nil unless /^nleq:(.*)$/ =~ label
        NormalText.new("&\#x02270;")
      end

      # alias ISOAMSN nlE 
      def ext_inline_verb_nleqq(label, content, visitor)
        label = label.to_s
        return nil unless /^nleqq:(.*)$/ =~ label
        NormalText.new("&\#x02266;&\#x00338;")
      end

      # alias ISOAMSN nles 
      def ext_inline_verb_nleqslant(label, content, visitor)
        label = label.to_s
        return nil unless /^nleqslant:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;&\#x00338;")
      end

      # alias ISOAMSN nlt 
      def ext_inline_verb_nless(label, content, visitor)
        label = label.to_s
        return nil unless /^nless:(.*)$/ =~ label
        NormalText.new("&\#x0226E;")
      end

      # alias ISONUM nbsp 
      def ext_inline_verb_NonBreakingSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^NonBreakingSpace:(.*)$/ =~ label
        NormalText.new("&\#x000A0;")
      end

      # alias ISOAMSN nequiv 
      def ext_inline_verb_NotCongruent(label, content, visitor)
        label = label.to_s
        return nil unless /^NotCongruent:(.*)$/ =~ label
        NormalText.new("&\#x02262;")
      end

      # alias ISOAMSN npar 
      def ext_inline_verb_NotDoubleVerticalBar(label, content, visitor)
        label = label.to_s
        return nil unless /^NotDoubleVerticalBar:(.*)$/ =~ label
        NormalText.new("&\#x02226;")
      end

      # alias ISOTECH notin 
      def ext_inline_verb_NotElement(label, content, visitor)
        label = label.to_s
        return nil unless /^NotElement:(.*)$/ =~ label
        NormalText.new("&\#x02209;")
      end

      # alias ISOTECH ne 
      def ext_inline_verb_NotEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotEqual:(.*)$/ =~ label
        NormalText.new("&\#x02260;")
      end

      # alias for  &nesim; 
      def ext_inline_verb_NotEqualTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotEqualTilde:(.*)$/ =~ label
        NormalText.new("&\#x02242;&\#x00338;")
      end

      # alias ISOAMSO nexist 
      def ext_inline_verb_NotExists(label, content, visitor)
        label = label.to_s
        return nil unless /^NotExists:(.*)$/ =~ label
        NormalText.new("&\#x02204;")
      end

      # alias ISOAMSN ngt 
      def ext_inline_verb_NotGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreater:(.*)$/ =~ label
        NormalText.new("&\#x0226F;")
      end

      # alias ISOAMSN nge 
      def ext_inline_verb_NotGreaterEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterEqual:(.*)$/ =~ label
        NormalText.new("&\#x02271;")
      end

      # alias ISOAMSN nlE 
      def ext_inline_verb_NotGreaterFullEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterFullEqual:(.*)$/ =~ label
        NormalText.new("&\#x02266;&\#x00338;")
      end

      # alias ISOAMSN nGtv 
      def ext_inline_verb_NotGreaterGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterGreater:(.*)$/ =~ label
        NormalText.new("&\#x0226B;&\#x00338;")
      end

      # alias ISOAMSN ntvgl 
      def ext_inline_verb_NotGreaterLess(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterLess:(.*)$/ =~ label
        NormalText.new("&\#x02279;")
      end

      # alias ISOAMSN nges 
      def ext_inline_verb_NotGreaterSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;&\#x00338;")
      end

      # alias ISOAMSN ngsim 
      def ext_inline_verb_NotGreaterTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotGreaterTilde:(.*)$/ =~ label
        NormalText.new("&\#x02275;")
      end

      # alias for &nbump; 
      def ext_inline_verb_NotHumpDownHump(label, content, visitor)
        label = label.to_s
        return nil unless /^NotHumpDownHump:(.*)$/ =~ label
        NormalText.new("&\#x0224E;&\#x00338;")
      end

      # alias ISOAMSN nltri 
      def ext_inline_verb_NotLeftTriangle(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLeftTriangle:(.*)$/ =~ label
        NormalText.new("&\#x022EA;")
      end

      # alias ISOAMSN nltrie 
      def ext_inline_verb_NotLeftTriangleEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLeftTriangleEqual:(.*)$/ =~ label
        NormalText.new("&\#x022EC;")
      end

      # alias ISOAMSN nlt 
      def ext_inline_verb_NotLess(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLess:(.*)$/ =~ label
        NormalText.new("&\#x0226E;")
      end

      # alias ISOAMSN nle 
      def ext_inline_verb_NotLessEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLessEqual:(.*)$/ =~ label
        NormalText.new("&\#x02270;")
      end

      # alias ISOAMSN ntvlg 
      def ext_inline_verb_NotLessGreater(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLessGreater:(.*)$/ =~ label
        NormalText.new("&\#x02278;")
      end

      # alias ISOAMSN nLtv 
      def ext_inline_verb_NotLessLess(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLessLess:(.*)$/ =~ label
        NormalText.new("&\#x0226A;&\#x00338;")
      end

      # alias ISOAMSN nles 
      def ext_inline_verb_NotLessSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLessSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;&\#x00338;")
      end

      # alias ISOAMSN nlsim 
      def ext_inline_verb_NotLessTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotLessTilde:(.*)$/ =~ label
        NormalText.new("&\#x02274;")
      end

      # alias ISOAMSN npr 
      def ext_inline_verb_NotPrecedes(label, content, visitor)
        label = label.to_s
        return nil unless /^NotPrecedes:(.*)$/ =~ label
        NormalText.new("&\#x02280;")
      end

      # alias ISOAMSN npre 
      def ext_inline_verb_NotPrecedesEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotPrecedesEqual:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;&\#x00338;")
      end

      # alias ISOAMSN nprcue 
      def ext_inline_verb_NotPrecedesSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotPrecedesSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x022E0;")
      end

      # alias ISOTECH notniva 
      def ext_inline_verb_NotReverseElement(label, content, visitor)
        label = label.to_s
        return nil unless /^NotReverseElement:(.*)$/ =~ label
        NormalText.new("&\#x0220C;")
      end

      # alias ISOAMSN nrtri 
      def ext_inline_verb_NotRightTriangle(label, content, visitor)
        label = label.to_s
        return nil unless /^NotRightTriangle:(.*)$/ =~ label
        NormalText.new("&\#x022EB;")
      end

      # alias ISOAMSN nrtrie 
      def ext_inline_verb_NotRightTriangleEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotRightTriangleEqual:(.*)$/ =~ label
        NormalText.new("&\#x022ED;")
      end

      # alias ISOAMSN nsqsube 
      def ext_inline_verb_NotSquareSubsetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSquareSubsetEqual:(.*)$/ =~ label
        NormalText.new("&\#x022E2;")
      end

      # alias ISOAMSN nsqsupe 
      def ext_inline_verb_NotSquareSupersetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSquareSupersetEqual:(.*)$/ =~ label
        NormalText.new("&\#x022E3;")
      end

      # alias ISOAMSN vnsub 
      def ext_inline_verb_NotSubset(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSubset:(.*)$/ =~ label
        NormalText.new("&\#x02282;&\#x020D2;")
      end

      # alias ISOAMSN nsube 
      def ext_inline_verb_NotSubsetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSubsetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02288;")
      end

      # alias ISOAMSN nsc 
      def ext_inline_verb_NotSucceeds(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSucceeds:(.*)$/ =~ label
        NormalText.new("&\#x02281;")
      end

      # alias ISOAMSN nsce 
      def ext_inline_verb_NotSucceedsEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSucceedsEqual:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;&\#x00338;")
      end

      # alias ISOAMSN nsccue 
      def ext_inline_verb_NotSucceedsSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSucceedsSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x022E1;")
      end

      # alias ISOAMSN vnsup 
      def ext_inline_verb_NotSuperset(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSuperset:(.*)$/ =~ label
        NormalText.new("&\#x02283;&\#x020D2;")
      end

      # alias ISOAMSN nsupe 
      def ext_inline_verb_NotSupersetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotSupersetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02289;")
      end

      # alias ISOAMSN nsim 
      def ext_inline_verb_NotTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotTilde:(.*)$/ =~ label
        NormalText.new("&\#x02241;")
      end

      # alias ISOAMSN nsime 
      def ext_inline_verb_NotTildeEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotTildeEqual:(.*)$/ =~ label
        NormalText.new("&\#x02244;")
      end

      # alias ISOAMSN ncong 
      def ext_inline_verb_NotTildeFullEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^NotTildeFullEqual:(.*)$/ =~ label
        NormalText.new("&\#x02247;")
      end

      # alias ISOAMSN nap 
      def ext_inline_verb_NotTildeTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^NotTildeTilde:(.*)$/ =~ label
        NormalText.new("&\#x02249;")
      end

      # alias ISOAMSN nmid 
      def ext_inline_verb_NotVerticalBar(label, content, visitor)
        label = label.to_s
        return nil unless /^NotVerticalBar:(.*)$/ =~ label
        NormalText.new("&\#x02224;")
      end

      # alias ISOAMSN npar 
      def ext_inline_verb_nparallel(label, content, visitor)
        label = label.to_s
        return nil unless /^nparallel:(.*)$/ =~ label
        NormalText.new("&\#x02226;")
      end

      # alias ISOAMSN npr 
      def ext_inline_verb_nprec(label, content, visitor)
        label = label.to_s
        return nil unless /^nprec:(.*)$/ =~ label
        NormalText.new("&\#x02280;")
      end

      # alias ISOAMSN npre 
      def ext_inline_verb_npreceq(label, content, visitor)
        label = label.to_s
        return nil unless /^npreceq:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;&\#x00338;")
      end

      # alias ISOAMSA nrArr 
      def ext_inline_verb_nRightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nRightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021CF;")
      end

      # alias ISOAMSA nrarr 
      def ext_inline_verb_nrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x0219B;")
      end

      # alias ISOAMSN nsmid 
      def ext_inline_verb_nshortmid(label, content, visitor)
        label = label.to_s
        return nil unless /^nshortmid:(.*)$/ =~ label
        NormalText.new("&\#x02224;")
      end

      # alias ISOAMSN nspar 
      def ext_inline_verb_nshortparallel(label, content, visitor)
        label = label.to_s
        return nil unless /^nshortparallel:(.*)$/ =~ label
        NormalText.new("&\#x02226;")
      end

      # alias ISOAMSN nsime 
      def ext_inline_verb_nsimeq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsimeq:(.*)$/ =~ label
        NormalText.new("&\#x02244;")
      end

      # alias ISOAMSN vnsub 
      def ext_inline_verb_nsubset(label, content, visitor)
        label = label.to_s
        return nil unless /^nsubset:(.*)$/ =~ label
        NormalText.new("&\#x02282;&\#x020D2;")
      end

      # alias ISOAMSN nsube 
      def ext_inline_verb_nsubseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsubseteq:(.*)$/ =~ label
        NormalText.new("&\#x02288;")
      end

      # alias ISOAMSN nsubE 
      def ext_inline_verb_nsubseteqq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsubseteqq:(.*)$/ =~ label
        NormalText.new("&\#x02AC5;&\#x00338;")
      end

      # alias ISOAMSN nsc 
      def ext_inline_verb_nsucc(label, content, visitor)
        label = label.to_s
        return nil unless /^nsucc:(.*)$/ =~ label
        NormalText.new("&\#x02281;")
      end

      # alias ISOAMSN nsce 
      def ext_inline_verb_nsucceq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsucceq:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;&\#x00338;")
      end

      # alias ISOAMSN vnsup 
      def ext_inline_verb_nsupset(label, content, visitor)
        label = label.to_s
        return nil unless /^nsupset:(.*)$/ =~ label
        NormalText.new("&\#x02283;&\#x020D2;")
      end

      # alias ISOAMSN nsupe 
      def ext_inline_verb_nsupseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsupseteq:(.*)$/ =~ label
        NormalText.new("&\#x02289;")
      end

      # alias ISOAMSN nsupE 
      def ext_inline_verb_nsupseteqq(label, content, visitor)
        label = label.to_s
        return nil unless /^nsupseteqq:(.*)$/ =~ label
        NormalText.new("&\#x02AC6;&\#x00338;")
      end

      # alias ISOAMSN nltri 
      def ext_inline_verb_ntriangleleft(label, content, visitor)
        label = label.to_s
        return nil unless /^ntriangleleft:(.*)$/ =~ label
        NormalText.new("&\#x022EA;")
      end

      # alias ISOAMSN nltrie 
      def ext_inline_verb_ntrianglelefteq(label, content, visitor)
        label = label.to_s
        return nil unless /^ntrianglelefteq:(.*)$/ =~ label
        NormalText.new("&\#x022EC;")
      end

      # alias ISOAMSN nrtri 
      def ext_inline_verb_ntriangleright(label, content, visitor)
        label = label.to_s
        return nil unless /^ntriangleright:(.*)$/ =~ label
        NormalText.new("&\#x022EB;")
      end

      # alias ISOAMSN nrtrie 
      def ext_inline_verb_ntrianglerighteq(label, content, visitor)
        label = label.to_s
        return nil unless /^ntrianglerighteq:(.*)$/ =~ label
        NormalText.new("&\#x022ED;")
      end

      # alias ISOAMSA nwarr 
      def ext_inline_verb_nwarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^nwarrow:(.*)$/ =~ label
        NormalText.new("&\#x02196;")
      end

      # alias ISOTECH conint 
      def ext_inline_verb_oint(label, content, visitor)
        label = label.to_s
        return nil unless /^oint:(.*)$/ =~ label
        NormalText.new("&\#x0222E;")
      end

      # alias ISONUM ldquo 
      def ext_inline_verb_OpenCurlyDoubleQuote(label, content, visitor)
        label = label.to_s
        return nil unless /^OpenCurlyDoubleQuote:(.*)$/ =~ label
        NormalText.new("&\#x0201C;")
      end

      # alias ISONUM lsquo 
      def ext_inline_verb_OpenCurlyQuote(label, content, visitor)
        label = label.to_s
        return nil unless /^OpenCurlyQuote:(.*)$/ =~ label
        NormalText.new("&\#x02018;")
      end

      # alias ISOTECH order 
      def ext_inline_verb_orderof(label, content, visitor)
        label = label.to_s
        return nil unless /^orderof:(.*)$/ =~ label
        NormalText.new("&\#x02134;")
      end

      # alias ISOTECH par 
      def ext_inline_verb_parallel(label, content, visitor)
        label = label.to_s
        return nil unless /^parallel:(.*)$/ =~ label
        NormalText.new("&\#x02225;")
      end

      # alias ISOTECH part 
      def ext_inline_verb_PartialD(label, content, visitor)
        label = label.to_s
        return nil unless /^PartialD:(.*)$/ =~ label
        NormalText.new("&\#x02202;")
      end

      # alias ISOAMSR fork 
      def ext_inline_verb_pitchfork(label, content, visitor)
        label = label.to_s
        return nil unless /^pitchfork:(.*)$/ =~ label
        NormalText.new("&\#x022D4;")
      end

      # alias ISONUM plusmn 
      def ext_inline_verb_PlusMinus(label, content, visitor)
        label = label.to_s
        return nil unless /^PlusMinus:(.*)$/ =~ label
        NormalText.new("&\#x000B1;")
      end

      # alias ISONUM plusmn 
      def ext_inline_verb_pm(label, content, visitor)
        label = label.to_s
        return nil unless /^pm:(.*)$/ =~ label
        NormalText.new("&\#x000B1;")
      end

      # the Poincare upper half-plane 
      def ext_inline_verb_Poincareplane(label, content, visitor)
        label = label.to_s
        return nil unless /^Poincareplane:(.*)$/ =~ label
        NormalText.new("&\#x0210C;")
      end

      # alias ISOAMSR pr 
      def ext_inline_verb_prec(label, content, visitor)
        label = label.to_s
        return nil unless /^prec:(.*)$/ =~ label
        NormalText.new("&\#x0227A;")
      end

      # alias ISOAMSR prap 
      def ext_inline_verb_precapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^precapprox:(.*)$/ =~ label
        NormalText.new("&\#x02AB7;")
      end

      # alias ISOAMSR prcue 
      def ext_inline_verb_preccurlyeq(label, content, visitor)
        label = label.to_s
        return nil unless /^preccurlyeq:(.*)$/ =~ label
        NormalText.new("&\#x0227C;")
      end

      # alias ISOAMSR pr 
      def ext_inline_verb_Precedes(label, content, visitor)
        label = label.to_s
        return nil unless /^Precedes:(.*)$/ =~ label
        NormalText.new("&\#x0227A;")
      end

      # alias ISOAMSR pre 
      def ext_inline_verb_PrecedesEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^PrecedesEqual:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;")
      end

      # alias ISOAMSR prcue 
      def ext_inline_verb_PrecedesSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^PrecedesSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x0227C;")
      end

      # alias ISOAMSR prsim 
      def ext_inline_verb_PrecedesTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^PrecedesTilde:(.*)$/ =~ label
        NormalText.new("&\#x0227E;")
      end

      # alias ISOAMSR pre 
      def ext_inline_verb_preceq(label, content, visitor)
        label = label.to_s
        return nil unless /^preceq:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;")
      end

      # alias ISOAMSN prnap 
      def ext_inline_verb_precnapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^precnapprox:(.*)$/ =~ label
        NormalText.new("&\#x02AB9;")
      end

      # alias ISOAMSN prnE 
      def ext_inline_verb_precneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^precneqq:(.*)$/ =~ label
        NormalText.new("&\#x02AB5;")
      end

      # alias ISOAMSN prnsim 
      def ext_inline_verb_precnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^precnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E8;")
      end

      # alias ISOAMSR prsim 
      def ext_inline_verb_precsim(label, content, visitor)
        label = label.to_s
        return nil unless /^precsim:(.*)$/ =~ label
        NormalText.new("&\#x0227E;")
      end

      # the prime natural numbers 
      def ext_inline_verb_primes(label, content, visitor)
        label = label.to_s
        return nil unless /^primes:(.*)$/ =~ label
        NormalText.new("&\#x02119;")
      end

      # alias ISOAMSR Colon 
      def ext_inline_verb_Proportion(label, content, visitor)
        label = label.to_s
        return nil unless /^Proportion:(.*)$/ =~ label
        NormalText.new("&\#x02237;")
      end

      # alias ISOTECH prop 
      def ext_inline_verb_Proportional(label, content, visitor)
        label = label.to_s
        return nil unless /^Proportional:(.*)$/ =~ label
        NormalText.new("&\#x0221D;")
      end

      # alias ISOTECH prop 
      def ext_inline_verb_propto(label, content, visitor)
        label = label.to_s
        return nil unless /^propto:(.*)$/ =~ label
        NormalText.new("&\#x0221D;")
      end

      # the ring (skew field) of quaternions 
      def ext_inline_verb_quaternions(label, content, visitor)
        label = label.to_s
        return nil unless /^quaternions:(.*)$/ =~ label
        NormalText.new("&\#x0210D;")
      end

      # alias ISOAMSR equest 
      def ext_inline_verb_questeq(label, content, visitor)
        label = label.to_s
        return nil unless /^questeq:(.*)$/ =~ label
        NormalText.new("&\#x0225F;")
      end

      # alias ISOTECH rang 
      def ext_inline_verb_rangle(label, content, visitor)
        label = label.to_s
        return nil unless /^rangle:(.*)$/ =~ label
        NormalText.new("&\#x0232A;")
      end

      # the field of rational numbers 
      def ext_inline_verb_rationals(label, content, visitor)
        label = label.to_s
        return nil unless /^rationals:(.*)$/ =~ label
        NormalText.new("&\#x0211A;")
      end

      # alias ISONUM rcub 
      def ext_inline_verb_rbrace(label, content, visitor)
        label = label.to_s
        return nil unless /^rbrace:(.*)$/ =~ label
        NormalText.new("&\#x0007D;")
      end

      # alias ISONUM rsqb 
      def ext_inline_verb_rbrack(label, content, visitor)
        label = label.to_s
        return nil unless /^rbrack:(.*)$/ =~ label
        NormalText.new("&\#x0005D;")
      end

      # alias ISOAMSO real 
      def ext_inline_verb_Re(label, content, visitor)
        label = label.to_s
        return nil unless /^Re:(.*)$/ =~ label
        NormalText.new("&\#x0211C;")
      end

      # the geometric real line 
      def ext_inline_verb_realine(label, content, visitor)
        label = label.to_s
        return nil unless /^realine:(.*)$/ =~ label
        NormalText.new("&\#x0211B;")
      end

      # alias ISOAMSO real 
      def ext_inline_verb_realpart(label, content, visitor)
        label = label.to_s
        return nil unless /^realpart:(.*)$/ =~ label
        NormalText.new("&\#x0211C;")
      end

      # the field of real numbers 
      def ext_inline_verb_reals(label, content, visitor)
        label = label.to_s
        return nil unless /^reals:(.*)$/ =~ label
        NormalText.new("&\#x0211D;")
      end

      # alias ISOTECH niv 
      def ext_inline_verb_ReverseElement(label, content, visitor)
        label = label.to_s
        return nil unless /^ReverseElement:(.*)$/ =~ label
        NormalText.new("&\#x0220B;")
      end

      # alias ISOAMSA lrhar 
      def ext_inline_verb_ReverseEquilibrium(label, content, visitor)
        label = label.to_s
        return nil unless /^ReverseEquilibrium:(.*)$/ =~ label
        NormalText.new("&\#x021CB;")
      end

      # alias ISOAMSA duhar 
      def ext_inline_verb_ReverseUpEquilibrium(label, content, visitor)
        label = label.to_s
        return nil unless /^ReverseUpEquilibrium:(.*)$/ =~ label
        NormalText.new("&\#x0296F;")
      end

      # alias ISOTECH rang 
      def ext_inline_verb_RightAngleBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^RightAngleBracket:(.*)$/ =~ label
        NormalText.new("&\#x0232A;")
      end

      # alias ISONUM rarr 
      def ext_inline_verb_RightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^RightArrow:(.*)$/ =~ label
        NormalText.new("&\#x02192;")
      end

      # alias ISOTECH rArr 
      def ext_inline_verb_Rightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Rightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021D2;")
      end

      # alias ISONUM rarr 
      def ext_inline_verb_rightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^rightarrow:(.*)$/ =~ label
        NormalText.new("&\#x02192;")
      end

      # alias for rarrb 
      def ext_inline_verb_RightArrowBar(label, content, visitor)
        label = label.to_s
        return nil unless /^RightArrowBar:(.*)$/ =~ label
        NormalText.new("&\#x021E5;")
      end

      # alias ISOAMSA rlarr 
      def ext_inline_verb_RightArrowLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^RightArrowLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x021C4;")
      end

      # alias ISOAMSA rarrtl 
      def ext_inline_verb_rightarrowtail(label, content, visitor)
        label = label.to_s
        return nil unless /^rightarrowtail:(.*)$/ =~ label
        NormalText.new("&\#x021A3;")
      end

      # alias ISOAMSC rceil 
      def ext_inline_verb_RightCeiling(label, content, visitor)
        label = label.to_s
        return nil unless /^RightCeiling:(.*)$/ =~ label
        NormalText.new("&\#x02309;")
      end

      # right double bracket delimiter 
      def ext_inline_verb_RightDoubleBracket(label, content, visitor)
        label = label.to_s
        return nil unless /^RightDoubleBracket:(.*)$/ =~ label
        NormalText.new("&\#x0301B;")
      end

      # alias ISOAMSA dharr 
      def ext_inline_verb_RightDownVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightDownVector:(.*)$/ =~ label
        NormalText.new("&\#x021C2;")
      end

      # alias ISOAMSC rfloor 
      def ext_inline_verb_RightFloor(label, content, visitor)
        label = label.to_s
        return nil unless /^RightFloor:(.*)$/ =~ label
        NormalText.new("&\#x0230B;")
      end

      # alias ISOAMSA rhard 
      def ext_inline_verb_rightharpoondown(label, content, visitor)
        label = label.to_s
        return nil unless /^rightharpoondown:(.*)$/ =~ label
        NormalText.new("&\#x021C1;")
      end

      # alias ISOAMSA rharu 
      def ext_inline_verb_rightharpoonup(label, content, visitor)
        label = label.to_s
        return nil unless /^rightharpoonup:(.*)$/ =~ label
        NormalText.new("&\#x021C0;")
      end

      # alias ISOAMSA rlarr 
      def ext_inline_verb_rightleftarrows(label, content, visitor)
        label = label.to_s
        return nil unless /^rightleftarrows:(.*)$/ =~ label
        NormalText.new("&\#x021C4;")
      end

      # alias ISOAMSA rlhar 
      def ext_inline_verb_rightleftharpoons(label, content, visitor)
        label = label.to_s
        return nil unless /^rightleftharpoons:(.*)$/ =~ label
        NormalText.new("&\#x021CC;")
      end

      # alias ISOAMSA rrarr 
      def ext_inline_verb_rightrightarrows(label, content, visitor)
        label = label.to_s
        return nil unless /^rightrightarrows:(.*)$/ =~ label
        NormalText.new("&\#x021C9;")
      end

      # alias ISOAMSA rarrw 
      def ext_inline_verb_rightsquigarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^rightsquigarrow:(.*)$/ =~ label
        NormalText.new("&\#x0219D;")
      end

      # alias ISOAMSR vdash 
      def ext_inline_verb_RightTee(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTee:(.*)$/ =~ label
        NormalText.new("&\#x022A2;")
      end

      # alias ISOAMSA map 
      def ext_inline_verb_RightTeeArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTeeArrow:(.*)$/ =~ label
        NormalText.new("&\#x021A6;")
      end

      # alias ISOAMSB rthree 
      def ext_inline_verb_rightthreetimes(label, content, visitor)
        label = label.to_s
        return nil unless /^rightthreetimes:(.*)$/ =~ label
        NormalText.new("&\#x022CC;")
      end

      # alias ISOAMSR vrtri 
      def ext_inline_verb_RightTriangle(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTriangle:(.*)$/ =~ label
        NormalText.new("&\#x022B3;")
      end

      # alias ISOAMSR rtrie 
      def ext_inline_verb_RightTriangleEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^RightTriangleEqual:(.*)$/ =~ label
        NormalText.new("&\#x022B5;")
      end

      # alias ISOAMSA uharr 
      def ext_inline_verb_RightUpVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightUpVector:(.*)$/ =~ label
        NormalText.new("&\#x021BE;")
      end

      # alias ISOAMSA rharu 
      def ext_inline_verb_RightVector(label, content, visitor)
        label = label.to_s
        return nil unless /^RightVector:(.*)$/ =~ label
        NormalText.new("&\#x021C0;")
      end

      # alias ISOAMSR erDot 
      def ext_inline_verb_risingdotseq(label, content, visitor)
        label = label.to_s
        return nil unless /^risingdotseq:(.*)$/ =~ label
        NormalText.new("&\#x02253;")
      end

      # alias ISOAMSC rmoust 
      def ext_inline_verb_rmoustache(label, content, visitor)
        label = label.to_s
        return nil unless /^rmoustache:(.*)$/ =~ label
        NormalText.new("&\#x023B1;")
      end

      # alias ISOAMSA rAarr 
      def ext_inline_verb_Rrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Rrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021DB;")
      end

      # alias ISOAMSA rsh 
      def ext_inline_verb_Rsh(label, content, visitor)
        label = label.to_s
        return nil unless /^Rsh:(.*)$/ =~ label
        NormalText.new("&\#x021B1;")
      end

      # alias ISOAMSA searr 
      def ext_inline_verb_searrow(label, content, visitor)
        label = label.to_s
        return nil unless /^searrow:(.*)$/ =~ label
        NormalText.new("&\#x02198;")
      end

      # alias ISOAMSB setmn 
      def ext_inline_verb_setminus(label, content, visitor)
        label = label.to_s
        return nil unless /^setminus:(.*)$/ =~ label
        NormalText.new("&\#x02216;")
      end

      # short down arrow 
      def ext_inline_verb_ShortDownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^ShortDownArrow:(.*)$/ =~ label
        NormalText.new("&\#x02193;")
      end

      # alias ISOAMSA slarr 
      def ext_inline_verb_ShortLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^ShortLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x02190;")
      end

      # alias ISOAMSR smid 
      def ext_inline_verb_shortmid(label, content, visitor)
        label = label.to_s
        return nil unless /^shortmid:(.*)$/ =~ label
        NormalText.new("&\#x02223;")
      end

      # alias ISOAMSR spar 
      def ext_inline_verb_shortparallel(label, content, visitor)
        label = label.to_s
        return nil unless /^shortparallel:(.*)$/ =~ label
        NormalText.new("&\#x02225;")
      end

      # alias ISOAMSA srarr 
      def ext_inline_verb_ShortRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^ShortRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x02192;")
      end

      # short up arrow  
      def ext_inline_verb_ShortUpArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^ShortUpArrow:(.*)$/ =~ label
        NormalText.new("&\#x02191;")
      end

      # alias ISOTECH sime 
      def ext_inline_verb_simeq(label, content, visitor)
        label = label.to_s
        return nil unless /^simeq:(.*)$/ =~ label
        NormalText.new("&\#x02243;")
      end

      # alias ISOTECH compfn 
      def ext_inline_verb_SmallCircle(label, content, visitor)
        label = label.to_s
        return nil unless /^SmallCircle:(.*)$/ =~ label
        NormalText.new("&\#x02218;")
      end

      # alias ISOAMSB ssetmn 
      def ext_inline_verb_smallsetminus(label, content, visitor)
        label = label.to_s
        return nil unless /^smallsetminus:(.*)$/ =~ label
        NormalText.new("&\#x02216;")
      end

      # ISOPUB    spades  
      def ext_inline_verb_spadesuit(label, content, visitor)
        label = label.to_s
        return nil unless /^spadesuit:(.*)$/ =~ label
        NormalText.new("&\#x02660;")
      end

      # alias ISOTECH radic 
      def ext_inline_verb_Sqrt(label, content, visitor)
        label = label.to_s
        return nil unless /^Sqrt:(.*)$/ =~ label
        NormalText.new("&\#x0221A;")
      end

      # alias ISOAMSR sqsub 
      def ext_inline_verb_sqsubset(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsubset:(.*)$/ =~ label
        NormalText.new("&\#x0228F;")
      end

      # alias ISOAMSR sqsube 
      def ext_inline_verb_sqsubseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsubseteq:(.*)$/ =~ label
        NormalText.new("&\#x02291;")
      end

      # alias ISOAMSR sqsup 
      def ext_inline_verb_sqsupset(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsupset:(.*)$/ =~ label
        NormalText.new("&\#x02290;")
      end

      # alias ISOAMSR sqsupe 
      def ext_inline_verb_sqsupseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsupseteq:(.*)$/ =~ label
        NormalText.new("&\#x02292;")
      end

      # alias for square 
      def ext_inline_verb_Square(label, content, visitor)
        label = label.to_s
        return nil unless /^Square:(.*)$/ =~ label
        NormalText.new("&\#x025A1;")
      end

      # alias ISOAMSB sqcap 
      def ext_inline_verb_SquareIntersection(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareIntersection:(.*)$/ =~ label
        NormalText.new("&\#x02293;")
      end

      # alias ISOAMSR sqsub 
      def ext_inline_verb_SquareSubset(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareSubset:(.*)$/ =~ label
        NormalText.new("&\#x0228F;")
      end

      # alias ISOAMSR sqsube 
      def ext_inline_verb_SquareSubsetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareSubsetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02291;")
      end

      # alias ISOAMSR sqsup 
      def ext_inline_verb_SquareSuperset(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareSuperset:(.*)$/ =~ label
        NormalText.new("&\#x02290;")
      end

      # alias ISOAMSR sqsupe 
      def ext_inline_verb_SquareSupersetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareSupersetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02292;")
      end

      # alias ISOAMSB sqcup 
      def ext_inline_verb_SquareUnion(label, content, visitor)
        label = label.to_s
        return nil unless /^SquareUnion:(.*)$/ =~ label
        NormalText.new("&\#x02294;")
      end

      # alias ISOAMSB sstarf 
      def ext_inline_verb_Star(label, content, visitor)
        label = label.to_s
        return nil unless /^Star:(.*)$/ =~ label
        NormalText.new("&\#x022C6;")
      end

      # alias ISOGRK3 epsi 
      def ext_inline_verb_straightepsilon(label, content, visitor)
        label = label.to_s
        return nil unless /^straightepsilon:(.*)$/ =~ label
        NormalText.new("&\#x003F5;")
      end

      # alias ISOGRK3 phi 
      def ext_inline_verb_straightphi(label, content, visitor)
        label = label.to_s
        return nil unless /^straightphi:(.*)$/ =~ label
        NormalText.new("&\#x003D5;")
      end

      # alias ISOAMSR Sub 
      def ext_inline_verb_Subset(label, content, visitor)
        label = label.to_s
        return nil unless /^Subset:(.*)$/ =~ label
        NormalText.new("&\#x022D0;")
      end

      # alias ISOTECH sub 
      def ext_inline_verb_subset(label, content, visitor)
        label = label.to_s
        return nil unless /^subset:(.*)$/ =~ label
        NormalText.new("&\#x02282;")
      end

      # alias ISOTECH sube 
      def ext_inline_verb_subseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^subseteq:(.*)$/ =~ label
        NormalText.new("&\#x02286;")
      end

      # alias ISOAMSR subE 
      def ext_inline_verb_subseteqq(label, content, visitor)
        label = label.to_s
        return nil unless /^subseteqq:(.*)$/ =~ label
        NormalText.new("&\#x02AC5;")
      end

      # alias ISOTECH sube 
      def ext_inline_verb_SubsetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SubsetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02286;")
      end

      # alias ISOAMSN subne 
      def ext_inline_verb_subsetneq(label, content, visitor)
        label = label.to_s
        return nil unless /^subsetneq:(.*)$/ =~ label
        NormalText.new("&\#x0228A;")
      end

      # alias ISOAMSN subnE 
      def ext_inline_verb_subsetneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^subsetneqq:(.*)$/ =~ label
        NormalText.new("&\#x02ACB;")
      end

      # alias ISOAMSR sc 
      def ext_inline_verb_succ(label, content, visitor)
        label = label.to_s
        return nil unless /^succ:(.*)$/ =~ label
        NormalText.new("&\#x0227B;")
      end

      # alias ISOAMSR scap 
      def ext_inline_verb_succapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^succapprox:(.*)$/ =~ label
        NormalText.new("&\#x02AB8;")
      end

      # alias ISOAMSR sccue 
      def ext_inline_verb_succcurlyeq(label, content, visitor)
        label = label.to_s
        return nil unless /^succcurlyeq:(.*)$/ =~ label
        NormalText.new("&\#x0227D;")
      end

      # alias ISOAMSR sc 
      def ext_inline_verb_Succeeds(label, content, visitor)
        label = label.to_s
        return nil unless /^Succeeds:(.*)$/ =~ label
        NormalText.new("&\#x0227B;")
      end

      # alias ISOAMSR sce 
      def ext_inline_verb_SucceedsEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SucceedsEqual:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;")
      end

      # alias ISOAMSR sccue 
      def ext_inline_verb_SucceedsSlantEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SucceedsSlantEqual:(.*)$/ =~ label
        NormalText.new("&\#x0227D;")
      end

      # alias ISOAMSR scsim 
      def ext_inline_verb_SucceedsTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^SucceedsTilde:(.*)$/ =~ label
        NormalText.new("&\#x0227F;")
      end

      # alias ISOAMSR sce 
      def ext_inline_verb_succeq(label, content, visitor)
        label = label.to_s
        return nil unless /^succeq:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;")
      end

      # alias ISOAMSN scnap 
      def ext_inline_verb_succnapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^succnapprox:(.*)$/ =~ label
        NormalText.new("&\#x02ABA;")
      end

      # alias ISOAMSN scnE 
      def ext_inline_verb_succneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^succneqq:(.*)$/ =~ label
        NormalText.new("&\#x02AB6;")
      end

      # alias ISOAMSN scnsim 
      def ext_inline_verb_succnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^succnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E9;")
      end

      # alias ISOAMSR scsim 
      def ext_inline_verb_succsim(label, content, visitor)
        label = label.to_s
        return nil unless /^succsim:(.*)$/ =~ label
        NormalText.new("&\#x0227F;")
      end

      # ISOTECH  ni 
      def ext_inline_verb_SuchThat(label, content, visitor)
        label = label.to_s
        return nil unless /^SuchThat:(.*)$/ =~ label
        NormalText.new("&\#x0220B;")
      end

      # alias ISOAMSB sum 
      def ext_inline_verb_Sum(label, content, visitor)
        label = label.to_s
        return nil unless /^Sum:(.*)$/ =~ label
        NormalText.new("&\#x02211;")
      end

      # alias ISOTECH sup 
      def ext_inline_verb_Superset(label, content, visitor)
        label = label.to_s
        return nil unless /^Superset:(.*)$/ =~ label
        NormalText.new("&\#x02283;")
      end

      # alias ISOTECH supe 
      def ext_inline_verb_SupersetEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^SupersetEqual:(.*)$/ =~ label
        NormalText.new("&\#x02287;")
      end

      # alias ISOAMSR Sup 
      def ext_inline_verb_Supset(label, content, visitor)
        label = label.to_s
        return nil unless /^Supset:(.*)$/ =~ label
        NormalText.new("&\#x022D1;")
      end

      # alias ISOTECH sup 
      def ext_inline_verb_supset(label, content, visitor)
        label = label.to_s
        return nil unless /^supset:(.*)$/ =~ label
        NormalText.new("&\#x02283;")
      end

      # alias ISOTECH supe 
      def ext_inline_verb_supseteq(label, content, visitor)
        label = label.to_s
        return nil unless /^supseteq:(.*)$/ =~ label
        NormalText.new("&\#x02287;")
      end

      # alias ISOAMSR supE 
      def ext_inline_verb_supseteqq(label, content, visitor)
        label = label.to_s
        return nil unless /^supseteqq:(.*)$/ =~ label
        NormalText.new("&\#x02AC6;")
      end

      # alias ISOAMSN supne 
      def ext_inline_verb_supsetneq(label, content, visitor)
        label = label.to_s
        return nil unless /^supsetneq:(.*)$/ =~ label
        NormalText.new("&\#x0228B;")
      end

      # alias ISOAMSN supnE 
      def ext_inline_verb_supsetneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^supsetneqq:(.*)$/ =~ label
        NormalText.new("&\#x02ACC;")
      end

      # alias ISOAMSA swarr 
      def ext_inline_verb_swarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^swarrow:(.*)$/ =~ label
        NormalText.new("&\#x02199;")
      end

      # alias ISOTECH there4 
      def ext_inline_verb_Therefore(label, content, visitor)
        label = label.to_s
        return nil unless /^Therefore:(.*)$/ =~ label
        NormalText.new("&\#x02234;")
      end

      # alias ISOTECH there4 
      def ext_inline_verb_therefore(label, content, visitor)
        label = label.to_s
        return nil unless /^therefore:(.*)$/ =~ label
        NormalText.new("&\#x02234;")
      end

      # ISOAMSR   thkap  
      def ext_inline_verb_thickapprox(label, content, visitor)
        label = label.to_s
        return nil unless /^thickapprox:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # ISOAMSR   thksim 
      def ext_inline_verb_thicksim(label, content, visitor)
        label = label.to_s
        return nil unless /^thicksim:(.*)$/ =~ label
        NormalText.new("&\#x0223C;")
      end

      # space of width 3/18 em alias ISOPUB thinsp 
      def ext_inline_verb_ThinSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^ThinSpace:(.*)$/ =~ label
        NormalText.new("&\#x02009;")
      end

      # alias ISOTECH sim 
      def ext_inline_verb_Tilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Tilde:(.*)$/ =~ label
        NormalText.new("&\#x0223C;")
      end

      # alias ISOTECH sime 
      def ext_inline_verb_TildeEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^TildeEqual:(.*)$/ =~ label
        NormalText.new("&\#x02243;")
      end

      # alias ISOTECH cong 
      def ext_inline_verb_TildeFullEqual(label, content, visitor)
        label = label.to_s
        return nil unless /^TildeFullEqual:(.*)$/ =~ label
        NormalText.new("&\#x02245;")
      end

      # alias ISOTECH ap 
      def ext_inline_verb_TildeTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^TildeTilde:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # alias ISOAMSA nesear 
      def ext_inline_verb_toea(label, content, visitor)
        label = label.to_s
        return nil unless /^toea:(.*)$/ =~ label
        NormalText.new("&\#x02928;")
      end

      # alias ISOAMSA seswar 
      def ext_inline_verb_tosa(label, content, visitor)
        label = label.to_s
        return nil unless /^tosa:(.*)$/ =~ label
        NormalText.new("&\#x02929;")
      end

      # alias ISOPUB utri 
      def ext_inline_verb_triangle(label, content, visitor)
        label = label.to_s
        return nil unless /^triangle:(.*)$/ =~ label
        NormalText.new("&\#x025B5;")
      end

      # alias ISOPUB dtri 
      def ext_inline_verb_triangledown(label, content, visitor)
        label = label.to_s
        return nil unless /^triangledown:(.*)$/ =~ label
        NormalText.new("&\#x025BF;")
      end

      # alias ISOPUB ltri 
      def ext_inline_verb_triangleleft(label, content, visitor)
        label = label.to_s
        return nil unless /^triangleleft:(.*)$/ =~ label
        NormalText.new("&\#x025C3;")
      end

      # alias ISOAMSR ltrie 
      def ext_inline_verb_trianglelefteq(label, content, visitor)
        label = label.to_s
        return nil unless /^trianglelefteq:(.*)$/ =~ label
        NormalText.new("&\#x022B4;")
      end

      # alias ISOAMSR trie 
      def ext_inline_verb_triangleq(label, content, visitor)
        label = label.to_s
        return nil unless /^triangleq:(.*)$/ =~ label
        NormalText.new("&\#x0225C;")
      end

      # alias ISOPUB rtri 
      def ext_inline_verb_triangleright(label, content, visitor)
        label = label.to_s
        return nil unless /^triangleright:(.*)$/ =~ label
        NormalText.new("&\#x025B9;")
      end

      # alias ISOAMSR rtrie 
      def ext_inline_verb_trianglerighteq(label, content, visitor)
        label = label.to_s
        return nil unless /^trianglerighteq:(.*)$/ =~ label
        NormalText.new("&\#x022B5;")
      end

      # alias ISOAMSA Larr 
      def ext_inline_verb_twoheadleftarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^twoheadleftarrow:(.*)$/ =~ label
        NormalText.new("&\#x0219E;")
      end

      # alias ISOAMSA Rarr 
      def ext_inline_verb_twoheadrightarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^twoheadrightarrow:(.*)$/ =~ label
        NormalText.new("&\#x021A0;")
      end

      # alias ISOAMSC ulcorn 
      def ext_inline_verb_ulcorner(label, content, visitor)
        label = label.to_s
        return nil unless /^ulcorner:(.*)$/ =~ label
        NormalText.new("&\#x0231C;")
      end

      # alias ISOAMSB xcup 
      def ext_inline_verb_Union(label, content, visitor)
        label = label.to_s
        return nil unless /^Union:(.*)$/ =~ label
        NormalText.new("&\#x022C3;")
      end

      # alias ISOAMSB uplus 
      def ext_inline_verb_UnionPlus(label, content, visitor)
        label = label.to_s
        return nil unless /^UnionPlus:(.*)$/ =~ label
        NormalText.new("&\#x0228E;")
      end

      # alias ISONUM uarr 
      def ext_inline_verb_UpArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpArrow:(.*)$/ =~ label
        NormalText.new("&\#x02191;")
      end

      # alias ISOAMSA uArr 
      def ext_inline_verb_Uparrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Uparrow:(.*)$/ =~ label
        NormalText.new("&\#x021D1;")
      end

      # alias ISONUM uarr 
      def ext_inline_verb_uparrow(label, content, visitor)
        label = label.to_s
        return nil unless /^uparrow:(.*)$/ =~ label
        NormalText.new("&\#x02191;")
      end

      # alias ISOAMSA udarr 
      def ext_inline_verb_UpArrowDownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpArrowDownArrow:(.*)$/ =~ label
        NormalText.new("&\#x021C5;")
      end

      # alias ISOAMSA varr 
      def ext_inline_verb_UpDownArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpDownArrow:(.*)$/ =~ label
        NormalText.new("&\#x02195;")
      end

      # alias ISOAMSA vArr 
      def ext_inline_verb_Updownarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^Updownarrow:(.*)$/ =~ label
        NormalText.new("&\#x021D5;")
      end

      # alias ISOAMSA varr 
      def ext_inline_verb_updownarrow(label, content, visitor)
        label = label.to_s
        return nil unless /^updownarrow:(.*)$/ =~ label
        NormalText.new("&\#x02195;")
      end

      # alias ISOAMSA udhar 
      def ext_inline_verb_UpEquilibrium(label, content, visitor)
        label = label.to_s
        return nil unless /^UpEquilibrium:(.*)$/ =~ label
        NormalText.new("&\#x0296E;")
      end

      # alias ISOAMSA uharl 
      def ext_inline_verb_upharpoonleft(label, content, visitor)
        label = label.to_s
        return nil unless /^upharpoonleft:(.*)$/ =~ label
        NormalText.new("&\#x021BF;")
      end

      # alias ISOAMSA uharr 
      def ext_inline_verb_upharpoonright(label, content, visitor)
        label = label.to_s
        return nil unless /^upharpoonright:(.*)$/ =~ label
        NormalText.new("&\#x021BE;")
      end

      # alias ISOAMSA nwarr 
      def ext_inline_verb_UpperLeftArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpperLeftArrow:(.*)$/ =~ label
        NormalText.new("&\#x02196;")
      end

      # alias ISOAMSA nearr 
      def ext_inline_verb_UpperRightArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpperRightArrow:(.*)$/ =~ label
        NormalText.new("&\#x02197;")
      end

      # alias ISOGRK3 upsi 
      def ext_inline_verb_upsilon(label, content, visitor)
        label = label.to_s
        return nil unless /^upsilon:(.*)$/ =~ label
        NormalText.new("&\#x003C5;")
      end

      # alias ISOTECH perp 
      def ext_inline_verb_UpTee(label, content, visitor)
        label = label.to_s
        return nil unless /^UpTee:(.*)$/ =~ label
        NormalText.new("&\#x022A5;")
      end

      # Alias mapstoup 
      def ext_inline_verb_UpTeeArrow(label, content, visitor)
        label = label.to_s
        return nil unless /^UpTeeArrow:(.*)$/ =~ label
        NormalText.new("&\#x021A5;")
      end

      # alias ISOAMSA uuarr 
      def ext_inline_verb_upuparrows(label, content, visitor)
        label = label.to_s
        return nil unless /^upuparrows:(.*)$/ =~ label
        NormalText.new("&\#x021C8;")
      end

      # alias ISOAMSC urcorn 
      def ext_inline_verb_urcorner(label, content, visitor)
        label = label.to_s
        return nil unless /^urcorner:(.*)$/ =~ label
        NormalText.new("&\#x0231D;")
      end

      # alias ISOGRK3 epsiv 
      def ext_inline_verb_varepsilon(label, content, visitor)
        label = label.to_s
        return nil unless /^varepsilon:(.*)$/ =~ label
        NormalText.new("&\#x003B5;")
      end

      # alias ISOGRK3 kappav 
      def ext_inline_verb_varkappa(label, content, visitor)
        label = label.to_s
        return nil unless /^varkappa:(.*)$/ =~ label
        NormalText.new("&\#x003F0;")
      end

      # alias ISOAMSO emptyv 
      def ext_inline_verb_varnothing(label, content, visitor)
        label = label.to_s
        return nil unless /^varnothing:(.*)$/ =~ label
        NormalText.new("&\#x02205;")
      end

      # alias ISOGRK3 phiv 
      def ext_inline_verb_varphi(label, content, visitor)
        label = label.to_s
        return nil unless /^varphi:(.*)$/ =~ label
        NormalText.new("&\#x003C6;")
      end

      # alias ISOGRK3 piv 
      def ext_inline_verb_varpi(label, content, visitor)
        label = label.to_s
        return nil unless /^varpi:(.*)$/ =~ label
        NormalText.new("&\#x003D6;")
      end

      # alias ISOAMSR vprop 
      def ext_inline_verb_varpropto(label, content, visitor)
        label = label.to_s
        return nil unless /^varpropto:(.*)$/ =~ label
        NormalText.new("&\#x0221D;")
      end

      # alias ISOGRK3 rhov 
      def ext_inline_verb_varrho(label, content, visitor)
        label = label.to_s
        return nil unless /^varrho:(.*)$/ =~ label
        NormalText.new("&\#x003F1;")
      end

      # alias ISOGRK3 sigmav 
      def ext_inline_verb_varsigma(label, content, visitor)
        label = label.to_s
        return nil unless /^varsigma:(.*)$/ =~ label
        NormalText.new("&\#x003C2;")
      end

      # alias ISOAMSN vsubne 
      def ext_inline_verb_varsubsetneq(label, content, visitor)
        label = label.to_s
        return nil unless /^varsubsetneq:(.*)$/ =~ label
        NormalText.new("&\#x0228A;&\#x0FE00;")
      end

      # alias ISOAMSN vsubnE 
      def ext_inline_verb_varsubsetneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^varsubsetneqq:(.*)$/ =~ label
        NormalText.new("&\#x02ACB;&\#x0FE00;")
      end

      # alias ISOAMSN vsupne 
      def ext_inline_verb_varsupsetneq(label, content, visitor)
        label = label.to_s
        return nil unless /^varsupsetneq:(.*)$/ =~ label
        NormalText.new("&\#x0228B;&\#x0FE00;")
      end

      # alias ISOAMSN vsupnE 
      def ext_inline_verb_varsupsetneqq(label, content, visitor)
        label = label.to_s
        return nil unless /^varsupsetneqq:(.*)$/ =~ label
        NormalText.new("&\#x02ACC;&\#x0FE00;")
      end

      # alias ISOGRK3 thetav 
      def ext_inline_verb_vartheta(label, content, visitor)
        label = label.to_s
        return nil unless /^vartheta:(.*)$/ =~ label
        NormalText.new("&\#x003D1;")
      end

      # alias ISOAMSR vltri 
      def ext_inline_verb_vartriangleleft(label, content, visitor)
        label = label.to_s
        return nil unless /^vartriangleleft:(.*)$/ =~ label
        NormalText.new("&\#x022B2;")
      end

      # alias ISOAMSR vrtri 
      def ext_inline_verb_vartriangleright(label, content, visitor)
        label = label.to_s
        return nil unless /^vartriangleright:(.*)$/ =~ label
        NormalText.new("&\#x022B3;")
      end

      # alias ISOAMSB xvee 
      def ext_inline_verb_Vee(label, content, visitor)
        label = label.to_s
        return nil unless /^Vee:(.*)$/ =~ label
        NormalText.new("&\#x022C1;")
      end

      # alias ISOTECH or 
      def ext_inline_verb_vee(label, content, visitor)
        label = label.to_s
        return nil unless /^vee:(.*)$/ =~ label
        NormalText.new("&\#x02228;")
      end

      # alias ISOTECH Verbar 
      def ext_inline_verb_Vert(label, content, visitor)
        label = label.to_s
        return nil unless /^Vert:(.*)$/ =~ label
        NormalText.new("&\#x02016;")
      end

      # alias ISONUM verbar 
      def ext_inline_verb_vert(label, content, visitor)
        label = label.to_s
        return nil unless /^vert:(.*)$/ =~ label
        NormalText.new("&\#x0007C;")
      end

      # alias ISOAMSR mid 
      def ext_inline_verb_VerticalBar(label, content, visitor)
        label = label.to_s
        return nil unless /^VerticalBar:(.*)$/ =~ label
        NormalText.new("&\#x02223;")
      end

      # alias ISOAMSB wreath 
      def ext_inline_verb_VerticalTilde(label, content, visitor)
        label = label.to_s
        return nil unless /^VerticalTilde:(.*)$/ =~ label
        NormalText.new("&\#x02240;")
      end

      # space of width 1/18 em alias ISOPUB hairsp 
      def ext_inline_verb_VeryThinSpace(label, content, visitor)
        label = label.to_s
        return nil unless /^VeryThinSpace:(.*)$/ =~ label
        NormalText.new("&\#x0200A;")
      end

      # alias ISOAMSB xwedge 
      def ext_inline_verb_Wedge(label, content, visitor)
        label = label.to_s
        return nil unless /^Wedge:(.*)$/ =~ label
        NormalText.new("&\#x022C0;")
      end

      # alias ISOTECH and 
      def ext_inline_verb_wedge(label, content, visitor)
        label = label.to_s
        return nil unless /^wedge:(.*)$/ =~ label
        NormalText.new("&\#x02227;")
      end

      # alias ISOAMSO weierp 
      def ext_inline_verb_wp(label, content, visitor)
        label = label.to_s
        return nil unless /^wp:(.*)$/ =~ label
        NormalText.new("&\#x02118;")
      end

      # alias ISOAMSB wreath 
      def ext_inline_verb_wr(label, content, visitor)
        label = label.to_s
        return nil unless /^wr:(.*)$/ =~ label
        NormalText.new("&\#x02240;")
      end

      # zee transform 
      def ext_inline_verb_zeetrf(label, content, visitor)
        label = label.to_s
        return nil unless /^zeetrf:(.*)$/ =~ label
        NormalText.new("&\#x02128;")
      end

    end
  end
end
