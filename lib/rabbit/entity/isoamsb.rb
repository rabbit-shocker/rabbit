require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamsb

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # most positive 
      def ext_inline_verb_ac(label, content, visitor)
        label = label.to_s
        return nil unless /^ac:(.*)$/ =~ label
        NormalText.new("&\#x0223E;")
      end

      # most positive, two lines below 
      def ext_inline_verb_acE(label, content, visitor)
        label = label.to_s
        return nil unless /^acE:(.*)$/ =~ label
        NormalText.new("&\#x0223E;&\#x00333;")
      end

      # /amalg B: amalgamation or coproduct 
      def ext_inline_verb_amalg(label, content, visitor)
        label = label.to_s
        return nil unless /^amalg:(.*)$/ =~ label
        NormalText.new("&\#x02A3F;")
      end

      # bar, vee 
      def ext_inline_verb_barvee(label, content, visitor)
        label = label.to_s
        return nil unless /^barvee:(.*)$/ =~ label
        NormalText.new("&\#x022BD;")
      end

      # /doublebarwedge B: log and, dbl bar above 
      def ext_inline_verb_Barwed(label, content, visitor)
        label = label.to_s
        return nil unless /^Barwed:(.*)$/ =~ label
        NormalText.new("&\#x02306;")
      end

      # /barwedge B: logical and, bar above 
      def ext_inline_verb_barwed(label, content, visitor)
        label = label.to_s
        return nil unless /^barwed:(.*)$/ =~ label
        NormalText.new("&\#x02305;")
      end

      # reverse solidus in square 
      def ext_inline_verb_bsolb(label, content, visitor)
        label = label.to_s
        return nil unless /^bsolb:(.*)$/ =~ label
        NormalText.new("&\#x029C5;")
      end

      # /Cap /doublecap B: dbl intersection 
      def ext_inline_verb_Cap(label, content, visitor)
        label = label.to_s
        return nil unless /^Cap:(.*)$/ =~ label
        NormalText.new("&\#x022D2;")
      end

      # intersection, and 
      def ext_inline_verb_capand(label, content, visitor)
        label = label.to_s
        return nil unless /^capand:(.*)$/ =~ label
        NormalText.new("&\#x02A44;")
      end

      # intersection, bar, union 
      def ext_inline_verb_capbrcup(label, content, visitor)
        label = label.to_s
        return nil unless /^capbrcup:(.*)$/ =~ label
        NormalText.new("&\#x02A49;")
      end

      # intersection, intersection, joined 
      def ext_inline_verb_capcap(label, content, visitor)
        label = label.to_s
        return nil unless /^capcap:(.*)$/ =~ label
        NormalText.new("&\#x02A4B;")
      end

      # intersection above union 
      def ext_inline_verb_capcup(label, content, visitor)
        label = label.to_s
        return nil unless /^capcup:(.*)$/ =~ label
        NormalText.new("&\#x02A47;")
      end

      # intersection, with dot 
      def ext_inline_verb_capdot(label, content, visitor)
        label = label.to_s
        return nil unless /^capdot:(.*)$/ =~ label
        NormalText.new("&\#x02A40;")
      end

      # intersection, serifs 
      def ext_inline_verb_caps(label, content, visitor)
        label = label.to_s
        return nil unless /^caps:(.*)$/ =~ label
        NormalText.new("&\#x02229;&\#x0FE00;")
      end

      # closed intersection, serifs 
      def ext_inline_verb_ccaps(label, content, visitor)
        label = label.to_s
        return nil unless /^ccaps:(.*)$/ =~ label
        NormalText.new("&\#x02A4D;")
      end

      # closed union, serifs 
      def ext_inline_verb_ccups(label, content, visitor)
        label = label.to_s
        return nil unless /^ccups:(.*)$/ =~ label
        NormalText.new("&\#x02A4C;")
      end

      # closed union, serifs, smash product 
      def ext_inline_verb_ccupssm(label, content, visitor)
        label = label.to_s
        return nil unless /^ccupssm:(.*)$/ =~ label
        NormalText.new("&\#x02A50;")
      end

      # /coprod L: coproduct operator 
      def ext_inline_verb_coprod(label, content, visitor)
        label = label.to_s
        return nil unless /^coprod:(.*)$/ =~ label
        NormalText.new("&\#x02210;")
      end

      # /Cup /doublecup B: dbl union 
      def ext_inline_verb_Cup(label, content, visitor)
        label = label.to_s
        return nil unless /^Cup:(.*)$/ =~ label
        NormalText.new("&\#x022D3;")
      end

      # union, bar, intersection 
      def ext_inline_verb_cupbrcap(label, content, visitor)
        label = label.to_s
        return nil unless /^cupbrcap:(.*)$/ =~ label
        NormalText.new("&\#x02A48;")
      end

      # union above intersection 
      def ext_inline_verb_cupcap(label, content, visitor)
        label = label.to_s
        return nil unless /^cupcap:(.*)$/ =~ label
        NormalText.new("&\#x02A46;")
      end

      # union, union, joined 
      def ext_inline_verb_cupcup(label, content, visitor)
        label = label.to_s
        return nil unless /^cupcup:(.*)$/ =~ label
        NormalText.new("&\#x02A4A;")
      end

      # union, with dot 
      def ext_inline_verb_cupdot(label, content, visitor)
        label = label.to_s
        return nil unless /^cupdot:(.*)$/ =~ label
        NormalText.new("&\#x0228D;")
      end

      # union, or 
      def ext_inline_verb_cupor(label, content, visitor)
        label = label.to_s
        return nil unless /^cupor:(.*)$/ =~ label
        NormalText.new("&\#x02A45;")
      end

      # union, serifs 
      def ext_inline_verb_cups(label, content, visitor)
        label = label.to_s
        return nil unless /^cups:(.*)$/ =~ label
        NormalText.new("&\#x0222A;&\#x0FE00;")
      end

      # /curlyvee B: curly logical or 
      def ext_inline_verb_cuvee(label, content, visitor)
        label = label.to_s
        return nil unless /^cuvee:(.*)$/ =~ label
        NormalText.new("&\#x022CE;")
      end

      # /curlywedge B: curly logical and 
      def ext_inline_verb_cuwed(label, content, visitor)
        label = label.to_s
        return nil unless /^cuwed:(.*)$/ =~ label
        NormalText.new("&\#x022CF;")
      end

      # /ddagger B: double dagger relation 
      def ext_inline_verb_Dagger(label, content, visitor)
        label = label.to_s
        return nil unless /^Dagger:(.*)$/ =~ label
        NormalText.new("&\#x02021;")
      end

      # /dagger B: dagger relation 
      def ext_inline_verb_dagger(label, content, visitor)
        label = label.to_s
        return nil unless /^dagger:(.*)$/ =~ label
        NormalText.new("&\#x02020;")
      end

      # /diamond B: open diamond 
      def ext_inline_verb_diam(label, content, visitor)
        label = label.to_s
        return nil unless /^diam:(.*)$/ =~ label
        NormalText.new("&\#x022C4;")
      end

      # /divideontimes B: division on times 
      def ext_inline_verb_divonx(label, content, visitor)
        label = label.to_s
        return nil unless /^divonx:(.*)$/ =~ label
        NormalText.new("&\#x022C7;")
      end

      # equal, plus 
      def ext_inline_verb_eplus(label, content, visitor)
        label = label.to_s
        return nil unless /^eplus:(.*)$/ =~ label
        NormalText.new("&\#x02A71;")
      end

      # hermitian conjugate matrix 
      def ext_inline_verb_hercon(label, content, visitor)
        label = label.to_s
        return nil unless /^hercon:(.*)$/ =~ label
        NormalText.new("&\#x022B9;")
      end

      # /intercal B: intercal 
      def ext_inline_verb_intcal(label, content, visitor)
        label = label.to_s
        return nil unless /^intcal:(.*)$/ =~ label
        NormalText.new("&\#x022BA;")
      end

      # /intprod 
      def ext_inline_verb_iprod(label, content, visitor)
        label = label.to_s
        return nil unless /^iprod:(.*)$/ =~ label
        NormalText.new("&\#x02A3C;")
      end

      # plus sign in left half circle 
      def ext_inline_verb_loplus(label, content, visitor)
        label = label.to_s
        return nil unless /^loplus:(.*)$/ =~ label
        NormalText.new("&\#x02A2D;")
      end

      # multiply sign in left half circle  
      def ext_inline_verb_lotimes(label, content, visitor)
        label = label.to_s
        return nil unless /^lotimes:(.*)$/ =~ label
        NormalText.new("&\#x02A34;")
      end

      # /leftthreetimes B: 
      def ext_inline_verb_lthree(label, content, visitor)
        label = label.to_s
        return nil unless /^lthree:(.*)$/ =~ label
        NormalText.new("&\#x022CB;")
      end

      # /ltimes B: times sign, left closed 
      def ext_inline_verb_ltimes(label, content, visitor)
        label = label.to_s
        return nil unless /^ltimes:(.*)$/ =~ label
        NormalText.new("&\#x022C9;")
      end

      # /ast B: asterisk 
      def ext_inline_verb_midast(label, content, visitor)
        label = label.to_s
        return nil unless /^midast:(.*)$/ =~ label
        NormalText.new("&\#x0002A;")
      end

      # /boxminus B: minus sign in box 
      def ext_inline_verb_minusb(label, content, visitor)
        label = label.to_s
        return nil unless /^minusb:(.*)$/ =~ label
        NormalText.new("&\#x0229F;")
      end

      # /dotminus B: minus sign, dot above 
      def ext_inline_verb_minusd(label, content, visitor)
        label = label.to_s
        return nil unless /^minusd:(.*)$/ =~ label
        NormalText.new("&\#x02238;")
      end

      # minus sign, dot below 
      def ext_inline_verb_minusdu(label, content, visitor)
        label = label.to_s
        return nil unless /^minusdu:(.*)$/ =~ label
        NormalText.new("&\#x02A2A;")
      end

      # bar, intersection 
      def ext_inline_verb_ncap(label, content, visitor)
        label = label.to_s
        return nil unless /^ncap:(.*)$/ =~ label
        NormalText.new("&\#x02A43;")
      end

      # bar, union 
      def ext_inline_verb_ncup(label, content, visitor)
        label = label.to_s
        return nil unless /^ncup:(.*)$/ =~ label
        NormalText.new("&\#x02A42;")
      end

      # /circledast B: asterisk in circle 
      def ext_inline_verb_oast(label, content, visitor)
        label = label.to_s
        return nil unless /^oast:(.*)$/ =~ label
        NormalText.new("&\#x0229B;")
      end

      # /circledcirc B: small circle in circle 
      def ext_inline_verb_ocir(label, content, visitor)
        label = label.to_s
        return nil unless /^ocir:(.*)$/ =~ label
        NormalText.new("&\#x0229A;")
      end

      # /circleddash B: hyphen in circle 
      def ext_inline_verb_odash(label, content, visitor)
        label = label.to_s
        return nil unless /^odash:(.*)$/ =~ label
        NormalText.new("&\#x0229D;")
      end

      # divide in circle 
      def ext_inline_verb_odiv(label, content, visitor)
        label = label.to_s
        return nil unless /^odiv:(.*)$/ =~ label
        NormalText.new("&\#x02A38;")
      end

      # /odot B: middle dot in circle 
      def ext_inline_verb_odot(label, content, visitor)
        label = label.to_s
        return nil unless /^odot:(.*)$/ =~ label
        NormalText.new("&\#x02299;")
      end

      # dot, solidus, dot in circle 
      def ext_inline_verb_odsold(label, content, visitor)
        label = label.to_s
        return nil unless /^odsold:(.*)$/ =~ label
        NormalText.new("&\#x029BC;")
      end

      # filled circle in circle 
      def ext_inline_verb_ofcir(label, content, visitor)
        label = label.to_s
        return nil unless /^ofcir:(.*)$/ =~ label
        NormalText.new("&\#x029BF;")
      end

      # greater-than in circle 
      def ext_inline_verb_ogt(label, content, visitor)
        label = label.to_s
        return nil unless /^ogt:(.*)$/ =~ label
        NormalText.new("&\#x029C1;")
      end

      # circle with horizontal bar 
      def ext_inline_verb_ohbar(label, content, visitor)
        label = label.to_s
        return nil unless /^ohbar:(.*)$/ =~ label
        NormalText.new("&\#x029B5;")
      end

      # large circle in circle 
      def ext_inline_verb_olcir(label, content, visitor)
        label = label.to_s
        return nil unless /^olcir:(.*)$/ =~ label
        NormalText.new("&\#x029BE;")
      end

      # less-than in circle 
      def ext_inline_verb_olt(label, content, visitor)
        label = label.to_s
        return nil unless /^olt:(.*)$/ =~ label
        NormalText.new("&\#x029C0;")
      end

      # vertical bar in circle 
      def ext_inline_verb_omid(label, content, visitor)
        label = label.to_s
        return nil unless /^omid:(.*)$/ =~ label
        NormalText.new("&\#x029B6;")
      end

      # /ominus B: minus sign in circle 
      def ext_inline_verb_ominus(label, content, visitor)
        label = label.to_s
        return nil unless /^ominus:(.*)$/ =~ label
        NormalText.new("&\#x02296;")
      end

      # parallel in circle 
      def ext_inline_verb_opar(label, content, visitor)
        label = label.to_s
        return nil unless /^opar:(.*)$/ =~ label
        NormalText.new("&\#x029B7;")
      end

      # perpendicular in circle 
      def ext_inline_verb_operp(label, content, visitor)
        label = label.to_s
        return nil unless /^operp:(.*)$/ =~ label
        NormalText.new("&\#x029B9;")
      end

      # /oplus B: plus sign in circle 
      def ext_inline_verb_oplus(label, content, visitor)
        label = label.to_s
        return nil unless /^oplus:(.*)$/ =~ label
        NormalText.new("&\#x02295;")
      end

      # /oslash B: solidus in circle 
      def ext_inline_verb_osol(label, content, visitor)
        label = label.to_s
        return nil unless /^osol:(.*)$/ =~ label
        NormalText.new("&\#x02298;")
      end

      # multiply sign in double circle 
      def ext_inline_verb_Otimes(label, content, visitor)
        label = label.to_s
        return nil unless /^Otimes:(.*)$/ =~ label
        NormalText.new("&\#x02A37;")
      end

      # /otimes B: multiply sign in circle 
      def ext_inline_verb_otimes(label, content, visitor)
        label = label.to_s
        return nil unless /^otimes:(.*)$/ =~ label
        NormalText.new("&\#x02297;")
      end

      # multiply sign in circle, circumflex accent 
      def ext_inline_verb_otimesas(label, content, visitor)
        label = label.to_s
        return nil unless /^otimesas:(.*)$/ =~ label
        NormalText.new("&\#x02A36;")
      end

      # circle with vertical bar 
      def ext_inline_verb_ovbar(label, content, visitor)
        label = label.to_s
        return nil unless /^ovbar:(.*)$/ =~ label
        NormalText.new("&\#x0233D;")
      end

      # plus, circumflex accent above 
      def ext_inline_verb_plusacir(label, content, visitor)
        label = label.to_s
        return nil unless /^plusacir:(.*)$/ =~ label
        NormalText.new("&\#x02A23;")
      end

      # /boxplus B: plus sign in box 
      def ext_inline_verb_plusb(label, content, visitor)
        label = label.to_s
        return nil unless /^plusb:(.*)$/ =~ label
        NormalText.new("&\#x0229E;")
      end

      # plus, small circle above 
      def ext_inline_verb_pluscir(label, content, visitor)
        label = label.to_s
        return nil unless /^pluscir:(.*)$/ =~ label
        NormalText.new("&\#x02A22;")
      end

      # /dotplus B: plus sign, dot above 
      def ext_inline_verb_plusdo(label, content, visitor)
        label = label.to_s
        return nil unless /^plusdo:(.*)$/ =~ label
        NormalText.new("&\#x02214;")
      end

      # plus sign, dot below 
      def ext_inline_verb_plusdu(label, content, visitor)
        label = label.to_s
        return nil unless /^plusdu:(.*)$/ =~ label
        NormalText.new("&\#x02A25;")
      end

      # plus, equals 
      def ext_inline_verb_pluse(label, content, visitor)
        label = label.to_s
        return nil unless /^pluse:(.*)$/ =~ label
        NormalText.new("&\#x02A72;")
      end

      # plus, similar below 
      def ext_inline_verb_plussim(label, content, visitor)
        label = label.to_s
        return nil unless /^plussim:(.*)$/ =~ label
        NormalText.new("&\#x02A26;")
      end

      # plus, two; Nim-addition 
      def ext_inline_verb_plustwo(label, content, visitor)
        label = label.to_s
        return nil unless /^plustwo:(.*)$/ =~ label
        NormalText.new("&\#x02A27;")
      end

      # /prod L: product operator 
      def ext_inline_verb_prod(label, content, visitor)
        label = label.to_s
        return nil unless /^prod:(.*)$/ =~ label
        NormalText.new("&\#x0220F;")
      end

      # reverse most positive, line below 
      def ext_inline_verb_race(label, content, visitor)
        label = label.to_s
        return nil unless /^race:(.*)$/ =~ label
        NormalText.new("&\#x029DA;")
      end

      # plus sign in right half circle 
      def ext_inline_verb_roplus(label, content, visitor)
        label = label.to_s
        return nil unless /^roplus:(.*)$/ =~ label
        NormalText.new("&\#x02A2E;")
      end

      # multiply sign in right half circle 
      def ext_inline_verb_rotimes(label, content, visitor)
        label = label.to_s
        return nil unless /^rotimes:(.*)$/ =~ label
        NormalText.new("&\#x02A35;")
      end

      # /rightthreetimes B: 
      def ext_inline_verb_rthree(label, content, visitor)
        label = label.to_s
        return nil unless /^rthree:(.*)$/ =~ label
        NormalText.new("&\#x022CC;")
      end

      # /rtimes B: times sign, right closed 
      def ext_inline_verb_rtimes(label, content, visitor)
        label = label.to_s
        return nil unless /^rtimes:(.*)$/ =~ label
        NormalText.new("&\#x022CA;")
      end

      # /cdot B: small middle dot 
      def ext_inline_verb_sdot(label, content, visitor)
        label = label.to_s
        return nil unless /^sdot:(.*)$/ =~ label
        NormalText.new("&\#x022C5;")
      end

      # /dotsquare /boxdot B: small dot in box 
      def ext_inline_verb_sdotb(label, content, visitor)
        label = label.to_s
        return nil unless /^sdotb:(.*)$/ =~ label
        NormalText.new("&\#x022A1;")
      end

      # /setminus B: reverse solidus 
      def ext_inline_verb_setmn(label, content, visitor)
        label = label.to_s
        return nil unless /^setmn:(.*)$/ =~ label
        NormalText.new("&\#x02216;")
      end

      # plus, similar above 
      def ext_inline_verb_simplus(label, content, visitor)
        label = label.to_s
        return nil unless /^simplus:(.*)$/ =~ label
        NormalText.new("&\#x02A24;")
      end

      # smash product 
      def ext_inline_verb_smashp(label, content, visitor)
        label = label.to_s
        return nil unless /^smashp:(.*)$/ =~ label
        NormalText.new("&\#x02A33;")
      end

      # solidus in square 
      def ext_inline_verb_solb(label, content, visitor)
        label = label.to_s
        return nil unless /^solb:(.*)$/ =~ label
        NormalText.new("&\#x029C4;")
      end

      # /sqcap B: square intersection 
      def ext_inline_verb_sqcap(label, content, visitor)
        label = label.to_s
        return nil unless /^sqcap:(.*)$/ =~ label
        NormalText.new("&\#x02293;")
      end

      # square intersection, serifs 
      def ext_inline_verb_sqcaps(label, content, visitor)
        label = label.to_s
        return nil unless /^sqcaps:(.*)$/ =~ label
        NormalText.new("&\#x02293;&\#x0FE00;")
      end

      # /sqcup B: square union 
      def ext_inline_verb_sqcup(label, content, visitor)
        label = label.to_s
        return nil unless /^sqcup:(.*)$/ =~ label
        NormalText.new("&\#x02294;")
      end

      # square union, serifs 
      def ext_inline_verb_sqcups(label, content, visitor)
        label = label.to_s
        return nil unless /^sqcups:(.*)$/ =~ label
        NormalText.new("&\#x02294;&\#x0FE00;")
      end

      # /smallsetminus B: sm reverse solidus 
      def ext_inline_verb_ssetmn(label, content, visitor)
        label = label.to_s
        return nil unless /^ssetmn:(.*)$/ =~ label
        NormalText.new("&\#x02216;")
      end

      # /star B: small star, filled 
      def ext_inline_verb_sstarf(label, content, visitor)
        label = label.to_s
        return nil unless /^sstarf:(.*)$/ =~ label
        NormalText.new("&\#x022C6;")
      end

      # subset, with dot 
      def ext_inline_verb_subdot(label, content, visitor)
        label = label.to_s
        return nil unless /^subdot:(.*)$/ =~ label
        NormalText.new("&\#x02ABD;")
      end

      # /sum L: summation operator 
      def ext_inline_verb_sum(label, content, visitor)
        label = label.to_s
        return nil unless /^sum:(.*)$/ =~ label
        NormalText.new("&\#x02211;")
      end

      # superset, with dot 
      def ext_inline_verb_supdot(label, content, visitor)
        label = label.to_s
        return nil unless /^supdot:(.*)$/ =~ label
        NormalText.new("&\#x02ABE;")
      end

      # /boxtimes B: multiply sign in box 
      def ext_inline_verb_timesb(label, content, visitor)
        label = label.to_s
        return nil unless /^timesb:(.*)$/ =~ label
        NormalText.new("&\#x022A0;")
      end

      # multiply sign, bar below 
      def ext_inline_verb_timesbar(label, content, visitor)
        label = label.to_s
        return nil unless /^timesbar:(.*)$/ =~ label
        NormalText.new("&\#x02A31;")
      end

      # times, dot 
      def ext_inline_verb_timesd(label, content, visitor)
        label = label.to_s
        return nil unless /^timesd:(.*)$/ =~ label
        NormalText.new("&\#x02A30;")
      end

      # dot in triangle 
      def ext_inline_verb_tridot(label, content, visitor)
        label = label.to_s
        return nil unless /^tridot:(.*)$/ =~ label
        NormalText.new("&\#x025EC;")
      end

      # minus in triangle 
      def ext_inline_verb_triminus(label, content, visitor)
        label = label.to_s
        return nil unless /^triminus:(.*)$/ =~ label
        NormalText.new("&\#x02A3A;")
      end

      # plus in triangle 
      def ext_inline_verb_triplus(label, content, visitor)
        label = label.to_s
        return nil unless /^triplus:(.*)$/ =~ label
        NormalText.new("&\#x02A39;")
      end

      # triangle, serifs at bottom 
      def ext_inline_verb_trisb(label, content, visitor)
        label = label.to_s
        return nil unless /^trisb:(.*)$/ =~ label
        NormalText.new("&\#x029CD;")
      end

      # multiply in triangle 
      def ext_inline_verb_tritime(label, content, visitor)
        label = label.to_s
        return nil unless /^tritime:(.*)$/ =~ label
        NormalText.new("&\#x02A3B;")
      end

      # /uplus B: plus sign in union 
      def ext_inline_verb_uplus(label, content, visitor)
        label = label.to_s
        return nil unless /^uplus:(.*)$/ =~ label
        NormalText.new("&\#x0228E;")
      end

      # /veebar B: logical or, bar below 
      def ext_inline_verb_veebar(label, content, visitor)
        label = label.to_s
        return nil unless /^veebar:(.*)$/ =~ label
        NormalText.new("&\#x022BB;")
      end

      # wedge, bar below 
      def ext_inline_verb_wedbar(label, content, visitor)
        label = label.to_s
        return nil unless /^wedbar:(.*)$/ =~ label
        NormalText.new("&\#x02A5F;")
      end

      # /wr B: wreath product 
      def ext_inline_verb_wreath(label, content, visitor)
        label = label.to_s
        return nil unless /^wreath:(.*)$/ =~ label
        NormalText.new("&\#x02240;")
      end

      # /bigcap L: intersection operator 
      def ext_inline_verb_xcap(label, content, visitor)
        label = label.to_s
        return nil unless /^xcap:(.*)$/ =~ label
        NormalText.new("&\#x022C2;")
      end

      # /bigcirc B: large circle 
      def ext_inline_verb_xcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^xcirc:(.*)$/ =~ label
        NormalText.new("&\#x025EF;")
      end

      # /bigcup L: union operator 
      def ext_inline_verb_xcup(label, content, visitor)
        label = label.to_s
        return nil unless /^xcup:(.*)$/ =~ label
        NormalText.new("&\#x022C3;")
      end

      # /bigtriangledown B: big dn tri, open 
      def ext_inline_verb_xdtri(label, content, visitor)
        label = label.to_s
        return nil unless /^xdtri:(.*)$/ =~ label
        NormalText.new("&\#x025BD;")
      end

      # /bigodot L: circle dot operator 
      def ext_inline_verb_xodot(label, content, visitor)
        label = label.to_s
        return nil unless /^xodot:(.*)$/ =~ label
        NormalText.new("&\#x02A00;")
      end

      # /bigoplus L: circle plus operator 
      def ext_inline_verb_xoplus(label, content, visitor)
        label = label.to_s
        return nil unless /^xoplus:(.*)$/ =~ label
        NormalText.new("&\#x02A01;")
      end

      # /bigotimes L: circle times operator 
      def ext_inline_verb_xotime(label, content, visitor)
        label = label.to_s
        return nil unless /^xotime:(.*)$/ =~ label
        NormalText.new("&\#x02A02;")
      end

      # /bigsqcup L: square union operator 
      def ext_inline_verb_xsqcup(label, content, visitor)
        label = label.to_s
        return nil unless /^xsqcup:(.*)$/ =~ label
        NormalText.new("&\#x02A06;")
      end

      # /biguplus L: 
      def ext_inline_verb_xuplus(label, content, visitor)
        label = label.to_s
        return nil unless /^xuplus:(.*)$/ =~ label
        NormalText.new("&\#x02A04;")
      end

      # /bigtriangleup B: big up tri, open 
      def ext_inline_verb_xutri(label, content, visitor)
        label = label.to_s
        return nil unless /^xutri:(.*)$/ =~ label
        NormalText.new("&\#x025B3;")
      end

      # /bigvee L: logical and operator 
      def ext_inline_verb_xvee(label, content, visitor)
        label = label.to_s
        return nil unless /^xvee:(.*)$/ =~ label
        NormalText.new("&\#x022C1;")
      end

      # /bigwedge L: logical or operator 
      def ext_inline_verb_xwedge(label, content, visitor)
        label = label.to_s
        return nil unless /^xwedge:(.*)$/ =~ label
        NormalText.new("&\#x022C0;")
      end

    end
  end
end
