require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamsr

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # approximately equal or equal to 
      def ext_inline_verb_apE(label, content, visitor)
        label = label.to_s
        return nil unless /^apE:(.*)$/ =~ label
        NormalText.new("&\#x02A70;")
      end

      # /approxeq R: approximate, equals 
      def ext_inline_verb_ape(label, content, visitor)
        label = label.to_s
        return nil unless /^ape:(.*)$/ =~ label
        NormalText.new("&\#x0224A;")
      end

      # approximately identical to 
      def ext_inline_verb_apid(label, content, visitor)
        label = label.to_s
        return nil unless /^apid:(.*)$/ =~ label
        NormalText.new("&\#x0224B;")
      end

      # /asymp R: asymptotically equal to 
      def ext_inline_verb_asymp(label, content, visitor)
        label = label.to_s
        return nil unless /^asymp:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # vert, dbl bar (over) 
      def ext_inline_verb_Barv(label, content, visitor)
        label = label.to_s
        return nil unless /^Barv:(.*)$/ =~ label
        NormalText.new("&\#x02AE7;")
      end

      # /backcong R: reverse congruent 
      def ext_inline_verb_bcong(label, content, visitor)
        label = label.to_s
        return nil unless /^bcong:(.*)$/ =~ label
        NormalText.new("&\#x0224C;")
      end

      # /backepsilon R: such that 
      def ext_inline_verb_bepsi(label, content, visitor)
        label = label.to_s
        return nil unless /^bepsi:(.*)$/ =~ label
        NormalText.new("&\#x003F6;")
      end

      # /bowtie R: 
      def ext_inline_verb_bowtie(label, content, visitor)
        label = label.to_s
        return nil unless /^bowtie:(.*)$/ =~ label
        NormalText.new("&\#x022C8;")
      end

      # /backsim R: reverse similar 
      def ext_inline_verb_bsim(label, content, visitor)
        label = label.to_s
        return nil unless /^bsim:(.*)$/ =~ label
        NormalText.new("&\#x0223D;")
      end

      # /backsimeq R: reverse similar, eq 
      def ext_inline_verb_bsime(label, content, visitor)
        label = label.to_s
        return nil unless /^bsime:(.*)$/ =~ label
        NormalText.new("&\#x022CD;")
      end

      # reverse solidus, subset 
      def ext_inline_verb_bsolhsub(label, content, visitor)
        label = label.to_s
        return nil unless /^bsolhsub:(.*)$/ =~ label
        NormalText.new("&\#x0005C;&\#x02282;")
      end

      # /Bumpeq R: bumpy equals 
      def ext_inline_verb_bump(label, content, visitor)
        label = label.to_s
        return nil unless /^bump:(.*)$/ =~ label
        NormalText.new("&\#x0224E;")
      end

      # bump, equals 
      def ext_inline_verb_bumpE(label, content, visitor)
        label = label.to_s
        return nil unless /^bumpE:(.*)$/ =~ label
        NormalText.new("&\#x02AAE;")
      end

      # /bumpeq R: bumpy equals, equals 
      def ext_inline_verb_bumpe(label, content, visitor)
        label = label.to_s
        return nil unless /^bumpe:(.*)$/ =~ label
        NormalText.new("&\#x0224F;")
      end

      # /circeq R: circle, equals 
      def ext_inline_verb_cire(label, content, visitor)
        label = label.to_s
        return nil unless /^cire:(.*)$/ =~ label
        NormalText.new("&\#x02257;")
      end

      # /Colon, two colons 
      def ext_inline_verb_Colon(label, content, visitor)
        label = label.to_s
        return nil unless /^Colon:(.*)$/ =~ label
        NormalText.new("&\#x02237;")
      end

      # double colon, equals 
      def ext_inline_verb_Colone(label, content, visitor)
        label = label.to_s
        return nil unless /^Colone:(.*)$/ =~ label
        NormalText.new("&\#x02A74;")
      end

      # /coloneq R: colon, equals 
      def ext_inline_verb_colone(label, content, visitor)
        label = label.to_s
        return nil unless /^colone:(.*)$/ =~ label
        NormalText.new("&\#x02254;")
      end

      # congruent, dot 
      def ext_inline_verb_congdot(label, content, visitor)
        label = label.to_s
        return nil unless /^congdot:(.*)$/ =~ label
        NormalText.new("&\#x02A6D;")
      end

      # subset, closed 
      def ext_inline_verb_csub(label, content, visitor)
        label = label.to_s
        return nil unless /^csub:(.*)$/ =~ label
        NormalText.new("&\#x02ACF;")
      end

      # subset, closed, equals 
      def ext_inline_verb_csube(label, content, visitor)
        label = label.to_s
        return nil unless /^csube:(.*)$/ =~ label
        NormalText.new("&\#x02AD1;")
      end

      # superset, closed 
      def ext_inline_verb_csup(label, content, visitor)
        label = label.to_s
        return nil unless /^csup:(.*)$/ =~ label
        NormalText.new("&\#x02AD0;")
      end

      # superset, closed, equals 
      def ext_inline_verb_csupe(label, content, visitor)
        label = label.to_s
        return nil unless /^csupe:(.*)$/ =~ label
        NormalText.new("&\#x02AD2;")
      end

      # /curlyeqprec R: curly eq, precedes 
      def ext_inline_verb_cuepr(label, content, visitor)
        label = label.to_s
        return nil unless /^cuepr:(.*)$/ =~ label
        NormalText.new("&\#x022DE;")
      end

      # /curlyeqsucc R: curly eq, succeeds 
      def ext_inline_verb_cuesc(label, content, visitor)
        label = label.to_s
        return nil unless /^cuesc:(.*)$/ =~ label
        NormalText.new("&\#x022DF;")
      end

      # dbl dash, vertical 
      def ext_inline_verb_Dashv(label, content, visitor)
        label = label.to_s
        return nil unless /^Dashv:(.*)$/ =~ label
        NormalText.new("&\#x02AE4;")
      end

      # /dashv R: dash, vertical 
      def ext_inline_verb_dashv(label, content, visitor)
        label = label.to_s
        return nil unless /^dashv:(.*)$/ =~ label
        NormalText.new("&\#x022A3;")
      end

      # equal, asterisk above 
      def ext_inline_verb_easter(label, content, visitor)
        label = label.to_s
        return nil unless /^easter:(.*)$/ =~ label
        NormalText.new("&\#x02A6E;")
      end

      # /eqcirc R: circle on equals sign 
      def ext_inline_verb_ecir(label, content, visitor)
        label = label.to_s
        return nil unless /^ecir:(.*)$/ =~ label
        NormalText.new("&\#x02256;")
      end

      # /eqcolon R: equals, colon 
      def ext_inline_verb_ecolon(label, content, visitor)
        label = label.to_s
        return nil unless /^ecolon:(.*)$/ =~ label
        NormalText.new("&\#x02255;")
      end

      # /ddotseq R: equal with four dots 
      def ext_inline_verb_eDDot(label, content, visitor)
        label = label.to_s
        return nil unless /^eDDot:(.*)$/ =~ label
        NormalText.new("&\#x02A77;")
      end

      # /doteqdot /Doteq R: eq, even dots 
      def ext_inline_verb_eDot(label, content, visitor)
        label = label.to_s
        return nil unless /^eDot:(.*)$/ =~ label
        NormalText.new("&\#x02251;")
      end

      # /fallingdotseq R: eq, falling dots 
      def ext_inline_verb_efDot(label, content, visitor)
        label = label.to_s
        return nil unless /^efDot:(.*)$/ =~ label
        NormalText.new("&\#x02252;")
      end

      # equal-or-greater 
      def ext_inline_verb_eg(label, content, visitor)
        label = label.to_s
        return nil unless /^eg:(.*)$/ =~ label
        NormalText.new("&\#x02A9A;")
      end

      # /eqslantgtr R: equal-or-gtr, slanted 
      def ext_inline_verb_egs(label, content, visitor)
        label = label.to_s
        return nil unless /^egs:(.*)$/ =~ label
        NormalText.new("&\#x02A96;")
      end

      # equal-or-greater, slanted, dot inside 
      def ext_inline_verb_egsdot(label, content, visitor)
        label = label.to_s
        return nil unless /^egsdot:(.*)$/ =~ label
        NormalText.new("&\#x02A98;")
      end

      # equal-or-less 
      def ext_inline_verb_el(label, content, visitor)
        label = label.to_s
        return nil unless /^el:(.*)$/ =~ label
        NormalText.new("&\#x02A99;")
      end

      # /eqslantless R: eq-or-less, slanted 
      def ext_inline_verb_els(label, content, visitor)
        label = label.to_s
        return nil unless /^els:(.*)$/ =~ label
        NormalText.new("&\#x02A95;")
      end

      # equal-or-less, slanted, dot inside 
      def ext_inline_verb_elsdot(label, content, visitor)
        label = label.to_s
        return nil unless /^elsdot:(.*)$/ =~ label
        NormalText.new("&\#x02A97;")
      end

      # /questeq R: equal with questionmark 
      def ext_inline_verb_equest(label, content, visitor)
        label = label.to_s
        return nil unless /^equest:(.*)$/ =~ label
        NormalText.new("&\#x0225F;")
      end

      # equivalent, four dots above 
      def ext_inline_verb_equivDD(label, content, visitor)
        label = label.to_s
        return nil unless /^equivDD:(.*)$/ =~ label
        NormalText.new("&\#x02A78;")
      end

      # /risingdotseq R: eq, rising dots 
      def ext_inline_verb_erDot(label, content, visitor)
        label = label.to_s
        return nil unless /^erDot:(.*)$/ =~ label
        NormalText.new("&\#x02253;")
      end

      # /doteq R: equals, single dot above 
      def ext_inline_verb_esdot(label, content, visitor)
        label = label.to_s
        return nil unless /^esdot:(.*)$/ =~ label
        NormalText.new("&\#x02250;")
      end

      # equal, similar 
      def ext_inline_verb_Esim(label, content, visitor)
        label = label.to_s
        return nil unless /^Esim:(.*)$/ =~ label
        NormalText.new("&\#x02A73;")
      end

      # /esim R: equals, similar 
      def ext_inline_verb_esim(label, content, visitor)
        label = label.to_s
        return nil unless /^esim:(.*)$/ =~ label
        NormalText.new("&\#x02242;")
      end

      # /pitchfork R: pitchfork 
      def ext_inline_verb_fork(label, content, visitor)
        label = label.to_s
        return nil unless /^fork:(.*)$/ =~ label
        NormalText.new("&\#x022D4;")
      end

      # fork, variant 
      def ext_inline_verb_forkv(label, content, visitor)
        label = label.to_s
        return nil unless /^forkv:(.*)$/ =~ label
        NormalText.new("&\#x02AD9;")
      end

      # /frown R: down curve 
      def ext_inline_verb_frown(label, content, visitor)
        label = label.to_s
        return nil unless /^frown:(.*)$/ =~ label
        NormalText.new("&\#x02322;")
      end

      # /gtrapprox R: greater, approximate 
      def ext_inline_verb_gap(label, content, visitor)
        label = label.to_s
        return nil unless /^gap:(.*)$/ =~ label
        NormalText.new("&\#x02A86;")
      end

      # /geqq R: greater, double equals 
      def ext_inline_verb_gE(label, content, visitor)
        label = label.to_s
        return nil unless /^gE:(.*)$/ =~ label
        NormalText.new("&\#x02267;")
      end

      # /gtreqqless R: gt, dbl equals, less 
      def ext_inline_verb_gEl(label, content, visitor)
        label = label.to_s
        return nil unless /^gEl:(.*)$/ =~ label
        NormalText.new("&\#x02A8C;")
      end

      # /gtreqless R: greater, equals, less 
      def ext_inline_verb_gel(label, content, visitor)
        label = label.to_s
        return nil unless /^gel:(.*)$/ =~ label
        NormalText.new("&\#x022DB;")
      end

      # /geqslant R: gt-or-equal, slanted 
      def ext_inline_verb_ges(label, content, visitor)
        label = label.to_s
        return nil unless /^ges:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;")
      end

      # greater than, closed by curve, equal, slanted 
      def ext_inline_verb_gescc(label, content, visitor)
        label = label.to_s
        return nil unless /^gescc:(.*)$/ =~ label
        NormalText.new("&\#x02AA9;")
      end

      # greater-than-or-equal, slanted, dot inside 
      def ext_inline_verb_gesdot(label, content, visitor)
        label = label.to_s
        return nil unless /^gesdot:(.*)$/ =~ label
        NormalText.new("&\#x02A80;")
      end

      # greater-than-or-equal, slanted, dot above 
      def ext_inline_verb_gesdoto(label, content, visitor)
        label = label.to_s
        return nil unless /^gesdoto:(.*)$/ =~ label
        NormalText.new("&\#x02A82;")
      end

      # greater-than-or-equal, slanted, dot above left 
      def ext_inline_verb_gesdotol(label, content, visitor)
        label = label.to_s
        return nil unless /^gesdotol:(.*)$/ =~ label
        NormalText.new("&\#x02A84;")
      end

      # greater, equal, slanted, less 
      def ext_inline_verb_gesl(label, content, visitor)
        label = label.to_s
        return nil unless /^gesl:(.*)$/ =~ label
        NormalText.new("&\#x022DB;&\#x0FE00;")
      end

      # greater, equal, slanted, less, equal, slanted 
      def ext_inline_verb_gesles(label, content, visitor)
        label = label.to_s
        return nil unless /^gesles:(.*)$/ =~ label
        NormalText.new("&\#x02A94;")
      end

      # /ggg /Gg /gggtr R: triple gtr-than 
      def ext_inline_verb_Gg(label, content, visitor)
        label = label.to_s
        return nil unless /^Gg:(.*)$/ =~ label
        NormalText.new("&\#x022D9;")
      end

      # /gtrless R: greater, less 
      def ext_inline_verb_gl(label, content, visitor)
        label = label.to_s
        return nil unless /^gl:(.*)$/ =~ label
        NormalText.new("&\#x02277;")
      end

      # greater, less, apart 
      def ext_inline_verb_gla(label, content, visitor)
        label = label.to_s
        return nil unless /^gla:(.*)$/ =~ label
        NormalText.new("&\#x02AA5;")
      end

      # greater, less, equal 
      def ext_inline_verb_glE(label, content, visitor)
        label = label.to_s
        return nil unless /^glE:(.*)$/ =~ label
        NormalText.new("&\#x02A92;")
      end

      # greater, less, overlapping 
      def ext_inline_verb_glj(label, content, visitor)
        label = label.to_s
        return nil unless /^glj:(.*)$/ =~ label
        NormalText.new("&\#x02AA4;")
      end

      # /gtrsim R: greater, similar 
      def ext_inline_verb_gsim(label, content, visitor)
        label = label.to_s
        return nil unless /^gsim:(.*)$/ =~ label
        NormalText.new("&\#x02273;")
      end

      # greater, similar, equal 
      def ext_inline_verb_gsime(label, content, visitor)
        label = label.to_s
        return nil unless /^gsime:(.*)$/ =~ label
        NormalText.new("&\#x02A8E;")
      end

      # greater, similar, less 
      def ext_inline_verb_gsiml(label, content, visitor)
        label = label.to_s
        return nil unless /^gsiml:(.*)$/ =~ label
        NormalText.new("&\#x02A90;")
      end

      # /gg R: dbl greater-than sign 
      def ext_inline_verb_Gt(label, content, visitor)
        label = label.to_s
        return nil unless /^Gt:(.*)$/ =~ label
        NormalText.new("&\#x0226B;")
      end

      # greater than, closed by curve 
      def ext_inline_verb_gtcc(label, content, visitor)
        label = label.to_s
        return nil unless /^gtcc:(.*)$/ =~ label
        NormalText.new("&\#x02AA7;")
      end

      # greater than, circle inside 
      def ext_inline_verb_gtcir(label, content, visitor)
        label = label.to_s
        return nil unless /^gtcir:(.*)$/ =~ label
        NormalText.new("&\#x02A7A;")
      end

      # /gtrdot R: greater than, with dot 
      def ext_inline_verb_gtdot(label, content, visitor)
        label = label.to_s
        return nil unless /^gtdot:(.*)$/ =~ label
        NormalText.new("&\#x022D7;")
      end

      # greater than, questionmark above 
      def ext_inline_verb_gtquest(label, content, visitor)
        label = label.to_s
        return nil unless /^gtquest:(.*)$/ =~ label
        NormalText.new("&\#x02A7C;")
      end

      # greater than, right arrow 
      def ext_inline_verb_gtrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^gtrarr:(.*)$/ =~ label
        NormalText.new("&\#x02978;")
      end

      # homothetic 
      def ext_inline_verb_homtht(label, content, visitor)
        label = label.to_s
        return nil unless /^homtht:(.*)$/ =~ label
        NormalText.new("&\#x0223B;")
      end

      # /lessapprox R: less, approximate 
      def ext_inline_verb_lap(label, content, visitor)
        label = label.to_s
        return nil unless /^lap:(.*)$/ =~ label
        NormalText.new("&\#x02A85;")
      end

      # larger than 
      def ext_inline_verb_lat(label, content, visitor)
        label = label.to_s
        return nil unless /^lat:(.*)$/ =~ label
        NormalText.new("&\#x02AAB;")
      end

      # larger than or equal 
      def ext_inline_verb_late(label, content, visitor)
        label = label.to_s
        return nil unless /^late:(.*)$/ =~ label
        NormalText.new("&\#x02AAD;")
      end

      # larger than or equal, slanted 
      def ext_inline_verb_lates(label, content, visitor)
        label = label.to_s
        return nil unless /^lates:(.*)$/ =~ label
        NormalText.new("&\#x02AAD;&\#x0FE00;")
      end

      # /leqq R: less, double equals 
      def ext_inline_verb_lE(label, content, visitor)
        label = label.to_s
        return nil unless /^lE:(.*)$/ =~ label
        NormalText.new("&\#x02266;")
      end

      # /lesseqqgtr R: less, dbl eq, greater 
      def ext_inline_verb_lEg(label, content, visitor)
        label = label.to_s
        return nil unless /^lEg:(.*)$/ =~ label
        NormalText.new("&\#x02A8B;")
      end

      # /lesseqgtr R: less, eq, greater 
      def ext_inline_verb_leg(label, content, visitor)
        label = label.to_s
        return nil unless /^leg:(.*)$/ =~ label
        NormalText.new("&\#x022DA;")
      end

      # /leqslant R: less-than-or-eq, slant 
      def ext_inline_verb_les(label, content, visitor)
        label = label.to_s
        return nil unless /^les:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;")
      end

      # less than, closed by curve, equal, slanted 
      def ext_inline_verb_lescc(label, content, visitor)
        label = label.to_s
        return nil unless /^lescc:(.*)$/ =~ label
        NormalText.new("&\#x02AA8;")
      end

      # less-than-or-equal, slanted, dot inside 
      def ext_inline_verb_lesdot(label, content, visitor)
        label = label.to_s
        return nil unless /^lesdot:(.*)$/ =~ label
        NormalText.new("&\#x02A7F;")
      end

      # less-than-or-equal, slanted, dot above 
      def ext_inline_verb_lesdoto(label, content, visitor)
        label = label.to_s
        return nil unless /^lesdoto:(.*)$/ =~ label
        NormalText.new("&\#x02A81;")
      end

      # less-than-or-equal, slanted, dot above right 
      def ext_inline_verb_lesdotor(label, content, visitor)
        label = label.to_s
        return nil unless /^lesdotor:(.*)$/ =~ label
        NormalText.new("&\#x02A83;")
      end

      # less, equal, slanted, greater 
      def ext_inline_verb_lesg(label, content, visitor)
        label = label.to_s
        return nil unless /^lesg:(.*)$/ =~ label
        NormalText.new("&\#x022DA;&\#x0FE00;")
      end

      # less, equal, slanted, greater, equal, slanted 
      def ext_inline_verb_lesges(label, content, visitor)
        label = label.to_s
        return nil unless /^lesges:(.*)$/ =~ label
        NormalText.new("&\#x02A93;")
      end

      # /lessgtr R: less, greater 
      def ext_inline_verb_lg(label, content, visitor)
        label = label.to_s
        return nil unless /^lg:(.*)$/ =~ label
        NormalText.new("&\#x02276;")
      end

      # less, greater, equal 
      def ext_inline_verb_lgE(label, content, visitor)
        label = label.to_s
        return nil unless /^lgE:(.*)$/ =~ label
        NormalText.new("&\#x02A91;")
      end

      # /Ll /lll /llless R: triple less-than 
      def ext_inline_verb_Ll(label, content, visitor)
        label = label.to_s
        return nil unless /^Ll:(.*)$/ =~ label
        NormalText.new("&\#x022D8;")
      end

      # /lesssim R: less, similar 
      def ext_inline_verb_lsim(label, content, visitor)
        label = label.to_s
        return nil unless /^lsim:(.*)$/ =~ label
        NormalText.new("&\#x02272;")
      end

      # less, similar, equal 
      def ext_inline_verb_lsime(label, content, visitor)
        label = label.to_s
        return nil unless /^lsime:(.*)$/ =~ label
        NormalText.new("&\#x02A8D;")
      end

      # less, similar, greater 
      def ext_inline_verb_lsimg(label, content, visitor)
        label = label.to_s
        return nil unless /^lsimg:(.*)$/ =~ label
        NormalText.new("&\#x02A8F;")
      end

      # /ll R: double less-than sign 
      def ext_inline_verb_Lt(label, content, visitor)
        label = label.to_s
        return nil unless /^Lt:(.*)$/ =~ label
        NormalText.new("&\#x0226A;")
      end

      # less than, closed by curve 
      def ext_inline_verb_ltcc(label, content, visitor)
        label = label.to_s
        return nil unless /^ltcc:(.*)$/ =~ label
        NormalText.new("&\#x02AA6;")
      end

      # less than, circle inside 
      def ext_inline_verb_ltcir(label, content, visitor)
        label = label.to_s
        return nil unless /^ltcir:(.*)$/ =~ label
        NormalText.new("&\#x02A79;")
      end

      # /lessdot R: less than, with dot 
      def ext_inline_verb_ltdot(label, content, visitor)
        label = label.to_s
        return nil unless /^ltdot:(.*)$/ =~ label
        NormalText.new("&\#x022D6;")
      end

      # less than, left arrow 
      def ext_inline_verb_ltlarr(label, content, visitor)
        label = label.to_s
        return nil unless /^ltlarr:(.*)$/ =~ label
        NormalText.new("&\#x02976;")
      end

      # less than, questionmark above 
      def ext_inline_verb_ltquest(label, content, visitor)
        label = label.to_s
        return nil unless /^ltquest:(.*)$/ =~ label
        NormalText.new("&\#x02A7B;")
      end

      # /trianglelefteq R: left triangle, eq 
      def ext_inline_verb_ltrie(label, content, visitor)
        label = label.to_s
        return nil unless /^ltrie:(.*)$/ =~ label
        NormalText.new("&\#x022B4;")
      end

      # minus, comma above 
      def ext_inline_verb_mcomma(label, content, visitor)
        label = label.to_s
        return nil unless /^mcomma:(.*)$/ =~ label
        NormalText.new("&\#x02A29;")
      end

      # minus with four dots, geometric properties 
      def ext_inline_verb_mDDot(label, content, visitor)
        label = label.to_s
        return nil unless /^mDDot:(.*)$/ =~ label
        NormalText.new("&\#x0223A;")
      end

      # /mid R: 
      def ext_inline_verb_mid(label, content, visitor)
        label = label.to_s
        return nil unless /^mid:(.*)$/ =~ label
        NormalText.new("&\#x02223;")
      end

      # /mlcp 
      def ext_inline_verb_mlcp(label, content, visitor)
        label = label.to_s
        return nil unless /^mlcp:(.*)$/ =~ label
        NormalText.new("&\#x02ADB;")
      end

      # /models R: 
      def ext_inline_verb_models(label, content, visitor)
        label = label.to_s
        return nil unless /^models:(.*)$/ =~ label
        NormalText.new("&\#x022A7;")
      end

      # most positive 
      def ext_inline_verb_mstpos(label, content, visitor)
        label = label.to_s
        return nil unless /^mstpos:(.*)$/ =~ label
        NormalText.new("&\#x0223E;")
      end

      # dbl precedes 
      def ext_inline_verb_Pr(label, content, visitor)
        label = label.to_s
        return nil unless /^Pr:(.*)$/ =~ label
        NormalText.new("&\#x02ABB;")
      end

      # /prec R: precedes 
      def ext_inline_verb_pr(label, content, visitor)
        label = label.to_s
        return nil unless /^pr:(.*)$/ =~ label
        NormalText.new("&\#x0227A;")
      end

      # /precapprox R: precedes, approximate 
      def ext_inline_verb_prap(label, content, visitor)
        label = label.to_s
        return nil unless /^prap:(.*)$/ =~ label
        NormalText.new("&\#x02AB7;")
      end

      # /preccurlyeq R: precedes, curly eq 
      def ext_inline_verb_prcue(label, content, visitor)
        label = label.to_s
        return nil unless /^prcue:(.*)$/ =~ label
        NormalText.new("&\#x0227C;")
      end

      # precedes, dbl equals 
      def ext_inline_verb_prE(label, content, visitor)
        label = label.to_s
        return nil unless /^prE:(.*)$/ =~ label
        NormalText.new("&\#x02AB3;")
      end

      # /preceq R: precedes, equals 
      def ext_inline_verb_pre(label, content, visitor)
        label = label.to_s
        return nil unless /^pre:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;")
      end

      # /precsim R: precedes, similar 
      def ext_inline_verb_prsim(label, content, visitor)
        label = label.to_s
        return nil unless /^prsim:(.*)$/ =~ label
        NormalText.new("&\#x0227E;")
      end

      # element precedes under relation 
      def ext_inline_verb_prurel(label, content, visitor)
        label = label.to_s
        return nil unless /^prurel:(.*)$/ =~ label
        NormalText.new("&\#x022B0;")
      end

      # /ratio 
      def ext_inline_verb_ratio(label, content, visitor)
        label = label.to_s
        return nil unless /^ratio:(.*)$/ =~ label
        NormalText.new("&\#x02236;")
      end

      # /trianglerighteq R: right tri, eq 
      def ext_inline_verb_rtrie(label, content, visitor)
        label = label.to_s
        return nil unless /^rtrie:(.*)$/ =~ label
        NormalText.new("&\#x022B5;")
      end

      # right triangle above left triangle 
      def ext_inline_verb_rtriltri(label, content, visitor)
        label = label.to_s
        return nil unless /^rtriltri:(.*)$/ =~ label
        NormalText.new("&\#x029CE;")
      end

      # dbl succeeds 
      def ext_inline_verb_Sc(label, content, visitor)
        label = label.to_s
        return nil unless /^Sc:(.*)$/ =~ label
        NormalText.new("&\#x02ABC;")
      end

      # /succ R: succeeds 
      def ext_inline_verb_sc(label, content, visitor)
        label = label.to_s
        return nil unless /^sc:(.*)$/ =~ label
        NormalText.new("&\#x0227B;")
      end

      # /succapprox R: succeeds, approximate 
      def ext_inline_verb_scap(label, content, visitor)
        label = label.to_s
        return nil unless /^scap:(.*)$/ =~ label
        NormalText.new("&\#x02AB8;")
      end

      # /succcurlyeq R: succeeds, curly eq 
      def ext_inline_verb_sccue(label, content, visitor)
        label = label.to_s
        return nil unless /^sccue:(.*)$/ =~ label
        NormalText.new("&\#x0227D;")
      end

      # succeeds, dbl equals 
      def ext_inline_verb_scE(label, content, visitor)
        label = label.to_s
        return nil unless /^scE:(.*)$/ =~ label
        NormalText.new("&\#x02AB4;")
      end

      # /succeq R: succeeds, equals 
      def ext_inline_verb_sce(label, content, visitor)
        label = label.to_s
        return nil unless /^sce:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;")
      end

      # /succsim R: succeeds, similar 
      def ext_inline_verb_scsim(label, content, visitor)
        label = label.to_s
        return nil unless /^scsim:(.*)$/ =~ label
        NormalText.new("&\#x0227F;")
      end

      # equal, dot below 
      def ext_inline_verb_sdote(label, content, visitor)
        label = label.to_s
        return nil unless /^sdote:(.*)$/ =~ label
        NormalText.new("&\#x02A66;")
      end

      # /smallfrown R: small down curve 
      def ext_inline_verb_sfrown(label, content, visitor)
        label = label.to_s
        return nil unless /^sfrown:(.*)$/ =~ label
        NormalText.new("&\#x02322;")
      end

      # similar, greater 
      def ext_inline_verb_simg(label, content, visitor)
        label = label.to_s
        return nil unless /^simg:(.*)$/ =~ label
        NormalText.new("&\#x02A9E;")
      end

      # similar, greater, equal 
      def ext_inline_verb_simgE(label, content, visitor)
        label = label.to_s
        return nil unless /^simgE:(.*)$/ =~ label
        NormalText.new("&\#x02AA0;")
      end

      # similar, less 
      def ext_inline_verb_siml(label, content, visitor)
        label = label.to_s
        return nil unless /^siml:(.*)$/ =~ label
        NormalText.new("&\#x02A9D;")
      end

      # similar, less, equal 
      def ext_inline_verb_simlE(label, content, visitor)
        label = label.to_s
        return nil unless /^simlE:(.*)$/ =~ label
        NormalText.new("&\#x02A9F;")
      end

      # /shortmid R: 
      def ext_inline_verb_smid(label, content, visitor)
        label = label.to_s
        return nil unless /^smid:(.*)$/ =~ label
        NormalText.new("&\#x02223;")
      end

      # /smile R: up curve 
      def ext_inline_verb_smile(label, content, visitor)
        label = label.to_s
        return nil unless /^smile:(.*)$/ =~ label
        NormalText.new("&\#x02323;")
      end

      # smaller than 
      def ext_inline_verb_smt(label, content, visitor)
        label = label.to_s
        return nil unless /^smt:(.*)$/ =~ label
        NormalText.new("&\#x02AAA;")
      end

      # smaller than or equal 
      def ext_inline_verb_smte(label, content, visitor)
        label = label.to_s
        return nil unless /^smte:(.*)$/ =~ label
        NormalText.new("&\#x02AAC;")
      end

      # smaller than or equal, slanted 
      def ext_inline_verb_smtes(label, content, visitor)
        label = label.to_s
        return nil unless /^smtes:(.*)$/ =~ label
        NormalText.new("&\#x02AAC;&\#x0FE00;")
      end

      # /shortparallel R: short parallel 
      def ext_inline_verb_spar(label, content, visitor)
        label = label.to_s
        return nil unless /^spar:(.*)$/ =~ label
        NormalText.new("&\#x02225;")
      end

      # /sqsubset R: square subset 
      def ext_inline_verb_sqsub(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsub:(.*)$/ =~ label
        NormalText.new("&\#x0228F;")
      end

      # /sqsubseteq R: square subset, equals 
      def ext_inline_verb_sqsube(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsube:(.*)$/ =~ label
        NormalText.new("&\#x02291;")
      end

      # /sqsupset R: square superset 
      def ext_inline_verb_sqsup(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsup:(.*)$/ =~ label
        NormalText.new("&\#x02290;")
      end

      # /sqsupseteq R: square superset, eq 
      def ext_inline_verb_sqsupe(label, content, visitor)
        label = label.to_s
        return nil unless /^sqsupe:(.*)$/ =~ label
        NormalText.new("&\#x02292;")
      end

      # /smallsmile R: small up curve 
      def ext_inline_verb_ssmile(label, content, visitor)
        label = label.to_s
        return nil unless /^ssmile:(.*)$/ =~ label
        NormalText.new("&\#x02323;")
      end

      # /Subset R: double subset 
      def ext_inline_verb_Sub(label, content, visitor)
        label = label.to_s
        return nil unless /^Sub:(.*)$/ =~ label
        NormalText.new("&\#x022D0;")
      end

      # /subseteqq R: subset, dbl equals 
      def ext_inline_verb_subE(label, content, visitor)
        label = label.to_s
        return nil unless /^subE:(.*)$/ =~ label
        NormalText.new("&\#x02AC5;")
      end

      # subset, equals, dot 
      def ext_inline_verb_subedot(label, content, visitor)
        label = label.to_s
        return nil unless /^subedot:(.*)$/ =~ label
        NormalText.new("&\#x02AC3;")
      end

      # subset, multiply 
      def ext_inline_verb_submult(label, content, visitor)
        label = label.to_s
        return nil unless /^submult:(.*)$/ =~ label
        NormalText.new("&\#x02AC1;")
      end

      # subset, plus 
      def ext_inline_verb_subplus(label, content, visitor)
        label = label.to_s
        return nil unless /^subplus:(.*)$/ =~ label
        NormalText.new("&\#x02ABF;")
      end

      # subset, right arrow 
      def ext_inline_verb_subrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^subrarr:(.*)$/ =~ label
        NormalText.new("&\#x02979;")
      end

      # subset, similar 
      def ext_inline_verb_subsim(label, content, visitor)
        label = label.to_s
        return nil unless /^subsim:(.*)$/ =~ label
        NormalText.new("&\#x02AC7;")
      end

      # subset above subset 
      def ext_inline_verb_subsub(label, content, visitor)
        label = label.to_s
        return nil unless /^subsub:(.*)$/ =~ label
        NormalText.new("&\#x02AD5;")
      end

      # subset above superset 
      def ext_inline_verb_subsup(label, content, visitor)
        label = label.to_s
        return nil unless /^subsup:(.*)$/ =~ label
        NormalText.new("&\#x02AD3;")
      end

      # /Supset R: dbl superset 
      def ext_inline_verb_Sup(label, content, visitor)
        label = label.to_s
        return nil unless /^Sup:(.*)$/ =~ label
        NormalText.new("&\#x022D1;")
      end

      # superset, subset, dash joining them 
      def ext_inline_verb_supdsub(label, content, visitor)
        label = label.to_s
        return nil unless /^supdsub:(.*)$/ =~ label
        NormalText.new("&\#x02AD8;")
      end

      # /supseteqq R: superset, dbl equals 
      def ext_inline_verb_supE(label, content, visitor)
        label = label.to_s
        return nil unless /^supE:(.*)$/ =~ label
        NormalText.new("&\#x02AC6;")
      end

      # superset, equals, dot 
      def ext_inline_verb_supedot(label, content, visitor)
        label = label.to_s
        return nil unless /^supedot:(.*)$/ =~ label
        NormalText.new("&\#x02AC4;")
      end

      # superset, solidus 
      def ext_inline_verb_suphsol(label, content, visitor)
        label = label.to_s
        return nil unless /^suphsol:(.*)$/ =~ label
        NormalText.new("&\#x02283;&\#x0002F;")
      end

      # superset, subset 
      def ext_inline_verb_suphsub(label, content, visitor)
        label = label.to_s
        return nil unless /^suphsub:(.*)$/ =~ label
        NormalText.new("&\#x02AD7;")
      end

      # superset, left arrow 
      def ext_inline_verb_suplarr(label, content, visitor)
        label = label.to_s
        return nil unless /^suplarr:(.*)$/ =~ label
        NormalText.new("&\#x0297B;")
      end

      # superset, multiply 
      def ext_inline_verb_supmult(label, content, visitor)
        label = label.to_s
        return nil unless /^supmult:(.*)$/ =~ label
        NormalText.new("&\#x02AC2;")
      end

      # superset, plus 
      def ext_inline_verb_supplus(label, content, visitor)
        label = label.to_s
        return nil unless /^supplus:(.*)$/ =~ label
        NormalText.new("&\#x02AC0;")
      end

      # superset, similar 
      def ext_inline_verb_supsim(label, content, visitor)
        label = label.to_s
        return nil unless /^supsim:(.*)$/ =~ label
        NormalText.new("&\#x02AC8;")
      end

      # superset above subset 
      def ext_inline_verb_supsub(label, content, visitor)
        label = label.to_s
        return nil unless /^supsub:(.*)$/ =~ label
        NormalText.new("&\#x02AD4;")
      end

      # superset above superset 
      def ext_inline_verb_supsup(label, content, visitor)
        label = label.to_s
        return nil unless /^supsup:(.*)$/ =~ label
        NormalText.new("&\#x02AD6;")
      end

      # /thickapprox R: thick approximate 
      def ext_inline_verb_thkap(label, content, visitor)
        label = label.to_s
        return nil unless /^thkap:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # /thicksim R: thick similar 
      def ext_inline_verb_thksim(label, content, visitor)
        label = label.to_s
        return nil unless /^thksim:(.*)$/ =~ label
        NormalText.new("&\#x0223C;")
      end

      # fork with top 
      def ext_inline_verb_topfork(label, content, visitor)
        label = label.to_s
        return nil unless /^topfork:(.*)$/ =~ label
        NormalText.new("&\#x02ADA;")
      end

      # /triangleq R: triangle, equals 
      def ext_inline_verb_trie(label, content, visitor)
        label = label.to_s
        return nil unless /^trie:(.*)$/ =~ label
        NormalText.new("&\#x0225C;")
      end

      # /between R: between 
      def ext_inline_verb_twixt(label, content, visitor)
        label = label.to_s
        return nil unless /^twixt:(.*)$/ =~ label
        NormalText.new("&\#x0226C;")
      end

      # dbl vert, bar (under) 
      def ext_inline_verb_Vbar(label, content, visitor)
        label = label.to_s
        return nil unless /^Vbar:(.*)$/ =~ label
        NormalText.new("&\#x02AEB;")
      end

      # vert, dbl bar (under) 
      def ext_inline_verb_vBar(label, content, visitor)
        label = label.to_s
        return nil unless /^vBar:(.*)$/ =~ label
        NormalText.new("&\#x02AE8;")
      end

      # dbl bar, vert over and under 
      def ext_inline_verb_vBarv(label, content, visitor)
        label = label.to_s
        return nil unless /^vBarv:(.*)$/ =~ label
        NormalText.new("&\#x02AE9;")
      end

      # dbl vert, dbl dash 
      def ext_inline_verb_VDash(label, content, visitor)
        label = label.to_s
        return nil unless /^VDash:(.*)$/ =~ label
        NormalText.new("&\#x022AB;")
      end

      # /Vdash R: dbl vertical, dash 
      def ext_inline_verb_Vdash(label, content, visitor)
        label = label.to_s
        return nil unless /^Vdash:(.*)$/ =~ label
        NormalText.new("&\#x022A9;")
      end

      # /vDash R: vertical, dbl dash 
      def ext_inline_verb_vDash(label, content, visitor)
        label = label.to_s
        return nil unless /^vDash:(.*)$/ =~ label
        NormalText.new("&\#x022A8;")
      end

      # /vdash R: vertical, dash 
      def ext_inline_verb_vdash(label, content, visitor)
        label = label.to_s
        return nil unless /^vdash:(.*)$/ =~ label
        NormalText.new("&\#x022A2;")
      end

      # vertical, dash (long) 
      def ext_inline_verb_Vdashl(label, content, visitor)
        label = label.to_s
        return nil unless /^Vdashl:(.*)$/ =~ label
        NormalText.new("&\#x02AE6;")
      end

      # /vartriangleleft R: l tri, open, var 
      def ext_inline_verb_vltri(label, content, visitor)
        label = label.to_s
        return nil unless /^vltri:(.*)$/ =~ label
        NormalText.new("&\#x022B2;")
      end

      # /varpropto R: proportional, variant 
      def ext_inline_verb_vprop(label, content, visitor)
        label = label.to_s
        return nil unless /^vprop:(.*)$/ =~ label
        NormalText.new("&\#x0221D;")
      end

      # /vartriangleright R: r tri, open, var 
      def ext_inline_verb_vrtri(label, content, visitor)
        label = label.to_s
        return nil unless /^vrtri:(.*)$/ =~ label
        NormalText.new("&\#x022B3;")
      end

      # /Vvdash R: triple vertical, dash 
      def ext_inline_verb_Vvdash(label, content, visitor)
        label = label.to_s
        return nil unless /^Vvdash:(.*)$/ =~ label
        NormalText.new("&\#x022AA;")
      end

    end
  end
end
