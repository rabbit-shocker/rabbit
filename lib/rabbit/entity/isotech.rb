require 'rabbit/element'

module Rabbit
  module Entity
    module Isotech

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # ac current 
      def ext_inline_verb_acd(label, content, visitor)
        label = label.to_s
        return nil unless /^acd:(.*)$/ =~ label
        NormalText.new("&\#x0223F;")
      end

      # /aleph aleph, Hebrew 
      def ext_inline_verb_aleph(label, content, visitor)
        label = label.to_s
        return nil unless /^aleph:(.*)$/ =~ label
        NormalText.new("&\#x02135;")
      end

      # dbl logical and 
      def ext_inline_verb_And(label, content, visitor)
        label = label.to_s
        return nil unless /^And:(.*)$/ =~ label
        NormalText.new("&\#x02A53;")
      end

      # /wedge /land B: logical and 
      def ext_inline_verb_and(label, content, visitor)
        label = label.to_s
        return nil unless /^and:(.*)$/ =~ label
        NormalText.new("&\#x02227;")
      end

      # two logical and 
      def ext_inline_verb_andand(label, content, visitor)
        label = label.to_s
        return nil unless /^andand:(.*)$/ =~ label
        NormalText.new("&\#x02A55;")
      end

      # and, horizontal dash 
      def ext_inline_verb_andd(label, content, visitor)
        label = label.to_s
        return nil unless /^andd:(.*)$/ =~ label
        NormalText.new("&\#x02A5C;")
      end

      # sloping large and 
      def ext_inline_verb_andslope(label, content, visitor)
        label = label.to_s
        return nil unless /^andslope:(.*)$/ =~ label
        NormalText.new("&\#x02A58;")
      end

      # and with middle stem 
      def ext_inline_verb_andv(label, content, visitor)
        label = label.to_s
        return nil unless /^andv:(.*)$/ =~ label
        NormalText.new("&\#x02A5A;")
      end

      # right (90 degree) angle 
      def ext_inline_verb_angrt(label, content, visitor)
        label = label.to_s
        return nil unless /^angrt:(.*)$/ =~ label
        NormalText.new("&\#x0221F;")
      end

      # /sphericalangle angle-spherical 
      def ext_inline_verb_angsph(label, content, visitor)
        label = label.to_s
        return nil unless /^angsph:(.*)$/ =~ label
        NormalText.new("&\#x02222;")
      end

      # Angstrom capital A, ring 
      def ext_inline_verb_angst(label, content, visitor)
        label = label.to_s
        return nil unless /^angst:(.*)$/ =~ label
        NormalText.new("&\#x0212B;")
      end

      # /approx R: approximate 
      def ext_inline_verb_ap(label, content, visitor)
        label = label.to_s
        return nil unless /^ap:(.*)$/ =~ label
        NormalText.new("&\#x02248;")
      end

      # approximate, circumflex accent 
      def ext_inline_verb_apacir(label, content, visitor)
        label = label.to_s
        return nil unless /^apacir:(.*)$/ =~ label
        NormalText.new("&\#x02A6F;")
      end

      # contour integral, anti-clockwise 
      def ext_inline_verb_awconint(label, content, visitor)
        label = label.to_s
        return nil unless /^awconint:(.*)$/ =~ label
        NormalText.new("&\#x02233;")
      end

      # anti clock-wise integration 
      def ext_inline_verb_awint(label, content, visitor)
        label = label.to_s
        return nil unless /^awint:(.*)$/ =~ label
        NormalText.new("&\#x02A11;")
      end

      # /because R: because 
      def ext_inline_verb_becaus(label, content, visitor)
        label = label.to_s
        return nil unless /^becaus:(.*)$/ =~ label
        NormalText.new("&\#x02235;")
      end

      # Bernoulli function (script capital B)  
      def ext_inline_verb_bernou(label, content, visitor)
        label = label.to_s
        return nil unless /^bernou:(.*)$/ =~ label
        NormalText.new("&\#x0212C;")
      end

      # reverse not equal 
      def ext_inline_verb_bne(label, content, visitor)
        label = label.to_s
        return nil unless /^bne:(.*)$/ =~ label
        NormalText.new("&\#x0003D;&\#x020E5;")
      end

      # reverse not equivalent 
      def ext_inline_verb_bnequiv(label, content, visitor)
        label = label.to_s
        return nil unless /^bnequiv:(.*)$/ =~ label
        NormalText.new("&\#x02261;&\#x020E5;")
      end

      # reverse not with two horizontal strokes 
      def ext_inline_verb_bNot(label, content, visitor)
        label = label.to_s
        return nil unless /^bNot:(.*)$/ =~ label
        NormalText.new("&\#x02AED;")
      end

      # reverse not 
      def ext_inline_verb_bnot(label, content, visitor)
        label = label.to_s
        return nil unless /^bnot:(.*)$/ =~ label
        NormalText.new("&\#x02310;")
      end

      # /bot bottom 
      def ext_inline_verb_bottom(label, content, visitor)
        label = label.to_s
        return nil unless /^bottom:(.*)$/ =~ label
        NormalText.new("&\#x022A5;")
      end

      # /cap B: intersection 
      def ext_inline_verb_cap(label, content, visitor)
        label = label.to_s
        return nil unless /^cap:(.*)$/ =~ label
        NormalText.new("&\#x02229;")
      end

      # triple contour integral operator 
      def ext_inline_verb_Cconint(label, content, visitor)
        label = label.to_s
        return nil unless /^Cconint:(.*)$/ =~ label
        NormalText.new("&\#x02230;")
      end

      # circulation function 
      def ext_inline_verb_cirfnint(label, content, visitor)
        label = label.to_s
        return nil unless /^cirfnint:(.*)$/ =~ label
        NormalText.new("&\#x02A10;")
      end

      # /circ B: composite function (small circle) 
      def ext_inline_verb_compfn(label, content, visitor)
        label = label.to_s
        return nil unless /^compfn:(.*)$/ =~ label
        NormalText.new("&\#x02218;")
      end

      # /cong R: congruent with 
      def ext_inline_verb_cong(label, content, visitor)
        label = label.to_s
        return nil unless /^cong:(.*)$/ =~ label
        NormalText.new("&\#x02245;")
      end

      # double contour integral operator 
      def ext_inline_verb_Conint(label, content, visitor)
        label = label.to_s
        return nil unless /^Conint:(.*)$/ =~ label
        NormalText.new("&\#x0222F;")
      end

      # /oint L: contour integral operator 
      def ext_inline_verb_conint(label, content, visitor)
        label = label.to_s
        return nil unless /^conint:(.*)$/ =~ label
        NormalText.new("&\#x0222E;")
      end

      # /cdots, three dots, centered 
      def ext_inline_verb_ctdot(label, content, visitor)
        label = label.to_s
        return nil unless /^ctdot:(.*)$/ =~ label
        NormalText.new("&\#x022EF;")
      end

      # /cup B: union or logical sum 
      def ext_inline_verb_cup(label, content, visitor)
        label = label.to_s
        return nil unless /^cup:(.*)$/ =~ label
        NormalText.new("&\#x0222A;")
      end

      # contour integral, clockwise 
      def ext_inline_verb_cwconint(label, content, visitor)
        label = label.to_s
        return nil unless /^cwconint:(.*)$/ =~ label
        NormalText.new("&\#x02232;")
      end

      # clockwise integral 
      def ext_inline_verb_cwint(label, content, visitor)
        label = label.to_s
        return nil unless /^cwint:(.*)$/ =~ label
        NormalText.new("&\#x02231;")
      end

      # cylindricity 
      def ext_inline_verb_cylcty(label, content, visitor)
        label = label.to_s
        return nil unless /^cylcty:(.*)$/ =~ label
        NormalText.new("&\#x0232D;")
      end

      # set membership, long horizontal stroke 
      def ext_inline_verb_disin(label, content, visitor)
        label = label.to_s
        return nil unless /^disin:(.*)$/ =~ label
        NormalText.new("&\#x022F2;")
      end

      # dieresis or umlaut mark 
      def ext_inline_verb_Dot(label, content, visitor)
        label = label.to_s
        return nil unless /^Dot:(.*)$/ =~ label
        NormalText.new("&\#x000A8;")
      end

      # solidus, bar above 
      def ext_inline_verb_dsol(label, content, visitor)
        label = label.to_s
        return nil unless /^dsol:(.*)$/ =~ label
        NormalText.new("&\#x029F6;")
      end

      # /ddots, three dots, descending 
      def ext_inline_verb_dtdot(label, content, visitor)
        label = label.to_s
        return nil unless /^dtdot:(.*)$/ =~ label
        NormalText.new("&\#x022F1;")
      end

      # large downward pointing angle 
      def ext_inline_verb_dwangle(label, content, visitor)
        label = label.to_s
        return nil unless /^dwangle:(.*)$/ =~ label
        NormalText.new("&\#x029A6;")
      end

      # electrical intersection 
      def ext_inline_verb_elinters(label, content, visitor)
        label = label.to_s
        return nil unless /^elinters:(.*)$/ =~ label
        NormalText.new("&\#x0FFFD;")
      end

      # parallel, equal; equal or parallel 
      def ext_inline_verb_epar(label, content, visitor)
        label = label.to_s
        return nil unless /^epar:(.*)$/ =~ label
        NormalText.new("&\#x022D5;")
      end

      # parallel, slanted, equal; homothetically congruent to 
      def ext_inline_verb_eparsl(label, content, visitor)
        label = label.to_s
        return nil unless /^eparsl:(.*)$/ =~ label
        NormalText.new("&\#x029E3;")
      end

      # /equiv R: identical with 
      def ext_inline_verb_equiv(label, content, visitor)
        label = label.to_s
        return nil unless /^equiv:(.*)$/ =~ label
        NormalText.new("&\#x02261;")
      end

      # equivalent, equal; congruent and parallel 
      def ext_inline_verb_eqvparsl(label, content, visitor)
        label = label.to_s
        return nil unless /^eqvparsl:(.*)$/ =~ label
        NormalText.new("&\#x029E5;")
      end

      # /exists at least one exists 
      def ext_inline_verb_exist(label, content, visitor)
        label = label.to_s
        return nil unless /^exist:(.*)$/ =~ label
        NormalText.new("&\#x02203;")
      end

      # flatness 
      def ext_inline_verb_fltns(label, content, visitor)
        label = label.to_s
        return nil unless /^fltns:(.*)$/ =~ label
        NormalText.new("&\#x025B1;")
      end

      # function of (italic small f) 
      def ext_inline_verb_fnof(label, content, visitor)
        label = label.to_s
        return nil unless /^fnof:(.*)$/ =~ label
        NormalText.new("&\#x00192;")
      end

      # /forall for all 
      def ext_inline_verb_forall(label, content, visitor)
        label = label.to_s
        return nil unless /^forall:(.*)$/ =~ label
        NormalText.new("&\#x02200;")
      end

      # finite part integral 
      def ext_inline_verb_fpartint(label, content, visitor)
        label = label.to_s
        return nil unless /^fpartint:(.*)$/ =~ label
        NormalText.new("&\#x02A0D;")
      end

      # /geq /ge R: greater-than-or-equal 
      def ext_inline_verb_ge(label, content, visitor)
        label = label.to_s
        return nil unless /^ge:(.*)$/ =~ label
        NormalText.new("&\#x02265;")
      end

      # Hamiltonian (script capital H)  
      def ext_inline_verb_hamilt(label, content, visitor)
        label = label.to_s
        return nil unless /^hamilt:(.*)$/ =~ label
        NormalText.new("&\#x0210B;")
      end

      # /iff if and only if  
      def ext_inline_verb_iff(label, content, visitor)
        label = label.to_s
        return nil unless /^iff:(.*)$/ =~ label
        NormalText.new("&\#x021D4;")
      end

      # infinity sign, incomplete 
      def ext_inline_verb_iinfin(label, content, visitor)
        label = label.to_s
        return nil unless /^iinfin:(.*)$/ =~ label
        NormalText.new("&\#x029DC;")
      end

      # impedance 
      def ext_inline_verb_imped(label, content, visitor)
        label = label.to_s
        return nil unless /^imped:(.*)$/ =~ label
        NormalText.new("&\#x001B5;")
      end

      # /infty infinity 
      def ext_inline_verb_infin(label, content, visitor)
        label = label.to_s
        return nil unless /^infin:(.*)$/ =~ label
        NormalText.new("&\#x0221E;")
      end

      # tie, infinity 
      def ext_inline_verb_infintie(label, content, visitor)
        label = label.to_s
        return nil unless /^infintie:(.*)$/ =~ label
        NormalText.new("&\#x029DD;")
      end

      # double integral operator 
      def ext_inline_verb_Int(label, content, visitor)
        label = label.to_s
        return nil unless /^Int:(.*)$/ =~ label
        NormalText.new("&\#x0222C;")
      end

      # /int L: integral operator 
      def ext_inline_verb_int(label, content, visitor)
        label = label.to_s
        return nil unless /^int:(.*)$/ =~ label
        NormalText.new("&\#x0222B;")
      end

      # integral, left arrow with hook 
      def ext_inline_verb_intlarhk(label, content, visitor)
        label = label.to_s
        return nil unless /^intlarhk:(.*)$/ =~ label
        NormalText.new("&\#x02A17;")
      end

      # /in R: set membership  
      def ext_inline_verb_isin(label, content, visitor)
        label = label.to_s
        return nil unless /^isin:(.*)$/ =~ label
        NormalText.new("&\#x02208;")
      end

      # set membership, dot above 
      def ext_inline_verb_isindot(label, content, visitor)
        label = label.to_s
        return nil unless /^isindot:(.*)$/ =~ label
        NormalText.new("&\#x022F5;")
      end

      # set membership, two horizontal strokes 
      def ext_inline_verb_isinE(label, content, visitor)
        label = label.to_s
        return nil unless /^isinE:(.*)$/ =~ label
        NormalText.new("&\#x022F9;")
      end

      # set membership, vertical bar on horizontal stroke 
      def ext_inline_verb_isins(label, content, visitor)
        label = label.to_s
        return nil unless /^isins:(.*)$/ =~ label
        NormalText.new("&\#x022F4;")
      end

      # large set membership, vertical bar on horizontal stroke 
      def ext_inline_verb_isinsv(label, content, visitor)
        label = label.to_s
        return nil unless /^isinsv:(.*)$/ =~ label
        NormalText.new("&\#x022F3;")
      end

      # set membership, variant 
      def ext_inline_verb_isinv(label, content, visitor)
        label = label.to_s
        return nil unless /^isinv:(.*)$/ =~ label
        NormalText.new("&\#x02208;")
      end

      # Lagrangian (script capital L)  
      def ext_inline_verb_lagran(label, content, visitor)
        label = label.to_s
        return nil unless /^lagran:(.*)$/ =~ label
        NormalText.new("&\#x02112;")
      end

      # left angle bracket, double 
      def ext_inline_verb_Lang(label, content, visitor)
        label = label.to_s
        return nil unless /^Lang:(.*)$/ =~ label
        NormalText.new("&\#x0300A;")
      end

      # /langle O: left angle bracket 
      def ext_inline_verb_lang(label, content, visitor)
        label = label.to_s
        return nil unless /^lang:(.*)$/ =~ label
        NormalText.new("&\#x02329;")
      end

      # /Leftarrow A: is implied by 
      def ext_inline_verb_lArr(label, content, visitor)
        label = label.to_s
        return nil unless /^lArr:(.*)$/ =~ label
        NormalText.new("&\#x021D0;")
      end

      # left broken bracket 
      def ext_inline_verb_lbbrk(label, content, visitor)
        label = label.to_s
        return nil unless /^lbbrk:(.*)$/ =~ label
        NormalText.new("&\#x03014;")
      end

      # /leq /le R: less-than-or-equal 
      def ext_inline_verb_le(label, content, visitor)
        label = label.to_s
        return nil unless /^le:(.*)$/ =~ label
        NormalText.new("&\#x02264;")
      end

      # left open angular bracket 
      def ext_inline_verb_loang(label, content, visitor)
        label = label.to_s
        return nil unless /^loang:(.*)$/ =~ label
        NormalText.new("&\#x03018;")
      end

      # left open bracket 
      def ext_inline_verb_lobrk(label, content, visitor)
        label = label.to_s
        return nil unless /^lobrk:(.*)$/ =~ label
        NormalText.new("&\#x0301A;")
      end

      # left open parenthesis 
      def ext_inline_verb_lopar(label, content, visitor)
        label = label.to_s
        return nil unless /^lopar:(.*)$/ =~ label
        NormalText.new("&\#x02985;")
      end

      # low asterisk 
      def ext_inline_verb_lowast(label, content, visitor)
        label = label.to_s
        return nil unless /^lowast:(.*)$/ =~ label
        NormalText.new("&\#x02217;")
      end

      # B: minus sign 
      def ext_inline_verb_minus(label, content, visitor)
        label = label.to_s
        return nil unless /^minus:(.*)$/ =~ label
        NormalText.new("&\#x02212;")
      end

      # /mp B: minus-or-plus sign 
      def ext_inline_verb_mnplus(label, content, visitor)
        label = label.to_s
        return nil unless /^mnplus:(.*)$/ =~ label
        NormalText.new("&\#x02213;")
      end

      # /nabla del, Hamilton operator 
      def ext_inline_verb_nabla(label, content, visitor)
        label = label.to_s
        return nil unless /^nabla:(.*)$/ =~ label
        NormalText.new("&\#x02207;")
      end

      # /ne /neq R: not equal 
      def ext_inline_verb_ne(label, content, visitor)
        label = label.to_s
        return nil unless /^ne:(.*)$/ =~ label
        NormalText.new("&\#x02260;")
      end

      # not equal, dot 
      def ext_inline_verb_nedot(label, content, visitor)
        label = label.to_s
        return nil unless /^nedot:(.*)$/ =~ label
        NormalText.new("&\#x02250;&\#x00338;")
      end

      # not, horizontal, parallel 
      def ext_inline_verb_nhpar(label, content, visitor)
        label = label.to_s
        return nil unless /^nhpar:(.*)$/ =~ label
        NormalText.new("&\#x02AF2;")
      end

      # /ni /owns R: contains 
      def ext_inline_verb_ni(label, content, visitor)
        label = label.to_s
        return nil unless /^ni:(.*)$/ =~ label
        NormalText.new("&\#x0220B;")
      end

      # contains, vertical bar on horizontal stroke 
      def ext_inline_verb_nis(label, content, visitor)
        label = label.to_s
        return nil unless /^nis:(.*)$/ =~ label
        NormalText.new("&\#x022FC;")
      end

      # contains, long horizontal stroke 
      def ext_inline_verb_nisd(label, content, visitor)
        label = label.to_s
        return nil unless /^nisd:(.*)$/ =~ label
        NormalText.new("&\#x022FA;")
      end

      # contains, variant 
      def ext_inline_verb_niv(label, content, visitor)
        label = label.to_s
        return nil unless /^niv:(.*)$/ =~ label
        NormalText.new("&\#x0220B;")
      end

      # not with two horizontal strokes 
      def ext_inline_verb_Not(label, content, visitor)
        label = label.to_s
        return nil unless /^Not:(.*)$/ =~ label
        NormalText.new("&\#x02AEC;")
      end

      # /notin N: negated set membership 
      def ext_inline_verb_notin(label, content, visitor)
        label = label.to_s
        return nil unless /^notin:(.*)$/ =~ label
        NormalText.new("&\#x02209;")
      end

      # negated set membership, dot above 
      def ext_inline_verb_notindot(label, content, visitor)
        label = label.to_s
        return nil unless /^notindot:(.*)$/ =~ label
        NormalText.new("&\#x022F5;&\#x00338;")
      end

      # negated set membership, two horizontal strokes 
      def ext_inline_verb_notinE(label, content, visitor)
        label = label.to_s
        return nil unless /^notinE:(.*)$/ =~ label
        NormalText.new("&\#x022F9;&\#x00338;")
      end

      # negated set membership, variant 
      def ext_inline_verb_notinva(label, content, visitor)
        label = label.to_s
        return nil unless /^notinva:(.*)$/ =~ label
        NormalText.new("&\#x02209;")
      end

      # negated set membership, variant 
      def ext_inline_verb_notinvb(label, content, visitor)
        label = label.to_s
        return nil unless /^notinvb:(.*)$/ =~ label
        NormalText.new("&\#x022F7;")
      end

      # negated set membership, variant 
      def ext_inline_verb_notinvc(label, content, visitor)
        label = label.to_s
        return nil unless /^notinvc:(.*)$/ =~ label
        NormalText.new("&\#x022F6;")
      end

      # negated contains 
      def ext_inline_verb_notni(label, content, visitor)
        label = label.to_s
        return nil unless /^notni:(.*)$/ =~ label
        NormalText.new("&\#x0220C;")
      end

      # negated contains, variant 
      def ext_inline_verb_notniva(label, content, visitor)
        label = label.to_s
        return nil unless /^notniva:(.*)$/ =~ label
        NormalText.new("&\#x0220C;")
      end

      # contains, variant 
      def ext_inline_verb_notnivb(label, content, visitor)
        label = label.to_s
        return nil unless /^notnivb:(.*)$/ =~ label
        NormalText.new("&\#x022FE;")
      end

      # contains, variant 
      def ext_inline_verb_notnivc(label, content, visitor)
        label = label.to_s
        return nil unless /^notnivc:(.*)$/ =~ label
        NormalText.new("&\#x022FD;")
      end

      # not parallel, slanted 
      def ext_inline_verb_nparsl(label, content, visitor)
        label = label.to_s
        return nil unless /^nparsl:(.*)$/ =~ label
        NormalText.new("&\#x02AFD;&\#x020E5;")
      end

      # not partial differential 
      def ext_inline_verb_npart(label, content, visitor)
        label = label.to_s
        return nil unless /^npart:(.*)$/ =~ label
        NormalText.new("&\#x02202;&\#x00338;")
      end

      # line integration, not including the pole 
      def ext_inline_verb_npolint(label, content, visitor)
        label = label.to_s
        return nil unless /^npolint:(.*)$/ =~ label
        NormalText.new("&\#x02A14;")
      end

      # not, vert, infinity 
      def ext_inline_verb_nvinfin(label, content, visitor)
        label = label.to_s
        return nil unless /^nvinfin:(.*)$/ =~ label
        NormalText.new("&\#x029DE;")
      end

      # circle, cross 
      def ext_inline_verb_olcross(label, content, visitor)
        label = label.to_s
        return nil unless /^olcross:(.*)$/ =~ label
        NormalText.new("&\#x029BB;")
      end

      # dbl logical or 
      def ext_inline_verb_Or(label, content, visitor)
        label = label.to_s
        return nil unless /^Or:(.*)$/ =~ label
        NormalText.new("&\#x02A54;")
      end

      # /vee /lor B: logical or 
      def ext_inline_verb_or(label, content, visitor)
        label = label.to_s
        return nil unless /^or:(.*)$/ =~ label
        NormalText.new("&\#x02228;")
      end

      # or, horizontal dash 
      def ext_inline_verb_ord(label, content, visitor)
        label = label.to_s
        return nil unless /^ord:(.*)$/ =~ label
        NormalText.new("&\#x02A5D;")
      end

      # order of (script small o)  
      def ext_inline_verb_order(label, content, visitor)
        label = label.to_s
        return nil unless /^order:(.*)$/ =~ label
        NormalText.new("&\#x02134;")
      end

      # two logical or 
      def ext_inline_verb_oror(label, content, visitor)
        label = label.to_s
        return nil unless /^oror:(.*)$/ =~ label
        NormalText.new("&\#x02A56;")
      end

      # sloping large or 
      def ext_inline_verb_orslope(label, content, visitor)
        label = label.to_s
        return nil unless /^orslope:(.*)$/ =~ label
        NormalText.new("&\#x02A57;")
      end

      # or with middle stem 
      def ext_inline_verb_orv(label, content, visitor)
        label = label.to_s
        return nil unless /^orv:(.*)$/ =~ label
        NormalText.new("&\#x02A5B;")
      end

      # /parallel R: parallel 
      def ext_inline_verb_par(label, content, visitor)
        label = label.to_s
        return nil unless /^par:(.*)$/ =~ label
        NormalText.new("&\#x02225;")
      end

      # parallel, slanted 
      def ext_inline_verb_parsl(label, content, visitor)
        label = label.to_s
        return nil unless /^parsl:(.*)$/ =~ label
        NormalText.new("&\#x02AFD;")
      end

      # /partial partial differential 
      def ext_inline_verb_part(label, content, visitor)
        label = label.to_s
        return nil unless /^part:(.*)$/ =~ label
        NormalText.new("&\#x02202;")
      end

      # per thousand 
      def ext_inline_verb_permil(label, content, visitor)
        label = label.to_s
        return nil unless /^permil:(.*)$/ =~ label
        NormalText.new("&\#x02030;")
      end

      # /perp R: perpendicular 
      def ext_inline_verb_perp(label, content, visitor)
        label = label.to_s
        return nil unless /^perp:(.*)$/ =~ label
        NormalText.new("&\#x022A5;")
      end

      # per 10 thousand 
      def ext_inline_verb_pertenk(label, content, visitor)
        label = label.to_s
        return nil unless /^pertenk:(.*)$/ =~ label
        NormalText.new("&\#x02031;")
      end

      # physics M-matrix (script capital M)  
      def ext_inline_verb_phmmat(label, content, visitor)
        label = label.to_s
        return nil unless /^phmmat:(.*)$/ =~ label
        NormalText.new("&\#x02133;")
      end

      # integral around a point operator 
      def ext_inline_verb_pointint(label, content, visitor)
        label = label.to_s
        return nil unless /^pointint:(.*)$/ =~ label
        NormalText.new("&\#x02A15;")
      end

      # double prime or second 
      def ext_inline_verb_Prime(label, content, visitor)
        label = label.to_s
        return nil unless /^Prime:(.*)$/ =~ label
        NormalText.new("&\#x02033;")
      end

      # /prime prime or minute 
      def ext_inline_verb_prime(label, content, visitor)
        label = label.to_s
        return nil unless /^prime:(.*)$/ =~ label
        NormalText.new("&\#x02032;")
      end

      # all-around profile 
      def ext_inline_verb_profalar(label, content, visitor)
        label = label.to_s
        return nil unless /^profalar:(.*)$/ =~ label
        NormalText.new("&\#x0232E;")
      end

      # profile of a line 
      def ext_inline_verb_profline(label, content, visitor)
        label = label.to_s
        return nil unless /^profline:(.*)$/ =~ label
        NormalText.new("&\#x02312;")
      end

      # profile of a surface 
      def ext_inline_verb_profsurf(label, content, visitor)
        label = label.to_s
        return nil unless /^profsurf:(.*)$/ =~ label
        NormalText.new("&\#x02313;")
      end

      # /propto R: is proportional to 
      def ext_inline_verb_prop(label, content, visitor)
        label = label.to_s
        return nil unless /^prop:(.*)$/ =~ label
        NormalText.new("&\#x0221D;")
      end

      # /iiiint quadruple integral operator 
      def ext_inline_verb_qint(label, content, visitor)
        label = label.to_s
        return nil unless /^qint:(.*)$/ =~ label
        NormalText.new("&\#x02A0C;")
      end

      # quadruple prime 
      def ext_inline_verb_qprime(label, content, visitor)
        label = label.to_s
        return nil unless /^qprime:(.*)$/ =~ label
        NormalText.new("&\#x02057;")
      end

      # quaternion integral operator 
      def ext_inline_verb_quatint(label, content, visitor)
        label = label.to_s
        return nil unless /^quatint:(.*)$/ =~ label
        NormalText.new("&\#x02A16;")
      end

      # /surd radical 
      def ext_inline_verb_radic(label, content, visitor)
        label = label.to_s
        return nil unless /^radic:(.*)$/ =~ label
        NormalText.new("&\#x0221A;")
      end

      # right angle bracket, double 
      def ext_inline_verb_Rang(label, content, visitor)
        label = label.to_s
        return nil unless /^Rang:(.*)$/ =~ label
        NormalText.new("&\#x0300B;")
      end

      # /rangle C: right angle bracket 
      def ext_inline_verb_rang(label, content, visitor)
        label = label.to_s
        return nil unless /^rang:(.*)$/ =~ label
        NormalText.new("&\#x0232A;")
      end

      # /Rightarrow A: implies 
      def ext_inline_verb_rArr(label, content, visitor)
        label = label.to_s
        return nil unless /^rArr:(.*)$/ =~ label
        NormalText.new("&\#x021D2;")
      end

      # right broken bracket 
      def ext_inline_verb_rbbrk(label, content, visitor)
        label = label.to_s
        return nil unless /^rbbrk:(.*)$/ =~ label
        NormalText.new("&\#x03015;")
      end

      # right open angular bracket 
      def ext_inline_verb_roang(label, content, visitor)
        label = label.to_s
        return nil unless /^roang:(.*)$/ =~ label
        NormalText.new("&\#x03019;")
      end

      # right open bracket 
      def ext_inline_verb_robrk(label, content, visitor)
        label = label.to_s
        return nil unless /^robrk:(.*)$/ =~ label
        NormalText.new("&\#x0301B;")
      end

      # right open parenthesis 
      def ext_inline_verb_ropar(label, content, visitor)
        label = label.to_s
        return nil unless /^ropar:(.*)$/ =~ label
        NormalText.new("&\#x02986;")
      end

      # line integration, rectangular path around pole 
      def ext_inline_verb_rppolint(label, content, visitor)
        label = label.to_s
        return nil unless /^rppolint:(.*)$/ =~ label
        NormalText.new("&\#x02A12;")
      end

      # line integration, semi-circular path around pole 
      def ext_inline_verb_scpolint(label, content, visitor)
        label = label.to_s
        return nil unless /^scpolint:(.*)$/ =~ label
        NormalText.new("&\#x02A13;")
      end

      # /sim R: similar 
      def ext_inline_verb_sim(label, content, visitor)
        label = label.to_s
        return nil unless /^sim:(.*)$/ =~ label
        NormalText.new("&\#x0223C;")
      end

      # similar, dot 
      def ext_inline_verb_simdot(label, content, visitor)
        label = label.to_s
        return nil unless /^simdot:(.*)$/ =~ label
        NormalText.new("&\#x02A6A;")
      end

      # /simeq R: similar, equals 
      def ext_inline_verb_sime(label, content, visitor)
        label = label.to_s
        return nil unless /^sime:(.*)$/ =~ label
        NormalText.new("&\#x02243;")
      end

      # similar, parallel, slanted, equal 
      def ext_inline_verb_smeparsl(label, content, visitor)
        label = label.to_s
        return nil unless /^smeparsl:(.*)$/ =~ label
        NormalText.new("&\#x029E4;")
      end

      # /square, square 
      def ext_inline_verb_square(label, content, visitor)
        label = label.to_s
        return nil unless /^square:(.*)$/ =~ label
        NormalText.new("&\#x025A1;")
      end

      # /blacksquare, square, filled  
      def ext_inline_verb_squarf(label, content, visitor)
        label = label.to_s
        return nil unless /^squarf:(.*)$/ =~ label
        NormalText.new("&\#x025AA;")
      end

      # straightness 
      def ext_inline_verb_strns(label, content, visitor)
        label = label.to_s
        return nil unless /^strns:(.*)$/ =~ label
        NormalText.new("&\#x000AF;")
      end

      # /subset R: subset or is implied by 
      def ext_inline_verb_sub(label, content, visitor)
        label = label.to_s
        return nil unless /^sub:(.*)$/ =~ label
        NormalText.new("&\#x02282;")
      end

      # /subseteq R: subset, equals 
      def ext_inline_verb_sube(label, content, visitor)
        label = label.to_s
        return nil unless /^sube:(.*)$/ =~ label
        NormalText.new("&\#x02286;")
      end

      # /supset R: superset or implies 
      def ext_inline_verb_sup(label, content, visitor)
        label = label.to_s
        return nil unless /^sup:(.*)$/ =~ label
        NormalText.new("&\#x02283;")
      end

      # /supseteq R: superset, equals 
      def ext_inline_verb_supe(label, content, visitor)
        label = label.to_s
        return nil unless /^supe:(.*)$/ =~ label
        NormalText.new("&\#x02287;")
      end

      # /therefore R: therefore 
      def ext_inline_verb_there4(label, content, visitor)
        label = label.to_s
        return nil unless /^there4:(.*)$/ =~ label
        NormalText.new("&\#x02234;")
      end

      # /iiint triple integral operator 
      def ext_inline_verb_tint(label, content, visitor)
        label = label.to_s
        return nil unless /^tint:(.*)$/ =~ label
        NormalText.new("&\#x0222D;")
      end

      # /top top 
      def ext_inline_verb_top(label, content, visitor)
        label = label.to_s
        return nil unless /^top:(.*)$/ =~ label
        NormalText.new("&\#x022A4;")
      end

      # top and bottom 
      def ext_inline_verb_topbot(label, content, visitor)
        label = label.to_s
        return nil unless /^topbot:(.*)$/ =~ label
        NormalText.new("&\#x02336;")
      end

      # top, circle below 
      def ext_inline_verb_topcir(label, content, visitor)
        label = label.to_s
        return nil unless /^topcir:(.*)$/ =~ label
        NormalText.new("&\#x02AF1;")
      end

      # triple prime 
      def ext_inline_verb_tprime(label, content, visitor)
        label = label.to_s
        return nil unless /^tprime:(.*)$/ =~ label
        NormalText.new("&\#x02034;")
      end

      # three dots, ascending 
      def ext_inline_verb_utdot(label, content, visitor)
        label = label.to_s
        return nil unless /^utdot:(.*)$/ =~ label
        NormalText.new("&\#x022F0;")
      end

      # large upward pointing angle 
      def ext_inline_verb_uwangle(label, content, visitor)
        label = label.to_s
        return nil unless /^uwangle:(.*)$/ =~ label
        NormalText.new("&\#x029A7;")
      end

      # right angle, variant 
      def ext_inline_verb_vangrt(label, content, visitor)
        label = label.to_s
        return nil unless /^vangrt:(.*)$/ =~ label
        NormalText.new("&\#x0299C;")
      end

      # logical or, equals 
      def ext_inline_verb_veeeq(label, content, visitor)
        label = label.to_s
        return nil unless /^veeeq:(.*)$/ =~ label
        NormalText.new("&\#x0225A;")
      end

      # /Vert dbl vertical bar 
      def ext_inline_verb_Verbar(label, content, visitor)
        label = label.to_s
        return nil unless /^Verbar:(.*)$/ =~ label
        NormalText.new("&\#x02016;")
      end

      # /wedgeq R: corresponds to (wedge, equals) 
      def ext_inline_verb_wedgeq(label, content, visitor)
        label = label.to_s
        return nil unless /^wedgeq:(.*)$/ =~ label
        NormalText.new("&\#x02259;")
      end

      # large contains, vertical bar on horizontal stroke 
      def ext_inline_verb_xnis(label, content, visitor)
        label = label.to_s
        return nil unless /^xnis:(.*)$/ =~ label
        NormalText.new("&\#x022FB;")
      end

    end
  end
end
