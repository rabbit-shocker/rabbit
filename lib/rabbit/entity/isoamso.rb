require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamso

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /angle - angle 
      def ext_inline_verb_ang(label, content, visitor)
        label = label.to_s
        return nil unless /^ang:(.*)$/ =~ label
        NormalText.new("&\#x02220;")
      end

      # angle, equal 
      def ext_inline_verb_ange(label, content, visitor)
        label = label.to_s
        return nil unless /^ange:(.*)$/ =~ label
        NormalText.new("&\#x029A4;")
      end

      # /measuredangle - angle-measured 
      def ext_inline_verb_angmsd(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsd:(.*)$/ =~ label
        NormalText.new("&\#x02221;")
      end

      # angle-measured, arrow, up, right 
      def ext_inline_verb_angmsdaa(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdaa:(.*)$/ =~ label
        NormalText.new("&\#x029A8;")
      end

      # angle-measured, arrow, up, left 
      def ext_inline_verb_angmsdab(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdab:(.*)$/ =~ label
        NormalText.new("&\#x029A9;")
      end

      # angle-measured, arrow, down, right 
      def ext_inline_verb_angmsdac(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdac:(.*)$/ =~ label
        NormalText.new("&\#x029AA;")
      end

      # angle-measured, arrow, down, left 
      def ext_inline_verb_angmsdad(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdad:(.*)$/ =~ label
        NormalText.new("&\#x029AB;")
      end

      # angle-measured, arrow, right, up 
      def ext_inline_verb_angmsdae(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdae:(.*)$/ =~ label
        NormalText.new("&\#x029AC;")
      end

      # angle-measured, arrow, left, up 
      def ext_inline_verb_angmsdaf(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdaf:(.*)$/ =~ label
        NormalText.new("&\#x029AD;")
      end

      # angle-measured, arrow, right, down 
      def ext_inline_verb_angmsdag(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdag:(.*)$/ =~ label
        NormalText.new("&\#x029AE;")
      end

      # angle-measured, arrow, left, down 
      def ext_inline_verb_angmsdah(label, content, visitor)
        label = label.to_s
        return nil unless /^angmsdah:(.*)$/ =~ label
        NormalText.new("&\#x029AF;")
      end

      # right angle-measured 
      def ext_inline_verb_angrtvb(label, content, visitor)
        label = label.to_s
        return nil unless /^angrtvb:(.*)$/ =~ label
        NormalText.new("&\#x022BE;")
      end

      # right angle-measured, dot 
      def ext_inline_verb_angrtvbd(label, content, visitor)
        label = label.to_s
        return nil unless /^angrtvbd:(.*)$/ =~ label
        NormalText.new("&\#x0299D;")
      end

      # bottom square bracket 
      def ext_inline_verb_bbrk(label, content, visitor)
        label = label.to_s
        return nil unless /^bbrk:(.*)$/ =~ label
        NormalText.new("&\#x023B5;")
      end

      # bottom above top square bracket 
      def ext_inline_verb_bbrktbrk(label, content, visitor)
        label = label.to_s
        return nil unless /^bbrktbrk:(.*)$/ =~ label
        NormalText.new("&\#x023B6;")
      end

      # reversed circle, slash 
      def ext_inline_verb_bemptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^bemptyv:(.*)$/ =~ label
        NormalText.new("&\#x029B0;")
      end

      # /beth - beth, Hebrew 
      def ext_inline_verb_beth(label, content, visitor)
        label = label.to_s
        return nil unless /^beth:(.*)$/ =~ label
        NormalText.new("&\#x02136;")
      end

      # two joined squares 
      def ext_inline_verb_boxbox(label, content, visitor)
        label = label.to_s
        return nil unless /^boxbox:(.*)$/ =~ label
        NormalText.new("&\#x029C9;")
      end

      # /backprime - reverse prime 
      def ext_inline_verb_bprime(label, content, visitor)
        label = label.to_s
        return nil unless /^bprime:(.*)$/ =~ label
        NormalText.new("&\#x02035;")
      end

      # reverse semi-colon 
      def ext_inline_verb_bsemi(label, content, visitor)
        label = label.to_s
        return nil unless /^bsemi:(.*)$/ =~ label
        NormalText.new("&\#x0204F;")
      end

      # circle, slash, small circle above 
      def ext_inline_verb_cemptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^cemptyv:(.*)$/ =~ label
        NormalText.new("&\#x029B2;")
      end

      # circle, two horizontal stroked to the right 
      def ext_inline_verb_cirE(label, content, visitor)
        label = label.to_s
        return nil unless /^cirE:(.*)$/ =~ label
        NormalText.new("&\#x029C3;")
      end

      # circle, small circle to the right 
      def ext_inline_verb_cirscir(label, content, visitor)
        label = label.to_s
        return nil unless /^cirscir:(.*)$/ =~ label
        NormalText.new("&\#x029C2;")
      end

      # /complement - complement sign 
      def ext_inline_verb_comp(label, content, visitor)
        label = label.to_s
        return nil unless /^comp:(.*)$/ =~ label
        NormalText.new("&\#x02201;")
      end

      # /daleth - daleth, Hebrew 
      def ext_inline_verb_daleth(label, content, visitor)
        label = label.to_s
        return nil unless /^daleth:(.*)$/ =~ label
        NormalText.new("&\#x02138;")
      end

      # circle, slash, bar above 
      def ext_inline_verb_demptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^demptyv:(.*)$/ =~ label
        NormalText.new("&\#x029B1;")
      end

      # /ell - cursive small l 
      def ext_inline_verb_ell(label, content, visitor)
        label = label.to_s
        return nil unless /^ell:(.*)$/ =~ label
        NormalText.new("&\#x02113;")
      end

      # /emptyset - zero, slash 
      def ext_inline_verb_empty(label, content, visitor)
        label = label.to_s
        return nil unless /^empty:(.*)$/ =~ label
        NormalText.new("&\#x02205;")
      end

      # /varnothing - circle, slash 
      def ext_inline_verb_emptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^emptyv:(.*)$/ =~ label
        NormalText.new("&\#x02205;")
      end

      # /gimel - gimel, Hebrew 
      def ext_inline_verb_gimel(label, content, visitor)
        label = label.to_s
        return nil unless /^gimel:(.*)$/ =~ label
        NormalText.new("&\#x02137;")
      end

      # inverted iota 
      def ext_inline_verb_iiota(label, content, visitor)
        label = label.to_s
        return nil unless /^iiota:(.*)$/ =~ label
        NormalText.new("&\#x02129;")
      end

      # /Im - imaginary   
      def ext_inline_verb_image(label, content, visitor)
        label = label.to_s
        return nil unless /^image:(.*)$/ =~ label
        NormalText.new("&\#x02111;")
      end

      # /imath - small i, no dot 
      def ext_inline_verb_imath(label, content, visitor)
        label = label.to_s
        return nil unless /^imath:(.*)$/ =~ label
        NormalText.new("&\#x00131;")
      end

      # /jmath - small j, no dot 
      def ext_inline_verb_jmath(label, content, visitor)
        label = label.to_s
        return nil unless /^jmath:(.*)$/ =~ label
        NormalText.new("&\#x0006A;")
      end

      # circle, slash, left arrow above 
      def ext_inline_verb_laemptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^laemptyv:(.*)$/ =~ label
        NormalText.new("&\#x029B4;")
      end

      # lower left triangle 
      def ext_inline_verb_lltri(label, content, visitor)
        label = label.to_s
        return nil unless /^lltri:(.*)$/ =~ label
        NormalText.new("&\#x025FA;")
      end

      # lower right triangle 
      def ext_inline_verb_lrtri(label, content, visitor)
        label = label.to_s
        return nil unless /^lrtri:(.*)$/ =~ label
        NormalText.new("&\#x022BF;")
      end

      # /mho - conductance 
      def ext_inline_verb_mho(label, content, visitor)
        label = label.to_s
        return nil unless /^mho:(.*)$/ =~ label
        NormalText.new("&\#x02127;")
      end

      # not, vert, angle 
      def ext_inline_verb_nang(label, content, visitor)
        label = label.to_s
        return nil unless /^nang:(.*)$/ =~ label
        NormalText.new("&\#x02220;&\#x020D2;")
      end

      # /nexists - negated exists 
      def ext_inline_verb_nexist(label, content, visitor)
        label = label.to_s
        return nil unless /^nexist:(.*)$/ =~ label
        NormalText.new("&\#x02204;")
      end

      # /circledS - capital S in circle 
      def ext_inline_verb_oS(label, content, visitor)
        label = label.to_s
        return nil unless /^oS:(.*)$/ =~ label
        NormalText.new("&\#x024C8;")
      end

      # /hbar - Planck's over 2pi 
      def ext_inline_verb_planck(label, content, visitor)
        label = label.to_s
        return nil unless /^planck:(.*)$/ =~ label
        NormalText.new("&\#x0210F;")
      end

      # /hslash - variant Planck's over 2pi 
      def ext_inline_verb_plankv(label, content, visitor)
        label = label.to_s
        return nil unless /^plankv:(.*)$/ =~ label
        NormalText.new("&\#x0210F;")
      end

      # circle, slash, right arrow above 
      def ext_inline_verb_raemptyv(label, content, visitor)
        label = label.to_s
        return nil unless /^raemptyv:(.*)$/ =~ label
        NormalText.new("&\#x029B3;")
      end

      # reverse angle, equal 
      def ext_inline_verb_range(label, content, visitor)
        label = label.to_s
        return nil unless /^range:(.*)$/ =~ label
        NormalText.new("&\#x029A5;")
      end

      # /Re - real 
      def ext_inline_verb_real(label, content, visitor)
        label = label.to_s
        return nil unless /^real:(.*)$/ =~ label
        NormalText.new("&\#x0211C;")
      end

      # top square bracket 
      def ext_inline_verb_tbrk(label, content, visitor)
        label = label.to_s
        return nil unless /^tbrk:(.*)$/ =~ label
        NormalText.new("&\#x023B4;")
      end

      # trapezium 
      def ext_inline_verb_trpezium(label, content, visitor)
        label = label.to_s
        return nil unless /^trpezium:(.*)$/ =~ label
        NormalText.new("&\#x0FFFD;")
      end

      # upper left triangle 
      def ext_inline_verb_ultri(label, content, visitor)
        label = label.to_s
        return nil unless /^ultri:(.*)$/ =~ label
        NormalText.new("&\#x025F8;")
      end

      # upper right triangle 
      def ext_inline_verb_urtri(label, content, visitor)
        label = label.to_s
        return nil unless /^urtri:(.*)$/ =~ label
        NormalText.new("&\#x025F9;")
      end

      # vertical zig-zag line 
      def ext_inline_verb_vzigzag(label, content, visitor)
        label = label.to_s
        return nil unless /^vzigzag:(.*)$/ =~ label
        NormalText.new("&\#x0299A;")
      end

      # /wp - Weierstrass p 
      def ext_inline_verb_weierp(label, content, visitor)
        label = label.to_s
        return nil unless /^weierp:(.*)$/ =~ label
        NormalText.new("&\#x02118;")
      end

    end
  end
end
