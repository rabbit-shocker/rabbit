require 'rabbit/element'

module Rabbit
  module Entity
    module Isopub

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =significant blank symbol 
      def ext_inline_verb_blank(label, content, visitor)
        label = label.to_s
        return nil unless /^blank:(.*)$/ =~ label
        NormalText.new("&\#x02423;")
      end

      # =50% shaded block 
      def ext_inline_verb_blk12(label, content, visitor)
        label = label.to_s
        return nil unless /^blk12:(.*)$/ =~ label
        NormalText.new("&\#x02592;")
      end

      # =25% shaded block 
      def ext_inline_verb_blk14(label, content, visitor)
        label = label.to_s
        return nil unless /^blk14:(.*)$/ =~ label
        NormalText.new("&\#x02591;")
      end

      # =75% shaded block 
      def ext_inline_verb_blk34(label, content, visitor)
        label = label.to_s
        return nil unless /^blk34:(.*)$/ =~ label
        NormalText.new("&\#x02593;")
      end

      # =full block 
      def ext_inline_verb_block(label, content, visitor)
        label = label.to_s
        return nil unless /^block:(.*)$/ =~ label
        NormalText.new("&\#x02588;")
      end

      # /bullet B: =round bullet, filled 
      def ext_inline_verb_bull(label, content, visitor)
        label = label.to_s
        return nil unless /^bull:(.*)$/ =~ label
        NormalText.new("&\#x02022;")
      end

      # =caret (insertion mark) 
      def ext_inline_verb_caret(label, content, visitor)
        label = label.to_s
        return nil unless /^caret:(.*)$/ =~ label
        NormalText.new("&\#x02041;")
      end

      # /checkmark =tick, check mark 
      def ext_inline_verb_check(label, content, visitor)
        label = label.to_s
        return nil unless /^check:(.*)$/ =~ label
        NormalText.new("&\#x02713;")
      end

      # /circ B: =circle, open 
      def ext_inline_verb_cir(label, content, visitor)
        label = label.to_s
        return nil unless /^cir:(.*)$/ =~ label
        NormalText.new("&\#x025CB;")
      end

      # /clubsuit =club suit symbol  
      def ext_inline_verb_clubs(label, content, visitor)
        label = label.to_s
        return nil unless /^clubs:(.*)$/ =~ label
        NormalText.new("&\#x02663;")
      end

      # =sound recording copyright sign 
      def ext_inline_verb_copysr(label, content, visitor)
        label = label.to_s
        return nil unless /^copysr:(.*)$/ =~ label
        NormalText.new("&\#x02117;")
      end

      # =ballot cross 
      def ext_inline_verb_cross(label, content, visitor)
        label = label.to_s
        return nil unless /^cross:(.*)$/ =~ label
        NormalText.new("&\#x02717;")
      end

      # /ddagger B: =double dagger 
      def ext_inline_verb_Dagger(label, content, visitor)
        label = label.to_s
        return nil unless /^Dagger:(.*)$/ =~ label
        NormalText.new("&\#x02021;")
      end

      # /dagger B: =dagger 
      def ext_inline_verb_dagger(label, content, visitor)
        label = label.to_s
        return nil unless /^dagger:(.*)$/ =~ label
        NormalText.new("&\#x02020;")
      end

      # =hyphen (true graphic) 
      def ext_inline_verb_dash(label, content, visitor)
        label = label.to_s
        return nil unless /^dash:(.*)$/ =~ label
        NormalText.new("&\#x02010;")
      end

      # /diamondsuit =diamond suit symbol  
      def ext_inline_verb_diams(label, content, visitor)
        label = label.to_s
        return nil unless /^diams:(.*)$/ =~ label
        NormalText.new("&\#x02666;")
      end

      # downward left crop mark  
      def ext_inline_verb_dlcrop(label, content, visitor)
        label = label.to_s
        return nil unless /^dlcrop:(.*)$/ =~ label
        NormalText.new("&\#x0230D;")
      end

      # downward right crop mark  
      def ext_inline_verb_drcrop(label, content, visitor)
        label = label.to_s
        return nil unless /^drcrop:(.*)$/ =~ label
        NormalText.new("&\#x0230C;")
      end

      # /triangledown =down triangle, open 
      def ext_inline_verb_dtri(label, content, visitor)
        label = label.to_s
        return nil unless /^dtri:(.*)$/ =~ label
        NormalText.new("&\#x025BF;")
      end

      # /blacktriangledown =dn tri, filled 
      def ext_inline_verb_dtrif(label, content, visitor)
        label = label.to_s
        return nil unless /^dtrif:(.*)$/ =~ label
        NormalText.new("&\#x025BE;")
      end

      # =em space 
      def ext_inline_verb_emsp(label, content, visitor)
        label = label.to_s
        return nil unless /^emsp:(.*)$/ =~ label
        NormalText.new("&\#x02003;")
      end

      # =1/3-em space 
      def ext_inline_verb_emsp13(label, content, visitor)
        label = label.to_s
        return nil unless /^emsp13:(.*)$/ =~ label
        NormalText.new("&\#x02004;")
      end

      # =1/4-em space 
      def ext_inline_verb_emsp14(label, content, visitor)
        label = label.to_s
        return nil unless /^emsp14:(.*)$/ =~ label
        NormalText.new("&\#x02005;")
      end

      # =en space (1/2-em) 
      def ext_inline_verb_ensp(label, content, visitor)
        label = label.to_s
        return nil unless /^ensp:(.*)$/ =~ label
        NormalText.new("&\#x02002;")
      end

      # =female symbol 
      def ext_inline_verb_female(label, content, visitor)
        label = label.to_s
        return nil unless /^female:(.*)$/ =~ label
        NormalText.new("&\#x02640;")
      end

      # small ffi ligature 
      def ext_inline_verb_ffilig(label, content, visitor)
        label = label.to_s
        return nil unless /^ffilig:(.*)$/ =~ label
        NormalText.new("&\#x0FB03;")
      end

      # small ff ligature 
      def ext_inline_verb_fflig(label, content, visitor)
        label = label.to_s
        return nil unless /^fflig:(.*)$/ =~ label
        NormalText.new("&\#x0FB00;")
      end

      # small ffl ligature 
      def ext_inline_verb_ffllig(label, content, visitor)
        label = label.to_s
        return nil unless /^ffllig:(.*)$/ =~ label
        NormalText.new("&\#x0FB04;")
      end

      # small fi ligature 
      def ext_inline_verb_filig(label, content, visitor)
        label = label.to_s
        return nil unless /^filig:(.*)$/ =~ label
        NormalText.new("&\#x0FB01;")
      end

      # /flat =musical flat 
      def ext_inline_verb_flat(label, content, visitor)
        label = label.to_s
        return nil unless /^flat:(.*)$/ =~ label
        NormalText.new("&\#x0266D;")
      end

      # small fl ligature 
      def ext_inline_verb_fllig(label, content, visitor)
        label = label.to_s
        return nil unless /^fllig:(.*)$/ =~ label
        NormalText.new("&\#x0FB02;")
      end

      # =fraction one-third 
      def ext_inline_verb_frac13(label, content, visitor)
        label = label.to_s
        return nil unless /^frac13:(.*)$/ =~ label
        NormalText.new("&\#x02153;")
      end

      # =fraction one-fifth 
      def ext_inline_verb_frac15(label, content, visitor)
        label = label.to_s
        return nil unless /^frac15:(.*)$/ =~ label
        NormalText.new("&\#x02155;")
      end

      # =fraction one-sixth 
      def ext_inline_verb_frac16(label, content, visitor)
        label = label.to_s
        return nil unless /^frac16:(.*)$/ =~ label
        NormalText.new("&\#x02159;")
      end

      # =fraction two-thirds 
      def ext_inline_verb_frac23(label, content, visitor)
        label = label.to_s
        return nil unless /^frac23:(.*)$/ =~ label
        NormalText.new("&\#x02154;")
      end

      # =fraction two-fifths 
      def ext_inline_verb_frac25(label, content, visitor)
        label = label.to_s
        return nil unless /^frac25:(.*)$/ =~ label
        NormalText.new("&\#x02156;")
      end

      # =fraction three-fifths 
      def ext_inline_verb_frac35(label, content, visitor)
        label = label.to_s
        return nil unless /^frac35:(.*)$/ =~ label
        NormalText.new("&\#x02157;")
      end

      # =fraction four-fifths 
      def ext_inline_verb_frac45(label, content, visitor)
        label = label.to_s
        return nil unless /^frac45:(.*)$/ =~ label
        NormalText.new("&\#x02158;")
      end

      # =fraction five-sixths 
      def ext_inline_verb_frac56(label, content, visitor)
        label = label.to_s
        return nil unless /^frac56:(.*)$/ =~ label
        NormalText.new("&\#x0215A;")
      end

      # =hair space 
      def ext_inline_verb_hairsp(label, content, visitor)
        label = label.to_s
        return nil unless /^hairsp:(.*)$/ =~ label
        NormalText.new("&\#x0200A;")
      end

      # /heartsuit =heart suit symbol 
      def ext_inline_verb_hearts(label, content, visitor)
        label = label.to_s
        return nil unless /^hearts:(.*)$/ =~ label
        NormalText.new("&\#x02665;")
      end

      # =ellipsis (horizontal) 
      def ext_inline_verb_hellip(label, content, visitor)
        label = label.to_s
        return nil unless /^hellip:(.*)$/ =~ label
        NormalText.new("&\#x02026;")
      end

      # rectangle, filled (hyphen bullet) 
      def ext_inline_verb_hybull(label, content, visitor)
        label = label.to_s
        return nil unless /^hybull:(.*)$/ =~ label
        NormalText.new("&\#x02043;")
      end

      # =in-care-of symbol 
      def ext_inline_verb_incare(label, content, visitor)
        label = label.to_s
        return nil unless /^incare:(.*)$/ =~ label
        NormalText.new("&\#x02105;")
      end

      # =rising dbl quote, left (low) 
      def ext_inline_verb_ldquor(label, content, visitor)
        label = label.to_s
        return nil unless /^ldquor:(.*)$/ =~ label
        NormalText.new("&\#x0201E;")
      end

      # =lower half block 
      def ext_inline_verb_lhblk(label, content, visitor)
        label = label.to_s
        return nil unless /^lhblk:(.*)$/ =~ label
        NormalText.new("&\#x02584;")
      end

      # /lozenge - lozenge or total mark 
      def ext_inline_verb_loz(label, content, visitor)
        label = label.to_s
        return nil unless /^loz:(.*)$/ =~ label
        NormalText.new("&\#x025CA;")
      end

      # /blacklozenge - lozenge, filled 
      def ext_inline_verb_lozf(label, content, visitor)
        label = label.to_s
        return nil unless /^lozf:(.*)$/ =~ label
        NormalText.new("&\#x029EB;")
      end

      # =rising single quote, left (low) 
      def ext_inline_verb_lsquor(label, content, visitor)
        label = label.to_s
        return nil unless /^lsquor:(.*)$/ =~ label
        NormalText.new("&\#x0201A;")
      end

      # /triangleleft B: l triangle, open 
      def ext_inline_verb_ltri(label, content, visitor)
        label = label.to_s
        return nil unless /^ltri:(.*)$/ =~ label
        NormalText.new("&\#x025C3;")
      end

      # /blacktriangleleft R: =l tri, filled 
      def ext_inline_verb_ltrif(label, content, visitor)
        label = label.to_s
        return nil unless /^ltrif:(.*)$/ =~ label
        NormalText.new("&\#x025C2;")
      end

      # =male symbol 
      def ext_inline_verb_male(label, content, visitor)
        label = label.to_s
        return nil unless /^male:(.*)$/ =~ label
        NormalText.new("&\#x02642;")
      end

      # /maltese =maltese cross 
      def ext_inline_verb_malt(label, content, visitor)
        label = label.to_s
        return nil unless /^malt:(.*)$/ =~ label
        NormalText.new("&\#x02720;")
      end

      # =histogram marker 
      def ext_inline_verb_marker(label, content, visitor)
        label = label.to_s
        return nil unless /^marker:(.*)$/ =~ label
        NormalText.new("&\#x025AE;")
      end

      # =em dash  
      def ext_inline_verb_mdash(label, content, visitor)
        label = label.to_s
        return nil unless /^mdash:(.*)$/ =~ label
        NormalText.new("&\#x02014;")
      end

      # em leader 
      def ext_inline_verb_mldr(label, content, visitor)
        label = label.to_s
        return nil unless /^mldr:(.*)$/ =~ label
        NormalText.new("&\#x02026;")
      end

      # /natural - music natural 
      def ext_inline_verb_natur(label, content, visitor)
        label = label.to_s
        return nil unless /^natur:(.*)$/ =~ label
        NormalText.new("&\#x0266E;")
      end

      # =en dash 
      def ext_inline_verb_ndash(label, content, visitor)
        label = label.to_s
        return nil unless /^ndash:(.*)$/ =~ label
        NormalText.new("&\#x02013;")
      end

      # =double baseline dot (en leader) 
      def ext_inline_verb_nldr(label, content, visitor)
        label = label.to_s
        return nil unless /^nldr:(.*)$/ =~ label
        NormalText.new("&\#x02025;")
      end

      # =digit space (width of a number) 
      def ext_inline_verb_numsp(label, content, visitor)
        label = label.to_s
        return nil unless /^numsp:(.*)$/ =~ label
        NormalText.new("&\#x02007;")
      end

      # =telephone symbol  
      def ext_inline_verb_phone(label, content, visitor)
        label = label.to_s
        return nil unless /^phone:(.*)$/ =~ label
        NormalText.new("&\#x0260E;")
      end

      # =punctuation space (width of comma) 
      def ext_inline_verb_puncsp(label, content, visitor)
        label = label.to_s
        return nil unless /^puncsp:(.*)$/ =~ label
        NormalText.new("&\#x02008;")
      end

      # rising dbl quote, right (high) 
      def ext_inline_verb_rdquor(label, content, visitor)
        label = label.to_s
        return nil unless /^rdquor:(.*)$/ =~ label
        NormalText.new("&\#x0201D;")
      end

      # =rectangle, open 
      def ext_inline_verb_rect(label, content, visitor)
        label = label.to_s
        return nil unless /^rect:(.*)$/ =~ label
        NormalText.new("&\#x025AD;")
      end

      # rising single quote, right (high) 
      def ext_inline_verb_rsquor(label, content, visitor)
        label = label.to_s
        return nil unless /^rsquor:(.*)$/ =~ label
        NormalText.new("&\#x02019;")
      end

      # /triangleright B: r triangle, open 
      def ext_inline_verb_rtri(label, content, visitor)
        label = label.to_s
        return nil unless /^rtri:(.*)$/ =~ label
        NormalText.new("&\#x025B9;")
      end

      # /blacktriangleright R: =r tri, filled 
      def ext_inline_verb_rtrif(label, content, visitor)
        label = label.to_s
        return nil unless /^rtrif:(.*)$/ =~ label
        NormalText.new("&\#x025B8;")
      end

      # pharmaceutical prescription (Rx) 
      def ext_inline_verb_rx(label, content, visitor)
        label = label.to_s
        return nil unless /^rx:(.*)$/ =~ label
        NormalText.new("&\#x0211E;")
      end

      # sextile (6-pointed star) 
      def ext_inline_verb_sext(label, content, visitor)
        label = label.to_s
        return nil unless /^sext:(.*)$/ =~ label
        NormalText.new("&\#x02736;")
      end

      # /sharp =musical sharp 
      def ext_inline_verb_sharp(label, content, visitor)
        label = label.to_s
        return nil unless /^sharp:(.*)$/ =~ label
        NormalText.new("&\#x0266F;")
      end

      # /spadesuit =spades suit symbol  
      def ext_inline_verb_spades(label, content, visitor)
        label = label.to_s
        return nil unless /^spades:(.*)$/ =~ label
        NormalText.new("&\#x02660;")
      end

      # =square, open 
      def ext_inline_verb_squ(label, content, visitor)
        label = label.to_s
        return nil unless /^squ:(.*)$/ =~ label
        NormalText.new("&\#x025A1;")
      end

      # /blacksquare =sq bullet, filled 
      def ext_inline_verb_squf(label, content, visitor)
        label = label.to_s
        return nil unless /^squf:(.*)$/ =~ label
        NormalText.new("&\#x025AA;")
      end

      # =star, open 
      def ext_inline_verb_star(label, content, visitor)
        label = label.to_s
        return nil unless /^star:(.*)$/ =~ label
        NormalText.new("&\#x02606;")
      end

      # /bigstar - star, filled  
      def ext_inline_verb_starf(label, content, visitor)
        label = label.to_s
        return nil unless /^starf:(.*)$/ =~ label
        NormalText.new("&\#x02605;")
      end

      # register mark or target 
      def ext_inline_verb_target(label, content, visitor)
        label = label.to_s
        return nil unless /^target:(.*)$/ =~ label
        NormalText.new("&\#x02316;")
      end

      # =telephone recorder symbol 
      def ext_inline_verb_telrec(label, content, visitor)
        label = label.to_s
        return nil unless /^telrec:(.*)$/ =~ label
        NormalText.new("&\#x02315;")
      end

      # =thin space (1/6-em) 
      def ext_inline_verb_thinsp(label, content, visitor)
        label = label.to_s
        return nil unless /^thinsp:(.*)$/ =~ label
        NormalText.new("&\#x02009;")
      end

      # =upper half block 
      def ext_inline_verb_uhblk(label, content, visitor)
        label = label.to_s
        return nil unless /^uhblk:(.*)$/ =~ label
        NormalText.new("&\#x02580;")
      end

      # upward left crop mark  
      def ext_inline_verb_ulcrop(label, content, visitor)
        label = label.to_s
        return nil unless /^ulcrop:(.*)$/ =~ label
        NormalText.new("&\#x0230F;")
      end

      # upward right crop mark  
      def ext_inline_verb_urcrop(label, content, visitor)
        label = label.to_s
        return nil unless /^urcrop:(.*)$/ =~ label
        NormalText.new("&\#x0230E;")
      end

      # /triangle =up triangle, open 
      def ext_inline_verb_utri(label, content, visitor)
        label = label.to_s
        return nil unless /^utri:(.*)$/ =~ label
        NormalText.new("&\#x025B5;")
      end

      # /blacktriangle =up tri, filled 
      def ext_inline_verb_utrif(label, content, visitor)
        label = label.to_s
        return nil unless /^utrif:(.*)$/ =~ label
        NormalText.new("&\#x025B4;")
      end

      # vertical ellipsis 
      def ext_inline_verb_vellip(label, content, visitor)
        label = label.to_s
        return nil unless /^vellip:(.*)$/ =~ label
        NormalText.new("&\#x022EE;")
      end

    end
  end
end
