require 'rabbit/element'

module Rabbit
  module Entity
    module Isolat2

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =capital A, breve 
      def ext_inline_verb_Abreve(label, content, visitor)
        label = label.to_s
        return nil unless /^Abreve:(.*)$/ =~ label
        NormalText.new("&\#x00102;")
      end

      # =small a, breve 
      def ext_inline_verb_abreve(label, content, visitor)
        label = label.to_s
        return nil unless /^abreve:(.*)$/ =~ label
        NormalText.new("&\#x00103;")
      end

      # =capital A, macron 
      def ext_inline_verb_Amacr(label, content, visitor)
        label = label.to_s
        return nil unless /^Amacr:(.*)$/ =~ label
        NormalText.new("&\#x00100;")
      end

      # =small a, macron 
      def ext_inline_verb_amacr(label, content, visitor)
        label = label.to_s
        return nil unless /^amacr:(.*)$/ =~ label
        NormalText.new("&\#x00101;")
      end

      # =capital A, ogonek 
      def ext_inline_verb_Aogon(label, content, visitor)
        label = label.to_s
        return nil unless /^Aogon:(.*)$/ =~ label
        NormalText.new("&\#x00104;")
      end

      # =small a, ogonek 
      def ext_inline_verb_aogon(label, content, visitor)
        label = label.to_s
        return nil unless /^aogon:(.*)$/ =~ label
        NormalText.new("&\#x00105;")
      end

      # =capital C, acute accent 
      def ext_inline_verb_Cacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Cacute:(.*)$/ =~ label
        NormalText.new("&\#x00106;")
      end

      # =small c, acute accent 
      def ext_inline_verb_cacute(label, content, visitor)
        label = label.to_s
        return nil unless /^cacute:(.*)$/ =~ label
        NormalText.new("&\#x00107;")
      end

      # =capital C, caron 
      def ext_inline_verb_Ccaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Ccaron:(.*)$/ =~ label
        NormalText.new("&\#x0010C;")
      end

      # =small c, caron 
      def ext_inline_verb_ccaron(label, content, visitor)
        label = label.to_s
        return nil unless /^ccaron:(.*)$/ =~ label
        NormalText.new("&\#x0010D;")
      end

      # =capital C, circumflex accent 
      def ext_inline_verb_Ccirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Ccirc:(.*)$/ =~ label
        NormalText.new("&\#x00108;")
      end

      # =small c, circumflex accent 
      def ext_inline_verb_ccirc(label, content, visitor)
        label = label.to_s
        return nil unless /^ccirc:(.*)$/ =~ label
        NormalText.new("&\#x00109;")
      end

      # =capital C, dot above 
      def ext_inline_verb_Cdot(label, content, visitor)
        label = label.to_s
        return nil unless /^Cdot:(.*)$/ =~ label
        NormalText.new("&\#x0010A;")
      end

      # =small c, dot above 
      def ext_inline_verb_cdot(label, content, visitor)
        label = label.to_s
        return nil unless /^cdot:(.*)$/ =~ label
        NormalText.new("&\#x0010B;")
      end

      # =capital D, caron 
      def ext_inline_verb_Dcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Dcaron:(.*)$/ =~ label
        NormalText.new("&\#x0010E;")
      end

      # =small d, caron 
      def ext_inline_verb_dcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^dcaron:(.*)$/ =~ label
        NormalText.new("&\#x0010F;")
      end

      # =capital D, stroke 
      def ext_inline_verb_Dstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^Dstrok:(.*)$/ =~ label
        NormalText.new("&\#x00110;")
      end

      # =small d, stroke 
      def ext_inline_verb_dstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^dstrok:(.*)$/ =~ label
        NormalText.new("&\#x00111;")
      end

      # =capital E, caron 
      def ext_inline_verb_Ecaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Ecaron:(.*)$/ =~ label
        NormalText.new("&\#x0011A;")
      end

      # =small e, caron 
      def ext_inline_verb_ecaron(label, content, visitor)
        label = label.to_s
        return nil unless /^ecaron:(.*)$/ =~ label
        NormalText.new("&\#x0011B;")
      end

      # =capital E, dot above 
      def ext_inline_verb_Edot(label, content, visitor)
        label = label.to_s
        return nil unless /^Edot:(.*)$/ =~ label
        NormalText.new("&\#x00116;")
      end

      # =small e, dot above 
      def ext_inline_verb_edot(label, content, visitor)
        label = label.to_s
        return nil unless /^edot:(.*)$/ =~ label
        NormalText.new("&\#x00117;")
      end

      # =capital E, macron 
      def ext_inline_verb_Emacr(label, content, visitor)
        label = label.to_s
        return nil unless /^Emacr:(.*)$/ =~ label
        NormalText.new("&\#x00112;")
      end

      # =small e, macron 
      def ext_inline_verb_emacr(label, content, visitor)
        label = label.to_s
        return nil unless /^emacr:(.*)$/ =~ label
        NormalText.new("&\#x00113;")
      end

      # =capital ENG, Lapp 
      def ext_inline_verb_ENG(label, content, visitor)
        label = label.to_s
        return nil unless /^ENG:(.*)$/ =~ label
        NormalText.new("&\#x0014A;")
      end

      # =small eng, Lapp 
      def ext_inline_verb_eng(label, content, visitor)
        label = label.to_s
        return nil unless /^eng:(.*)$/ =~ label
        NormalText.new("&\#x0014B;")
      end

      # =capital E, ogonek 
      def ext_inline_verb_Eogon(label, content, visitor)
        label = label.to_s
        return nil unless /^Eogon:(.*)$/ =~ label
        NormalText.new("&\#x00118;")
      end

      # =small e, ogonek 
      def ext_inline_verb_eogon(label, content, visitor)
        label = label.to_s
        return nil unless /^eogon:(.*)$/ =~ label
        NormalText.new("&\#x00119;")
      end

      # =small g, acute accent 
      def ext_inline_verb_gacute(label, content, visitor)
        label = label.to_s
        return nil unless /^gacute:(.*)$/ =~ label
        NormalText.new("&\#x001F5;")
      end

      # =capital G, breve 
      def ext_inline_verb_Gbreve(label, content, visitor)
        label = label.to_s
        return nil unless /^Gbreve:(.*)$/ =~ label
        NormalText.new("&\#x0011E;")
      end

      # =small g, breve 
      def ext_inline_verb_gbreve(label, content, visitor)
        label = label.to_s
        return nil unless /^gbreve:(.*)$/ =~ label
        NormalText.new("&\#x0011F;")
      end

      # =capital G, cedilla 
      def ext_inline_verb_Gcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Gcedil:(.*)$/ =~ label
        NormalText.new("&\#x00122;")
      end

      # =capital G, circumflex accent 
      def ext_inline_verb_Gcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Gcirc:(.*)$/ =~ label
        NormalText.new("&\#x0011C;")
      end

      # =small g, circumflex accent 
      def ext_inline_verb_gcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^gcirc:(.*)$/ =~ label
        NormalText.new("&\#x0011D;")
      end

      # =capital G, dot above 
      def ext_inline_verb_Gdot(label, content, visitor)
        label = label.to_s
        return nil unless /^Gdot:(.*)$/ =~ label
        NormalText.new("&\#x00120;")
      end

      # =small g, dot above 
      def ext_inline_verb_gdot(label, content, visitor)
        label = label.to_s
        return nil unless /^gdot:(.*)$/ =~ label
        NormalText.new("&\#x00121;")
      end

      # =capital H, circumflex accent 
      def ext_inline_verb_Hcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Hcirc:(.*)$/ =~ label
        NormalText.new("&\#x00124;")
      end

      # =small h, circumflex accent 
      def ext_inline_verb_hcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^hcirc:(.*)$/ =~ label
        NormalText.new("&\#x00125;")
      end

      # =capital H, stroke 
      def ext_inline_verb_Hstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^Hstrok:(.*)$/ =~ label
        NormalText.new("&\#x00126;")
      end

      # =small h, stroke 
      def ext_inline_verb_hstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^hstrok:(.*)$/ =~ label
        NormalText.new("&\#x00127;")
      end

      # =capital I, dot above 
      def ext_inline_verb_Idot(label, content, visitor)
        label = label.to_s
        return nil unless /^Idot:(.*)$/ =~ label
        NormalText.new("&\#x00130;")
      end

      # =capital IJ ligature 
      def ext_inline_verb_IJlig(label, content, visitor)
        label = label.to_s
        return nil unless /^IJlig:(.*)$/ =~ label
        NormalText.new("&\#x00132;")
      end

      # =small ij ligature 
      def ext_inline_verb_ijlig(label, content, visitor)
        label = label.to_s
        return nil unless /^ijlig:(.*)$/ =~ label
        NormalText.new("&\#x00133;")
      end

      # =capital I, macron 
      def ext_inline_verb_Imacr(label, content, visitor)
        label = label.to_s
        return nil unless /^Imacr:(.*)$/ =~ label
        NormalText.new("&\#x0012A;")
      end

      # =small i, macron 
      def ext_inline_verb_imacr(label, content, visitor)
        label = label.to_s
        return nil unless /^imacr:(.*)$/ =~ label
        NormalText.new("&\#x0012B;")
      end

      # =small i without dot 
      def ext_inline_verb_inodot(label, content, visitor)
        label = label.to_s
        return nil unless /^inodot:(.*)$/ =~ label
        NormalText.new("&\#x00131;")
      end

      # =capital I, ogonek 
      def ext_inline_verb_Iogon(label, content, visitor)
        label = label.to_s
        return nil unless /^Iogon:(.*)$/ =~ label
        NormalText.new("&\#x0012E;")
      end

      # =small i, ogonek 
      def ext_inline_verb_iogon(label, content, visitor)
        label = label.to_s
        return nil unless /^iogon:(.*)$/ =~ label
        NormalText.new("&\#x0012F;")
      end

      # =capital I, tilde 
      def ext_inline_verb_Itilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Itilde:(.*)$/ =~ label
        NormalText.new("&\#x00128;")
      end

      # =small i, tilde 
      def ext_inline_verb_itilde(label, content, visitor)
        label = label.to_s
        return nil unless /^itilde:(.*)$/ =~ label
        NormalText.new("&\#x00129;")
      end

      # =capital J, circumflex accent 
      def ext_inline_verb_Jcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Jcirc:(.*)$/ =~ label
        NormalText.new("&\#x00134;")
      end

      # =small j, circumflex accent 
      def ext_inline_verb_jcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^jcirc:(.*)$/ =~ label
        NormalText.new("&\#x00135;")
      end

      # =capital K, cedilla 
      def ext_inline_verb_Kcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Kcedil:(.*)$/ =~ label
        NormalText.new("&\#x00136;")
      end

      # =small k, cedilla 
      def ext_inline_verb_kcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^kcedil:(.*)$/ =~ label
        NormalText.new("&\#x00137;")
      end

      # =small k, Greenlandic 
      def ext_inline_verb_kgreen(label, content, visitor)
        label = label.to_s
        return nil unless /^kgreen:(.*)$/ =~ label
        NormalText.new("&\#x00138;")
      end

      # =capital L, acute accent 
      def ext_inline_verb_Lacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Lacute:(.*)$/ =~ label
        NormalText.new("&\#x00139;")
      end

      # =small l, acute accent 
      def ext_inline_verb_lacute(label, content, visitor)
        label = label.to_s
        return nil unless /^lacute:(.*)$/ =~ label
        NormalText.new("&\#x0013A;")
      end

      # =capital L, caron 
      def ext_inline_verb_Lcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Lcaron:(.*)$/ =~ label
        NormalText.new("&\#x0013D;")
      end

      # =small l, caron 
      def ext_inline_verb_lcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^lcaron:(.*)$/ =~ label
        NormalText.new("&\#x0013E;")
      end

      # =capital L, cedilla 
      def ext_inline_verb_Lcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Lcedil:(.*)$/ =~ label
        NormalText.new("&\#x0013B;")
      end

      # =small l, cedilla 
      def ext_inline_verb_lcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^lcedil:(.*)$/ =~ label
        NormalText.new("&\#x0013C;")
      end

      # =capital L, middle dot 
      def ext_inline_verb_Lmidot(label, content, visitor)
        label = label.to_s
        return nil unless /^Lmidot:(.*)$/ =~ label
        NormalText.new("&\#x0013F;")
      end

      # =small l, middle dot 
      def ext_inline_verb_lmidot(label, content, visitor)
        label = label.to_s
        return nil unless /^lmidot:(.*)$/ =~ label
        NormalText.new("&\#x00140;")
      end

      # =capital L, stroke 
      def ext_inline_verb_Lstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^Lstrok:(.*)$/ =~ label
        NormalText.new("&\#x00141;")
      end

      # =small l, stroke 
      def ext_inline_verb_lstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^lstrok:(.*)$/ =~ label
        NormalText.new("&\#x00142;")
      end

      # =capital N, acute accent 
      def ext_inline_verb_Nacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Nacute:(.*)$/ =~ label
        NormalText.new("&\#x00143;")
      end

      # =small n, acute accent 
      def ext_inline_verb_nacute(label, content, visitor)
        label = label.to_s
        return nil unless /^nacute:(.*)$/ =~ label
        NormalText.new("&\#x00144;")
      end

      # =small n, apostrophe 
      def ext_inline_verb_napos(label, content, visitor)
        label = label.to_s
        return nil unless /^napos:(.*)$/ =~ label
        NormalText.new("&\#x00149;")
      end

      # =capital N, caron 
      def ext_inline_verb_Ncaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Ncaron:(.*)$/ =~ label
        NormalText.new("&\#x00147;")
      end

      # =small n, caron 
      def ext_inline_verb_ncaron(label, content, visitor)
        label = label.to_s
        return nil unless /^ncaron:(.*)$/ =~ label
        NormalText.new("&\#x00148;")
      end

      # =capital N, cedilla 
      def ext_inline_verb_Ncedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Ncedil:(.*)$/ =~ label
        NormalText.new("&\#x00145;")
      end

      # =small n, cedilla 
      def ext_inline_verb_ncedil(label, content, visitor)
        label = label.to_s
        return nil unless /^ncedil:(.*)$/ =~ label
        NormalText.new("&\#x00146;")
      end

      # =capital O, double acute accent 
      def ext_inline_verb_Odblac(label, content, visitor)
        label = label.to_s
        return nil unless /^Odblac:(.*)$/ =~ label
        NormalText.new("&\#x00150;")
      end

      # =small o, double acute accent 
      def ext_inline_verb_odblac(label, content, visitor)
        label = label.to_s
        return nil unless /^odblac:(.*)$/ =~ label
        NormalText.new("&\#x00151;")
      end

      # =capital OE ligature 
      def ext_inline_verb_OElig(label, content, visitor)
        label = label.to_s
        return nil unless /^OElig:(.*)$/ =~ label
        NormalText.new("&\#x00152;")
      end

      # =small oe ligature 
      def ext_inline_verb_oelig(label, content, visitor)
        label = label.to_s
        return nil unless /^oelig:(.*)$/ =~ label
        NormalText.new("&\#x00153;")
      end

      # =capital O, macron 
      def ext_inline_verb_Omacr(label, content, visitor)
        label = label.to_s
        return nil unless /^Omacr:(.*)$/ =~ label
        NormalText.new("&\#x0014C;")
      end

      # =small o, macron 
      def ext_inline_verb_omacr(label, content, visitor)
        label = label.to_s
        return nil unless /^omacr:(.*)$/ =~ label
        NormalText.new("&\#x0014D;")
      end

      # =capital R, acute accent 
      def ext_inline_verb_Racute(label, content, visitor)
        label = label.to_s
        return nil unless /^Racute:(.*)$/ =~ label
        NormalText.new("&\#x00154;")
      end

      # =small r, acute accent 
      def ext_inline_verb_racute(label, content, visitor)
        label = label.to_s
        return nil unless /^racute:(.*)$/ =~ label
        NormalText.new("&\#x00155;")
      end

      # =capital R, caron 
      def ext_inline_verb_Rcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Rcaron:(.*)$/ =~ label
        NormalText.new("&\#x00158;")
      end

      # =small r, caron 
      def ext_inline_verb_rcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^rcaron:(.*)$/ =~ label
        NormalText.new("&\#x00159;")
      end

      # =capital R, cedilla 
      def ext_inline_verb_Rcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Rcedil:(.*)$/ =~ label
        NormalText.new("&\#x00156;")
      end

      # =small r, cedilla 
      def ext_inline_verb_rcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^rcedil:(.*)$/ =~ label
        NormalText.new("&\#x00157;")
      end

      # =capital S, acute accent 
      def ext_inline_verb_Sacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Sacute:(.*)$/ =~ label
        NormalText.new("&\#x0015A;")
      end

      # =small s, acute accent 
      def ext_inline_verb_sacute(label, content, visitor)
        label = label.to_s
        return nil unless /^sacute:(.*)$/ =~ label
        NormalText.new("&\#x0015B;")
      end

      # =capital S, caron 
      def ext_inline_verb_Scaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Scaron:(.*)$/ =~ label
        NormalText.new("&\#x00160;")
      end

      # =small s, caron 
      def ext_inline_verb_scaron(label, content, visitor)
        label = label.to_s
        return nil unless /^scaron:(.*)$/ =~ label
        NormalText.new("&\#x00161;")
      end

      # =capital S, cedilla 
      def ext_inline_verb_Scedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Scedil:(.*)$/ =~ label
        NormalText.new("&\#x0015E;")
      end

      # =small s, cedilla 
      def ext_inline_verb_scedil(label, content, visitor)
        label = label.to_s
        return nil unless /^scedil:(.*)$/ =~ label
        NormalText.new("&\#x0015F;")
      end

      # =capital S, circumflex accent 
      def ext_inline_verb_Scirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Scirc:(.*)$/ =~ label
        NormalText.new("&\#x0015C;")
      end

      # =small s, circumflex accent 
      def ext_inline_verb_scirc(label, content, visitor)
        label = label.to_s
        return nil unless /^scirc:(.*)$/ =~ label
        NormalText.new("&\#x0015D;")
      end

      # =capital T, caron 
      def ext_inline_verb_Tcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Tcaron:(.*)$/ =~ label
        NormalText.new("&\#x00164;")
      end

      # =small t, caron 
      def ext_inline_verb_tcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^tcaron:(.*)$/ =~ label
        NormalText.new("&\#x00165;")
      end

      # =capital T, cedilla 
      def ext_inline_verb_Tcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Tcedil:(.*)$/ =~ label
        NormalText.new("&\#x00162;")
      end

      # =small t, cedilla 
      def ext_inline_verb_tcedil(label, content, visitor)
        label = label.to_s
        return nil unless /^tcedil:(.*)$/ =~ label
        NormalText.new("&\#x00163;")
      end

      # =capital T, stroke 
      def ext_inline_verb_Tstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^Tstrok:(.*)$/ =~ label
        NormalText.new("&\#x00166;")
      end

      # =small t, stroke 
      def ext_inline_verb_tstrok(label, content, visitor)
        label = label.to_s
        return nil unless /^tstrok:(.*)$/ =~ label
        NormalText.new("&\#x00167;")
      end

      # =capital U, breve 
      def ext_inline_verb_Ubreve(label, content, visitor)
        label = label.to_s
        return nil unless /^Ubreve:(.*)$/ =~ label
        NormalText.new("&\#x0016C;")
      end

      # =small u, breve 
      def ext_inline_verb_ubreve(label, content, visitor)
        label = label.to_s
        return nil unless /^ubreve:(.*)$/ =~ label
        NormalText.new("&\#x0016D;")
      end

      # =capital U, double acute accent 
      def ext_inline_verb_Udblac(label, content, visitor)
        label = label.to_s
        return nil unless /^Udblac:(.*)$/ =~ label
        NormalText.new("&\#x00170;")
      end

      # =small u, double acute accent 
      def ext_inline_verb_udblac(label, content, visitor)
        label = label.to_s
        return nil unless /^udblac:(.*)$/ =~ label
        NormalText.new("&\#x00171;")
      end

      # =capital U, macron 
      def ext_inline_verb_Umacr(label, content, visitor)
        label = label.to_s
        return nil unless /^Umacr:(.*)$/ =~ label
        NormalText.new("&\#x0016A;")
      end

      # =small u, macron 
      def ext_inline_verb_umacr(label, content, visitor)
        label = label.to_s
        return nil unless /^umacr:(.*)$/ =~ label
        NormalText.new("&\#x0016B;")
      end

      # =capital U, ogonek 
      def ext_inline_verb_Uogon(label, content, visitor)
        label = label.to_s
        return nil unless /^Uogon:(.*)$/ =~ label
        NormalText.new("&\#x00172;")
      end

      # =small u, ogonek 
      def ext_inline_verb_uogon(label, content, visitor)
        label = label.to_s
        return nil unless /^uogon:(.*)$/ =~ label
        NormalText.new("&\#x00173;")
      end

      # =capital U, ring 
      def ext_inline_verb_Uring(label, content, visitor)
        label = label.to_s
        return nil unless /^Uring:(.*)$/ =~ label
        NormalText.new("&\#x0016E;")
      end

      # =small u, ring 
      def ext_inline_verb_uring(label, content, visitor)
        label = label.to_s
        return nil unless /^uring:(.*)$/ =~ label
        NormalText.new("&\#x0016F;")
      end

      # =capital U, tilde 
      def ext_inline_verb_Utilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Utilde:(.*)$/ =~ label
        NormalText.new("&\#x00168;")
      end

      # =small u, tilde 
      def ext_inline_verb_utilde(label, content, visitor)
        label = label.to_s
        return nil unless /^utilde:(.*)$/ =~ label
        NormalText.new("&\#x00169;")
      end

      # =capital W, circumflex accent 
      def ext_inline_verb_Wcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Wcirc:(.*)$/ =~ label
        NormalText.new("&\#x00174;")
      end

      # =small w, circumflex accent 
      def ext_inline_verb_wcirc(label, content, visitor)
        label = label.to_s
        return nil unless /^wcirc:(.*)$/ =~ label
        NormalText.new("&\#x00175;")
      end

      # =capital Y, circumflex accent 
      def ext_inline_verb_Ycirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Ycirc:(.*)$/ =~ label
        NormalText.new("&\#x00176;")
      end

      # =small y, circumflex accent 
      def ext_inline_verb_ycirc(label, content, visitor)
        label = label.to_s
        return nil unless /^ycirc:(.*)$/ =~ label
        NormalText.new("&\#x00177;")
      end

      # =capital Y, dieresis or umlaut mark 
      def ext_inline_verb_Yuml(label, content, visitor)
        label = label.to_s
        return nil unless /^Yuml:(.*)$/ =~ label
        NormalText.new("&\#x00178;")
      end

      # =capital Z, acute accent 
      def ext_inline_verb_Zacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Zacute:(.*)$/ =~ label
        NormalText.new("&\#x00179;")
      end

      # =small z, acute accent 
      def ext_inline_verb_zacute(label, content, visitor)
        label = label.to_s
        return nil unless /^zacute:(.*)$/ =~ label
        NormalText.new("&\#x0017A;")
      end

      # =capital Z, caron 
      def ext_inline_verb_Zcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^Zcaron:(.*)$/ =~ label
        NormalText.new("&\#x0017D;")
      end

      # =small z, caron 
      def ext_inline_verb_zcaron(label, content, visitor)
        label = label.to_s
        return nil unless /^zcaron:(.*)$/ =~ label
        NormalText.new("&\#x0017E;")
      end

      # =capital Z, dot above 
      def ext_inline_verb_Zdot(label, content, visitor)
        label = label.to_s
        return nil unless /^Zdot:(.*)$/ =~ label
        NormalText.new("&\#x0017B;")
      end

      # =small z, dot above 
      def ext_inline_verb_zdot(label, content, visitor)
        label = label.to_s
        return nil unless /^zdot:(.*)$/ =~ label
        NormalText.new("&\#x0017C;")
      end

    end
  end
end
