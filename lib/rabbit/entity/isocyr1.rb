require 'rabbit/element'

module Rabbit
  module Entity
    module Isocyr1

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =capital A, Cyrillic 
      def ext_inline_verb_Acy(label, content, visitor)
        label = label.to_s
        return nil unless /^Acy:(.*)$/ =~ label
        NormalText.new("&\#x00410;")
      end

      # =small a, Cyrillic 
      def ext_inline_verb_acy(label, content, visitor)
        label = label.to_s
        return nil unless /^acy:(.*)$/ =~ label
        NormalText.new("&\#x00430;")
      end

      # =capital BE, Cyrillic 
      def ext_inline_verb_Bcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Bcy:(.*)$/ =~ label
        NormalText.new("&\#x00411;")
      end

      # =small be, Cyrillic 
      def ext_inline_verb_bcy(label, content, visitor)
        label = label.to_s
        return nil unless /^bcy:(.*)$/ =~ label
        NormalText.new("&\#x00431;")
      end

      # =capital CHE, Cyrillic 
      def ext_inline_verb_CHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^CHcy:(.*)$/ =~ label
        NormalText.new("&\#x00427;")
      end

      # =small che, Cyrillic 
      def ext_inline_verb_chcy(label, content, visitor)
        label = label.to_s
        return nil unless /^chcy:(.*)$/ =~ label
        NormalText.new("&\#x00447;")
      end

      # =capital DE, Cyrillic 
      def ext_inline_verb_Dcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Dcy:(.*)$/ =~ label
        NormalText.new("&\#x00414;")
      end

      # =small de, Cyrillic 
      def ext_inline_verb_dcy(label, content, visitor)
        label = label.to_s
        return nil unless /^dcy:(.*)$/ =~ label
        NormalText.new("&\#x00434;")
      end

      # =capital E, Cyrillic 
      def ext_inline_verb_Ecy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ecy:(.*)$/ =~ label
        NormalText.new("&\#x0042D;")
      end

      # =small e, Cyrillic 
      def ext_inline_verb_ecy(label, content, visitor)
        label = label.to_s
        return nil unless /^ecy:(.*)$/ =~ label
        NormalText.new("&\#x0044D;")
      end

      # =capital EF, Cyrillic 
      def ext_inline_verb_Fcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Fcy:(.*)$/ =~ label
        NormalText.new("&\#x00424;")
      end

      # =small ef, Cyrillic 
      def ext_inline_verb_fcy(label, content, visitor)
        label = label.to_s
        return nil unless /^fcy:(.*)$/ =~ label
        NormalText.new("&\#x00444;")
      end

      # =capital GHE, Cyrillic 
      def ext_inline_verb_Gcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Gcy:(.*)$/ =~ label
        NormalText.new("&\#x00413;")
      end

      # =small ghe, Cyrillic 
      def ext_inline_verb_gcy(label, content, visitor)
        label = label.to_s
        return nil unless /^gcy:(.*)$/ =~ label
        NormalText.new("&\#x00433;")
      end

      # =capital HARD sign, Cyrillic 
      def ext_inline_verb_HARDcy(label, content, visitor)
        label = label.to_s
        return nil unless /^HARDcy:(.*)$/ =~ label
        NormalText.new("&\#x0042A;")
      end

      # =small hard sign, Cyrillic 
      def ext_inline_verb_hardcy(label, content, visitor)
        label = label.to_s
        return nil unless /^hardcy:(.*)$/ =~ label
        NormalText.new("&\#x0044A;")
      end

      # =capital I, Cyrillic 
      def ext_inline_verb_Icy(label, content, visitor)
        label = label.to_s
        return nil unless /^Icy:(.*)$/ =~ label
        NormalText.new("&\#x00418;")
      end

      # =small i, Cyrillic 
      def ext_inline_verb_icy(label, content, visitor)
        label = label.to_s
        return nil unless /^icy:(.*)$/ =~ label
        NormalText.new("&\#x00438;")
      end

      # =capital IE, Cyrillic 
      def ext_inline_verb_IEcy(label, content, visitor)
        label = label.to_s
        return nil unless /^IEcy:(.*)$/ =~ label
        NormalText.new("&\#x00415;")
      end

      # =small ie, Cyrillic 
      def ext_inline_verb_iecy(label, content, visitor)
        label = label.to_s
        return nil unless /^iecy:(.*)$/ =~ label
        NormalText.new("&\#x00435;")
      end

      # =capital IO, Russian 
      def ext_inline_verb_IOcy(label, content, visitor)
        label = label.to_s
        return nil unless /^IOcy:(.*)$/ =~ label
        NormalText.new("&\#x00401;")
      end

      # =small io, Russian 
      def ext_inline_verb_iocy(label, content, visitor)
        label = label.to_s
        return nil unless /^iocy:(.*)$/ =~ label
        NormalText.new("&\#x00451;")
      end

      # =capital short I, Cyrillic 
      def ext_inline_verb_Jcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Jcy:(.*)$/ =~ label
        NormalText.new("&\#x00419;")
      end

      # =small short i, Cyrillic 
      def ext_inline_verb_jcy(label, content, visitor)
        label = label.to_s
        return nil unless /^jcy:(.*)$/ =~ label
        NormalText.new("&\#x00439;")
      end

      # =capital KA, Cyrillic 
      def ext_inline_verb_Kcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Kcy:(.*)$/ =~ label
        NormalText.new("&\#x0041A;")
      end

      # =small ka, Cyrillic 
      def ext_inline_verb_kcy(label, content, visitor)
        label = label.to_s
        return nil unless /^kcy:(.*)$/ =~ label
        NormalText.new("&\#x0043A;")
      end

      # =capital HA, Cyrillic 
      def ext_inline_verb_KHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^KHcy:(.*)$/ =~ label
        NormalText.new("&\#x00425;")
      end

      # =small ha, Cyrillic 
      def ext_inline_verb_khcy(label, content, visitor)
        label = label.to_s
        return nil unless /^khcy:(.*)$/ =~ label
        NormalText.new("&\#x00445;")
      end

      # =capital EL, Cyrillic 
      def ext_inline_verb_Lcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Lcy:(.*)$/ =~ label
        NormalText.new("&\#x0041B;")
      end

      # =small el, Cyrillic 
      def ext_inline_verb_lcy(label, content, visitor)
        label = label.to_s
        return nil unless /^lcy:(.*)$/ =~ label
        NormalText.new("&\#x0043B;")
      end

      # =capital EM, Cyrillic 
      def ext_inline_verb_Mcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Mcy:(.*)$/ =~ label
        NormalText.new("&\#x0041C;")
      end

      # =small em, Cyrillic 
      def ext_inline_verb_mcy(label, content, visitor)
        label = label.to_s
        return nil unless /^mcy:(.*)$/ =~ label
        NormalText.new("&\#x0043C;")
      end

      # =capital EN, Cyrillic 
      def ext_inline_verb_Ncy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ncy:(.*)$/ =~ label
        NormalText.new("&\#x0041D;")
      end

      # =small en, Cyrillic 
      def ext_inline_verb_ncy(label, content, visitor)
        label = label.to_s
        return nil unless /^ncy:(.*)$/ =~ label
        NormalText.new("&\#x0043D;")
      end

      # =numero sign 
      def ext_inline_verb_numero(label, content, visitor)
        label = label.to_s
        return nil unless /^numero:(.*)$/ =~ label
        NormalText.new("&\#x02116;")
      end

      # =capital O, Cyrillic 
      def ext_inline_verb_Ocy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ocy:(.*)$/ =~ label
        NormalText.new("&\#x0041E;")
      end

      # =small o, Cyrillic 
      def ext_inline_verb_ocy(label, content, visitor)
        label = label.to_s
        return nil unless /^ocy:(.*)$/ =~ label
        NormalText.new("&\#x0043E;")
      end

      # =capital PE, Cyrillic 
      def ext_inline_verb_Pcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Pcy:(.*)$/ =~ label
        NormalText.new("&\#x0041F;")
      end

      # =small pe, Cyrillic 
      def ext_inline_verb_pcy(label, content, visitor)
        label = label.to_s
        return nil unless /^pcy:(.*)$/ =~ label
        NormalText.new("&\#x0043F;")
      end

      # =capital ER, Cyrillic 
      def ext_inline_verb_Rcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Rcy:(.*)$/ =~ label
        NormalText.new("&\#x00420;")
      end

      # =small er, Cyrillic 
      def ext_inline_verb_rcy(label, content, visitor)
        label = label.to_s
        return nil unless /^rcy:(.*)$/ =~ label
        NormalText.new("&\#x00440;")
      end

      # =capital ES, Cyrillic 
      def ext_inline_verb_Scy(label, content, visitor)
        label = label.to_s
        return nil unless /^Scy:(.*)$/ =~ label
        NormalText.new("&\#x00421;")
      end

      # =small es, Cyrillic 
      def ext_inline_verb_scy(label, content, visitor)
        label = label.to_s
        return nil unless /^scy:(.*)$/ =~ label
        NormalText.new("&\#x00441;")
      end

      # =capital SHCHA, Cyrillic 
      def ext_inline_verb_SHCHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^SHCHcy:(.*)$/ =~ label
        NormalText.new("&\#x00429;")
      end

      # =small shcha, Cyrillic 
      def ext_inline_verb_shchcy(label, content, visitor)
        label = label.to_s
        return nil unless /^shchcy:(.*)$/ =~ label
        NormalText.new("&\#x00449;")
      end

      # =capital SHA, Cyrillic 
      def ext_inline_verb_SHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^SHcy:(.*)$/ =~ label
        NormalText.new("&\#x00428;")
      end

      # =small sha, Cyrillic 
      def ext_inline_verb_shcy(label, content, visitor)
        label = label.to_s
        return nil unless /^shcy:(.*)$/ =~ label
        NormalText.new("&\#x00448;")
      end

      # =capital SOFT sign, Cyrillic 
      def ext_inline_verb_SOFTcy(label, content, visitor)
        label = label.to_s
        return nil unless /^SOFTcy:(.*)$/ =~ label
        NormalText.new("&\#x0042C;")
      end

      # =small soft sign, Cyrillic 
      def ext_inline_verb_softcy(label, content, visitor)
        label = label.to_s
        return nil unless /^softcy:(.*)$/ =~ label
        NormalText.new("&\#x0044C;")
      end

      # =capital TE, Cyrillic 
      def ext_inline_verb_Tcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Tcy:(.*)$/ =~ label
        NormalText.new("&\#x00422;")
      end

      # =small te, Cyrillic 
      def ext_inline_verb_tcy(label, content, visitor)
        label = label.to_s
        return nil unless /^tcy:(.*)$/ =~ label
        NormalText.new("&\#x00442;")
      end

      # =capital TSE, Cyrillic 
      def ext_inline_verb_TScy(label, content, visitor)
        label = label.to_s
        return nil unless /^TScy:(.*)$/ =~ label
        NormalText.new("&\#x00426;")
      end

      # =small tse, Cyrillic 
      def ext_inline_verb_tscy(label, content, visitor)
        label = label.to_s
        return nil unless /^tscy:(.*)$/ =~ label
        NormalText.new("&\#x00446;")
      end

      # =capital U, Cyrillic 
      def ext_inline_verb_Ucy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ucy:(.*)$/ =~ label
        NormalText.new("&\#x00423;")
      end

      # =small u, Cyrillic 
      def ext_inline_verb_ucy(label, content, visitor)
        label = label.to_s
        return nil unless /^ucy:(.*)$/ =~ label
        NormalText.new("&\#x00443;")
      end

      # =capital VE, Cyrillic 
      def ext_inline_verb_Vcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Vcy:(.*)$/ =~ label
        NormalText.new("&\#x00412;")
      end

      # =small ve, Cyrillic 
      def ext_inline_verb_vcy(label, content, visitor)
        label = label.to_s
        return nil unless /^vcy:(.*)$/ =~ label
        NormalText.new("&\#x00432;")
      end

      # =capital YA, Cyrillic 
      def ext_inline_verb_YAcy(label, content, visitor)
        label = label.to_s
        return nil unless /^YAcy:(.*)$/ =~ label
        NormalText.new("&\#x0042F;")
      end

      # =small ya, Cyrillic 
      def ext_inline_verb_yacy(label, content, visitor)
        label = label.to_s
        return nil unless /^yacy:(.*)$/ =~ label
        NormalText.new("&\#x0044F;")
      end

      # =capital YERU, Cyrillic 
      def ext_inline_verb_Ycy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ycy:(.*)$/ =~ label
        NormalText.new("&\#x0042B;")
      end

      # =small yeru, Cyrillic 
      def ext_inline_verb_ycy(label, content, visitor)
        label = label.to_s
        return nil unless /^ycy:(.*)$/ =~ label
        NormalText.new("&\#x0044B;")
      end

      # =capital YU, Cyrillic 
      def ext_inline_verb_YUcy(label, content, visitor)
        label = label.to_s
        return nil unless /^YUcy:(.*)$/ =~ label
        NormalText.new("&\#x0042E;")
      end

      # =small yu, Cyrillic 
      def ext_inline_verb_yucy(label, content, visitor)
        label = label.to_s
        return nil unless /^yucy:(.*)$/ =~ label
        NormalText.new("&\#x0044E;")
      end

      # =capital ZE, Cyrillic 
      def ext_inline_verb_Zcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Zcy:(.*)$/ =~ label
        NormalText.new("&\#x00417;")
      end

      # =small ze, Cyrillic 
      def ext_inline_verb_zcy(label, content, visitor)
        label = label.to_s
        return nil unless /^zcy:(.*)$/ =~ label
        NormalText.new("&\#x00437;")
      end

      # =capital ZHE, Cyrillic 
      def ext_inline_verb_ZHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^ZHcy:(.*)$/ =~ label
        NormalText.new("&\#x00416;")
      end

      # =small zhe, Cyrillic 
      def ext_inline_verb_zhcy(label, content, visitor)
        label = label.to_s
        return nil unless /^zhcy:(.*)$/ =~ label
        NormalText.new("&\#x00436;")
      end

    end
  end
end
