require 'rabbit/element'

module Rabbit
  module Entity
    module Isomscr

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /scr A, script letter A 
      def ext_inline_verb_Ascr(label, content, visitor)
        label = label.to_s
        return nil unless /^Ascr:(.*)$/ =~ label
        NormalText.new("&\#x1D49C;")
      end

      # /scr a, script letter a 
      def ext_inline_verb_ascr(label, content, visitor)
        label = label.to_s
        return nil unless /^ascr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B6;")
      end

      # /scr B, script letter B 
      def ext_inline_verb_Bscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Bscr:(.*)$/ =~ label
        NormalText.new("&\#x0212C;")
      end

      # /scr b, script letter b 
      def ext_inline_verb_bscr(label, content, visitor)
        label = label.to_s
        return nil unless /^bscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B7;")
      end

      # /scr C, script letter C 
      def ext_inline_verb_Cscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Cscr:(.*)$/ =~ label
        NormalText.new("&\#x1D49E;")
      end

      # /scr c, script letter c 
      def ext_inline_verb_cscr(label, content, visitor)
        label = label.to_s
        return nil unless /^cscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B8;")
      end

      # /scr D, script letter D 
      def ext_inline_verb_Dscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Dscr:(.*)$/ =~ label
        NormalText.new("&\#x1D49F;")
      end

      # /scr d, script letter d 
      def ext_inline_verb_dscr(label, content, visitor)
        label = label.to_s
        return nil unless /^dscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B9;")
      end

      # /scr E, script letter E 
      def ext_inline_verb_Escr(label, content, visitor)
        label = label.to_s
        return nil unless /^Escr:(.*)$/ =~ label
        NormalText.new("&\#x02130;")
      end

      # /scr e, script letter e 
      def ext_inline_verb_escr(label, content, visitor)
        label = label.to_s
        return nil unless /^escr:(.*)$/ =~ label
        NormalText.new("&\#x0212F;")
      end

      # /scr F, script letter F 
      def ext_inline_verb_Fscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Fscr:(.*)$/ =~ label
        NormalText.new("&\#x02131;")
      end

      # /scr f, script letter f 
      def ext_inline_verb_fscr(label, content, visitor)
        label = label.to_s
        return nil unless /^fscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4BB;")
      end

      # /scr G, script letter G 
      def ext_inline_verb_Gscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Gscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4A2;")
      end

      # /scr g, script letter g 
      def ext_inline_verb_gscr(label, content, visitor)
        label = label.to_s
        return nil unless /^gscr:(.*)$/ =~ label
        NormalText.new("&\#x0210A;")
      end

      # /scr H, script letter H 
      def ext_inline_verb_Hscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Hscr:(.*)$/ =~ label
        NormalText.new("&\#x0210B;")
      end

      # /scr h, script letter h 
      def ext_inline_verb_hscr(label, content, visitor)
        label = label.to_s
        return nil unless /^hscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4BD;")
      end

      # /scr I, script letter I 
      def ext_inline_verb_Iscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Iscr:(.*)$/ =~ label
        NormalText.new("&\#x02110;")
      end

      # /scr i, script letter i 
      def ext_inline_verb_iscr(label, content, visitor)
        label = label.to_s
        return nil unless /^iscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4BE;")
      end

      # /scr J, script letter J 
      def ext_inline_verb_Jscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Jscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4A5;")
      end

      # /scr j, script letter j 
      def ext_inline_verb_jscr(label, content, visitor)
        label = label.to_s
        return nil unless /^jscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4BF;")
      end

      # /scr K, script letter K 
      def ext_inline_verb_Kscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Kscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4A6;")
      end

      # /scr k, script letter k 
      def ext_inline_verb_kscr(label, content, visitor)
        label = label.to_s
        return nil unless /^kscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C0;")
      end

      # /scr L, script letter L 
      def ext_inline_verb_Lscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Lscr:(.*)$/ =~ label
        NormalText.new("&\#x02112;")
      end

      # /scr l, script letter l 
      def ext_inline_verb_lscr(label, content, visitor)
        label = label.to_s
        return nil unless /^lscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C1;")
      end

      # /scr M, script letter M 
      def ext_inline_verb_Mscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Mscr:(.*)$/ =~ label
        NormalText.new("&\#x02133;")
      end

      # /scr m, script letter m 
      def ext_inline_verb_mscr(label, content, visitor)
        label = label.to_s
        return nil unless /^mscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C2;")
      end

      # /scr N, script letter N 
      def ext_inline_verb_Nscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Nscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4A9;")
      end

      # /scr n, script letter n 
      def ext_inline_verb_nscr(label, content, visitor)
        label = label.to_s
        return nil unless /^nscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C3;")
      end

      # /scr O, script letter O 
      def ext_inline_verb_Oscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Oscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4AA;")
      end

      # /scr o, script letter o 
      def ext_inline_verb_oscr(label, content, visitor)
        label = label.to_s
        return nil unless /^oscr:(.*)$/ =~ label
        NormalText.new("&\#x02134;")
      end

      # /scr P, script letter P 
      def ext_inline_verb_Pscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Pscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4AB;")
      end

      # /scr p, script letter p 
      def ext_inline_verb_pscr(label, content, visitor)
        label = label.to_s
        return nil unless /^pscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C5;")
      end

      # /scr Q, script letter Q 
      def ext_inline_verb_Qscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Qscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4AC;")
      end

      # /scr q, script letter q 
      def ext_inline_verb_qscr(label, content, visitor)
        label = label.to_s
        return nil unless /^qscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C6;")
      end

      # /scr R, script letter R 
      def ext_inline_verb_Rscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Rscr:(.*)$/ =~ label
        NormalText.new("&\#x0211B;")
      end

      # /scr r, script letter r 
      def ext_inline_verb_rscr(label, content, visitor)
        label = label.to_s
        return nil unless /^rscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C7;")
      end

      # /scr S, script letter S 
      def ext_inline_verb_Sscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Sscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4AE;")
      end

      # /scr s, script letter s 
      def ext_inline_verb_sscr(label, content, visitor)
        label = label.to_s
        return nil unless /^sscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C8;")
      end

      # /scr T, script letter T 
      def ext_inline_verb_Tscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Tscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4AF;")
      end

      # /scr t, script letter t 
      def ext_inline_verb_tscr(label, content, visitor)
        label = label.to_s
        return nil unless /^tscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4C9;")
      end

      # /scr U, script letter U 
      def ext_inline_verb_Uscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Uscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B0;")
      end

      # /scr u, script letter u 
      def ext_inline_verb_uscr(label, content, visitor)
        label = label.to_s
        return nil unless /^uscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CA;")
      end

      # /scr V, script letter V 
      def ext_inline_verb_Vscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Vscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B1;")
      end

      # /scr v, script letter v 
      def ext_inline_verb_vscr(label, content, visitor)
        label = label.to_s
        return nil unless /^vscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CB;")
      end

      # /scr W, script letter W 
      def ext_inline_verb_Wscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Wscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B2;")
      end

      # /scr w, script letter w 
      def ext_inline_verb_wscr(label, content, visitor)
        label = label.to_s
        return nil unless /^wscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CC;")
      end

      # /scr X, script letter X 
      def ext_inline_verb_Xscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Xscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B3;")
      end

      # /scr x, script letter x 
      def ext_inline_verb_xscr(label, content, visitor)
        label = label.to_s
        return nil unless /^xscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CD;")
      end

      # /scr Y, script letter Y 
      def ext_inline_verb_Yscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Yscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B4;")
      end

      # /scr y, script letter y 
      def ext_inline_verb_yscr(label, content, visitor)
        label = label.to_s
        return nil unless /^yscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CE;")
      end

      # /scr Z, script letter Z 
      def ext_inline_verb_Zscr(label, content, visitor)
        label = label.to_s
        return nil unless /^Zscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4B5;")
      end

      # /scr z, script letter z 
      def ext_inline_verb_zscr(label, content, visitor)
        label = label.to_s
        return nil unless /^zscr:(.*)$/ =~ label
        NormalText.new("&\#x1D4CF;")
      end

    end
  end
end
