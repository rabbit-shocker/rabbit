require 'rabbit/element'

module Rabbit
  module Entity
    module Isomfrk

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /frak A, upper case a 
      def ext_inline_verb_Afr(label, content, visitor)
        label = label.to_s
        return nil unless /^Afr:(.*)$/ =~ label
        NormalText.new("&\#x1D504;")
      end

      # /frak a, lower case a 
      def ext_inline_verb_afr(label, content, visitor)
        label = label.to_s
        return nil unless /^afr:(.*)$/ =~ label
        NormalText.new("&\#x1D51E;")
      end

      # /frak B, upper case b 
      def ext_inline_verb_Bfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Bfr:(.*)$/ =~ label
        NormalText.new("&\#x1D505;")
      end

      # /frak b, lower case b 
      def ext_inline_verb_bfr(label, content, visitor)
        label = label.to_s
        return nil unless /^bfr:(.*)$/ =~ label
        NormalText.new("&\#x1D51F;")
      end

      # /frak C, upper case c 
      def ext_inline_verb_Cfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Cfr:(.*)$/ =~ label
        NormalText.new("&\#x0212D;")
      end

      # /frak c, lower case c 
      def ext_inline_verb_cfr(label, content, visitor)
        label = label.to_s
        return nil unless /^cfr:(.*)$/ =~ label
        NormalText.new("&\#x1D520;")
      end

      # /frak D, upper case d 
      def ext_inline_verb_Dfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Dfr:(.*)$/ =~ label
        NormalText.new("&\#x1D507;")
      end

      # /frak d, lower case d 
      def ext_inline_verb_dfr(label, content, visitor)
        label = label.to_s
        return nil unless /^dfr:(.*)$/ =~ label
        NormalText.new("&\#x1D521;")
      end

      # /frak E, upper case e 
      def ext_inline_verb_Efr(label, content, visitor)
        label = label.to_s
        return nil unless /^Efr:(.*)$/ =~ label
        NormalText.new("&\#x1D508;")
      end

      # /frak e, lower case e 
      def ext_inline_verb_efr(label, content, visitor)
        label = label.to_s
        return nil unless /^efr:(.*)$/ =~ label
        NormalText.new("&\#x1D522;")
      end

      # /frak F, upper case f 
      def ext_inline_verb_Ffr(label, content, visitor)
        label = label.to_s
        return nil unless /^Ffr:(.*)$/ =~ label
        NormalText.new("&\#x1D509;")
      end

      # /frak f, lower case f 
      def ext_inline_verb_ffr(label, content, visitor)
        label = label.to_s
        return nil unless /^ffr:(.*)$/ =~ label
        NormalText.new("&\#x1D523;")
      end

      # /frak G, upper case g 
      def ext_inline_verb_Gfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Gfr:(.*)$/ =~ label
        NormalText.new("&\#x1D50A;")
      end

      # /frak g, lower case g 
      def ext_inline_verb_gfr(label, content, visitor)
        label = label.to_s
        return nil unless /^gfr:(.*)$/ =~ label
        NormalText.new("&\#x1D524;")
      end

      # /frak H, upper case h 
      def ext_inline_verb_Hfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Hfr:(.*)$/ =~ label
        NormalText.new("&\#x0210C;")
      end

      # /frak h, lower case h 
      def ext_inline_verb_hfr(label, content, visitor)
        label = label.to_s
        return nil unless /^hfr:(.*)$/ =~ label
        NormalText.new("&\#x1D525;")
      end

      # /frak I, upper case i 
      def ext_inline_verb_Ifr(label, content, visitor)
        label = label.to_s
        return nil unless /^Ifr:(.*)$/ =~ label
        NormalText.new("&\#x02111;")
      end

      # /frak i, lower case i 
      def ext_inline_verb_ifr(label, content, visitor)
        label = label.to_s
        return nil unless /^ifr:(.*)$/ =~ label
        NormalText.new("&\#x1D526;")
      end

      # /frak J, upper case j 
      def ext_inline_verb_Jfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Jfr:(.*)$/ =~ label
        NormalText.new("&\#x1D50D;")
      end

      # /frak j, lower case j 
      def ext_inline_verb_jfr(label, content, visitor)
        label = label.to_s
        return nil unless /^jfr:(.*)$/ =~ label
        NormalText.new("&\#x1D527;")
      end

      # /frak K, upper case k 
      def ext_inline_verb_Kfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Kfr:(.*)$/ =~ label
        NormalText.new("&\#x1D50E;")
      end

      # /frak k, lower case k 
      def ext_inline_verb_kfr(label, content, visitor)
        label = label.to_s
        return nil unless /^kfr:(.*)$/ =~ label
        NormalText.new("&\#x1D528;")
      end

      # /frak L, upper case l 
      def ext_inline_verb_Lfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Lfr:(.*)$/ =~ label
        NormalText.new("&\#x1D50F;")
      end

      # /frak l, lower case l 
      def ext_inline_verb_lfr(label, content, visitor)
        label = label.to_s
        return nil unless /^lfr:(.*)$/ =~ label
        NormalText.new("&\#x1D529;")
      end

      # /frak M, upper case m 
      def ext_inline_verb_Mfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Mfr:(.*)$/ =~ label
        NormalText.new("&\#x1D510;")
      end

      # /frak m, lower case m 
      def ext_inline_verb_mfr(label, content, visitor)
        label = label.to_s
        return nil unless /^mfr:(.*)$/ =~ label
        NormalText.new("&\#x1D52A;")
      end

      # /frak N, upper case n 
      def ext_inline_verb_Nfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Nfr:(.*)$/ =~ label
        NormalText.new("&\#x1D511;")
      end

      # /frak n, lower case n 
      def ext_inline_verb_nfr(label, content, visitor)
        label = label.to_s
        return nil unless /^nfr:(.*)$/ =~ label
        NormalText.new("&\#x1D52B;")
      end

      # /frak O, upper case o 
      def ext_inline_verb_Ofr(label, content, visitor)
        label = label.to_s
        return nil unless /^Ofr:(.*)$/ =~ label
        NormalText.new("&\#x1D512;")
      end

      # /frak o, lower case o 
      def ext_inline_verb_ofr(label, content, visitor)
        label = label.to_s
        return nil unless /^ofr:(.*)$/ =~ label
        NormalText.new("&\#x1D52C;")
      end

      # /frak P, upper case p 
      def ext_inline_verb_Pfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Pfr:(.*)$/ =~ label
        NormalText.new("&\#x1D513;")
      end

      # /frak p, lower case p 
      def ext_inline_verb_pfr(label, content, visitor)
        label = label.to_s
        return nil unless /^pfr:(.*)$/ =~ label
        NormalText.new("&\#x1D52D;")
      end

      # /frak Q, upper case q 
      def ext_inline_verb_Qfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Qfr:(.*)$/ =~ label
        NormalText.new("&\#x1D514;")
      end

      # /frak q, lower case q 
      def ext_inline_verb_qfr(label, content, visitor)
        label = label.to_s
        return nil unless /^qfr:(.*)$/ =~ label
        NormalText.new("&\#x1D52E;")
      end

      # /frak R, upper case r 
      def ext_inline_verb_Rfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Rfr:(.*)$/ =~ label
        NormalText.new("&\#x0211C;")
      end

      # /frak r, lower case r 
      def ext_inline_verb_rfr(label, content, visitor)
        label = label.to_s
        return nil unless /^rfr:(.*)$/ =~ label
        NormalText.new("&\#x1D52F;")
      end

      # /frak S, upper case s 
      def ext_inline_verb_Sfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Sfr:(.*)$/ =~ label
        NormalText.new("&\#x1D516;")
      end

      # /frak s, lower case s 
      def ext_inline_verb_sfr(label, content, visitor)
        label = label.to_s
        return nil unless /^sfr:(.*)$/ =~ label
        NormalText.new("&\#x1D530;")
      end

      # /frak T, upper case t 
      def ext_inline_verb_Tfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Tfr:(.*)$/ =~ label
        NormalText.new("&\#x1D517;")
      end

      # /frak t, lower case t 
      def ext_inline_verb_tfr(label, content, visitor)
        label = label.to_s
        return nil unless /^tfr:(.*)$/ =~ label
        NormalText.new("&\#x1D531;")
      end

      # /frak U, upper case u 
      def ext_inline_verb_Ufr(label, content, visitor)
        label = label.to_s
        return nil unless /^Ufr:(.*)$/ =~ label
        NormalText.new("&\#x1D518;")
      end

      # /frak u, lower case u 
      def ext_inline_verb_ufr(label, content, visitor)
        label = label.to_s
        return nil unless /^ufr:(.*)$/ =~ label
        NormalText.new("&\#x1D532;")
      end

      # /frak V, upper case v 
      def ext_inline_verb_Vfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Vfr:(.*)$/ =~ label
        NormalText.new("&\#x1D519;")
      end

      # /frak v, lower case v 
      def ext_inline_verb_vfr(label, content, visitor)
        label = label.to_s
        return nil unless /^vfr:(.*)$/ =~ label
        NormalText.new("&\#x1D533;")
      end

      # /frak W, upper case w 
      def ext_inline_verb_Wfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Wfr:(.*)$/ =~ label
        NormalText.new("&\#x1D51A;")
      end

      # /frak w, lower case w 
      def ext_inline_verb_wfr(label, content, visitor)
        label = label.to_s
        return nil unless /^wfr:(.*)$/ =~ label
        NormalText.new("&\#x1D534;")
      end

      # /frak X, upper case x 
      def ext_inline_verb_Xfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Xfr:(.*)$/ =~ label
        NormalText.new("&\#x1D51B;")
      end

      # /frak x, lower case x 
      def ext_inline_verb_xfr(label, content, visitor)
        label = label.to_s
        return nil unless /^xfr:(.*)$/ =~ label
        NormalText.new("&\#x1D535;")
      end

      # /frak Y, upper case y 
      def ext_inline_verb_Yfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Yfr:(.*)$/ =~ label
        NormalText.new("&\#x1D51C;")
      end

      # /frak y, lower case y 
      def ext_inline_verb_yfr(label, content, visitor)
        label = label.to_s
        return nil unless /^yfr:(.*)$/ =~ label
        NormalText.new("&\#x1D536;")
      end

      # /frak Z, upper case z  
      def ext_inline_verb_Zfr(label, content, visitor)
        label = label.to_s
        return nil unless /^Zfr:(.*)$/ =~ label
        NormalText.new("&\#x02128;")
      end

      # /frak z, lower case z 
      def ext_inline_verb_zfr(label, content, visitor)
        label = label.to_s
        return nil unless /^zfr:(.*)$/ =~ label
        NormalText.new("&\#x1D537;")
      end

    end
  end
end
