require 'rabbit/element'

module Rabbit
  module Entity
    module Isolat1

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =capital A, acute accent 
      def ext_inline_verb_Aacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Aacute:(.*)$/ =~ label
        NormalText.new("&\#x000C1;")
      end

      # =small a, acute accent 
      def ext_inline_verb_aacute(label, content, visitor)
        label = label.to_s
        return nil unless /^aacute:(.*)$/ =~ label
        NormalText.new("&\#x000E1;")
      end

      # =capital A, circumflex accent 
      def ext_inline_verb_Acirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Acirc:(.*)$/ =~ label
        NormalText.new("&\#x000C2;")
      end

      # =small a, circumflex accent 
      def ext_inline_verb_acirc(label, content, visitor)
        label = label.to_s
        return nil unless /^acirc:(.*)$/ =~ label
        NormalText.new("&\#x000E2;")
      end

      # =capital AE diphthong (ligature) 
      def ext_inline_verb_AElig(label, content, visitor)
        label = label.to_s
        return nil unless /^AElig:(.*)$/ =~ label
        NormalText.new("&\#x000C6;")
      end

      # =small ae diphthong (ligature) 
      def ext_inline_verb_aelig(label, content, visitor)
        label = label.to_s
        return nil unless /^aelig:(.*)$/ =~ label
        NormalText.new("&\#x000E6;")
      end

      # =capital A, grave accent 
      def ext_inline_verb_Agrave(label, content, visitor)
        label = label.to_s
        return nil unless /^Agrave:(.*)$/ =~ label
        NormalText.new("&\#x000C0;")
      end

      # =small a, grave accent 
      def ext_inline_verb_agrave(label, content, visitor)
        label = label.to_s
        return nil unless /^agrave:(.*)$/ =~ label
        NormalText.new("&\#x000E0;")
      end

      # =capital A, ring 
      def ext_inline_verb_Aring(label, content, visitor)
        label = label.to_s
        return nil unless /^Aring:(.*)$/ =~ label
        NormalText.new("&\#x000C5;")
      end

      # =small a, ring 
      def ext_inline_verb_aring(label, content, visitor)
        label = label.to_s
        return nil unless /^aring:(.*)$/ =~ label
        NormalText.new("&\#x000E5;")
      end

      # =capital A, tilde 
      def ext_inline_verb_Atilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Atilde:(.*)$/ =~ label
        NormalText.new("&\#x000C3;")
      end

      # =small a, tilde 
      def ext_inline_verb_atilde(label, content, visitor)
        label = label.to_s
        return nil unless /^atilde:(.*)$/ =~ label
        NormalText.new("&\#x000E3;")
      end

      # =capital A, dieresis or umlaut mark 
      def ext_inline_verb_Auml(label, content, visitor)
        label = label.to_s
        return nil unless /^Auml:(.*)$/ =~ label
        NormalText.new("&\#x000C4;")
      end

      # =small a, dieresis or umlaut mark 
      def ext_inline_verb_auml(label, content, visitor)
        label = label.to_s
        return nil unless /^auml:(.*)$/ =~ label
        NormalText.new("&\#x000E4;")
      end

      # =capital C, cedilla 
      def ext_inline_verb_Ccedil(label, content, visitor)
        label = label.to_s
        return nil unless /^Ccedil:(.*)$/ =~ label
        NormalText.new("&\#x000C7;")
      end

      # =small c, cedilla 
      def ext_inline_verb_ccedil(label, content, visitor)
        label = label.to_s
        return nil unless /^ccedil:(.*)$/ =~ label
        NormalText.new("&\#x000E7;")
      end

      # =capital E, acute accent 
      def ext_inline_verb_Eacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Eacute:(.*)$/ =~ label
        NormalText.new("&\#x000C9;")
      end

      # =small e, acute accent 
      def ext_inline_verb_eacute(label, content, visitor)
        label = label.to_s
        return nil unless /^eacute:(.*)$/ =~ label
        NormalText.new("&\#x000E9;")
      end

      # =capital E, circumflex accent 
      def ext_inline_verb_Ecirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Ecirc:(.*)$/ =~ label
        NormalText.new("&\#x000CA;")
      end

      # =small e, circumflex accent 
      def ext_inline_verb_ecirc(label, content, visitor)
        label = label.to_s
        return nil unless /^ecirc:(.*)$/ =~ label
        NormalText.new("&\#x000EA;")
      end

      # =capital E, grave accent 
      def ext_inline_verb_Egrave(label, content, visitor)
        label = label.to_s
        return nil unless /^Egrave:(.*)$/ =~ label
        NormalText.new("&\#x000C8;")
      end

      # =small e, grave accent 
      def ext_inline_verb_egrave(label, content, visitor)
        label = label.to_s
        return nil unless /^egrave:(.*)$/ =~ label
        NormalText.new("&\#x000E8;")
      end

      # =capital Eth, Icelandic 
      def ext_inline_verb_ETH(label, content, visitor)
        label = label.to_s
        return nil unless /^ETH:(.*)$/ =~ label
        NormalText.new("&\#x000D0;")
      end

      # =small eth, Icelandic 
      def ext_inline_verb_eth(label, content, visitor)
        label = label.to_s
        return nil unless /^eth:(.*)$/ =~ label
        NormalText.new("&\#x000F0;")
      end

      # =capital E, dieresis or umlaut mark 
      def ext_inline_verb_Euml(label, content, visitor)
        label = label.to_s
        return nil unless /^Euml:(.*)$/ =~ label
        NormalText.new("&\#x000CB;")
      end

      # =small e, dieresis or umlaut mark 
      def ext_inline_verb_euml(label, content, visitor)
        label = label.to_s
        return nil unless /^euml:(.*)$/ =~ label
        NormalText.new("&\#x000EB;")
      end

      # =capital I, acute accent 
      def ext_inline_verb_Iacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Iacute:(.*)$/ =~ label
        NormalText.new("&\#x000CD;")
      end

      # =small i, acute accent 
      def ext_inline_verb_iacute(label, content, visitor)
        label = label.to_s
        return nil unless /^iacute:(.*)$/ =~ label
        NormalText.new("&\#x000ED;")
      end

      # =capital I, circumflex accent 
      def ext_inline_verb_Icirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Icirc:(.*)$/ =~ label
        NormalText.new("&\#x000CE;")
      end

      # =small i, circumflex accent 
      def ext_inline_verb_icirc(label, content, visitor)
        label = label.to_s
        return nil unless /^icirc:(.*)$/ =~ label
        NormalText.new("&\#x000EE;")
      end

      # =capital I, grave accent 
      def ext_inline_verb_Igrave(label, content, visitor)
        label = label.to_s
        return nil unless /^Igrave:(.*)$/ =~ label
        NormalText.new("&\#x000CC;")
      end

      # =small i, grave accent 
      def ext_inline_verb_igrave(label, content, visitor)
        label = label.to_s
        return nil unless /^igrave:(.*)$/ =~ label
        NormalText.new("&\#x000EC;")
      end

      # =capital I, dieresis or umlaut mark 
      def ext_inline_verb_Iuml(label, content, visitor)
        label = label.to_s
        return nil unless /^Iuml:(.*)$/ =~ label
        NormalText.new("&\#x000CF;")
      end

      # =small i, dieresis or umlaut mark 
      def ext_inline_verb_iuml(label, content, visitor)
        label = label.to_s
        return nil unless /^iuml:(.*)$/ =~ label
        NormalText.new("&\#x000EF;")
      end

      # =capital N, tilde 
      def ext_inline_verb_Ntilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Ntilde:(.*)$/ =~ label
        NormalText.new("&\#x000D1;")
      end

      # =small n, tilde 
      def ext_inline_verb_ntilde(label, content, visitor)
        label = label.to_s
        return nil unless /^ntilde:(.*)$/ =~ label
        NormalText.new("&\#x000F1;")
      end

      # =capital O, acute accent 
      def ext_inline_verb_Oacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Oacute:(.*)$/ =~ label
        NormalText.new("&\#x000D3;")
      end

      # =small o, acute accent 
      def ext_inline_verb_oacute(label, content, visitor)
        label = label.to_s
        return nil unless /^oacute:(.*)$/ =~ label
        NormalText.new("&\#x000F3;")
      end

      # =capital O, circumflex accent 
      def ext_inline_verb_Ocirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Ocirc:(.*)$/ =~ label
        NormalText.new("&\#x000D4;")
      end

      # =small o, circumflex accent 
      def ext_inline_verb_ocirc(label, content, visitor)
        label = label.to_s
        return nil unless /^ocirc:(.*)$/ =~ label
        NormalText.new("&\#x000F4;")
      end

      # =capital O, grave accent 
      def ext_inline_verb_Ograve(label, content, visitor)
        label = label.to_s
        return nil unless /^Ograve:(.*)$/ =~ label
        NormalText.new("&\#x000D2;")
      end

      # =small o, grave accent 
      def ext_inline_verb_ograve(label, content, visitor)
        label = label.to_s
        return nil unless /^ograve:(.*)$/ =~ label
        NormalText.new("&\#x000F2;")
      end

      # =capital O, slash 
      def ext_inline_verb_Oslash(label, content, visitor)
        label = label.to_s
        return nil unless /^Oslash:(.*)$/ =~ label
        NormalText.new("&\#x000D8;")
      end

      # latin small letter o with stroke 
      def ext_inline_verb_oslash(label, content, visitor)
        label = label.to_s
        return nil unless /^oslash:(.*)$/ =~ label
        NormalText.new("&\#x000F8;")
      end

      # =capital O, tilde 
      def ext_inline_verb_Otilde(label, content, visitor)
        label = label.to_s
        return nil unless /^Otilde:(.*)$/ =~ label
        NormalText.new("&\#x000D5;")
      end

      # =small o, tilde 
      def ext_inline_verb_otilde(label, content, visitor)
        label = label.to_s
        return nil unless /^otilde:(.*)$/ =~ label
        NormalText.new("&\#x000F5;")
      end

      # =capital O, dieresis or umlaut mark 
      def ext_inline_verb_Ouml(label, content, visitor)
        label = label.to_s
        return nil unless /^Ouml:(.*)$/ =~ label
        NormalText.new("&\#x000D6;")
      end

      # =small o, dieresis or umlaut mark 
      def ext_inline_verb_ouml(label, content, visitor)
        label = label.to_s
        return nil unless /^ouml:(.*)$/ =~ label
        NormalText.new("&\#x000F6;")
      end

      # =small sharp s, German (sz ligature) 
      def ext_inline_verb_szlig(label, content, visitor)
        label = label.to_s
        return nil unless /^szlig:(.*)$/ =~ label
        NormalText.new("&\#x000DF;")
      end

      # =capital THORN, Icelandic 
      def ext_inline_verb_THORN(label, content, visitor)
        label = label.to_s
        return nil unless /^THORN:(.*)$/ =~ label
        NormalText.new("&\#x000DE;")
      end

      # =small thorn, Icelandic 
      def ext_inline_verb_thorn(label, content, visitor)
        label = label.to_s
        return nil unless /^thorn:(.*)$/ =~ label
        NormalText.new("&\#x000FE;")
      end

      # =capital U, acute accent 
      def ext_inline_verb_Uacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Uacute:(.*)$/ =~ label
        NormalText.new("&\#x000DA;")
      end

      # =small u, acute accent 
      def ext_inline_verb_uacute(label, content, visitor)
        label = label.to_s
        return nil unless /^uacute:(.*)$/ =~ label
        NormalText.new("&\#x000FA;")
      end

      # =capital U, circumflex accent 
      def ext_inline_verb_Ucirc(label, content, visitor)
        label = label.to_s
        return nil unless /^Ucirc:(.*)$/ =~ label
        NormalText.new("&\#x000DB;")
      end

      # =small u, circumflex accent 
      def ext_inline_verb_ucirc(label, content, visitor)
        label = label.to_s
        return nil unless /^ucirc:(.*)$/ =~ label
        NormalText.new("&\#x000FB;")
      end

      # =capital U, grave accent 
      def ext_inline_verb_Ugrave(label, content, visitor)
        label = label.to_s
        return nil unless /^Ugrave:(.*)$/ =~ label
        NormalText.new("&\#x000D9;")
      end

      # =small u, grave accent 
      def ext_inline_verb_ugrave(label, content, visitor)
        label = label.to_s
        return nil unless /^ugrave:(.*)$/ =~ label
        NormalText.new("&\#x000F9;")
      end

      # =capital U, dieresis or umlaut mark 
      def ext_inline_verb_Uuml(label, content, visitor)
        label = label.to_s
        return nil unless /^Uuml:(.*)$/ =~ label
        NormalText.new("&\#x000DC;")
      end

      # =small u, dieresis or umlaut mark 
      def ext_inline_verb_uuml(label, content, visitor)
        label = label.to_s
        return nil unless /^uuml:(.*)$/ =~ label
        NormalText.new("&\#x000FC;")
      end

      # =capital Y, acute accent 
      def ext_inline_verb_Yacute(label, content, visitor)
        label = label.to_s
        return nil unless /^Yacute:(.*)$/ =~ label
        NormalText.new("&\#x000DD;")
      end

      # =small y, acute accent 
      def ext_inline_verb_yacute(label, content, visitor)
        label = label.to_s
        return nil unless /^yacute:(.*)$/ =~ label
        NormalText.new("&\#x000FD;")
      end

      # =small y, dieresis or umlaut mark 
      def ext_inline_verb_yuml(label, content, visitor)
        label = label.to_s
        return nil unless /^yuml:(.*)$/ =~ label
        NormalText.new("&\#x000FF;")
      end

    end
  end
end
