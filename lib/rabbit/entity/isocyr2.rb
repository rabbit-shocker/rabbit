require 'rabbit/element'

module Rabbit
  module Entity
    module Isocyr2

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # =capital DJE, Serbian 
      def ext_inline_verb_DJcy(label, content, visitor)
        label = label.to_s
        return nil unless /^DJcy:(.*)$/ =~ label
        NormalText.new("&\#x00402;")
      end

      # =small dje, Serbian 
      def ext_inline_verb_djcy(label, content, visitor)
        label = label.to_s
        return nil unless /^djcy:(.*)$/ =~ label
        NormalText.new("&\#x00452;")
      end

      # =capital DSE, Macedonian 
      def ext_inline_verb_DScy(label, content, visitor)
        label = label.to_s
        return nil unless /^DScy:(.*)$/ =~ label
        NormalText.new("&\#x00405;")
      end

      # =small dse, Macedonian 
      def ext_inline_verb_dscy(label, content, visitor)
        label = label.to_s
        return nil unless /^dscy:(.*)$/ =~ label
        NormalText.new("&\#x00455;")
      end

      # =capital dze, Serbian 
      def ext_inline_verb_DZcy(label, content, visitor)
        label = label.to_s
        return nil unless /^DZcy:(.*)$/ =~ label
        NormalText.new("&\#x0040F;")
      end

      # =small dze, Serbian 
      def ext_inline_verb_dzcy(label, content, visitor)
        label = label.to_s
        return nil unless /^dzcy:(.*)$/ =~ label
        NormalText.new("&\#x0045F;")
      end

      # =capital GJE Macedonian 
      def ext_inline_verb_GJcy(label, content, visitor)
        label = label.to_s
        return nil unless /^GJcy:(.*)$/ =~ label
        NormalText.new("&\#x00403;")
      end

      # =small gje, Macedonian 
      def ext_inline_verb_gjcy(label, content, visitor)
        label = label.to_s
        return nil unless /^gjcy:(.*)$/ =~ label
        NormalText.new("&\#x00453;")
      end

      # =capital I, Ukrainian 
      def ext_inline_verb_Iukcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Iukcy:(.*)$/ =~ label
        NormalText.new("&\#x00406;")
      end

      # =small i, Ukrainian 
      def ext_inline_verb_iukcy(label, content, visitor)
        label = label.to_s
        return nil unless /^iukcy:(.*)$/ =~ label
        NormalText.new("&\#x00456;")
      end

      # =capital JE, Serbian 
      def ext_inline_verb_Jsercy(label, content, visitor)
        label = label.to_s
        return nil unless /^Jsercy:(.*)$/ =~ label
        NormalText.new("&\#x00408;")
      end

      # =small je, Serbian 
      def ext_inline_verb_jsercy(label, content, visitor)
        label = label.to_s
        return nil unless /^jsercy:(.*)$/ =~ label
        NormalText.new("&\#x00458;")
      end

      # =capital JE, Ukrainian 
      def ext_inline_verb_Jukcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Jukcy:(.*)$/ =~ label
        NormalText.new("&\#x00404;")
      end

      # =small je, Ukrainian 
      def ext_inline_verb_jukcy(label, content, visitor)
        label = label.to_s
        return nil unless /^jukcy:(.*)$/ =~ label
        NormalText.new("&\#x00454;")
      end

      # =capital KJE, Macedonian 
      def ext_inline_verb_KJcy(label, content, visitor)
        label = label.to_s
        return nil unless /^KJcy:(.*)$/ =~ label
        NormalText.new("&\#x0040C;")
      end

      # =small kje Macedonian 
      def ext_inline_verb_kjcy(label, content, visitor)
        label = label.to_s
        return nil unless /^kjcy:(.*)$/ =~ label
        NormalText.new("&\#x0045C;")
      end

      # =capital LJE, Serbian 
      def ext_inline_verb_LJcy(label, content, visitor)
        label = label.to_s
        return nil unless /^LJcy:(.*)$/ =~ label
        NormalText.new("&\#x00409;")
      end

      # =small lje, Serbian 
      def ext_inline_verb_ljcy(label, content, visitor)
        label = label.to_s
        return nil unless /^ljcy:(.*)$/ =~ label
        NormalText.new("&\#x00459;")
      end

      # =capital NJE, Serbian 
      def ext_inline_verb_NJcy(label, content, visitor)
        label = label.to_s
        return nil unless /^NJcy:(.*)$/ =~ label
        NormalText.new("&\#x0040A;")
      end

      # =small nje, Serbian 
      def ext_inline_verb_njcy(label, content, visitor)
        label = label.to_s
        return nil unless /^njcy:(.*)$/ =~ label
        NormalText.new("&\#x0045A;")
      end

      # =capital TSHE, Serbian 
      def ext_inline_verb_TSHcy(label, content, visitor)
        label = label.to_s
        return nil unless /^TSHcy:(.*)$/ =~ label
        NormalText.new("&\#x0040B;")
      end

      # =small tshe, Serbian 
      def ext_inline_verb_tshcy(label, content, visitor)
        label = label.to_s
        return nil unless /^tshcy:(.*)$/ =~ label
        NormalText.new("&\#x0045B;")
      end

      # =capital U, Byelorussian 
      def ext_inline_verb_Ubrcy(label, content, visitor)
        label = label.to_s
        return nil unless /^Ubrcy:(.*)$/ =~ label
        NormalText.new("&\#x0040E;")
      end

      # =small u, Byelorussian 
      def ext_inline_verb_ubrcy(label, content, visitor)
        label = label.to_s
        return nil unless /^ubrcy:(.*)$/ =~ label
        NormalText.new("&\#x0045E;")
      end

      # =capital YI, Ukrainian 
      def ext_inline_verb_YIcy(label, content, visitor)
        label = label.to_s
        return nil unless /^YIcy:(.*)$/ =~ label
        NormalText.new("&\#x00407;")
      end

      # =small yi, Ukrainian 
      def ext_inline_verb_yicy(label, content, visitor)
        label = label.to_s
        return nil unless /^yicy:(.*)$/ =~ label
        NormalText.new("&\#x00457;")
      end

    end
  end
end
