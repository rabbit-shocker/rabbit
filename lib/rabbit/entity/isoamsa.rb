require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamsa

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # angle with down zig-zag arrow 
      def ext_inline_verb_angzarr(label, content, visitor)
        label = label.to_s
        return nil unless /^angzarr:(.*)$/ =~ label
        NormalText.new("&\#x0237C;")
      end

      # circle, mid below 
      def ext_inline_verb_cirmid(label, content, visitor)
        label = label.to_s
        return nil unless /^cirmid:(.*)$/ =~ label
        NormalText.new("&\#x02AEF;")
      end

      # left, curved, down arrow 
      def ext_inline_verb_cudarrl(label, content, visitor)
        label = label.to_s
        return nil unless /^cudarrl:(.*)$/ =~ label
        NormalText.new("&\#x02938;")
      end

      # right, curved, down arrow 
      def ext_inline_verb_cudarrr(label, content, visitor)
        label = label.to_s
        return nil unless /^cudarrr:(.*)$/ =~ label
        NormalText.new("&\#x02935;")
      end

      # /curvearrowleft A: left curved arrow 
      def ext_inline_verb_cularr(label, content, visitor)
        label = label.to_s
        return nil unless /^cularr:(.*)$/ =~ label
        NormalText.new("&\#x021B6;")
      end

      # curved left arrow with plus 
      def ext_inline_verb_cularrp(label, content, visitor)
        label = label.to_s
        return nil unless /^cularrp:(.*)$/ =~ label
        NormalText.new("&\#x0293D;")
      end

      # /curvearrowright A: rt curved arrow 
      def ext_inline_verb_curarr(label, content, visitor)
        label = label.to_s
        return nil unless /^curarr:(.*)$/ =~ label
        NormalText.new("&\#x021B7;")
      end

      # curved right arrow with minus 
      def ext_inline_verb_curarrm(label, content, visitor)
        label = label.to_s
        return nil unless /^curarrm:(.*)$/ =~ label
        NormalText.new("&\#x0293C;")
      end

      # down two-headed arrow 
      def ext_inline_verb_Darr(label, content, visitor)
        label = label.to_s
        return nil unless /^Darr:(.*)$/ =~ label
        NormalText.new("&\#x021A1;")
      end

      # /Downarrow A: down dbl arrow 
      def ext_inline_verb_dArr(label, content, visitor)
        label = label.to_s
        return nil unless /^dArr:(.*)$/ =~ label
        NormalText.new("&\#x021D3;")
      end

      # /downdownarrows A: two down arrows 
      def ext_inline_verb_ddarr(label, content, visitor)
        label = label.to_s
        return nil unless /^ddarr:(.*)$/ =~ label
        NormalText.new("&\#x021CA;")
      end

      # right arrow with dotted stem 
      def ext_inline_verb_DDotrahd(label, content, visitor)
        label = label.to_s
        return nil unless /^DDotrahd:(.*)$/ =~ label
        NormalText.new("&\#x02911;")
      end

      # down fish tail 
      def ext_inline_verb_dfisht(label, content, visitor)
        label = label.to_s
        return nil unless /^dfisht:(.*)$/ =~ label
        NormalText.new("&\#x0297F;")
      end

      # down harpoon-left, down harpoon-right 
      def ext_inline_verb_dHar(label, content, visitor)
        label = label.to_s
        return nil unless /^dHar:(.*)$/ =~ label
        NormalText.new("&\#x02965;")
      end

      # /downharpoonleft A: dn harpoon-left 
      def ext_inline_verb_dharl(label, content, visitor)
        label = label.to_s
        return nil unless /^dharl:(.*)$/ =~ label
        NormalText.new("&\#x021C3;")
      end

      # /downharpoonright A: down harpoon-rt 
      def ext_inline_verb_dharr(label, content, visitor)
        label = label.to_s
        return nil unless /^dharr:(.*)$/ =~ label
        NormalText.new("&\#x021C2;")
      end

      # down arrow, up arrow 
      def ext_inline_verb_duarr(label, content, visitor)
        label = label.to_s
        return nil unless /^duarr:(.*)$/ =~ label
        NormalText.new("&\#x021F5;")
      end

      # down harp, up harp 
      def ext_inline_verb_duhar(label, content, visitor)
        label = label.to_s
        return nil unless /^duhar:(.*)$/ =~ label
        NormalText.new("&\#x0296F;")
      end

      # right long zig-zag arrow 
      def ext_inline_verb_dzigrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^dzigrarr:(.*)$/ =~ label
        NormalText.new("&\#x027FF;")
      end

      # equal, right arrow below 
      def ext_inline_verb_erarr(label, content, visitor)
        label = label.to_s
        return nil unless /^erarr:(.*)$/ =~ label
        NormalText.new("&\#x02971;")
      end

      # /Leftrightarrow A: l&r dbl arrow 
      def ext_inline_verb_hArr(label, content, visitor)
        label = label.to_s
        return nil unless /^hArr:(.*)$/ =~ label
        NormalText.new("&\#x021D4;")
      end

      # /leftrightarrow A: l&r arrow 
      def ext_inline_verb_harr(label, content, visitor)
        label = label.to_s
        return nil unless /^harr:(.*)$/ =~ label
        NormalText.new("&\#x02194;")
      end

      # left and right arrow with a circle 
      def ext_inline_verb_harrcir(label, content, visitor)
        label = label.to_s
        return nil unless /^harrcir:(.*)$/ =~ label
        NormalText.new("&\#x02948;")
      end

      # /leftrightsquigarrow A: l&r arr-wavy 
      def ext_inline_verb_harrw(label, content, visitor)
        label = label.to_s
        return nil unless /^harrw:(.*)$/ =~ label
        NormalText.new("&\#x021AD;")
      end

      # horizontal open arrow 
      def ext_inline_verb_hoarr(label, content, visitor)
        label = label.to_s
        return nil unless /^hoarr:(.*)$/ =~ label
        NormalText.new("&\#x021FF;")
      end

      # image of 
      def ext_inline_verb_imof(label, content, visitor)
        label = label.to_s
        return nil unless /^imof:(.*)$/ =~ label
        NormalText.new("&\#x022B7;")
      end

      # /Lleftarrow A: left triple arrow 
      def ext_inline_verb_lAarr(label, content, visitor)
        label = label.to_s
        return nil unless /^lAarr:(.*)$/ =~ label
        NormalText.new("&\#x021DA;")
      end

      # /twoheadleftarrow A: 
      def ext_inline_verb_Larr(label, content, visitor)
        label = label.to_s
        return nil unless /^Larr:(.*)$/ =~ label
        NormalText.new("&\#x0219E;")
      end

      # left arrow-bar, filled square 
      def ext_inline_verb_larrbfs(label, content, visitor)
        label = label.to_s
        return nil unless /^larrbfs:(.*)$/ =~ label
        NormalText.new("&\#x0291F;")
      end

      # left arrow, filled square 
      def ext_inline_verb_larrfs(label, content, visitor)
        label = label.to_s
        return nil unless /^larrfs:(.*)$/ =~ label
        NormalText.new("&\#x0291D;")
      end

      # /hookleftarrow A: left arrow-hooked 
      def ext_inline_verb_larrhk(label, content, visitor)
        label = label.to_s
        return nil unless /^larrhk:(.*)$/ =~ label
        NormalText.new("&\#x021A9;")
      end

      # /looparrowleft A: left arrow-looped 
      def ext_inline_verb_larrlp(label, content, visitor)
        label = label.to_s
        return nil unless /^larrlp:(.*)$/ =~ label
        NormalText.new("&\#x021AB;")
      end

      # left arrow, plus 
      def ext_inline_verb_larrpl(label, content, visitor)
        label = label.to_s
        return nil unless /^larrpl:(.*)$/ =~ label
        NormalText.new("&\#x02939;")
      end

      # left arrow, similar 
      def ext_inline_verb_larrsim(label, content, visitor)
        label = label.to_s
        return nil unless /^larrsim:(.*)$/ =~ label
        NormalText.new("&\#x02973;")
      end

      # /leftarrowtail A: left arrow-tailed 
      def ext_inline_verb_larrtl(label, content, visitor)
        label = label.to_s
        return nil unless /^larrtl:(.*)$/ =~ label
        NormalText.new("&\#x021A2;")
      end

      # left double arrow-tail 
      def ext_inline_verb_lAtail(label, content, visitor)
        label = label.to_s
        return nil unless /^lAtail:(.*)$/ =~ label
        NormalText.new("&\#x0291B;")
      end

      # left arrow-tail 
      def ext_inline_verb_latail(label, content, visitor)
        label = label.to_s
        return nil unless /^latail:(.*)$/ =~ label
        NormalText.new("&\#x02919;")
      end

      # left doubly broken arrow 
      def ext_inline_verb_lBarr(label, content, visitor)
        label = label.to_s
        return nil unless /^lBarr:(.*)$/ =~ label
        NormalText.new("&\#x0290E;")
      end

      # left broken arrow 
      def ext_inline_verb_lbarr(label, content, visitor)
        label = label.to_s
        return nil unless /^lbarr:(.*)$/ =~ label
        NormalText.new("&\#x0290C;")
      end

      # left down curved arrow 
      def ext_inline_verb_ldca(label, content, visitor)
        label = label.to_s
        return nil unless /^ldca:(.*)$/ =~ label
        NormalText.new("&\#x02936;")
      end

      # left harpoon-down over right harpoon-down 
      def ext_inline_verb_ldrdhar(label, content, visitor)
        label = label.to_s
        return nil unless /^ldrdhar:(.*)$/ =~ label
        NormalText.new("&\#x02967;")
      end

      # left-down-right-up harpoon 
      def ext_inline_verb_ldrushar(label, content, visitor)
        label = label.to_s
        return nil unless /^ldrushar:(.*)$/ =~ label
        NormalText.new("&\#x0294B;")
      end

      # left down angled arrow 
      def ext_inline_verb_ldsh(label, content, visitor)
        label = label.to_s
        return nil unless /^ldsh:(.*)$/ =~ label
        NormalText.new("&\#x021B2;")
      end

      # left fish tail 
      def ext_inline_verb_lfisht(label, content, visitor)
        label = label.to_s
        return nil unless /^lfisht:(.*)$/ =~ label
        NormalText.new("&\#x0297C;")
      end

      # left harpoon-up over left harpoon-down 
      def ext_inline_verb_lHar(label, content, visitor)
        label = label.to_s
        return nil unless /^lHar:(.*)$/ =~ label
        NormalText.new("&\#x02962;")
      end

      # /leftharpoondown A: l harpoon-down 
      def ext_inline_verb_lhard(label, content, visitor)
        label = label.to_s
        return nil unless /^lhard:(.*)$/ =~ label
        NormalText.new("&\#x021BD;")
      end

      # /leftharpoonup A: left harpoon-up 
      def ext_inline_verb_lharu(label, content, visitor)
        label = label.to_s
        return nil unless /^lharu:(.*)$/ =~ label
        NormalText.new("&\#x021BC;")
      end

      # left harpoon-up over long dash 
      def ext_inline_verb_lharul(label, content, visitor)
        label = label.to_s
        return nil unless /^lharul:(.*)$/ =~ label
        NormalText.new("&\#x0296A;")
      end

      # /leftleftarrows A: two left arrows 
      def ext_inline_verb_llarr(label, content, visitor)
        label = label.to_s
        return nil unless /^llarr:(.*)$/ =~ label
        NormalText.new("&\#x021C7;")
      end

      # left harpoon-down below long dash 
      def ext_inline_verb_llhard(label, content, visitor)
        label = label.to_s
        return nil unless /^llhard:(.*)$/ =~ label
        NormalText.new("&\#x0296B;")
      end

      # left open arrow 
      def ext_inline_verb_loarr(label, content, visitor)
        label = label.to_s
        return nil unless /^loarr:(.*)$/ =~ label
        NormalText.new("&\#x021FD;")
      end

      # /leftrightarrows A: l arr over r arr 
      def ext_inline_verb_lrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^lrarr:(.*)$/ =~ label
        NormalText.new("&\#x021C6;")
      end

      # /leftrightharpoons A: l harp over r 
      def ext_inline_verb_lrhar(label, content, visitor)
        label = label.to_s
        return nil unless /^lrhar:(.*)$/ =~ label
        NormalText.new("&\#x021CB;")
      end

      # right harpoon-down below long dash 
      def ext_inline_verb_lrhard(label, content, visitor)
        label = label.to_s
        return nil unless /^lrhard:(.*)$/ =~ label
        NormalText.new("&\#x0296D;")
      end

      # /Lsh A: 
      def ext_inline_verb_lsh(label, content, visitor)
        label = label.to_s
        return nil unless /^lsh:(.*)$/ =~ label
        NormalText.new("&\#x021B0;")
      end

      # left-up-right-down harpoon 
      def ext_inline_verb_lurdshar(label, content, visitor)
        label = label.to_s
        return nil unless /^lurdshar:(.*)$/ =~ label
        NormalText.new("&\#x0294A;")
      end

      # left harpoon-up over right harpoon-up 
      def ext_inline_verb_luruhar(label, content, visitor)
        label = label.to_s
        return nil unless /^luruhar:(.*)$/ =~ label
        NormalText.new("&\#x02966;")
      end

      # twoheaded mapsto 
      def ext_inline_verb_Map(label, content, visitor)
        label = label.to_s
        return nil unless /^Map:(.*)$/ =~ label
        NormalText.new("&\#x02905;")
      end

      # /mapsto A: 
      def ext_inline_verb_map(label, content, visitor)
        label = label.to_s
        return nil unless /^map:(.*)$/ =~ label
        NormalText.new("&\#x021A6;")
      end

      # mid, circle below  
      def ext_inline_verb_midcir(label, content, visitor)
        label = label.to_s
        return nil unless /^midcir:(.*)$/ =~ label
        NormalText.new("&\#x02AF0;")
      end

      # /multimap A: 
      def ext_inline_verb_mumap(label, content, visitor)
        label = label.to_s
        return nil unless /^mumap:(.*)$/ =~ label
        NormalText.new("&\#x022B8;")
      end

      # NE arrow-hooked 
      def ext_inline_verb_nearhk(label, content, visitor)
        label = label.to_s
        return nil unless /^nearhk:(.*)$/ =~ label
        NormalText.new("&\#x02924;")
      end

      # NE pointing dbl arrow 
      def ext_inline_verb_neArr(label, content, visitor)
        label = label.to_s
        return nil unless /^neArr:(.*)$/ =~ label
        NormalText.new("&\#x021D7;")
      end

      # /nearrow A: NE pointing arrow 
      def ext_inline_verb_nearr(label, content, visitor)
        label = label.to_s
        return nil unless /^nearr:(.*)$/ =~ label
        NormalText.new("&\#x02197;")
      end

      # /toea A: NE & SE arrows 
      def ext_inline_verb_nesear(label, content, visitor)
        label = label.to_s
        return nil unless /^nesear:(.*)$/ =~ label
        NormalText.new("&\#x02928;")
      end

      # /nLeftrightarrow A: not l&r dbl arr 
      def ext_inline_verb_nhArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nhArr:(.*)$/ =~ label
        NormalText.new("&\#x021CE;")
      end

      # /nleftrightarrow A: not l&r arrow 
      def ext_inline_verb_nharr(label, content, visitor)
        label = label.to_s
        return nil unless /^nharr:(.*)$/ =~ label
        NormalText.new("&\#x021AE;")
      end

      # /nLeftarrow A: not implied by 
      def ext_inline_verb_nlArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nlArr:(.*)$/ =~ label
        NormalText.new("&\#x021CD;")
      end

      # /nleftarrow A: not left arrow 
      def ext_inline_verb_nlarr(label, content, visitor)
        label = label.to_s
        return nil unless /^nlarr:(.*)$/ =~ label
        NormalText.new("&\#x0219A;")
      end

      # /nRightarrow A: not implies 
      def ext_inline_verb_nrArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nrArr:(.*)$/ =~ label
        NormalText.new("&\#x021CF;")
      end

      # /nrightarrow A: not right arrow 
      def ext_inline_verb_nrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^nrarr:(.*)$/ =~ label
        NormalText.new("&\#x0219B;")
      end

      # not right arrow-curved 
      def ext_inline_verb_nrarrc(label, content, visitor)
        label = label.to_s
        return nil unless /^nrarrc:(.*)$/ =~ label
        NormalText.new("&\#x02933;&\#x00338;")
      end

      # not right arrow-wavy 
      def ext_inline_verb_nrarrw(label, content, visitor)
        label = label.to_s
        return nil unless /^nrarrw:(.*)$/ =~ label
        NormalText.new("&\#x0219D;&\#x00338;")
      end

      # not, vert, left and right double arrow  
      def ext_inline_verb_nvHarr(label, content, visitor)
        label = label.to_s
        return nil unless /^nvHarr:(.*)$/ =~ label
        NormalText.new("&\#x02904;")
      end

      # not, vert, left double arrow 
      def ext_inline_verb_nvlArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nvlArr:(.*)$/ =~ label
        NormalText.new("&\#x02902;")
      end

      # not, vert, right double arrow 
      def ext_inline_verb_nvrArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nvrArr:(.*)$/ =~ label
        NormalText.new("&\#x02903;")
      end

      # NW arrow-hooked 
      def ext_inline_verb_nwarhk(label, content, visitor)
        label = label.to_s
        return nil unless /^nwarhk:(.*)$/ =~ label
        NormalText.new("&\#x02923;")
      end

      # NW pointing dbl arrow 
      def ext_inline_verb_nwArr(label, content, visitor)
        label = label.to_s
        return nil unless /^nwArr:(.*)$/ =~ label
        NormalText.new("&\#x021D6;")
      end

      # /nwarrow A: NW pointing arrow 
      def ext_inline_verb_nwarr(label, content, visitor)
        label = label.to_s
        return nil unless /^nwarr:(.*)$/ =~ label
        NormalText.new("&\#x02196;")
      end

      # NW & NE arrows 
      def ext_inline_verb_nwnear(label, content, visitor)
        label = label.to_s
        return nil unless /^nwnear:(.*)$/ =~ label
        NormalText.new("&\#x02927;")
      end

      # /circlearrowleft A: l arr in circle 
      def ext_inline_verb_olarr(label, content, visitor)
        label = label.to_s
        return nil unless /^olarr:(.*)$/ =~ label
        NormalText.new("&\#x021BA;")
      end

      # /circlearrowright A: r arr in circle 
      def ext_inline_verb_orarr(label, content, visitor)
        label = label.to_s
        return nil unless /^orarr:(.*)$/ =~ label
        NormalText.new("&\#x021BB;")
      end

      # original of 
      def ext_inline_verb_origof(label, content, visitor)
        label = label.to_s
        return nil unless /^origof:(.*)$/ =~ label
        NormalText.new("&\#x022B6;")
      end

      # /Rrightarrow A: right triple arrow 
      def ext_inline_verb_rAarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rAarr:(.*)$/ =~ label
        NormalText.new("&\#x021DB;")
      end

      # /twoheadrightarrow A: 
      def ext_inline_verb_Rarr(label, content, visitor)
        label = label.to_s
        return nil unless /^Rarr:(.*)$/ =~ label
        NormalText.new("&\#x021A0;")
      end

      # approximate, right arrow above 
      def ext_inline_verb_rarrap(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrap:(.*)$/ =~ label
        NormalText.new("&\#x02975;")
      end

      # right arrow-bar, filled square 
      def ext_inline_verb_rarrbfs(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrbfs:(.*)$/ =~ label
        NormalText.new("&\#x02920;")
      end

      # right arrow-curved 
      def ext_inline_verb_rarrc(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrc:(.*)$/ =~ label
        NormalText.new("&\#x02933;")
      end

      # right arrow, filled square 
      def ext_inline_verb_rarrfs(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrfs:(.*)$/ =~ label
        NormalText.new("&\#x0291E;")
      end

      # /hookrightarrow A: rt arrow-hooked 
      def ext_inline_verb_rarrhk(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrhk:(.*)$/ =~ label
        NormalText.new("&\#x021AA;")
      end

      # /looparrowright A: rt arrow-looped 
      def ext_inline_verb_rarrlp(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrlp:(.*)$/ =~ label
        NormalText.new("&\#x021AC;")
      end

      # right arrow, plus 
      def ext_inline_verb_rarrpl(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrpl:(.*)$/ =~ label
        NormalText.new("&\#x02945;")
      end

      # right arrow, similar 
      def ext_inline_verb_rarrsim(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrsim:(.*)$/ =~ label
        NormalText.new("&\#x02974;")
      end

      # right two-headed arrow with tail 
      def ext_inline_verb_Rarrtl(label, content, visitor)
        label = label.to_s
        return nil unless /^Rarrtl:(.*)$/ =~ label
        NormalText.new("&\#x02916;")
      end

      # /rightarrowtail A: rt arrow-tailed 
      def ext_inline_verb_rarrtl(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrtl:(.*)$/ =~ label
        NormalText.new("&\#x021A3;")
      end

      # /rightsquigarrow A: rt arrow-wavy 
      def ext_inline_verb_rarrw(label, content, visitor)
        label = label.to_s
        return nil unless /^rarrw:(.*)$/ =~ label
        NormalText.new("&\#x0219D;")
      end

      # right double arrow-tail 
      def ext_inline_verb_rAtail(label, content, visitor)
        label = label.to_s
        return nil unless /^rAtail:(.*)$/ =~ label
        NormalText.new("&\#x0291C;")
      end

      # right arrow-tail 
      def ext_inline_verb_ratail(label, content, visitor)
        label = label.to_s
        return nil unless /^ratail:(.*)$/ =~ label
        NormalText.new("&\#x0291A;")
      end

      # /drbkarow A: twoheaded right broken arrow 
      def ext_inline_verb_RBarr(label, content, visitor)
        label = label.to_s
        return nil unless /^RBarr:(.*)$/ =~ label
        NormalText.new("&\#x02910;")
      end

      # /dbkarow A: right doubly broken arrow 
      def ext_inline_verb_rBarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rBarr:(.*)$/ =~ label
        NormalText.new("&\#x0290F;")
      end

      # /bkarow A: right broken arrow 
      def ext_inline_verb_rbarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rbarr:(.*)$/ =~ label
        NormalText.new("&\#x0290D;")
      end

      # right down curved arrow 
      def ext_inline_verb_rdca(label, content, visitor)
        label = label.to_s
        return nil unless /^rdca:(.*)$/ =~ label
        NormalText.new("&\#x02937;")
      end

      # right harpoon-down over left harpoon-down 
      def ext_inline_verb_rdldhar(label, content, visitor)
        label = label.to_s
        return nil unless /^rdldhar:(.*)$/ =~ label
        NormalText.new("&\#x02969;")
      end

      # right down angled arrow 
      def ext_inline_verb_rdsh(label, content, visitor)
        label = label.to_s
        return nil unless /^rdsh:(.*)$/ =~ label
        NormalText.new("&\#x021B3;")
      end

      # right fish tail 
      def ext_inline_verb_rfisht(label, content, visitor)
        label = label.to_s
        return nil unless /^rfisht:(.*)$/ =~ label
        NormalText.new("&\#x0297D;")
      end

      # right harpoon-up over right harpoon-down 
      def ext_inline_verb_rHar(label, content, visitor)
        label = label.to_s
        return nil unless /^rHar:(.*)$/ =~ label
        NormalText.new("&\#x02964;")
      end

      # /rightharpoondown A: rt harpoon-down 
      def ext_inline_verb_rhard(label, content, visitor)
        label = label.to_s
        return nil unless /^rhard:(.*)$/ =~ label
        NormalText.new("&\#x021C1;")
      end

      # /rightharpoonup A: rt harpoon-up 
      def ext_inline_verb_rharu(label, content, visitor)
        label = label.to_s
        return nil unless /^rharu:(.*)$/ =~ label
        NormalText.new("&\#x021C0;")
      end

      # right harpoon-up over long dash 
      def ext_inline_verb_rharul(label, content, visitor)
        label = label.to_s
        return nil unless /^rharul:(.*)$/ =~ label
        NormalText.new("&\#x0296C;")
      end

      # /rightleftarrows A: r arr over l arr 
      def ext_inline_verb_rlarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rlarr:(.*)$/ =~ label
        NormalText.new("&\#x021C4;")
      end

      # /rightleftharpoons A: r harp over l 
      def ext_inline_verb_rlhar(label, content, visitor)
        label = label.to_s
        return nil unless /^rlhar:(.*)$/ =~ label
        NormalText.new("&\#x021CC;")
      end

      # right open arrow 
      def ext_inline_verb_roarr(label, content, visitor)
        label = label.to_s
        return nil unless /^roarr:(.*)$/ =~ label
        NormalText.new("&\#x021FE;")
      end

      # /rightrightarrows A: two rt arrows 
      def ext_inline_verb_rrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^rrarr:(.*)$/ =~ label
        NormalText.new("&\#x021C9;")
      end

      # /Rsh A: 
      def ext_inline_verb_rsh(label, content, visitor)
        label = label.to_s
        return nil unless /^rsh:(.*)$/ =~ label
        NormalText.new("&\#x021B1;")
      end

      # right harpoon-up over left harpoon-up 
      def ext_inline_verb_ruluhar(label, content, visitor)
        label = label.to_s
        return nil unless /^ruluhar:(.*)$/ =~ label
        NormalText.new("&\#x02968;")
      end

      # /hksearow A: SE arrow-hooken 
      def ext_inline_verb_searhk(label, content, visitor)
        label = label.to_s
        return nil unless /^searhk:(.*)$/ =~ label
        NormalText.new("&\#x02925;")
      end

      # SE pointing dbl arrow 
      def ext_inline_verb_seArr(label, content, visitor)
        label = label.to_s
        return nil unless /^seArr:(.*)$/ =~ label
        NormalText.new("&\#x021D8;")
      end

      # /searrow A: SE pointing arrow 
      def ext_inline_verb_searr(label, content, visitor)
        label = label.to_s
        return nil unless /^searr:(.*)$/ =~ label
        NormalText.new("&\#x02198;")
      end

      # /tosa A: SE & SW arrows 
      def ext_inline_verb_seswar(label, content, visitor)
        label = label.to_s
        return nil unless /^seswar:(.*)$/ =~ label
        NormalText.new("&\#x02929;")
      end

      # similar, right arrow below 
      def ext_inline_verb_simrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^simrarr:(.*)$/ =~ label
        NormalText.new("&\#x02972;")
      end

      # short left arrow 
      def ext_inline_verb_slarr(label, content, visitor)
        label = label.to_s
        return nil unless /^slarr:(.*)$/ =~ label
        NormalText.new("&\#x02190;")
      end

      # short right arrow 
      def ext_inline_verb_srarr(label, content, visitor)
        label = label.to_s
        return nil unless /^srarr:(.*)$/ =~ label
        NormalText.new("&\#x02192;")
      end

      # /hkswarow A: SW arrow-hooked 
      def ext_inline_verb_swarhk(label, content, visitor)
        label = label.to_s
        return nil unless /^swarhk:(.*)$/ =~ label
        NormalText.new("&\#x02926;")
      end

      # SW pointing dbl arrow 
      def ext_inline_verb_swArr(label, content, visitor)
        label = label.to_s
        return nil unless /^swArr:(.*)$/ =~ label
        NormalText.new("&\#x021D9;")
      end

      # /swarrow A: SW pointing arrow 
      def ext_inline_verb_swarr(label, content, visitor)
        label = label.to_s
        return nil unless /^swarr:(.*)$/ =~ label
        NormalText.new("&\#x02199;")
      end

      # SW & NW arrows 
      def ext_inline_verb_swnwar(label, content, visitor)
        label = label.to_s
        return nil unless /^swnwar:(.*)$/ =~ label
        NormalText.new("&\#x0292A;")
      end

      # up two-headed arrow 
      def ext_inline_verb_Uarr(label, content, visitor)
        label = label.to_s
        return nil unless /^Uarr:(.*)$/ =~ label
        NormalText.new("&\#x0219F;")
      end

      # /Uparrow A: up dbl arrow 
      def ext_inline_verb_uArr(label, content, visitor)
        label = label.to_s
        return nil unless /^uArr:(.*)$/ =~ label
        NormalText.new("&\#x021D1;")
      end

      # up two-headed arrow above circle 
      def ext_inline_verb_Uarrocir(label, content, visitor)
        label = label.to_s
        return nil unless /^Uarrocir:(.*)$/ =~ label
        NormalText.new("&\#x02949;")
      end

      # up arrow, down arrow 
      def ext_inline_verb_udarr(label, content, visitor)
        label = label.to_s
        return nil unless /^udarr:(.*)$/ =~ label
        NormalText.new("&\#x021C5;")
      end

      # up harp, down harp 
      def ext_inline_verb_udhar(label, content, visitor)
        label = label.to_s
        return nil unless /^udhar:(.*)$/ =~ label
        NormalText.new("&\#x0296E;")
      end

      # up fish tail 
      def ext_inline_verb_ufisht(label, content, visitor)
        label = label.to_s
        return nil unless /^ufisht:(.*)$/ =~ label
        NormalText.new("&\#x0297E;")
      end

      # up harpoon-left, up harpoon-right 
      def ext_inline_verb_uHar(label, content, visitor)
        label = label.to_s
        return nil unless /^uHar:(.*)$/ =~ label
        NormalText.new("&\#x02963;")
      end

      # /upharpoonleft A: up harpoon-left 
      def ext_inline_verb_uharl(label, content, visitor)
        label = label.to_s
        return nil unless /^uharl:(.*)$/ =~ label
        NormalText.new("&\#x021BF;")
      end

      # /upharpoonright /restriction A: up harp-r 
      def ext_inline_verb_uharr(label, content, visitor)
        label = label.to_s
        return nil unless /^uharr:(.*)$/ =~ label
        NormalText.new("&\#x021BE;")
      end

      # /upuparrows A: two up arrows 
      def ext_inline_verb_uuarr(label, content, visitor)
        label = label.to_s
        return nil unless /^uuarr:(.*)$/ =~ label
        NormalText.new("&\#x021C8;")
      end

      # /Updownarrow A: up&down dbl arrow 
      def ext_inline_verb_vArr(label, content, visitor)
        label = label.to_s
        return nil unless /^vArr:(.*)$/ =~ label
        NormalText.new("&\#x021D5;")
      end

      # /updownarrow A: up&down arrow 
      def ext_inline_verb_varr(label, content, visitor)
        label = label.to_s
        return nil unless /^varr:(.*)$/ =~ label
        NormalText.new("&\#x02195;")
      end

      # /Longleftrightarrow A: long l&r dbl arr 
      def ext_inline_verb_xhArr(label, content, visitor)
        label = label.to_s
        return nil unless /^xhArr:(.*)$/ =~ label
        NormalText.new("&\#x027FA;")
      end

      # /longleftrightarrow A: long l&r arr 
      def ext_inline_verb_xharr(label, content, visitor)
        label = label.to_s
        return nil unless /^xharr:(.*)$/ =~ label
        NormalText.new("&\#x027F7;")
      end

      # /Longleftarrow A: long l dbl arrow 
      def ext_inline_verb_xlArr(label, content, visitor)
        label = label.to_s
        return nil unless /^xlArr:(.*)$/ =~ label
        NormalText.new("&\#x027F8;")
      end

      # /longleftarrow A: long left arrow 
      def ext_inline_verb_xlarr(label, content, visitor)
        label = label.to_s
        return nil unless /^xlarr:(.*)$/ =~ label
        NormalText.new("&\#x027F5;")
      end

      # /longmapsto A: 
      def ext_inline_verb_xmap(label, content, visitor)
        label = label.to_s
        return nil unless /^xmap:(.*)$/ =~ label
        NormalText.new("&\#x027FC;")
      end

      # /Longrightarrow A: long rt dbl arr 
      def ext_inline_verb_xrArr(label, content, visitor)
        label = label.to_s
        return nil unless /^xrArr:(.*)$/ =~ label
        NormalText.new("&\#x027F9;")
      end

      # /longrightarrow A: long right arrow 
      def ext_inline_verb_xrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^xrarr:(.*)$/ =~ label
        NormalText.new("&\#x027F6;")
      end

      # right zig-zag arrow 
      def ext_inline_verb_zigrarr(label, content, visitor)
        label = label.to_s
        return nil unless /^zigrarr:(.*)$/ =~ label
        NormalText.new("&\#x021DD;")
      end

    end
  end
end
