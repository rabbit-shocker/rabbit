require 'rabbit/element'

module Rabbit
  module Entity
    module Isoamsn

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /gnapprox N: greater, not approximate 
      def ext_inline_verb_gnap(label, content, visitor)
        label = label.to_s
        return nil unless /^gnap:(.*)$/ =~ label
        NormalText.new("&\#x02A8A;")
      end

      # /gneqq N: greater, not dbl equals 
      def ext_inline_verb_gnE(label, content, visitor)
        label = label.to_s
        return nil unless /^gnE:(.*)$/ =~ label
        NormalText.new("&\#x02269;")
      end

      # /gneq N: greater, not equals 
      def ext_inline_verb_gne(label, content, visitor)
        label = label.to_s
        return nil unless /^gne:(.*)$/ =~ label
        NormalText.new("&\#x02A88;")
      end

      # /gnsim N: greater, not similar 
      def ext_inline_verb_gnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^gnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E7;")
      end

      # /gvertneqq N: gt, vert, not dbl eq 
      def ext_inline_verb_gvnE(label, content, visitor)
        label = label.to_s
        return nil unless /^gvnE:(.*)$/ =~ label
        NormalText.new("&\#x02269;&\#x0FE00;")
      end

      # /lnapprox N: less, not approximate 
      def ext_inline_verb_lnap(label, content, visitor)
        label = label.to_s
        return nil unless /^lnap:(.*)$/ =~ label
        NormalText.new("&\#x02A89;")
      end

      # /lneqq N: less, not double equals 
      def ext_inline_verb_lnE(label, content, visitor)
        label = label.to_s
        return nil unless /^lnE:(.*)$/ =~ label
        NormalText.new("&\#x02268;")
      end

      # /lneq N: less, not equals 
      def ext_inline_verb_lne(label, content, visitor)
        label = label.to_s
        return nil unless /^lne:(.*)$/ =~ label
        NormalText.new("&\#x02A87;")
      end

      # /lnsim N: less, not similar 
      def ext_inline_verb_lnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^lnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E6;")
      end

      # /lvertneqq N: less, vert, not dbl eq 
      def ext_inline_verb_lvnE(label, content, visitor)
        label = label.to_s
        return nil unless /^lvnE:(.*)$/ =~ label
        NormalText.new("&\#x02268;&\#x0FE00;")
      end

      # /napprox N: not approximate 
      def ext_inline_verb_nap(label, content, visitor)
        label = label.to_s
        return nil unless /^nap:(.*)$/ =~ label
        NormalText.new("&\#x02249;")
      end

      # not approximately equal or equal to 
      def ext_inline_verb_napE(label, content, visitor)
        label = label.to_s
        return nil unless /^napE:(.*)$/ =~ label
        NormalText.new("&\#x02A70;&\#x00338;")
      end

      # not approximately identical to 
      def ext_inline_verb_napid(label, content, visitor)
        label = label.to_s
        return nil unless /^napid:(.*)$/ =~ label
        NormalText.new("&\#x0224B;&\#x00338;")
      end

      # /ncong N: not congruent with 
      def ext_inline_verb_ncong(label, content, visitor)
        label = label.to_s
        return nil unless /^ncong:(.*)$/ =~ label
        NormalText.new("&\#x02247;")
      end

      # not congruent, dot 
      def ext_inline_verb_ncongdot(label, content, visitor)
        label = label.to_s
        return nil unless /^ncongdot:(.*)$/ =~ label
        NormalText.new("&\#x02A6D;&\#x00338;")
      end

      # /nequiv N: not identical with 
      def ext_inline_verb_nequiv(label, content, visitor)
        label = label.to_s
        return nil unless /^nequiv:(.*)$/ =~ label
        NormalText.new("&\#x02262;")
      end

      # /ngeqq N: not greater, dbl equals 
      def ext_inline_verb_ngE(label, content, visitor)
        label = label.to_s
        return nil unless /^ngE:(.*)$/ =~ label
        NormalText.new("&\#x02267;&\#x00338;")
      end

      # /ngeq N: not greater-than-or-equal 
      def ext_inline_verb_nge(label, content, visitor)
        label = label.to_s
        return nil unless /^nge:(.*)$/ =~ label
        NormalText.new("&\#x02271;")
      end

      # /ngeqslant N: not gt-or-eq, slanted 
      def ext_inline_verb_nges(label, content, visitor)
        label = label.to_s
        return nil unless /^nges:(.*)$/ =~ label
        NormalText.new("&\#x02A7E;&\#x00338;")
      end

      # not triple greater than 
      def ext_inline_verb_nGg(label, content, visitor)
        label = label.to_s
        return nil unless /^nGg:(.*)$/ =~ label
        NormalText.new("&\#x022D9;&\#x00338;")
      end

      # not greater, similar 
      def ext_inline_verb_ngsim(label, content, visitor)
        label = label.to_s
        return nil unless /^ngsim:(.*)$/ =~ label
        NormalText.new("&\#x02275;")
      end

      # not, vert, much greater than 
      def ext_inline_verb_nGt(label, content, visitor)
        label = label.to_s
        return nil unless /^nGt:(.*)$/ =~ label
        NormalText.new("&\#x0226B;&\#x020D2;")
      end

      # /ngtr N: not greater-than 
      def ext_inline_verb_ngt(label, content, visitor)
        label = label.to_s
        return nil unless /^ngt:(.*)$/ =~ label
        NormalText.new("&\#x0226F;")
      end

      # not much greater than, variant 
      def ext_inline_verb_nGtv(label, content, visitor)
        label = label.to_s
        return nil unless /^nGtv:(.*)$/ =~ label
        NormalText.new("&\#x0226B;&\#x00338;")
      end

      # /nleqq N: not less, dbl equals 
      def ext_inline_verb_nlE(label, content, visitor)
        label = label.to_s
        return nil unless /^nlE:(.*)$/ =~ label
        NormalText.new("&\#x02266;&\#x00338;")
      end

      # /nleq N: not less-than-or-equal 
      def ext_inline_verb_nle(label, content, visitor)
        label = label.to_s
        return nil unless /^nle:(.*)$/ =~ label
        NormalText.new("&\#x02270;")
      end

      # /nleqslant N: not less-or-eq, slant 
      def ext_inline_verb_nles(label, content, visitor)
        label = label.to_s
        return nil unless /^nles:(.*)$/ =~ label
        NormalText.new("&\#x02A7D;&\#x00338;")
      end

      # not triple less than 
      def ext_inline_verb_nLl(label, content, visitor)
        label = label.to_s
        return nil unless /^nLl:(.*)$/ =~ label
        NormalText.new("&\#x022D8;&\#x00338;")
      end

      # not less, similar 
      def ext_inline_verb_nlsim(label, content, visitor)
        label = label.to_s
        return nil unless /^nlsim:(.*)$/ =~ label
        NormalText.new("&\#x02274;")
      end

      # not, vert, much less than 
      def ext_inline_verb_nLt(label, content, visitor)
        label = label.to_s
        return nil unless /^nLt:(.*)$/ =~ label
        NormalText.new("&\#x0226A;&\#x020D2;")
      end

      # /nless N: not less-than 
      def ext_inline_verb_nlt(label, content, visitor)
        label = label.to_s
        return nil unless /^nlt:(.*)$/ =~ label
        NormalText.new("&\#x0226E;")
      end

      # /ntriangleleft N: not left triangle 
      def ext_inline_verb_nltri(label, content, visitor)
        label = label.to_s
        return nil unless /^nltri:(.*)$/ =~ label
        NormalText.new("&\#x022EA;")
      end

      # /ntrianglelefteq N: not l tri, eq 
      def ext_inline_verb_nltrie(label, content, visitor)
        label = label.to_s
        return nil unless /^nltrie:(.*)$/ =~ label
        NormalText.new("&\#x022EC;")
      end

      # not much less than, variant 
      def ext_inline_verb_nLtv(label, content, visitor)
        label = label.to_s
        return nil unless /^nLtv:(.*)$/ =~ label
        NormalText.new("&\#x0226A;&\#x00338;")
      end

      # /nmid 
      def ext_inline_verb_nmid(label, content, visitor)
        label = label.to_s
        return nil unless /^nmid:(.*)$/ =~ label
        NormalText.new("&\#x02224;")
      end

      # /nparallel N: not parallel 
      def ext_inline_verb_npar(label, content, visitor)
        label = label.to_s
        return nil unless /^npar:(.*)$/ =~ label
        NormalText.new("&\#x02226;")
      end

      # /nprec N: not precedes 
      def ext_inline_verb_npr(label, content, visitor)
        label = label.to_s
        return nil unless /^npr:(.*)$/ =~ label
        NormalText.new("&\#x02280;")
      end

      # not curly precedes, eq 
      def ext_inline_verb_nprcue(label, content, visitor)
        label = label.to_s
        return nil unless /^nprcue:(.*)$/ =~ label
        NormalText.new("&\#x022E0;")
      end

      # /npreceq N: not precedes, equals 
      def ext_inline_verb_npre(label, content, visitor)
        label = label.to_s
        return nil unless /^npre:(.*)$/ =~ label
        NormalText.new("&\#x02AAF;&\#x00338;")
      end

      # /ntriangleright N: not rt triangle 
      def ext_inline_verb_nrtri(label, content, visitor)
        label = label.to_s
        return nil unless /^nrtri:(.*)$/ =~ label
        NormalText.new("&\#x022EB;")
      end

      # /ntrianglerighteq N: not r tri, eq 
      def ext_inline_verb_nrtrie(label, content, visitor)
        label = label.to_s
        return nil unless /^nrtrie:(.*)$/ =~ label
        NormalText.new("&\#x022ED;")
      end

      # /nsucc N: not succeeds 
      def ext_inline_verb_nsc(label, content, visitor)
        label = label.to_s
        return nil unless /^nsc:(.*)$/ =~ label
        NormalText.new("&\#x02281;")
      end

      # not succeeds, curly eq 
      def ext_inline_verb_nsccue(label, content, visitor)
        label = label.to_s
        return nil unless /^nsccue:(.*)$/ =~ label
        NormalText.new("&\#x022E1;")
      end

      # /nsucceq N: not succeeds, equals 
      def ext_inline_verb_nsce(label, content, visitor)
        label = label.to_s
        return nil unless /^nsce:(.*)$/ =~ label
        NormalText.new("&\#x02AB0;&\#x00338;")
      end

      # /nsim N: not similar 
      def ext_inline_verb_nsim(label, content, visitor)
        label = label.to_s
        return nil unless /^nsim:(.*)$/ =~ label
        NormalText.new("&\#x02241;")
      end

      # /nsimeq N: not similar, equals 
      def ext_inline_verb_nsime(label, content, visitor)
        label = label.to_s
        return nil unless /^nsime:(.*)$/ =~ label
        NormalText.new("&\#x02244;")
      end

      # /nshortmid 
      def ext_inline_verb_nsmid(label, content, visitor)
        label = label.to_s
        return nil unless /^nsmid:(.*)$/ =~ label
        NormalText.new("&\#x02224;")
      end

      # /nshortparallel N: not short par 
      def ext_inline_verb_nspar(label, content, visitor)
        label = label.to_s
        return nil unless /^nspar:(.*)$/ =~ label
        NormalText.new("&\#x02226;")
      end

      # not, square subset, equals 
      def ext_inline_verb_nsqsube(label, content, visitor)
        label = label.to_s
        return nil unless /^nsqsube:(.*)$/ =~ label
        NormalText.new("&\#x022E2;")
      end

      # not, square superset, equals 
      def ext_inline_verb_nsqsupe(label, content, visitor)
        label = label.to_s
        return nil unless /^nsqsupe:(.*)$/ =~ label
        NormalText.new("&\#x022E3;")
      end

      # not subset 
      def ext_inline_verb_nsub(label, content, visitor)
        label = label.to_s
        return nil unless /^nsub:(.*)$/ =~ label
        NormalText.new("&\#x02284;")
      end

      # /nsubseteqq N: not subset, dbl eq 
      def ext_inline_verb_nsubE(label, content, visitor)
        label = label.to_s
        return nil unless /^nsubE:(.*)$/ =~ label
        NormalText.new("&\#x02AC5;&\#x00338;")
      end

      # /nsubseteq N: not subset, equals 
      def ext_inline_verb_nsube(label, content, visitor)
        label = label.to_s
        return nil unless /^nsube:(.*)$/ =~ label
        NormalText.new("&\#x02288;")
      end

      # not superset 
      def ext_inline_verb_nsup(label, content, visitor)
        label = label.to_s
        return nil unless /^nsup:(.*)$/ =~ label
        NormalText.new("&\#x02285;")
      end

      # /nsupseteqq N: not superset, dbl eq 
      def ext_inline_verb_nsupE(label, content, visitor)
        label = label.to_s
        return nil unless /^nsupE:(.*)$/ =~ label
        NormalText.new("&\#x02AC6;&\#x00338;")
      end

      # /nsupseteq N: not superset, equals 
      def ext_inline_verb_nsupe(label, content, visitor)
        label = label.to_s
        return nil unless /^nsupe:(.*)$/ =~ label
        NormalText.new("&\#x02289;")
      end

      # not greater, less 
      def ext_inline_verb_ntgl(label, content, visitor)
        label = label.to_s
        return nil unless /^ntgl:(.*)$/ =~ label
        NormalText.new("&\#x02279;")
      end

      # not less, greater 
      def ext_inline_verb_ntlg(label, content, visitor)
        label = label.to_s
        return nil unless /^ntlg:(.*)$/ =~ label
        NormalText.new("&\#x02278;")
      end

      # not, vert, approximate 
      def ext_inline_verb_nvap(label, content, visitor)
        label = label.to_s
        return nil unless /^nvap:(.*)$/ =~ label
        NormalText.new("&\#x0224D;&\#x020D2;")
      end

      # /nVDash N: not dbl vert, dbl dash 
      def ext_inline_verb_nVDash(label, content, visitor)
        label = label.to_s
        return nil unless /^nVDash:(.*)$/ =~ label
        NormalText.new("&\#x022AF;")
      end

      # /nVdash N: not dbl vertical, dash 
      def ext_inline_verb_nVdash(label, content, visitor)
        label = label.to_s
        return nil unless /^nVdash:(.*)$/ =~ label
        NormalText.new("&\#x022AE;")
      end

      # /nvDash N: not vertical, dbl dash 
      def ext_inline_verb_nvDash(label, content, visitor)
        label = label.to_s
        return nil unless /^nvDash:(.*)$/ =~ label
        NormalText.new("&\#x022AD;")
      end

      # /nvdash N: not vertical, dash 
      def ext_inline_verb_nvdash(label, content, visitor)
        label = label.to_s
        return nil unless /^nvdash:(.*)$/ =~ label
        NormalText.new("&\#x022AC;")
      end

      # not, vert, greater-than-or-equal 
      def ext_inline_verb_nvge(label, content, visitor)
        label = label.to_s
        return nil unless /^nvge:(.*)$/ =~ label
        NormalText.new("&\#x02265;&\#x020D2;")
      end

      # not, vert, greater-than 
      def ext_inline_verb_nvgt(label, content, visitor)
        label = label.to_s
        return nil unless /^nvgt:(.*)$/ =~ label
        NormalText.new("&\#x0003E;&\#x020D2;")
      end

      # not, vert, less-than-or-equal 
      def ext_inline_verb_nvle(label, content, visitor)
        label = label.to_s
        return nil unless /^nvle:(.*)$/ =~ label
        NormalText.new("&\#x02264;&\#x020D2;")
      end

      # not, vert, less-than 
      def ext_inline_verb_nvlt(label, content, visitor)
        label = label.to_s
        return nil unless /^nvlt:(.*)$/ =~ label
        NormalText.new("&\#38;\#x0003C;&\#x020D2;")
      end

      # not, vert, left triangle, equals 
      def ext_inline_verb_nvltrie(label, content, visitor)
        label = label.to_s
        return nil unless /^nvltrie:(.*)$/ =~ label
        NormalText.new("&\#x022B4;&\#x020D2;")
      end

      # not, vert, right triangle, equals 
      def ext_inline_verb_nvrtrie(label, content, visitor)
        label = label.to_s
        return nil unless /^nvrtrie:(.*)$/ =~ label
        NormalText.new("&\#x022B5;&\#x020D2;")
      end

      # not, vert, similar 
      def ext_inline_verb_nvsim(label, content, visitor)
        label = label.to_s
        return nil unless /^nvsim:(.*)$/ =~ label
        NormalText.new("&\#x0223C;&\#x020D2;")
      end

      # parallel, similar 
      def ext_inline_verb_parsim(label, content, visitor)
        label = label.to_s
        return nil unless /^parsim:(.*)$/ =~ label
        NormalText.new("&\#x02AF3;")
      end

      # /precnapprox N: precedes, not approx 
      def ext_inline_verb_prnap(label, content, visitor)
        label = label.to_s
        return nil unless /^prnap:(.*)$/ =~ label
        NormalText.new("&\#x02AB9;")
      end

      # /precneqq N: precedes, not dbl eq 
      def ext_inline_verb_prnE(label, content, visitor)
        label = label.to_s
        return nil unless /^prnE:(.*)$/ =~ label
        NormalText.new("&\#x02AB5;")
      end

      # /precnsim N: precedes, not similar 
      def ext_inline_verb_prnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^prnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E8;")
      end

      # reverse /nmid 
      def ext_inline_verb_rnmid(label, content, visitor)
        label = label.to_s
        return nil unless /^rnmid:(.*)$/ =~ label
        NormalText.new("&\#x02AEE;")
      end

      # /succnapprox N: succeeds, not approx 
      def ext_inline_verb_scnap(label, content, visitor)
        label = label.to_s
        return nil unless /^scnap:(.*)$/ =~ label
        NormalText.new("&\#x02ABA;")
      end

      # /succneqq N: succeeds, not dbl eq 
      def ext_inline_verb_scnE(label, content, visitor)
        label = label.to_s
        return nil unless /^scnE:(.*)$/ =~ label
        NormalText.new("&\#x02AB6;")
      end

      # /succnsim N: succeeds, not similar 
      def ext_inline_verb_scnsim(label, content, visitor)
        label = label.to_s
        return nil unless /^scnsim:(.*)$/ =~ label
        NormalText.new("&\#x022E9;")
      end

      # similar, not equals 
      def ext_inline_verb_simne(label, content, visitor)
        label = label.to_s
        return nil unless /^simne:(.*)$/ =~ label
        NormalText.new("&\#x02246;")
      end

      # solidus, bar through 
      def ext_inline_verb_solbar(label, content, visitor)
        label = label.to_s
        return nil unless /^solbar:(.*)$/ =~ label
        NormalText.new("&\#x0233F;")
      end

      # /subsetneqq N: subset, not dbl eq 
      def ext_inline_verb_subnE(label, content, visitor)
        label = label.to_s
        return nil unless /^subnE:(.*)$/ =~ label
        NormalText.new("&\#x02ACB;")
      end

      # /subsetneq N: subset, not equals 
      def ext_inline_verb_subne(label, content, visitor)
        label = label.to_s
        return nil unless /^subne:(.*)$/ =~ label
        NormalText.new("&\#x0228A;")
      end

      # /supsetneqq N: superset, not dbl eq 
      def ext_inline_verb_supnE(label, content, visitor)
        label = label.to_s
        return nil unless /^supnE:(.*)$/ =~ label
        NormalText.new("&\#x02ACC;")
      end

      # /supsetneq N: superset, not equals 
      def ext_inline_verb_supne(label, content, visitor)
        label = label.to_s
        return nil unless /^supne:(.*)$/ =~ label
        NormalText.new("&\#x0228B;")
      end

      # /nsubset N: not subset, var 
      def ext_inline_verb_vnsub(label, content, visitor)
        label = label.to_s
        return nil unless /^vnsub:(.*)$/ =~ label
        NormalText.new("&\#x02282;&\#x020D2;")
      end

      # /nsupset N: not superset, var 
      def ext_inline_verb_vnsup(label, content, visitor)
        label = label.to_s
        return nil unless /^vnsup:(.*)$/ =~ label
        NormalText.new("&\#x02283;&\#x020D2;")
      end

      # /varsubsetneqq N: subset not dbl eq, var 
      def ext_inline_verb_vsubnE(label, content, visitor)
        label = label.to_s
        return nil unless /^vsubnE:(.*)$/ =~ label
        NormalText.new("&\#x02ACB;&\#x0FE00;")
      end

      # /varsubsetneq N: subset, not eq, var 
      def ext_inline_verb_vsubne(label, content, visitor)
        label = label.to_s
        return nil unless /^vsubne:(.*)$/ =~ label
        NormalText.new("&\#x0228A;&\#x0FE00;")
      end

      # /varsupsetneqq N: super not dbl eq, var 
      def ext_inline_verb_vsupnE(label, content, visitor)
        label = label.to_s
        return nil unless /^vsupnE:(.*)$/ =~ label
        NormalText.new("&\#x02ACC;&\#x0FE00;")
      end

      # /varsupsetneq N: superset, not eq, var 
      def ext_inline_verb_vsupne(label, content, visitor)
        label = label.to_s
        return nil unless /^vsupne:(.*)$/ =~ label
        NormalText.new("&\#x0228B;&\#x0FE00;")
      end

    end
  end
end
