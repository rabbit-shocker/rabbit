require 'rabbit/element'

module Rabbit
  module Entity
    module Isogrk3

      include Element

      class << self
        def included(mod)
          self.instance_methods.each do |meth|
            mod.method_added(meth)
          end
        end
      end


      # /alpha small alpha, Greek 
      def ext_inline_verb_alpha(label, content, visitor)
        label = label.to_s
        return nil unless /^alpha:(.*)$/ =~ label
        NormalText.new("&\#x003B1;")
      end

      # /beta small beta, Greek 
      def ext_inline_verb_beta(label, content, visitor)
        label = label.to_s
        return nil unless /^beta:(.*)$/ =~ label
        NormalText.new("&\#x003B2;")
      end

      # /chi small chi, Greek 
      def ext_inline_verb_chi(label, content, visitor)
        label = label.to_s
        return nil unless /^chi:(.*)$/ =~ label
        NormalText.new("&\#x003C7;")
      end

      # /Delta capital Delta, Greek 
      def ext_inline_verb_Delta(label, content, visitor)
        label = label.to_s
        return nil unless /^Delta:(.*)$/ =~ label
        NormalText.new("&\#x00394;")
      end

      # /delta small delta, Greek 
      def ext_inline_verb_delta(label, content, visitor)
        label = label.to_s
        return nil unless /^delta:(.*)$/ =~ label
        NormalText.new("&\#x003B4;")
      end

      # /straightepsilon, small epsilon, Greek 
      def ext_inline_verb_epsi(label, content, visitor)
        label = label.to_s
        return nil unless /^epsi:(.*)$/ =~ label
        NormalText.new("&\#x003F5;")
      end

      # /varepsilon 
      def ext_inline_verb_epsiv(label, content, visitor)
        label = label.to_s
        return nil unless /^epsiv:(.*)$/ =~ label
        NormalText.new("&\#x003B5;")
      end

      # /eta small eta, Greek 
      def ext_inline_verb_eta(label, content, visitor)
        label = label.to_s
        return nil unless /^eta:(.*)$/ =~ label
        NormalText.new("&\#x003B7;")
      end

      # /Gamma capital Gamma, Greek 
      def ext_inline_verb_Gamma(label, content, visitor)
        label = label.to_s
        return nil unless /^Gamma:(.*)$/ =~ label
        NormalText.new("&\#x00393;")
      end

      # /gamma small gamma, Greek 
      def ext_inline_verb_gamma(label, content, visitor)
        label = label.to_s
        return nil unless /^gamma:(.*)$/ =~ label
        NormalText.new("&\#x003B3;")
      end

      # capital digamma 
      def ext_inline_verb_Gammad(label, content, visitor)
        label = label.to_s
        return nil unless /^Gammad:(.*)$/ =~ label
        NormalText.new("&\#x003DC;")
      end

      # /digamma 
      def ext_inline_verb_gammad(label, content, visitor)
        label = label.to_s
        return nil unless /^gammad:(.*)$/ =~ label
        NormalText.new("&\#x003DD;")
      end

      # /iota small iota, Greek 
      def ext_inline_verb_iota(label, content, visitor)
        label = label.to_s
        return nil unless /^iota:(.*)$/ =~ label
        NormalText.new("&\#x003B9;")
      end

      # /kappa small kappa, Greek 
      def ext_inline_verb_kappa(label, content, visitor)
        label = label.to_s
        return nil unless /^kappa:(.*)$/ =~ label
        NormalText.new("&\#x003BA;")
      end

      # /varkappa 
      def ext_inline_verb_kappav(label, content, visitor)
        label = label.to_s
        return nil unless /^kappav:(.*)$/ =~ label
        NormalText.new("&\#x003F0;")
      end

      # /Lambda capital Lambda, Greek 
      def ext_inline_verb_Lambda(label, content, visitor)
        label = label.to_s
        return nil unless /^Lambda:(.*)$/ =~ label
        NormalText.new("&\#x0039B;")
      end

      # /lambda small lambda, Greek 
      def ext_inline_verb_lambda(label, content, visitor)
        label = label.to_s
        return nil unless /^lambda:(.*)$/ =~ label
        NormalText.new("&\#x003BB;")
      end

      # /mu small mu, Greek 
      def ext_inline_verb_mu(label, content, visitor)
        label = label.to_s
        return nil unless /^mu:(.*)$/ =~ label
        NormalText.new("&\#x003BC;")
      end

      # /nu small nu, Greek 
      def ext_inline_verb_nu(label, content, visitor)
        label = label.to_s
        return nil unless /^nu:(.*)$/ =~ label
        NormalText.new("&\#x003BD;")
      end

      # /Omega capital Omega, Greek 
      def ext_inline_verb_Omega(label, content, visitor)
        label = label.to_s
        return nil unless /^Omega:(.*)$/ =~ label
        NormalText.new("&\#x003A9;")
      end

      # /omega small omega, Greek 
      def ext_inline_verb_omega(label, content, visitor)
        label = label.to_s
        return nil unless /^omega:(.*)$/ =~ label
        NormalText.new("&\#x003C9;")
      end

      # /Phi capital Phi, Greek 
      def ext_inline_verb_Phi(label, content, visitor)
        label = label.to_s
        return nil unless /^Phi:(.*)$/ =~ label
        NormalText.new("&\#x003A6;")
      end

      # /straightphi - small phi, Greek 
      def ext_inline_verb_phi(label, content, visitor)
        label = label.to_s
        return nil unless /^phi:(.*)$/ =~ label
        NormalText.new("&\#x003D5;")
      end

      # /varphi - curly or open phi 
      def ext_inline_verb_phiv(label, content, visitor)
        label = label.to_s
        return nil unless /^phiv:(.*)$/ =~ label
        NormalText.new("&\#x003C6;")
      end

      # /Pi capital Pi, Greek 
      def ext_inline_verb_Pi(label, content, visitor)
        label = label.to_s
        return nil unless /^Pi:(.*)$/ =~ label
        NormalText.new("&\#x003A0;")
      end

      # /pi small pi, Greek 
      def ext_inline_verb_pi(label, content, visitor)
        label = label.to_s
        return nil unless /^pi:(.*)$/ =~ label
        NormalText.new("&\#x003C0;")
      end

      # /varpi 
      def ext_inline_verb_piv(label, content, visitor)
        label = label.to_s
        return nil unless /^piv:(.*)$/ =~ label
        NormalText.new("&\#x003D6;")
      end

      # /Psi capital Psi, Greek 
      def ext_inline_verb_Psi(label, content, visitor)
        label = label.to_s
        return nil unless /^Psi:(.*)$/ =~ label
        NormalText.new("&\#x003A8;")
      end

      # /psi small psi, Greek 
      def ext_inline_verb_psi(label, content, visitor)
        label = label.to_s
        return nil unless /^psi:(.*)$/ =~ label
        NormalText.new("&\#x003C8;")
      end

      # /rho small rho, Greek 
      def ext_inline_verb_rho(label, content, visitor)
        label = label.to_s
        return nil unless /^rho:(.*)$/ =~ label
        NormalText.new("&\#x003C1;")
      end

      # /varrho 
      def ext_inline_verb_rhov(label, content, visitor)
        label = label.to_s
        return nil unless /^rhov:(.*)$/ =~ label
        NormalText.new("&\#x003F1;")
      end

      # /Sigma capital Sigma, Greek 
      def ext_inline_verb_Sigma(label, content, visitor)
        label = label.to_s
        return nil unless /^Sigma:(.*)$/ =~ label
        NormalText.new("&\#x003A3;")
      end

      # /sigma small sigma, Greek 
      def ext_inline_verb_sigma(label, content, visitor)
        label = label.to_s
        return nil unless /^sigma:(.*)$/ =~ label
        NormalText.new("&\#x003C3;")
      end

      # /varsigma 
      def ext_inline_verb_sigmav(label, content, visitor)
        label = label.to_s
        return nil unless /^sigmav:(.*)$/ =~ label
        NormalText.new("&\#x003C2;")
      end

      # /tau small tau, Greek 
      def ext_inline_verb_tau(label, content, visitor)
        label = label.to_s
        return nil unless /^tau:(.*)$/ =~ label
        NormalText.new("&\#x003C4;")
      end

      # /Theta capital Theta, Greek 
      def ext_inline_verb_Theta(label, content, visitor)
        label = label.to_s
        return nil unless /^Theta:(.*)$/ =~ label
        NormalText.new("&\#x00398;")
      end

      # /theta straight theta, small theta, Greek 
      def ext_inline_verb_theta(label, content, visitor)
        label = label.to_s
        return nil unless /^theta:(.*)$/ =~ label
        NormalText.new("&\#x003B8;")
      end

      # /vartheta - curly or open theta 
      def ext_inline_verb_thetav(label, content, visitor)
        label = label.to_s
        return nil unless /^thetav:(.*)$/ =~ label
        NormalText.new("&\#x003D1;")
      end

      # /Upsilon capital Upsilon, Greek 
      def ext_inline_verb_Upsi(label, content, visitor)
        label = label.to_s
        return nil unless /^Upsi:(.*)$/ =~ label
        NormalText.new("&\#x003D2;")
      end

      # /upsilon small upsilon, Greek 
      def ext_inline_verb_upsi(label, content, visitor)
        label = label.to_s
        return nil unless /^upsi:(.*)$/ =~ label
        NormalText.new("&\#x003C5;")
      end

      # /Xi capital Xi, Greek 
      def ext_inline_verb_Xi(label, content, visitor)
        label = label.to_s
        return nil unless /^Xi:(.*)$/ =~ label
        NormalText.new("&\#x0039E;")
      end

      # /xi small xi, Greek 
      def ext_inline_verb_xi(label, content, visitor)
        label = label.to_s
        return nil unless /^xi:(.*)$/ =~ label
        NormalText.new("&\#x003BE;")
      end

      # /zeta small zeta, Greek 
      def ext_inline_verb_zeta(label, content, visitor)
        label = label.to_s
        return nil unless /^zeta:(.*)$/ =~ label
        NormalText.new("&\#x003B6;")
      end

    end
  end
end
