require "gettext"

module Rabbit
  module GetText
    DOMAIN = "rabbit"

    class << self
      def included(mod)
        mod.send(:include, ::GetText)
        mod.bindtextdomain(DOMAIN)
      end
    end
  end
end
