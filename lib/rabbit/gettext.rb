require "gettext"

module Rabbit
  module GetText

    DOMAIN = "rabbit"
    module_function
    def bindtextdomain(path=nil, locale=nil, charset=nil)
      if Rabbit::Config.const_defined?(:GETTEXT_PATH)
        path ||= Rabbit::Config::GETTEXT_PATH
      end
      charset ||= "UTF-8"
      ::GetText.bindtextdomain(DOMAIN, path, locale, charset)
    end

    def _(msgid)
      ::GetText.gettext(msgid)
    end

    def N_(msgid)
      msgid
    end

    def locale=(locale)
      ::GetText.locale = locale
    end
  end

  module Locale
    module_function
    def get
      ::Locale.get
    end
  end
end
