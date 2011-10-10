begin
  require "rubygems"
  raise LoadError if Gem.respond_to?(:_deprecated_all_load_paths)
  require "gettext"
  module GetText
    if ::GetText::TextDomainManager.respond_to?(:textdomain)
      alias _gettext gettext
      module_function :_gettext
      def gettext(msgid)
        return msgid
        if @@__textdomain[callersrc]
          _gettext(msgid)
        else
          msgid
        end
      end
    end
  end
rescue LoadError
  module GetText

    module_function
    def bindtextdomain(*args)
    end
    
    def gettext(msgid)
      msgid
    end
  end

  module Locale
    module_function
    def get
      ["LC_ALL", "LC_MESSAGES", "LANG"].each do |env|
        ret = ENV[env]
        break if ret
      end
      ret = "C" unless ret
      ret
    end
  end
end

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
      if defined?(::GetText::TextDomainManager) and path
        if ::GetText::TextDomainManager.respond_to?(:textdomain)
          # workaround for Ruby-GetText 1.6.0 < 2.0.0
          textdomain = ::GetText::TextDomainManager.textdomain(DOMAIN)
          locale_paths = ["#{path}/%{locale}/LC_MESSAGES/%{name}.mo",
                        "#{path}/%{locale}/%{name}.mo"]
          textdomain.locale_paths.concat(locale_paths)
          locale ||= textdomain.current_locale || Locale.get
          textdomain.set_locale(locale, true)
        end
      end
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
