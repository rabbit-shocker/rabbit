begin
  require "gettext"
rescue LoadError
  module GetText

    def bindtextdomain(*args)
    end
    
    def gettext(msgid)
      msgid
    end
  end
end

module Rabbit
  module GetText

    module_function
    def bindtextdomain(path=nil, locale=nil, charset=nil)
      charset ||= "UTF-8"
      ::GetText.bindtextdomain("rabbit", path, locale, charset)
    end
    
    def _(msgid)
      ::GetText.gettext(msgid)
    end

    def N_(msgid)
      msgid
    end

  end
end
