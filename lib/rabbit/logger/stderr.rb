require "rabbit/logger/base"

module Rabbit
  module Logger

    class STDERR
      include Base

      private
      def do_log(severity, message)
        begin
          message = GLib.locale_from_utf8(message)
        rescue GLib::ConvertError
          format = _("can't convert to current locale from UTF-8: %s")
          ::STDERR.puts(format % message)
        end
        ::STDERR.puts(format_severity(severity), message)
        exit if severity >= FATAL
      end
      
    end
    
  end
end
