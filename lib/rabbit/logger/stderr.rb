require "glib2"

require "rabbit/logger/base"

module Rabbit
  module Logger
    class STDERR
      include Base

      private
      def do_log(severity, prog_name, message)
        begin
          message = message.encode("locale")
        rescue EncodingError
          format = _("can't convert to current locale from UTF-8: %s")
          $stderr.puts(format % message.encode(format.encoding))
        end
        $stderr.puts(format_severity(severity))
        $stderr.print("[#{prog_name}]: ") if prog_name
        $stderr.puts(message)
        exit if severity >= Severity::FATAL
      end
    end
  end
end
