require "rabbit/logger/base"

module Rabbit
  module Logger

    class STDERR
      include Base

      private
      def do_log(severity, message)
        message = GLib.filename_from_utf8(message)
        ::STDERR.puts(format_severity(severity), message)
        exit if severity >= FATAL
      end
      
    end
    
  end
end
