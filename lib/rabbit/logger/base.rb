require "rabbit/gettext"

module Rabbit
  module Logger

    module Severity
      DEBUG = 0
      INFO = 1
      WARN = 2
      ERROR = 3
      FATAL = 4
      UNKNOWN = 5
    end
    
    module Base
      include Severity
      include GetText
      
      def initialize(level=DEBUG)
        @level = level
      end
      
      def debug(message_or_error)
        log(DEBUG, make_message(message_or_error))
      end
      
      def info(message_or_error)
        log(INFO, make_message(message_or_error))
      end
      
      def warn(message_or_error)
        log(WARN, make_message(message_or_error))
      end
      
      def error(message_or_error)
        log(ERROR, make_message(message_or_error))
      end

      def fatal(message_or_error)
        log(FATAL, make_message(message_or_error))
      end
      
      def unknon(message_or_error)
        log(UNKNOWN, make_message(message_or_error))
      end
      
      private
      def log(severity, message)
        if severity >= @level
          do_log(severity, message)
        end
      end

      def severity_name(severity)
        name = Severity.constants.find do |name|
          Severity.const_get(name) == severity
        end
        name || "ANY"
      end
      
      def format_severity(severity)
        "[#{severity_name(severity)}]"
      end

      def format_datetime(datetime)
        datetime.strftime("%Y-%m-%dT%H:%M:%S.") << "%06d " % datetime.usec
      end

      def make_message(message_or_error)
        if message_or_error.is_a?(Exception)
          "#{message_or_error.class}: #{message_or_error.message}\n" +
            %Q[#{message_or_error.backtrace.join("\n")}]
        else
          message_or_error
        end
      end
      
    end
    
  end
end
