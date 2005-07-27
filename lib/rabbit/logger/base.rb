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
      
      def initialize(level=INFO)
        @level = level
      end
      
      def debug(message_or_error=nil, &block)
        log(DEBUG, message_or_error, &block)
      end
      
      def info(message_or_error=nil, &block)
        log(INFO, message_or_error, &block)
      end
      
      def warn(message_or_error=nil, &block)
        log(WARN, message_or_error, &block)
      end
      
      def error(message_or_error=nil, &block)
        log(ERROR, message_or_error, &block)
      end

      def fatal(message_or_error=nil, &block)
        log(FATAL, message_or_error, &block)
      end
      
      def unknown(message_or_error=nil, &block)
        log(UNKNOWN, message_or_error, &block)
      end

      def <<(message_or_error)
        info(message_or_error)
      end
      
      private
      def log(severity, message_or_error, &block)
        if severity >= @level
          if message_or_error.nil? and block_given?
            message_or_error = yield
          end
          if message_or_error
            do_log(severity, make_message(message_or_error))
          end
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
