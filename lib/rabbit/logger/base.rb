require "rabbit/gettext"

module Rabbit
  module Logger

    module Severity
      class << self
        def names
          MARK_TABLE.sort_by {|key, _| key}.collect {|_, value| value.downcase}
        end

        def name(level)
          MARK_TABLE[level].downcase
        end

        def level(name)
          MARK_TABLE.find do |key, value|
            value.downcase == name.downcase
          end[0]
        end

        def N_(message)
          message
        end
      end

      DEBUG = 0
      INFO = 1
      WARNING = 2
      ERROR = 3
      FATAL = 4
      UNKNOWN = 5

      MARK_TABLE = {
        DEBUG => N_("DEBUG"),
        INFO => N_("INFO"),
        WARNING => N_("WARNING"),
        ERROR => N_("ERROR"),
        FATAL => N_("FATAL"),
        UNKNOWN => N_("UNKNOWN"),
      }
    end

    module Base
      include GetText

      attr_accessor :level, :webrick_mode
      def initialize(level=Severity::INFO, prog_name=nil)
        @level = level
        @prog_name = prog_name
        @webrick_mode = false
      end

      def debug?; @level <= Severity::DEBUG; end
      def info?; @level <= Severity::INFO; end
      def warning?; @level <= Severity::WARNING; end
      def error?; @level <= Severity::ERROR; end
      def fatal?; @level <= Severity::FATAL; end
      def unknown?; @level <= Severity::UNKNOWN; end

      def debug(message_or_error=nil, &block)
        log(Severity::DEBUG, message_or_error, &block)
      end

      def info(message_or_error=nil, &block)
        log(Severity::INFO, message_or_error, &block)
      end

      def warning(message_or_error=nil, &block)
        log(Severity::WARNING, message_or_error, &block)
      end
      alias_method :warn, :warning # for backward compatibility

      def error(message_or_error=nil, &block)
        log(Severity::ERROR, message_or_error, &block)
      end

      def fatal(message_or_error=nil, &block)
        log(Severity::FATAL, message_or_error, &block)
      end

      def unknown(message_or_error=nil, &block)
        log(Severity::UNKNOWN, message_or_error, &block)
      end

      def <<(message_or_error)
        info(message_or_error)
      end

      def log(severity, message_or_error, prog_name=nil, &block)
        severity ||= Severity::UNKNOWN
        prog_name ||= @prog_name
        if need_log?(severity)
          if message_or_error.nil? and block_given?
            message_or_error = yield
          end
          if message_or_error
            do_log(severity, prog_name, make_message(message_or_error))
          end
        end
      end
      alias add log

      private
      def format_severity(severity)
        "[#{_(Severity::MARK_TABLE[severity])}]"
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

      def need_log?(severity)
        if @webrick_mode
          severity <= WEBrick::Log::DEBUG - @level
        else
          severity >= @level
        end
      end
    end
  end
end
