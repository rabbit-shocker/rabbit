require 'English'

require "optparse"
require "ostruct"

require "rabbit/rabbit"
require "rabbit/logger"

require "rabbit/console/roff"

Thread.abort_on_exception = true

include Rabbit::GetText

module Rabbit
  module Console
    @@locale_dir_option_name = "--locale-dir"

    module_function
    def parse!(args, logger=nil)
      bindtextdomain
      logger ||= guess_default_logger
      options = OpenStruct.new
      options.logger = logger
      options.default_logger = logger

      process_locale_options(args)

      opts = OptionParser.new(banner) do |opts|
        yield(opts, options)
        setup_common_options(opts, options)
      end

      begin
        opts.parse!(args)
      rescue
        logger.fatal($!.message)
      end

      [options, options.logger]
    end

    def banner
      _("Usage: %s [options]") % File.basename($0, '.*')
    end

    def process_locale_options(args)
      args.each_with_index do |arg, i|
        if arg == @@locale_dir_option_name
          bindtextdomain(args[i + 1])
        elsif /#{@@locale_dir_option_name}=/ =~ arg
          bindtextdomain($POSTMATCH)
        end
      end
    end

    def setup_common_options(opts, options)
      opts.separator ""
      opts.separator _("Common options")

      setup_locale_options(opts, options)
      setup_logger_options(opts, options)
      setup_common_options_on_tail(opts, options)
    end
    
    def setup_locale_options(opts, options)
      opts.on("--locale-dir=DIR",
              _("Specify locale dir as [DIR]."),
              _("(auto)")) do |d|
        bindtextdomain(d)
      end

      opts.separator ""
    end

    def setup_logger_options(opts, options)
      logger_type_names = Rabbit::Logger.types.collect do |x|
        get_last_name(x).downcase
      end

      opts.on("--logger-type=TYPE",
              logger_type_names,
              _("Specify logger type as [TYPE]."),
              _("Select from [%s].") % logger_type_names.join(', '),
              "(#{get_last_name(options.logger.class)})") do |logger_type|
        logger_class = Rabbit::Logger.types.find do |t|
          get_last_name(t).downcase == logger_type.downcase
        end
        if logger_class.nil?
          options.logger = options.default_logger
          # logger.error("Unknown logger type: #{t}")
        else
          options.logger = logger_class.new
        end
      end

      level_names = Logger::Severity.names
      opts.on("--log-level=LEVEL",
              level_names,
              _("Specify log level as [LEVEL]."),
              _("Select from [%s].") % level_names.join(', '),
              "(#{Logger::Severity.name(options.logger.level)})") do |name|
        options.logger.level = Logger::Severity.level(name)
      end

      opts.separator ""
    end

    def setup_common_options_on_tail(opts, options)
      opts.on_tail("--help", _("Show this message.")) do
        output_info_and_exit(options, opts.to_s)
      end

      opts.on_tail("--version", _("Show version.")) do
        output_info_and_exit(options, "#{VERSION}\n")
      end
    end

    def output_info_and_exit(options, message)
      if options.logger.is_a?(Logger::STDERR) and
          options.default_logger == options.logger
        print(GLib.locale_from_utf8(message))
      else
        options.logger.info(message)
      end
      exit
    end

    def get_last_name(klass)
      klass.name.split("::").last
    end

    def guess_default_logger
      if Utils.support_console_output? or !Logger.defined?(:GUI)
        Logger::STDERR.new
      else
        Logger::GUI.new
      end
    end
  end
end
