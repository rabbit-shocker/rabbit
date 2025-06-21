# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require 'English'

require "shellwords"
require "optparse"
require "optparse/date"
require "optparse/time"
require "ostruct"
require "uri"
require "pathname"

require "rabbit/rabbit"

require "rabbit/console/roff"

Thread.abort_on_exception = true

module Rabbit
  class Console
    include GetText

    @@locale_dir_option_name = "--locale-dir"

    class << self
      def parse!(args, &block)
        new.parse!(args, &block)
      end

      def get_last_name(klass)
        klass.name.split("::").last
      end
    end

    def initialize
      @have_custom_logger = false
    end

    def parse!(args)
      options = OpenStruct.new
      options.druby_uri = "druby://127.0.0.1:10101"
      options.version = VERSION
      options.options_file = nil
      options.rest = []
      options.before_hooks = []
      options.after_hooks = []

      process_locale_options(args)

      parser = OptionParser.new(banner) do |_parser|
        yield(_parser, options)
        setup_common_options(_parser, options)
      end

      begin
        options_file = options.options_file
        if options_file and File.file?(options_file)
          read_options_file(parser, options, options_file)
        end
        options.before_hooks.each do |hook|
          hook.call(self, parser, options)
        end
        options.rest.concat(parser.parse!(args))
        options.after_hooks.each do |hook|
          hook.call(self, parser, options)
        end
      rescue => error
        Rabbit.logger.error("#{error.class}: #{error.message}")
        error.backtrace.each do |line|
          Rabbit.logger.error(line)
        end
        raise
      end

      options
    end

    def read_options_file(parser, options, options_file)
      options_in_file = []
      File.open(options_file) do |file|
        file.each_line do |line|
          options_in_file.concat(Shellwords.split(line))
        end
      end
      source_info = parser.parse(options_in_file)

      source_info = source_info.collect do |path|
        if URI(path).scheme
          path
        else
          if Pathname(path).absolute?
            path
          else
            File.join(File.dirname(options_file), path)
          end
        end
      end
      options.rest.concat(source_info)
    end

    private
    def banner
      _("Usage: %s [options]") % File.basename($0, '.*')
    end

    def process_locale_options(args)
      args.each_with_index do |arg, i|
        if arg == @@locale_dir_option_name
          self.class.bindtextdomain(GetText::DOMAIN, :path => args[i + 1])
        elsif /#{@@locale_dir_option_name}=/ =~ arg
          self.class.bindtextdomain(GetText::DOMAIN, :path => $POSTMATCH)
        end
      end
    end

    def setup_common_options(parser, options)
      parser.separator ""
      parser.separator _("Common options")

      setup_options_options(parser, options)
      setup_locale_options(parser, options)
      setup_logger_options(parser, options)
      setup_common_options_on_tail(parser, options)
    end

    def setup_options_options(parser, options)
      parser.on("--options-file=FILE",
                _("Load options from FILE."),
                _("(none)")) do |file|
        read_options_file(parser, options, file)
      end

      parser.separator ""
    end

    def setup_locale_options(parser, options)
      parser.on("--locale-dir=DIR",
                _("Specify locale dir as [DIR]."),
                _("(auto)")) do |directory|
        self.class.bindtextdomain(GetText::DOMAIN, :path => directory)
      end

      parser.separator ""
    end

    def setup_logger_options(parser, options)
      logger_type_names = Rabbit::Logger.types.collect do |x|
        get_last_name(x).downcase
      end

      default_logger_class = ::Rabbit.logger.class
      parser.on("--logger-type=TYPE",
                logger_type_names,
                _("Specify logger type as [TYPE]."),
                _("Select from [%s].") % logger_type_names.join(', '),
                "(#{get_last_name(default_logger_class)})") do |logger_type|
        logger_class = Rabbit::Logger.types.find do |t|
          get_last_name(t).downcase == logger_type.downcase
        end
        if logger_class.nil?
          ::Rabbit.logger.error("Unknown logger type: #{t}")
        else
          ::Rabbit.logger = logger_class.new
          @have_custom_logger = true
        end
      end

      level_names = Logger::Severity.names
      default_logger_level = ::Rabbit.logger.level
      parser.on("--log-level=LEVEL",
                level_names,
                _("Specify log level as [LEVEL]."),
                _("Select from [%s].") % level_names.join(', '),
                "(#{Logger::Severity.name(default_logger_level)})") do |name|
        options.logger.level = Logger::Severity.level(name)
      end

      parser.separator ""
    end

    def setup_common_options_on_tail(parser, options)
      parser.on_tail("--help", _("Show this message.")) do
        output_info_and_exit(options, parser.to_s)
      end

      parser.on_tail("--version", _("Show version.")) do
        output_info_and_exit(options, "#{options.version}\n")
      end
    end

    def output_info_and_exit(options, message)
      if !@have_custom_logger and ::Rabbit.logger.is_a?(Logger::STDERR)
        ::Rabbit.logger.info(message)
      else
        puts(message)
      end
      exit
    end

    def get_last_name(klass)
      self.class.get_last_name(klass)
    end
  end
end
