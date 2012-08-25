# Copyright (C) 2005-2012  Kouhei Sutou <kou@cozmixng.org>
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
require "ostruct"
require "uri"
require "pathname"

require "rabbit/rabbit"
require "rabbit/logger"

require "rabbit/console/roff"

Thread.abort_on_exception = true

module Rabbit
  class Console
    include GetText

    @@locale_dir_option_name = "--locale-dir"

    class << self
      def parse!(args, logger=nil, &block)
        new(logger).parse!(args, &block)
      end

      def get_last_name(klass)
        klass.name.split("::").last
      end
    end

    def initialize(logger=nil)
      @logger = logger || guess_default_logger
    end

    def parse!(args)
      options = OpenStruct.new
      options.logger = @logger
      options.default_logger = @logger
      options.druby_uri = "druby://127.0.0.1:10101"
      options.version = VERSION
      options.options_file = nil
      options.rest = []
      options.after_hooks = []

      process_locale_options(args)

      parser = OptionParser.new(banner) do |_parser|
        yield(_parser, options)
        setup_common_options(_parser, options)
      end

      begin
        options_file = options.options_file
        if options_file and File.exist?(options_file)
          read_options_file(parser, options, options_file)
        end
        options.rest.concat(parser.parse!(args))
        options.after_hooks.each do |hook|
          hook.call(self, parser, options)
        end
      rescue
        @logger.fatal($!.message)
      end

      [options, options.logger]
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

      parser.on("--logger-type=TYPE",
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
      parser.on("--log-level=LEVEL",
                level_names,
                _("Specify log level as [LEVEL]."),
                _("Select from [%s].") % level_names.join(', '),
                "(#{Logger::Severity.name(options.logger.level)})") do |name|
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
      if options.logger.is_a?(Logger::STDERR) and
          options.default_logger == options.logger
        print(GLib.locale_from_utf8(message))
      else
        options.logger.info(message)
      end
      exit
    end

    def get_last_name(klass)
      self.class.get_last_name(klass)
    end

    def guess_default_logger
      if Utils.support_console_output? or !Logger.const_defined?(:GUI)
        Logger::STDERR.new
      else
        Logger::GUI.new
      end
    end
  end
end
