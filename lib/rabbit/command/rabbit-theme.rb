# Copyright (C) 2012  Kouhei Sutou <kou@cozmixng.org>
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

require "yaml"

require "rabbit/console"
require "rabbit/author-configuration"
require "rabbit/theme-configuration"
require "rabbit/path-manipulatable"
require "rabbit/source-generator"

module Rabbit
  module Command
    class RabbitTheme
      include GetText
      include PathManipulatable

      class << self
        def run(*arguments)
          new.run(arguments)
        end
      end

      def initialize
        @theme_conf = nil
        @author_conf = nil
        @logger = nil
      end

      def run(arguments)
        @options, @logger = parse_command_line_arguments(arguments)

        validate
        unless @validation_errors.empty?
          messages = (@validation_errors + [_("See --help for example")])
          @logger.error(messages.join("\n"))
          return false
        end

        generate
        @author_conf.save
        true
      end

      private
      def parse_command_line_arguments(arguments)
        Rabbit::Console.parse!(ARGV) do |parser, options|
          @logger = options.default_logger
          @author_conf = AuthorConfiguration.new(@logger)
          @author_conf.load
          @theme_conf = ThemeConfiguration.new(@logger)
          @theme_conf.author = @author_conf

          format = _("Usage: %s new [options]\n" \
                     " e.g.: %s new \\\n" \
                     "          --id rubykaigi2012 \\\n" \
                     "          --name \"Kouhei Sutou\" \\\n" \
                     "          --email kou@cozmixng.org \\\n" \
                     "          --rubygems-user kou")

          program = File.basename($0, ".*")
          parser.banner = format % [program, program]

          parser.separator("")

          parser.separator(_("Theme information"))

          parser.on("--id=ID",
                    _("Theme ID"),
                    _("(e.g.: %s)") % "--id=rubykaigi2012",
                    _("(must)")) do |id|
            @theme_conf.id = id
          end

          parser.separator(_("Your information"))

          messages = [
            _("Author name of the new slide"),
            _("(e.g.: %s)") % "--name=\"Kouhei Sutou\"",
          ]
          if @author_conf.name
            messages << _("(default: %s)") % @author_conf.name
          end
          messages << _("(optional)")
          parser.on("--name=NAME",
                    *messages) do |name|
            @author_conf.name = name
          end

          messages = [
            _("Author e-mail of the new slide"),
            _("(e.g.: %s)") % "--email=kou@cozmixng.org",
          ]
          if @author_conf.email
            messages << _("(default: %s)") % @author_conf.email
          end
          messages << _("(optional)")
          parser.on("--email=EMAIL",
                    *messages) do |email|
            @author_conf.email = email
          end

          messages = [
            _("Account for %s") % "RubyGems.org",
            _("It is used to publish your slide to %s") % "RubyGems.org",
            _("(e.g.: %s)") % "--rubygems-user=kou",
          ]
          if @author_conf.rubygems_user
            messages << _("(default: %s)") % @author_conf.rubygems_user
          end
          messages << _("(optional)")
          parser.on("--rubygems-user=USER",
                    *messages) do |user|
            @author_conf.rubygems_user = user
          end
        end
      end

      def validate
        @validation_errors = []
        validate_command
        validate_id
      end

      def validate_command
        if @options.rest.empty?
          @options.rest << "new"
        end
        if @options.rest.size != 1
          message = _("too many commands: %s") % @options.rest.inspect
          @validation_errors << message
        end
        @command = @options.rest[0]
        if @command != "new"
          format = _("invalid command: <%s>: available commands: %s")
          message = format % [@command, "[new]"]
          @validation_errors << message
        end
      end

      def validate_id
        if @theme_conf.id.nil?
          @validation_errors << (_("%s is missing") % "--id")
        end
      end

      def generate
        generate_directory
        generate_data_directory
        generate_dot_gitignore
        generate_theme_configuration
        generate_readme
        generate_rakefile
        generate_property_rb
        generate_theme_rb
      end

      def generate_directory
        create_directory(@theme_conf.id)
      end

      def generate_data_directory
        create_directory(File.join(@theme_conf.id, "data"))
      end

      def generate_dot_gitignore
        create_file(".gitignore") do |dot_gitignore|
          dot_gitignore.puts(<<-EOD)
/.tmp/
/pkg/
/pdf/
EOD
        end
      end

      def generate_theme_configuration
        @theme_conf.save(@theme_conf.id)
      end

      def generate_readme
        create_file("README.#{readme_extension}") do |readme|
          readme.puts(readme_content)
        end
      end

      def readme_content
        markup_language = @author_conf.markup_language || :rd
        generator = Rabbit::SourceGenerator.find(markup_language)

        content = ""
        content << generator.heading(1, _("TODO: THEME TITLE"))
        content << "\n\n"
        content << _("TODO: THEME DESCRIPTION")
        content << "\n\n"

        content << generator.heading(2, _("For author"))
        content << "\n\n"
        content << generator.heading(3, _("Show"))
        content << "\n\n"
        content << generator.preformatted_line("rake")
        content << "\n\n"
        content << generator.heading(3, _("Publish"))
        content << "\n\n"
        content << generator.preformatted_line("rake publish")
        content << "\n\n"

        content << generator.heading(2, _("For users"))
        content << "\n\n"
        content << generator.heading(3, _("Install"))
        content << "\n\n"
        install_command = "gem install #{@theme_conf.gem_name}"
        content << generator.preformatted_line(install_command)
        content << "\n\n"
        content << generator.heading(3, _("Show"))
        content << "\n\n"
        theme_benchmark_gem = _("rabbit-theme-benchmark-en.gem")
        show_command = "rabbit -t #{@theme_conf.gem_name} #{theme_benchmark_gem}"
        content << generator.preformatted_line(show_command)
        content << "\n\n"
      end

      def generate_rakefile
        create_file("Rakefile") do |rakefile|
          rakefile.puts(<<-EOR)
require "rabbit/task/theme"

# Edit ./config.yaml to customize meta data

Rabbit::Task::Theme.new do |task|
  # task.spec.licenses = ["CC BY-SA 3.0"]
  # task.spec.files += Dir.glob("doc/**/*.*")
  # task.spec.files -= Dir.glob("private/**/*.*")
  # task.spec.add_runtime_dependency("DEPENDED THEME")
end
EOR
        end
      end

      def generate_property_rb
        create_file("property.rb") do |property_rb|
          property_rb.puts(<<-EOP)
@category = N_("#{category}")
@title = N_("#{@theme_conf.id}")
# @abstract = N_("TODO")
# @description = N_("TODO")
EOP
        end
      end

      def generate_theme_rb
        return if image_theme?
        create_file("theme.rb") do |theme_rb|
          theme_rb.puts(<<-EOT)
include_theme("default")
EOT
        end
      end

      def image_theme?
        @theme_conf.id.end_with?("-images")
      end

      def category
        if image_theme?
          "Image"
        else
          "Theme"
        end
      end

      def readme_extension
        case @author_conf.markup_language
        when :rd
          "rd"
        when :hiki
          "hiki"
        when :markdown
          "md"
        else
          "rd"
        end
      end

      def create_file(path, &block)
        super(File.join(@theme_conf.id, path), &block)
      end
    end
  end
end
