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
require "rabbit/path-manipulatable"

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
        @config_yaml_path = "config.yaml"
        @id = nil
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
            @id = id
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
        if @id.nil?
          @validation_errors << (_("%s is missing") % "--id")
        end
      end

      def generate
        generate_directory
        generate_data_directory
        generate_readme
        generate_rakefile
        generate_property_rb
        generate_theme_rb
      end

      def generate_directory
        create_directory(@id)
      end

      def generate_data_directory
        create_directory(File.join(@id, "data"))
      end

      def generate_readme
        create_file("README.#{readme_extension}") do |readme|
          readme.puts(readme_content)
        end
      end

      def readme_content
        markup_language = @author_conf.markup_language || :rd
        syntax = markup_syntax(markup_language)

        content = ""
        content << (syntax[:heading1] % {:title => _("TODO: THEME TITLE")})
        content << "\n\n"
        content << _("TODO: THEME DESCRIPTION")
        content << "\n\n"

        content << (syntax[:heading2] % {:title => _("For author")})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Show")})
        content << "\n\n"
        content << (syntax[:preformatted_line] % {:content => "rake"})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Publish")})
        content << "\n\n"
        content << (syntax[:preformatted_line] % {:content => "rake publish"})
        content << "\n\n"

        content << (syntax[:heading2] % {:title => _("For users")})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Install")})
        content << "\n\n"
        install_command = "gem install #{gem_name}"
        content << (syntax[:preformatted_line] % {:content => install_command})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Show")})
        content << "\n\n"
        show_command = "rabbit -t #{gem_name} theme-bench.sample"
        content << (syntax[:preformatted_line] % {:content => show_command})
        content << "\n\n"
      end

      def generate_rakefile
        create_file("Rakefile") do |rakefile|
          rakefile.puts(<<-EOR)
require "time"
require "yaml"
require "rabbit/task/theme"

config = YAML.load(File.read("#{@config_yaml_path}"))

theme_id = config["id"]

version = "1.0.0"

name = config["name"]
email = config["email"]
rubygems_user = config["rubygems_user"]

readme = File.read(Dir.glob("README*")[0])

readme_blocks = readme.split(/(?:\\r?\\n){2,}/)
summary = (readme_blocks[0] || "TODO").gsub(/\\A(?:[=*!]+|h\\d\\.)\s*/, "")
description = readme_blocks[1] || "TODO"

specification = Gem::Specification.new do |spec|
  prefix = "#{gem_name_prefix}"
  spec.name = "\#{prefix}-\#{theme_id}"
  spec.version = version
  spec.homepage = "http://theme.rabbit-shockers.org/\#{theme_id}/"
  spec.authors = [name]
  spec.email = [email]
  spec.summary = summary
  spec.description = description
  spec.licenses = [] # ["CC BY-SA 3.0"]

  spec.files = ["#{@config_yaml_path}", "Rakefile"]
  spec.files += Dir.glob("{theme.rb,COPYING,GPL,README*}")
  spec.files += Dir.glob("data/**/*.{svg,png,jpg,jpeg,gif,eps,pdf}")
  spec.files += Dir.glob("locale/**/*.mo")
  spec.files += Dir.glob("po/*/*.po")

  spec.add_runtime_dependency("rabbit")
end

Rabbit::Task::Theme.new(specification) do |task|
  task.rubygems_user = rubygems_user
end
EOR
        end
      end

      def generate_property_rb
        create_file("property.rb") do |property_rb|
          property_rb.puts(<<-EOP)
@category = N_("#{category}")
@title = N_("#{@id}")
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
        @id.end_with?("-images")
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

      def markup_syntax(markup_language)
        case markup_language
        when :rd
          {
            :heading1             => "= %{title}",
            :heading2             => "== %{title}",
            :heading3             => "=== %{title}",
            :definition_list_item => ": %{item}\n   %{description}",
            :unorderd_list_item   => "  * %{item}",
            :image                =>
              "  # image\n" +
              "  # src = %{src}\n" +
              "  # relative_height = %{relative_height}",
            :preformatted_line    => "  %{content}",
            :comment              => "# %{content}",
          }
        when :hiki
          {
            :heading1             => "! %{title}",
            :heading2             => "!! %{title}",
            :heading3             => "!!! %{title}",
            :definition_list_item => ":%{item}:%{description}",
            :unorderd_list_item   => "* %{item}",
            :image                =>
              "{{image(\"%{src}\",\n" +
              "        {\n" +
              "          :relative_height => %{relative_height},\n" +
              "        })}}",
            :preformatted_line    => " %{content}",
            :comment              => "// %{content}",
          }
        when :markdown
          {
            :heading1             => "# %{title}",
            :heading2             => "## %{title}",
            :heading3             => "### %{title}",
            :definition_list_item => "%{item}\n   %{description}",
            :unorderd_list_item   => "* %{item}",
            :image                =>
              "![](%{src}){:relative_height='%{relative_height}'}",
            :preformatted_line    => "    %{content}",
            :comment              => "",
          }
        else
          nil
        end
      end

      def gem_name
        "#{gem_name_prefix}-#{@id}"
      end

      def gem_name_prefix
        "rabbit-theme"
      end
    end
  end
end
