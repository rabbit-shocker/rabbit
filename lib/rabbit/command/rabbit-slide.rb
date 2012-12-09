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
require "rabbit/slide-configuration"
require "rabbit/path-manipulatable"
require "rabbit/source-generator"

module Rabbit
  module Command
    class RabbitSlide
      include GetText
      include PathManipulatable

      class << self
        def run(*arguments)
          new.run(arguments)
        end
      end

      def initialize
        @title = nil
        @allotted_time = nil
        @slide_conf = nil
        @author_conf = nil
        @logger = nil
      end

      def run(arguments)
        parse_command_line_arguments(arguments)

        validate
        unless @validation_errors.empty?
          messages = (@validation_errors + [_("See --help for example")])
          @logger.error(messages.join("\n"))
          return false
        end

        run_command
        @author_conf.save
        true
      end

      private
      def parse_command_line_arguments(arguments)
        Rabbit::Console.parse!(ARGV) do |parser, options|
          setup_options(parser, options)
        end
        @command = @options.rest.first || default_command
      end

      def setup_options(parser, options)
        @options = options
        @logger = @options.default_logger
        @author_conf = AuthorConfiguration.new(@logger)
        @author_conf.load
        @slide_conf = SlideConfiguration.new(@logger)
        @slide_conf.author = @author_conf

        format = _("Usage: %s COMMAND [OPTIONS]\n" \
                   " e.g.: %s new \\\n" \
                   "          --id rubykaigi2012 \\\n" \
                   "          --base-name rabbit-introduction \\\n" \
                   "          --markup-language rd \\\n" \
                   "          --name \"Kouhei Sutou\" \\\n" \
                   "          --email kou@cozmixng.org \\\n" \
                   "          --rubygems-user kou \\\n" \
                   "          --slideshare-user kou \\\n" \
                   "          --speaker-deck-user kou")

        program = File.basename($0, ".*")
        parser.banner = format % [program, program]

        parser.separator("")
        parser.separator(_("COMMAND"))
        parser.separator(_("  new:    create a new slide"))
        parser.separator(_("  change: change an existing slide"))

        parser.separator("")
        parser.separator(_("Slide information"))

        parser.on("--id=ID",
                  _("Slide ID"),
                  _("(e.g.: %s)") % "--id=rubykaigi2012",
                  _("(must)")) do |id|
          @slide_conf.id = id
        end

        messages = [
          _("Base name for the slide source file and generated PDF file"),
          _("(e.g.: %s)") % "--base-name=rabbit-introduction",
          _("(must)"),
        ]
        parser.on("--base-name=NAME",
                  *messages) do |base_name|
          @slide_conf.base_name = base_name
        end

        available_markup_languages = [:rd, :hiki, :markdown]
        label = "[" + available_markup_languages.join(", ") + "]"
        messages = [
          _("Markup language for the new slide"),
          _("(e.g.: %s)") % "--markup-language=rd",
          _("(available markup languages: %s)") % label,
        ]
        if @author_conf.markup_language
          messages << _("(default: %s)") % @author_conf.markup_language
        end
        messages << _("(optional)")
        parser.on("--markup-language=LANGUAGE", available_markup_languages,
                  *messages) do |language|
          @author_conf.markup_language = language
        end

        parser.on("--title=TITLE",
                  _("Title of the new slide"),
                  _("(e.g.: %s)") % _("--title=\"Rabbit Introduction\""),
                  _("(optional)")) do |title|
          @title = title
        end

        parser.on("--tags=TAG,TAG,...",
                  Array,
                  _("Tags of the new slide"),
                  _("(e.g.: %s)") % "--tags=rabbit,presentation,ruby",
               _("(optional)")) do |tags|
          @slide_conf.tags.concat(tags)
        end

        parser.on("--allotted-time=TIME",
                  _("Allotted time in presentaion"),
                  _("(e.g.: %s)") % "--allotted-time=5m",
                  _("(optional)")) do |allotted_time|
          @allotted_time = allotted_time
        end

        parser.on("--presentation-date=DATE",
                  _("Presentation date with the new slide"),
                  _("(e.g.: %s)") % "--presentation-date=2012/06/29",
                  _("(optional)")) do |date|
          @slide_conf.presentation_date = date
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

        messages = [
          _("Account for %s") % "SlideShare",
          _("It is used to publish your slide to %s") % "SlideShare",
          _("(e.g.: %s)") % "--slideshare-user=kou",
        ]
        if @author_conf.slideshare_user
          messages << _("(default: %s)") % @author_conf.slideshare_user
        end
        messages << _("(optional)")
        parser.on("--slideshare-user=USER",
                  *messages) do |user|
          @author_conf.slideshare_user = user
        end

        messages = [
          _("Account for %s") % "Speaker Deck",
          _("It is used to publish your slide to %s") % "Speaker Deck",
          _("(e.g.: %s)") % "--speaker-deck-user=kou",
        ]
        if @author_conf.speaker_deck_user
          messages << _("(default: %s)") % @author_conf.speaker_deck_user
        end
        messages << _("(optional)")
        parser.on("--speaker-deck-user=USER",
                  *messages) do |user|
          @author_conf.speaker_deck_user = user
        end
      end

      def default_command
        if File.file(".rabbit")
          "change"
        else
          "new"
        end
      end

      def available_commands
        ["new", "change"]
      end

      def validate
        @validation_errors = []
        validate_command
        validate_id
        validate_base_name
      end

      def validate_command
        if @options.rest.size > 1
          message = _("too many commands: %s") % @options.rest.inspect
          @validation_errors << message
        end
        unless available_commands.include?(@command)
          format = _("invalid command: <%s>: available commands: %s")
          message = format % [@command, "[#{available_commands.join(', ')}]"]
          @validation_errors << message
        end
      end

      def validate_id
        if @slide_conf.id.nil?
          @validation_errors << (_("%s is missing") % "--id")
        end
      end

      def validate_base_name
        if @slide_conf.base_name.nil?
          @validation_errors << (_("%s is missing") % "--base-name")
        end
      end

      def run_command
        __send__("run_command_#{@command}")
      end

      def run_command_new
        generate_directory
        generate_dot_gitignore
        generate_dot_rabbit
        generate_slide_configuration
        generate_readme
        generate_rakefile
        generate_slide
      end

      def run_command_change
      end

      def generate_directory
        create_directory(@slide_conf.id)
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

      def generate_dot_rabbit
        create_file(".rabbit") do |dot_rabbit|
          options = []
          if @author_conf.markup_language.nil? and @allotted_time
            options << "--allotted-time #{@allotted_time}"
          end
          options << slide_path
          dot_rabbit.puts(options.join("\n"))
        end
      end

      def generate_slide_configuration
        @slide_conf.save(@slide_conf.id)
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
        title = @title || _("TODO: SLIDE TITLE")
        content << generator.heading(1, title)
        content << "\n\n"
        content << _("TODO: SLIDE DESCRIPTION")
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

        content << generator.heading(2, _("For viewers"))
        content << "\n\n"
        content << generator.heading(3, _("Install"))
        content << "\n\n"
        install_command = "gem install #{@slide_conf.gem_name}"
        content << generator.preformatted_line(install_command)
        content << "\n\n"
        content << generator.heading(3, _("Show"))
        content << "\n\n"
        show_command = "rabbit #{@slide_conf.gem_name}.gem"
        content << generator.preformatted_line(show_command)
        content << "\n\n"
      end

      def generate_rakefile
        create_file("Rakefile") do |rakefile|
          rakefile.puts(<<-EOR)
require "rabbit/task/slide"

# Edit ./config.yaml to customize meta data

Rabbit::Task::Slide.new do |task|
  # task.spec.licenses = ["CC BY-SA 3.0"]
  # task.spec.files += Dir.glob("doc/**/*.*")
  # task.spec.files -= Dir.glob("private/**/*.*")
  # task.spec.add_runtime_dependency("YOUR THEME")
end
EOR
        end
      end

      def generate_slide
        source = slide_source
        return if source.nil?
        create_file(slide_path) do |slide|
          slide.puts(source)
        end
      end

      def slide_path
        "#{@slide_conf.base_name}.#{slide_source_extension}"
      end

      def slide_source_extension
        case @author_conf.markup_language
        when :rd
          "rab"
        when :hiki
          "hiki"
        when :markdown
          "md"
        else
          "pdf"
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

      def slide_source
        generator = Rabbit::SourceGenerator.find(@author_conf.markup_language)
        return nil if generator.nil?

        source = ""
        slide_source_title(source, generator, @title || _("TITLE"))
        slide_source_metadata(source, generator)
        slide_source_title(source, generator, _("FIRST SLIDE"))
        slide_source_items(source, generator)
        slide_source_title(source, generator, _("SECOND SLIDE"))
        slide_source_image(source, generator)
      end

      def slide_source_title(source, generator, title)
        source << generator.heading(1, title)
        source << "\n\n"
      end

      def slide_source_metadata(source, generator)
        presentation_date = @slide_conf.presentation_date
        slide_metadata = [
          ["subtitle",       nil,                _("SUBTITLE")],
          ["author",         @author_conf.name,  _("AUTHOR")],
          ["institution",    nil,                _("INSTITUTION")],
          ["content-source", nil,                _("EVENT NAME")],
          ["date",           presentation_date,  Time.now.strftime("%Y/%m/%d")],
          ["allotted-time",  @allotted_time,     "5m"],
          ["theme",          nil,                "default"],
        ]
        slide_metadata.each do |key, value, default_value|
          item = generator.definition_list_item(key, value || default_value)
          item << "\n"
          if value
            source << item
          else
            item.each_line do |line|
              source << generator.comment(line)
            end
          end
        end
        source << "\n\n"
      end

      def slide_source_items(source, generator)
        1.upto(3) do |i|
          source << generator.unordered_list_item(_("ITEM %d") % i)
          source << "\n"
        end
        source << "\n"
      end

      def slide_source_image(source, generator)
        lavie = "https://raw.github.com/rabbit-shocker/rabbit/master/sample/lavie.png"
        options = {
          :relative_height => 100,
        }
        source << generator.image(lavie, options)
        source << "\n"
      end

      def create_file(path, &block)
        super(File.join(@slide_conf.id, path), &block)
      end
    end
  end
end
