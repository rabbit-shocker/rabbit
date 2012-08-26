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

require "fileutils"
require "yaml"

require "rabbit/console"

module Rabbit
  module Command
    class RabbitSlide
      include GetText

      class ValidationError < StandardError
      end

      class << self
        def run(*arguments)
          new.run(arguments)
        end
      end

      def initialize
        @slide_conf_path = File.expand_path("~/.rabbit/slide.yaml")
        @config_yaml_path = "config.yaml"
        @id = nil
        @base_name = nil
        @markup = nil
        @title = nil
        @tags = []
        @allotted_time = nil
        @presentation_date = nil
        @author = nil
        @email = nil
        @rubygems_user = nil
        @slideshare_user = nil
        @speaker_deck_user = nil
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
        save_slide_conf
        true
      end

      private
      def parse_command_line_arguments(arguments)
        Rabbit::Console.parse!(ARGV) do |parser, options|
          @logger = options.default_logger
          load_slide_conf

          format = _("Usage: %s new [options]\n" \
                     " e.g.: %s new \\\n" \
                     "          --id rubykaigi2012 \\\n" \
                     "          --base-name rabbit-introduction \\\n" \
                     "          --markup rd \\\n" \
                     "          --author \"Kouhei Sutou\" \\\n" \
                     "          --email kou@cozmixng.org \\\n" \
                     "          --rubygems-user kou \\\n" \
                     "          --slideshare-user kou \\\n" \
                     "          --speaker-deck-user kou")

          program = File.basename($0, ".*")
          parser.banner = format % [program, program]

          parser.separator("")

          parser.separator(_("Slide information"))

          parser.on("--id=ID",
                    _("Slide ID"),
                    _("(e.g.: --id=rubykaigi2012)"),
                    _("(must)")) do |id|
            @id = id
          end

          messages = [
            _("Base name for the slide source file and generated PDF file"),
            _("(e.g.: --base-name=rabbit-introduction)"),
            _("(must)"),
          ]
          parser.on("--base-name=NAME",
                    *messages) do |base_name|
            @base_name = base_name
          end

          available_markups = [:rd, :hiki, :markdown]
          available_markups_label = "[" + available_markups.join(", ") + "]"
          messages = [
            _("Markup language for the new slide"),
            _("(e.g.: --markup=rd)"),
            _("(available markups: %s)") % available_markups_label,
          ]
          if @markup
            messages << _("(default: %s)") % @markup
          end
          messages << _("(must)")
          parser.on("--markup=MARKUP", available_markups,
                    *messages) do |markup|
            @markup = markup
          end

          parser.on("--title=TITLE",
                    _("Title of the new slide"),
                    _("(e.g.: --title=\"Rabbit Introduction\")"),
                    _("(optional)")) do |title|
            @title = title
          end

          parser.on("--tags=TAG,TAG,...",
                    Array,
                    _("Tags of the new slide"),
                    _("(e.g.: --tags=Rabbit,Presentation,Ruby)"),
                 _("(optional)")) do |tags|
            @tags.concat(tags)
          end

          parser.on("--allotted-time=TIME",
                    _("Allotted time in presentaion"),
                    _("(e.g.: --allotted-time=5m)"),
                    _("(optional)")) do |allotted_time|
            @allotted_time = allotted_time
          end

          parser.on("--presentation-date=DATE",
                    _("Presentation date with the new slide"),
                    _("(e.g.: --presentation-date=2012/06/29)"),
                    _("(optional)")) do |date|
            @presentation_date = date
          end

          parser.separator(_("Your information"))

          messages = [
            _("Author of the new slide"),
            _("(e.g.: --author=\"Kouhei Sutou\")"),
          ]
          if @author
            messages << _("(default: %s)") % @author
          end
          messages << _("(optional)")
          parser.on("--author=AUTHOR",
                    *messages) do |author|
            @author = author
          end

          messages = [
            _("Author e-mail of the new slide"),
            _("(e.g.: --email=kou@cozmixng.org)"),
          ]
          if @email
            messages << _("(default: %s)") % @email
          end
          messages << _("(optional)")
          parser.on("--email=EMAIL",
                    *messages) do |email|
            @email = email
          end

          messages = [
            _("Account for %s") % "RubyGems.org",
            _("It is used to publish your slide to %s") % "RubyGems.org",
            _("(e.g.: --rubygems-user=kou)"),
          ]
          if @rubygems_user
            messages << _("(default: %s)") % @rubygems_user
          end
          messages << _("(optional)")
          parser.on("--rubygems-user=USER",
                    *messages) do |user|
            @rubygems_user = user
          end

          messages = [
            _("Account for %s") % "SlideShare",
            _("It is used to publish your slide to %s") % "SlideShare",
            _("(e.g.: --slideshare-user=kou)"),
          ]
          if @slideshare_user
            messages << _("(default: %s)") % @slideshare_user
          end
          messages << _("(optional)")
          parser.on("--slideshare-user=USER",
                    *messages) do |user|
            @slideshare_user = user
          end

          messages = [
            _("Account for %s") % "Speaker Deck",
            _("It is used to publish your slide to %s") % "Speaker Deck",
            _("(e.g.: --speaker-deck-user=kou)"),
          ]
          if @speaker_deck_user
            messages << _("(default: %s)") % @speaker_deck_user
          end
          messages << _("(optional)")
          parser.on("--speaker-deck-user=USER",
                    *messages) do |user|
            @speaker_deck_user = user
          end
        end
      end

      def validate
        @validation_errors = []
        validate_command
        validate_id
        validate_base_name
        validate_markup
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

      def validate_base_name
        if @base_name.nil?
          @validation_errors << (_("%s is missing") % "--base-name")
        end
      end

      def validate_markup
        if @markup.nil?
          @validation_errors << (_("%s is missing") % "--markup")
        end
      end

      def generate
        generate_directory
        generate_dot_rabbit
        generate_config_yaml
        generate_readme
        generate_rakefile
        generate_slide
      end

      def generate_directory
        create_directory(@id)
      end

      def generate_dot_rabbit
        create_file(".rabbit") do |dot_rabbit|
          options = []
          if @markup.nil? and @allotted_time
            options << "--allotted-time #{@allotted_time}"
          end
          options << slide_path
          dot_rabbit.puts(options.join("\n"))
        end
      end

      def generate_config_yaml
        create_file(@config_yaml_path) do |config_yaml|
          config = {
            "id"                => @id,
            "tags"              => @tags,
            "base_name"         => @base_name,
            "author"            => @author,
            "presentation_date" => @presentation_date,
            "email"             => @email,
            "rubygems_user"     => @rubygems_user,
            "slideshare_user"   => @slideshare_user,
            "speaker_deck_user" => @speaker_deck_user,
          }
          config_yaml.puts(config.to_yaml)
        end
      end

      def generate_readme
        create_file("README.#{readme_extension}") do |readme|
          readme.puts(readme_content)
        end
      end

      def readme_content
        markup = @markup || :rd
        syntax = markup_syntax(markup)

        content = ""
        content << (syntax[:heading1] % {:title => _("TODO: SLIDE TITLE")})
        content << "\n\n"
        content << _("TODO: SLIDE DESCRIPTION")
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

        content << (syntax[:heading2] % {:title => _("For viewers")})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Install")})
        content << "\n\n"
        install_command = "gem install #{gem_name}"
        content << (syntax[:preformatted_line] % {:content => install_command})
        content << "\n\n"
        content << (syntax[:heading3] % {:title => _("Show")})
        content << "\n\n"
        show_command = "rabbit #{gem_name}.gem"
        content << (syntax[:preformatted_line] % {:content => show_command})
        content << "\n\n"
      end

      def generate_rakefile
        create_file("Rakefile") do |rakefile|
          rakefile.puts(<<-EOR)
require "time"
require "yaml"
require "rabbit/task"

config = YAML.load(File.read("#{@config_yaml_path}"))

slide_id = config["id"]
tags = config["tags"]
base_name = config["base_name"]
pdf_base_path = "\#{base_name}.pdf"

version = nil
presentation_date = config["presentation_date"]
parsed_presentation_date = nil
if presentation_date
  begin
    parsed_presentation_date = Time.parse(presentation_date)
  rescue ArgumentError
  end
  if parsed_presentation_date
    version = parsed_presentation_date.strftime("%Y.%m.%d")
  end
end
version ||= "1.0.0"

author = config["author"]
email = config["email"]
rubygems_user = config["rubygems_user"]
slideshare_user = config["slideshare_user"]
speaker_deck_user = config["speaker_deck_user"]

readme = File.read(Dir.glob("README*")[0])

readme_blocks = readme.split(/(?:\\r?\\n){2,}/)
summary = (readme_blocks[0] || "TODO").gsub(/\\A(?:[=*!]+|h\\d\\.)\s*/, "")
description = readme_blocks[1] || "TODO"

specification = Gem::Specification.new do |spec|
  prefix = "rabbit-slide"
  spec.name = "\#{prefix}-\#{rubygems_user}-\#{slide_id}"
  spec.version = version
  spec.homepage = "http://slide.rabbit-shockers.org/\#{rubygems_user}/\#{slide_id}/"
  spec.authors = [author]
  spec.email = [email]
  spec.summary = summary
  spec.description = description
  spec.licenses = [] # ["CC BY-SA 3.0"]

  spec.files = [".rabbit"]
  spec.files += Dir.glob("{COPYING,GPL,Rakefile,README*}")
  spec.files += Dir.glob("rabbit/**/*.*")
  spec.files += Dir.glob("**/*.{svg,png,jpg,jpeg,gif,eps,pdf}")
  spec.files += Dir.glob("*.{rd,rab,hiki,md,pdf}")
  spec.files -= Dir.glob("{pkg,pdf}/**/*.*")

  spec.add_runtime_dependency("rabbit")
end

Rabbit::Task::Slide.new(specification) do |task|
  task.rubygems_user = rubygems_user
  task.slideshare_user = slideshare_user
  task.speaker_deck_user = speaker_deck_user
  task.pdf_base_path = pdf_base_path
  task.tags = tags
  task.presentation_date = parsed_presentation_date
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
        "#{@base_name}.#{slide_source_extension}"
      end

      def slide_source_extension
        case @markup
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
        case @markup
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
        syntax = slide_source_syntax
        return nil if syntax.nil?

        source = ""
        slide_source_title(source, syntax, _("TITLE"))
        slide_source_metadata(source, syntax)
        slide_source_title(source, syntax, _("FIRST SLIDE"))
        slide_source_items(source, syntax)
        slide_source_title(source, syntax, _("SECOND SLIDE"))
        slide_source_image(source, syntax)
      end

      def slide_source_title(source, syntax, title)
        source << (syntax[:heading1] % {:title => _("TITLE")})
        source << "\n\n"
      end

      def slide_source_metadata(source, syntax)
        slide_metadata = [
          ["subtitle",       nil,                _("SUBTITLE")],
          ["author",         @author,            _("AUTHOR")],
          ["institution",    nil,                _("INSTITUTION")],
          ["content-source", nil,                _("EVENT NAME")],
          ["date",           @presentation_date, Time.now.strftime("%Y/%m/%d")],
          ["allotted-time",  @allotted_time,     "5m"],
          ["theme",          nil,                "default"],
        ]
        slide_metadata.each do |key, value, default_value|
          data = {:item => key, :description => value || default_value}
          item = syntax[:definition_list_item] % data
          item << "\n"
          if value
            source << item
          else
            item.each_line do |line|
              source << (syntax[:comment] % {:content => line})
            end
          end
        end
        source << "\n\n"
      end

      def slide_source_items(source, syntax)
        1.upto(3) do |i|
          source << syntax[:unorderd_list_item] % {:item => _("ITEM %d") % i}
          source << "\n"
        end
        source << "\n"
      end

      def slide_source_image(source, syntax)
        lavie = "https://raw.github.com/shockers/rabbit/master/sample/lavie.png"
        data = {
          :src => lavie,
          :relative_height => 100,
        }
        source << syntax[:image] % data
        source << "\n"
      end

      def slide_source_syntax
        markup_syntax(@markup)
      end

      def markup_syntax(markup)
        case markup
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

      def create_directory(path)
        @logger.info(_("Creating directory: %s") % path)
        FileUtils.mkdir_p(path)
      end

      def create_file(path, &block)
        unless Pathname(path).absolute?
          path = File.join(@id, path)
        end
        @logger.info(_("Creating file:      %s") % path)
        File.open(path, "w", &block)
      end

      def load_slide_conf
        return unless File.exist?(@slide_conf_path)
        conf = YAML.load(File.read(@slide_conf_path))
        @markup            = conf["markup"]
        @author            = conf["author"]
        @email             = conf["email"]
        @rubygems_user     = conf["rubygems_user"]
        @slideshare_user   = conf["slideshare_user"]
        @speaker_deck_user = conf["speaker_deck_user"]
      rescue
        format = _("Failed to read slide configuration: %s: %s")
        @logger.error(format % [@slide_conf_path, $!.message])
      end

      def save_slide_conf
        conf = {
          "markup"            => @markup,
          "author"            => @author,
          "email"             => @email,
          "rubygems_user"     => @rubygems_user,
          "slideshare_user"   => @slideshare_user,
          "speaker_deck_user" => @speaker_deck_user,
        }
        create_directory(File.dirname(@slide_conf_path))
        create_file(@slide_conf_path) do |slide_conf|
          slide_conf.print(conf.to_yaml)
        end
      rescue
        format = _("Failed to write slide configuration: %s: %s")
        @logger.error(format % [@slide_conf_path, $!.message])
      end

      def gem_name
        "rabbit-slide-#{@rubygems_user}-#{@id}"
      end
    end
  end
end
