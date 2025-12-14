# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../author-configuration"
require_relative "../console"
require_relative "../path-manipulatable"
require_relative "../slide-configuration"
require_relative "../source-generator"

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

      class Data < Struct.new(:title,
                              :allotted_time,
                              :slide_conf,
                              :author_conf,
                              :pdf)
        def available_markup_languages
          {
            :markdown => "Markdown",
            :rd => "RD",
            :hiki => "Hiki",
            :pdf => "PDF",
          }
        end

        def default_markup_language
          :rd
        end

        def available_readme_markup_languages
          {
            :markdown => "Markdown",
            :rd => "RD",
            :hiki => "Hiki",
          }
        end

        def default_readme_markup_language
          if markup_language == :pdf
            :markdown
          else
            markup_language
          end
        end

        def markup_language
          slide_conf.markup_language || author_conf.markup_language || default_markup_language
        end

        def readme_markup_language
          slide_conf.readme_markup_language || default_readme_markup_language
        end

        def save
          author_conf.save
        end
      end

      def initialize
        @use_gui = true
        @data = Data.new
      end

      def run(arguments)
        parse_command_line_arguments(arguments)

        if @use_gui
          return false unless show_gui
        end

        validate
        unless @validation_errors.empty?
          messages = (@validation_errors + [_("See --help for example")])
          Rabbit.logger.error(messages.join("\n"))
          return false
        end

        run_command
        @data.save
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
        @data.author_conf = AuthorConfiguration.new
        @data.author_conf.load
        @data.slide_conf = SlideConfiguration.new
        @data.slide_conf.author = @data.author_conf
        @data.slide_conf.markup_language = @data.markup_language

        format = _("Usage: %s COMMAND [OPTIONS]\n" \
                   " e.g.: %s new \\\n" \
                   "          --id rubykaigi2012 \\\n" \
                   "          --base-name rabbit-introduction \\\n" \
                   "          --markup-language rd \\\n" \
                   "          --width 800 \\\n" \
                   "          --height 450 \\\n" \
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
        parser.separator(_("User interface"))
        parser.on("--no-use-gui",
                  _("Don't use GUI")) do |boolean|
          @use_gui = boolean
        end

        parser.separator("")
        parser.separator(_("Slide information"))

        parser.on("--id=ID",
                  _("Slide ID"),
                  _("(e.g.: %s)") % "--id=rubykaigi2012",
                  _("(must)")) do |id|
          @data.slide_conf.id = id
        end

        messages = [
          _("Base name for the slide source file and generated PDF file"),
          _("(e.g.: %s)") % "--base-name=rabbit-introduction",
          _("(must)"),
        ]
        parser.on("--base-name=NAME",
                  *messages) do |base_name|
          @data.slide_conf.base_name = base_name
        end

        available_markup_languages = @data.available_markup_languages
        label = "[" + available_markup_languages.keys.join(", ") + "]"
        messages = [
          _("Markup language for the new slide"),
          _("(e.g.: %s)") % "--markup-language=rd",
          _("(available markup languages: %s)") % label,
        ]
        messages << _("(default: %s)") % @data.markup_language
        messages << _("(optional)")
        parser.on("--markup-language=LANGUAGE", available_markup_languages.keys,
                  *messages) do |language|
          @data.slide_conf.markup_language = language
        end

        available_readme_markup_languages = @data.available_readme_markup_languages
        label = "[" + available_readme_markup_languages.keys.join(", ") + "]"
        messages = [
          "Markup language for README of the new slide",
          _("(e.g.: %s)") % "--readme-markup-language=rd",
          _("(available markup languages: %s)") % label,
        ]
        messages << "(default: If --markup-language is pdf, then markdown. Otherwise, the same as --markup-language.)"
        messages << _("(optional)")
        parser.on("--readme-markup-language=LANGUAGE", available_readme_markup_languages.keys,
                  *messages) do |language|
          @data.slide_conf.readme_markup_language = language
        end

        parser.on("--pdf=FILE",
                  "Specify the PDF file to copy when using the pdf markup language.",
                  "(must only when --markup-language=pdf)") do |file|
          @data.pdf = file
        end

        parser.on("--title=TITLE",
                  _("Title of the new slide"),
                  _("(e.g.: %s)") % _("--title=\"Rabbit Introduction\""),
                  _("(optional)")) do |title|
          @data.title = title
        end

        parser.on("--licenses=LICENSE,LICENSE,...",
                  Array,
                  _("License of the new slide"),
                  _("(e.g.: %s)") % "--licenses=CC-BY-SA-4.0,GFDL-1.3-or-later",
                  _("(optional)")) do |licenses|
          @data.slide_conf.licenses.concat(licenses)
        end

        parser.on("--tags=TAG,TAG,...",
                  Array,
                  _("Tags of the new slide"),
                  _("(e.g.: %s)") % "--tags=rabbit,presentation,ruby",
               _("(optional)")) do |tags|
          @data.slide_conf.tags.concat(tags)
        end

        parser.on("--allotted-time=TIME",
                  _("Allotted time in presentation"),
                  _("(e.g.: %s)") % "--allotted-time=5m",
                  _("(optional)")) do |allotted_time|
          @data.allotted_time = allotted_time
        end

        parser.on("--presentation-date=DATE", Date,
                  _("Presentation date with the new slide"),
                  _("(e.g.: %s)") % "--presentation-date=2012-06-29",
                  _("(optional)")) do |date|
          @data.slide_conf.presentation_date = date
        end

        presentation_start_time_example =
          "--presentation-start-time=2012-06-29T10:30:00+0900"
        parser.on("--presentation-start-time=TIME", Time,
                  _("Presentation start time"),
                  _("(e.g.: %s)") % presentation_start_time_example,
                  _("(optional)")) do |time|
          @data.slide_conf.presentation_start_time = time
        end

        presentation_end_time_example =
          "--presentation-end-time=2012-06-29T11:00:00+0900"
        parser.on("--presentation-end-time=TIME", Time,
                  _("Presentation end time"),
                  _("(e.g.: %s)") % presentation_end_time_example,
                  _("(optional)")) do |time|
          @data.slide_conf.presentation_end_time = time
        end

        width_example = "--width=800"
        parser.on("--width=WIDTH", Integer,
                  _("The default slide width"),
                  _("(e.g.: %s)") % width_example,
                  _("(optional)")) do |width|
          @data.slide_conf.width = width
        end

        height_example = "--width=450"
        parser.on("--height=HEIGHT", Integer,
                  _("The default slide height"),
                  _("(e.g.: %s)") % height_example,
                  _("(optional)")) do |height|
          @data.slide_conf.height = height
        end

        parser.separator(_("Your information"))

        messages = [
          _("Author name of the new slide"),
          _("(e.g.: %s)") % "--name=\"Kouhei Sutou\"",
        ]
        if @data.author_conf.name
          messages << _("(default: %s)") % @data.author_conf.name
        end
        messages << _("(optional)")
        parser.on("--name=NAME",
                  *messages) do |name|
          @data.author_conf.name = name
        end

        messages = [
          _("Author e-mail of the new slide"),
          _("(e.g.: %s)") % "--email=kou@cozmixng.org",
        ]
        if @data.author_conf.email
          messages << _("(default: %s)") % @data.author_conf.email
        end
        messages << _("(optional)")
        parser.on("--email=EMAIL",
                  *messages) do |email|
          @data.author_conf.email = email
        end

        messages = [
          _("Account for %s") % "RubyGems.org",
          _("It is used to publish your slide to %s") % "RubyGems.org",
          _("(e.g.: %s)") % "--rubygems-user=kou",
        ]
        if @data.author_conf.rubygems_user
          messages << _("(default: %s)") % @data.author_conf.rubygems_user
        end
        messages << _("(optional)")
        parser.on("--rubygems-user=USER",
                  *messages) do |user|
          @data.author_conf.rubygems_user = user
        end

        messages = [
          _("Account for %s") % "SlideShare",
          _("It is used to publish your slide to %s") % "SlideShare",
          _("(e.g.: %s)") % "--slideshare-user=kou",
        ]
        if @data.author_conf.slideshare_user
          messages << _("(default: %s)") % @data.author_conf.slideshare_user
        end
        messages << _("(optional)")
        parser.on("--slideshare-user=USER",
                  *messages) do |user|
          @data.author_conf.slideshare_user = user
        end

        messages = [
          _("Account for %s") % "Speaker Deck",
          _("It is used to publish your slide to %s") % "Speaker Deck",
          _("(e.g.: %s)") % "--speaker-deck-user=kou",
        ]
        if @data.author_conf.speaker_deck_user
          messages << _("(default: %s)") % @data.author_conf.speaker_deck_user
        end
        messages << _("(optional)")
        parser.on("--speaker-deck-user=USER",
                  *messages) do |user|
          @data.author_conf.speaker_deck_user = user
        end
      end

      def default_command
        if File.file?("config.yaml")
          "change"
        else
          "new"
        end
      end

      def available_commands
        ["new", "change"]
      end

      class TextMapper
        def initialize(data, label_widget, entry_widget)
          @data = data
          @label_widget = label_widget
          @entry_widget = entry_widget

          widgets = [@label_widget, @entry_widget]
          if Gtk::Version::MAJOR < 4
            update_class = lambda do |valid|
              if valid
                widgets.each do |widget|
                  widget.style_context.remove_class(Gtk::STYLE_CLASS_ERROR)
                end
              else
                widgets.each do |widget|
                  widget.style_context.add_class(Gtk::STYLE_CLASS_ERROR)
                end
              end
            end
          else
            update_class = lambda do |valid|
              if valid
                widgets.each do |widget|
                  widget.remove_css_class("error")
                end
              else
                widgets.each do |widget|
                  widget.add_css_class("error")
                end
              end
            end
          end
          @entry_widget.signal_connect(:notify) do |_, param_spec|
            if param_spec.name == "text"
              update_class.call(valid?(@entry_widget.buffer.text))
            end
          end
          if value
            @entry_widget.buffer.text = value
          else
            update_class.call(valid?(@entry_widget.buffer.text))
          end
        end

        def apply
          apply_value(@entry_widget.buffer.text)
        end
      end

      class IntegerMapper
        def initialize(data, widget)
          @data = data
          @widget = widget

          @widget.value = value if value
        end

        def apply
          apply_value(@widget.value_as_int)
        end
      end

      class SlideIDMapper < TextMapper
        private
        def valid?(value)
          not value.empty?
        end

        def value
          @data.slide_conf.id
        end

        def apply_value(value)
          @data.slide_conf.id = value
        end
      end

      class SlideBaseNameMapper < TextMapper
        private
        def valid?(value)
          not value.empty?
        end

        def value
          @data.slide_conf.base_name
        end

        def apply_value(value)
          @data.slide_conf.base_name = value
        end
      end

      class SlideMarkupLanguageMapper
        def initialize(data, widget)
          @data = data
          @widget = widget
        end

        def apply
          if Gtk::Version::MAJOR < 4
            combo_box = @widget
            id = combo_box.active_id
            id = id.to_sym if id
          else
            drop_down = @widget
            id = @data.available_markup_languages.keys[drop_down.selected]
          end
          @data.slide_conf.markup_language = id
        end
      end

      class SlideWidthMapper < IntegerMapper
        private
        def value
          @data.slide_conf.width
        end

        def apply_value(value)
          @data.slide_conf.width = value
        end
      end

      class SlideHeightMapper < IntegerMapper
        private
        def value
          @data.slide_conf.height
        end

        def apply_value(value)
          @data.slide_conf.height = value
        end
      end

      class SlidePDFMapper < TextMapper
        private
        def valid?(value)
          if @data.markup_language == :pdf
            not value.empty?
          else
            value.empty?
          end
        end

        def value
          @data.pdf
        end

        def apply_value(value)
          @data.pdf = value
        end
      end

      def show_gui
        require_relative "../gtk"

        @application = Gtk::Application.new("org.rabbit_shocker.RabbitSlide",
                                            [:non_unique, :handles_command_line])
        succeeded = false
        @application.signal_connect(:command_line) do |_, command_line|
          GLib.application_name = "Rabbit Slide"
          @application.activate
          succeeded ? 0 : 1
        end
        @application.signal_connect(:activate) do |_, command_line|
          show_window do
            successed = true
          end
        end
        @application.run
      end

      def show_window(&on_success)
        window = Gtk::ApplicationWindow.new(@application)

        grid = Gtk::Grid.new
        grid.column_homogeneous = true
        nth_row = 0
        mappers = []

        mappers << add_id_widget(grid, nth_row)
        nth_row += 1

        mappers << add_base_name_widget(grid, nth_row)
        nth_row += 1

        mappers << add_markup_language_widget(grid, nth_row)
        nth_row += 1

        mappers << add_height_widget(grid, nth_row)
        nth_row += 1

        mappers << add_width_widget(grid, nth_row)
        nth_row += 1

        mappers << add_pdf_widget(grid, nth_row)
        nth_row += 1

        nth_column = 0
        cancel_button = Gtk::Button.new(mnemonic: _("_Cancel"))
        cancel_button.signal_connect(:clicked) do
          window.destroy
        end
        grid.attach(cancel_button, nth_column, nth_row, 1, 1)
	nth_column += 1
        apply_button = Gtk::Button.new(mnemonic: _("_Apply"))
        apply_button.signal_connect(:clicked) do
          mappers.each(&:apply)
          on_success.call
          window.destroy
        end
        grid.attach(apply_button, nth_column, nth_row, 1, 1)

        frame = Gtk::Frame.new(_("Slide information"))
        frame.child = grid
        window.child = frame

        window.show_all if window.respond_to?(:show_all)
        window.present
      end

      def add_id_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new(_("ID:"))
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)
        nth_column += 1
        entry_widget = Gtk::Entry.new
        grid.attach(entry_widget, nth_column, nth_row, 1, 1)
        SlideIDMapper.new(@data, label_widget, entry_widget)
      end

      def add_base_name_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new(_("Base name:"))
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)
        nth_column += 1
        entry_widget = Gtk::Entry.new
        grid.attach(entry_widget, nth_column, nth_row, 1, 1)
        SlideBaseNameMapper.new(@data, label_widget, entry_widget)
      end

      def add_markup_language_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new(_("Markup language:"))
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)
        nth_column += 1
        if Gtk::Version::MAJOR < 4
          widget = Gtk::ComboBoxText.new
          @data.available_markup_languages.each do |key, value|
            widget.append(key.to_s, value)
          end
          widget.active_id = @data.markup_language
        else
          markups = @data.available_markup_languages.values
          widget = Gtk::DropDown.new(Gtk::StringList.new(markups))
          ids = @data.available_markup_languages.keys
          widget.selected = ids.index(@data.markup_language) || 0
        end
        grid.attach(widget, nth_column, nth_row, 1, 1)
        SlideMarkupLanguageMapper.new(@data, widget)
      end

      def add_height_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new(_("Height:"))
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)
        nth_column += 1
        adjustment = Gtk::Adjustment.new(@data.slide_conf.height,
                                         100,
                                         4000,
                                         10,
                                         100,
                                         10)
        widget = Gtk::SpinButton.new(adjustment, 10, 0)
        grid.attach(widget, nth_column, nth_row, 1, 1)
        SlideHeightMapper.new(@data, widget)
      end

      def add_width_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new(_("Width:"))
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)
        nth_column += 1
        adjustment = Gtk::Adjustment.new(@data.slide_conf.width,
                                         100,
                                         4000,
                                         10,
                                         100,
                                         10)
        widget = Gtk::SpinButton.new(adjustment, 10, 0)
        grid.attach(widget, nth_column, nth_row, 1, 1)
        SlideWidthMapper.new(@data, widget)
      end

      def add_source_dialog_filter(dialog, name, pattern)
        filter = Gtk::FileFilter.new
        filter.name = "#{name} (#{pattern})"
        filter.add_pattern(pattern)
        dialog.add_filter(filter)
      end

      def add_pdf_widget(grid, nth_row)
        nth_column = 0
        label_widget = Gtk::Label.new("PDF")
        label_widget.halign = :end
        grid.attach(label_widget,
                    nth_column, nth_row,
                    1, 1)

        nth_column += 1
        hbox_widget = Gtk::Box.new(:horizontal, 0)
        entry_widget = Gtk::Entry.new
        hbox_widget.pack_start(entry_widget,  expand: true,  fill: true,  padding: 0)
        button_widget = Gtk::Button.new(label: "Open")
        button_widget.signal_connect("clicked") do
          dialog = Gtk::FileChooserDialog.new(:title => "Choose a PDF file",
                                              :action => :open,
                                              :buttons => [[Gtk::Stock::CANCEL, :cancel],
                                                          [Gtk::Stock::OPEN, :accept]])
          dialog.set_filename(@data.pdf) if @data.pdf
          add_source_dialog_filter(dialog, "PDF files", "*.pdf")
          add_source_dialog_filter(dialog, "All files", "*")
          if dialog.run == Gtk::ResponseType::ACCEPT
            entry_widget.text = dialog.filename
          end
          dialog.destroy
        end
        hbox_widget.pack_start(button_widget, expand: false, fill: false, padding: 0)
        grid.attach(hbox_widget, nth_column, nth_row, 1, 1)

        SlidePDFMapper.new(@data, label_widget, entry_widget)
      end

      def validate
        @validation_errors = []
        validate_command
        validate_id
        validate_base_name
        validate_pdf
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
        if @data.slide_conf.id.nil?
          @validation_errors << (_("%s is missing") % "--id")
        end
      end

      def validate_base_name
        if @data.slide_conf.base_name.nil?
          @validation_errors << (_("%s is missing") % "--base-name")
        end
      end

      def validate_pdf
        return unless @data.markup_language == :pdf
        if @data.pdf.nil?
          @validation_errors << (_("%s is missing") % "--pdf")
          return
        end
        unless File.file?(@data.pdf)
          @validation_errors << (_("not a file: %s") % @data.pdf)
        end
      end

      def run_command
        __send__("run_command_#{@command}")
      end

      def run_command_new
        generate_directory
        generate_template
      end

      def run_command_change
        merge_config_yaml
        generate_template
      end

      def merge_config_yaml
        existing_slide_conf = SlideConfiguration.new
        existing_slide_conf.load
        existing_slide_conf.merge!(@data.slide_conf.to_hash)
        @data.slide_conf = existing_slide_conf
        @data.author_conf = @data.slide_conf.author
      end

      def generate_directory
        create_directory(base_directory)
        create_directory(File.join(base_directory, "pdf")) if @data.markup_language == :pdf
      end

      def generate_template
        generate_dot_gitignore
        generate_dot_rabbit
        generate_slide_configuration
        generate_readme
        generate_rakefile
        generate_slide
      end

      def generate_dot_gitignore
        create_file(".gitignore") do |dot_gitignore|
          lines = [
            ".DS_Store",
            "/.tmp/",
            "/pkg/",
          ]
          lines << "/pdf/" unless @data.markup_language == :pdf
          dot_gitignore.puts(lines.join("\n"))
        end
      end

      def generate_dot_rabbit
        create_file(".rabbit") do |dot_rabbit|
          options = []
          size = [@data.slide_conf.width, @data.slide_conf.height].join(",")
          options << "--size #{size}"
          if @data.slide_conf.markup_language.nil? and @data.allotted_time
            options << "--allotted-time #{@data.allotted_time}"
          end
          options << slide_path
          dot_rabbit.puts(options.join("\n"))
        end
      end

      def generate_slide_configuration
        @data.slide_conf.save(base_directory)
      end

      def generate_readme
        create_file("README.#{readme_extension}") do |readme|
          readme.puts(readme_content)
        end
      end

      def readme_content
        markup_language = @data.readme_markup_language
        generator = Rabbit::SourceGenerator.find(markup_language)

        content = ""
        title = @data.title || _("TODO: SLIDE TITLE")
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
        install_command = "gem install #{@data.slide_conf.gem_name}"
        content << generator.preformatted_line(install_command)
        content << "\n\n"
        content << generator.heading(3, _("Show"))
        content << "\n\n"
        show_command = "rabbit #{@data.slide_conf.gem_name}.gem"
        content << generator.preformatted_line(show_command)
        content << "\n\n"
      end

      def generate_rakefile
        create_file("Rakefile") do |rakefile|
          rakefile.puts(<<-'RAKEFILE')
require "rabbit/task/slide"

# Edit ./config.yaml to customize meta data

spec = nil
Rabbit::Task::Slide.new do |task|
  spec = task.spec
  # spec.files += Dir.glob("doc/**/*.*")
  # spec.files -= Dir.glob("private/**/*.*")
  # spec.add_runtime_dependency("rabbit-theme-YOUR-THEME")
end

desc "Tag #{spec.version}"
task :tag do
  sh("git", "tag", "-a", spec.version.to_s, "-m", "Publish #{spec.version}")
  sh("git", "push", "--tags")
end
          RAKEFILE
        end
      end

      def generate_slide
        if @data.markup_language == :pdf
          copy_file(@data.pdf, slide_path)
          return
        end

        source = slide_source
        return if source.nil?
        create_file(slide_path) do |slide|
          slide.puts(source)
        end
      end

      def slide_path
        case @data.markup_language
        when :pdf
          File.join("pdf", @data.slide_conf.pdf_base_path)
        else
          "#{@data.slide_conf.base_name}.#{slide_source_extension}"
        end
      end

      def slide_source_extension
        case @data.markup_language
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
        case @data.readme_markup_language
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
        generator = Rabbit::SourceGenerator.find(@data.markup_language)
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
        presentation_date = @data.slide_conf.presentation_date
        presentation_date_default = Time.now
        allotted_time_default = "5m"
        allotted_time =
          Utils.ensure_time(@data.allotted_time || allotted_time_default)
        start_time = @data.slide_conf.presentation_start_time
        end_time = @data.slide_conf.presentation_end_time
        if presentation_date
          start_time ||= presentation_date
          end_time ||= presentation_date + allotted_time
          presentation_date = presentation_date.strftime("%Y-%m-%d")
        end
        start_time = start_time.iso8601 if start_time
        end_time = end_time.iso8601 if end_time
        start_time_default = presentation_date_default
        end_time_default = start_time_default + allotted_time
        slide_metadata = [
          ["subtitle",       nil,                     _("SUBTITLE")],
          ["author",         @data.author_conf.name,  _("AUTHOR")],
          ["institution",    nil,                     _("INSTITUTION")],
          ["content-source", nil,                     _("EVENT NAME")],
          [
            "date",
            presentation_date,
            presentation_date_default.strftime("%Y-%m-%d"),
          ],
          ["allotted-time",  @data.allotted_time, "5m"],
          ["start-time",     start_time,          start_time_default.iso8601],
          ["end-time",       end_time,            end_time_default.iso8601],
          ["theme",          nil,                 "default"],
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
        source << "\n"
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

      def base_directory
        case @command
        when "change"
          "."
        else
          @data.slide_conf.id
        end
      end

      def create_file(path, &block)
        super(File.join(base_directory, path), &block)
      end

      def copy_file(from, to)
        super(from, File.join(base_directory, to))
      end
    end
  end
end
