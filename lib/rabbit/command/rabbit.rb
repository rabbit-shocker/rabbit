# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "English"

require_relative "../rabbit"
require_relative "../slide-configuration"

module Rabbit
  module Command
    class Rabbit
      include GetText

      class << self
        def run(*arguments)
          new.run(arguments)
        end
      end

      def run(arguments)
        GC.disable
        require "rbconfig"

        require_relative "../console"
        require_relative "../front"
        require_relative "../renderer"
        require_relative "../source"

        @options = parse_command_line_arguments(arguments)

        require_relative "../canvas"
        GC.enable

        application = ::Rabbit.application
        succeeded = false
        application.signal_connect("command-line") do |_, command_line|
          application.activate
          succeeded ? 0 : 1
        end
        application.signal_connect("activate") do
          begin
            succeeded = catch do |abort_tag|
              @abort_tag = abort_tag
              __send__("do_#{@options.action}")
            end
          rescue
            ::Rabbit.logger.error($!)
          end
        end
        exit_code = application.run
        application.windows.each(&:destroy)
        application.unref

        exit_code.zero?
      end

      private
      def parse_command_line_arguments(arguments)
        Console.parse!(arguments) do |parser, options|
          rest_arguments_from_options_file = []
          options.before_hooks << lambda do |_, _, _|
            rest_arguments_from_options_file = options.rest.dup
            options.rest.clear
          end
          options.after_hooks << lambda do |console, _, _|
            if options.rest.empty?
              options.rest = rest_arguments_from_options_file
            end
          end

          options.after_hooks << lambda do |console, _, _|
            adjust_rest_arguments(console, parser, options)
          end

          options.action = :display
          options.options_file = ".rabbit"
          options.theme = "default"
          options.theme_specified = false
          options.comment_theme = nil
          options.allotted_time = ENV["RABBIT_ALLOTTED_TIME"]
          options.base = nil
          options.source_type = :auto
          options.full_screen = false
          options.index_mode = false
          options.initial_slide = 0
          options.geometry = nil
          options.width = 800
          options.height = 600
          options.paper_width = nil
          options.paper_height = nil
          options.saved_image_base_name = nil
          options.saved_image_type = "png"
          options.output_html = false
          options.output_index_html = false
          options.rss_base_uri = nil
          options.encoding = nil
          options.print_out_filename = nil
          options.slides_per_page = 1
          options.draw_scaled_image = nil
          options.margin_left = nil
          options.margin_right = nil
          options.margin_top = nil
          options.margin_bottom = nil
          options.page_margin_left = nil
          options.page_margin_right = nil
          options.page_margin_top = nil
          options.page_margin_bottom = nil
          options.use_druby = true
          options.output_druby_uri = false
          options.use_soap = false
          options.soap_host = "0.0.0.0"
          options.soap_port = 10103
          options.use_xmlrpc = false
          options.xmlrpc_host = "0.0.0.0"
          options.xmlrpc_port = 10104
          options.default_public_level = "all"
          options.public_level = nil
          options.migemo_dictionary_search_path = [
            File.join(RbConfig::CONFIG["prefix"], "share"),
            File.join("", "usr", "local", "share"),
            File.join("", "usr", "share"),
          ].uniq
          options.migemo_dictionary_name = "migemo-dict"
          options.use_gl = false
          options.show_native_window_id = false
          options.source_filename = nil


          parser.banner = "#{parser.banner} [SOURCE_INFOS]"

          parser.category _("Theme")

          parser.on("-I", "--include=PATH",
                   _("Add [PATH] to load path.")) do |path|
            $LOAD_PATH.unshift(File.expand_path(path))
          end

          parser.on("-t", "--theme=THEME",
                    _("Use [THEME] as theme."),
                   "(#{options.theme})") do |theme|
            options.theme = theme
            options.theme_specified = true
          end

          parser.on("--comment-theme=THEME",
                    _("Use [THEME] for comment."),
                    "(#{options.comment_theme})") do |theme|
            options.comment_theme = theme
          end

          parser.on("--allotted-time=TIME",
                    _("Use [TIME] as allotted time."),
                    "(#{options.allotted_time})") do |time|
            options.allotted_time = time
          end


          parser.category _("Source")

          source_type_names = Source.types.collect do |x|
            Console.get_last_name(x).downcase
          end
          source_type_names.unshift("auto")
          source_type_descs = Source.types.collect do |x|
            message = _("When select %s\nspecify %s\nas [SOURCE_INFOS].")
            type = Console.get_last_name(x)
            message = message % [type, _(x.initial_args_description)]
            message.split(/\n/) + [" "]
          end.flatten
          parser.on("-T", "--type=TYPE",
                    source_type_names,
                    _("Specify source type as [TYPE]."),
                    _("Select from [%s].") % source_type_names.join(', '),
                    _("Note: case insensitive."),
                    "(auto)",
                    " ",
                    *source_type_descs) do |source_type|
            if source_type == "auto"
              options.source_type = :auto
            else
              options.source_type = Source.types.find do |t|
                Console.get_last_name(t).downcase == source_type.downcase
              end
            end
          end

          parser.on("-e", "--encoding=ENCODING",
                    _("Specify source encoding as [ENCODING]."),
                    _("(auto)")) do |encoding|
            options.encoding = encoding
          end

          parser.on("-B", "--base=BASE",
                    _("Specify base URI or path of source as [BASE]."),
                    _("(auto)")) do |base|
            options.base = base
          end


          parser.category _("Initial state")

          parser.on("-f", "--[no-]full-screen",
                    _("Toggle full screen mode."),
                    "(#{options.full_screen ? 'on' : 'off'})") do |bool|
            options.full_screen = bool
          end

          parser.on("--[no-]index-mode",
                    _("Toggle index mode."),
                    "(#{options.index_mode ? 'on' : 'off'})") do |bool|
            options.index_mode = bool
          end

          parser.on("--initial-slide=N", Integer,
                    _("Show the Nth slide. (zero-based)"),
                    "(#{options.initial_slide})") do |n|
            options.initial_slide = n
          end


          parser.category _("Size")

          parser.on("-g", "--geometry=GEOMETRY",
                    _("Set window geometry [GEOMETRY]."),
                    _("Format: WIDTHxHEIGHT+X+Y"),
                    "(#{options.geometry.inspect})") do |geometry|
            options.geometry = geometry
          end

          parser.on("-w", "--width=WIDTH",
                    Integer,
                    _("Set window width to [WIDTH]."),
                    "(#{options.width})") do |width|
            options.width = width
          end

          parser.on("-h", "--height=HEIGHT",
                    Integer,
                    _("Set window height to [HEIGHT]."),
                    "(#{options.height})") do |height|
            options.height = height
          end

          message = _("Set window width and height to\n" \
                      "[WIDTH] and [HEIGHT].")
          message = message.split(/\n/) +
            ["(#{options.width},#{options.height})"]
          parser.on("-S", "--size=WIDTH,HEIGHT",
                    Array,
                    *message) do |size|
            width, height = size.collect{|x| Integer(x)}
            options.width = width
            options.height = height
          end


          parser.category _("Save")

          parser.on("-s", "--save-as-image",
                    _("Save as image and exit.")) do
            options.action = :save_as_image
          end

          parser.on("-i", "--saved-image-type=TYPE",
                    _("Specify saved image type as [TYPE]."),
                    "(#{options.saved_image_type})") do |t|
            options.saved_image_type = t
          end

          parser.on("-b", "--saved-image-base-name=BASE_NAME",
                    "--saved-image-basename=BASE_NAME",
                    _("Specify saved image base name as [BASE_NAME]."),
                    "(" + _("Title of slide") + ")") do |b|
            options.saved_image_base_name = b
          end

          parser.on("--[no-]output-html",
                    _("Output HTML for viewing saved images."),
                    "(#{options.output_html})") do |bool|
            options.output_html = bool
          end

          parser.on("--[no-]output-index-html",
                    _("Output index HTML for navigating slides."),
                    "(#{options.output_index_html})") do |bool|
            options.output_index_html = bool
          end

          parser.on("--rss-base-uri=URI",
                    _("Specify base URI of RSS as [URI]."),
                    _("RSS is generated only when HTML is output."),
                    "(#{options.rss_base_uri})") do |uri|
            options.rss_base_uri = uri
          end

          parser.on("--source-filename=FILENAME",
                    _("Specify source filenam as [FILENAME]."),
                    "(#{options.source_filename})") do |filename|
            options.source_filename = filename
          end

          parser.category _("Print")

          parser.on("-p", "--print",
                    _("Print and exit.")) do
            options.action = :print
          end

          parser.on("-o", "--output-filename=FILENAME",
                    _("Specify printed out filename as [FILENAME]."),
                    "(\#{%s}.pdf)" % _("Title of slide")) do |f|
            options.print_out_filename = f
          end

          parser.on("--slides-per-page=SLIDES",
                    Integer,
                    _("Set slides per page."),
                    "(1)") do |slides|
            options.slides_per_page = slides
          end

          parser.on("--[no-]draw-scaled-image",
                    _("Draw scaled image."),
                    _("Better look for displaying but lesser look for printing."),
                    "(auto)") do |boolean|
            options.draw_scaled_image = boolean
          end

          parser.category _("Paper")

          parser.on("--paper-width=WIDTH",
                    Integer,
                    _("Set paper width to [WIDTH] Pt."),
                    _("(landscape A4 width)")) do |width|
            options.paper_width = width
          end

          parser.on("--paper-height=HEIGHT",
                    Integer,
                    _("Set paper height to [HEIGHT] Pt."),
                    _("(landscape A4 height)")) do |height|
            options.paper_height = height
          end

          message = _("Set paper width and height to\n" \
                        "[WIDTH] Pt and [HEIGHT] Pt.")
          message = message.split(/\n/) + [_("(landscape A4 size)")]
          parser.on("--paper-size=WIDTH,HEIGHT",
                    Array,
                    *message) do |size|
            width, height = size.collect{|x| Integer(x)}
            options.paper_width = width
            options.paper_height = height
          end


          parser.category _("Margin")

          parser.on("--margin-left=MARGIN",
                    Integer,
                    _("Set left margin for slides per page mode print."),
                    _("(auto)")) do |margin|
            options.margin_left = margin
          end

          parser.on("--margin-right=MARGIN",
                    Integer,
                    _("Set right margin for slides per page mode print."),
                    _("(auto)")) do |margin|
            options.margin_right = margin
          end

          parser.on("--margin-top=MARGIN",
                    Integer,
                    _("Set top margin for slides per page mode print."),
                    _("(auto)")) do |margin|
            options.margin_top = margin
          end

          parser.on("--margin-bottom=MARGIN",
                    Integer,
                    _("Set bottom margin for slides per page mode print."),
                    _("(auto)")) do |margin|
            options.margin_bottom = margin
          end

          margin1 = _("[ALL]")
          margin2 = _("[TOP_BOTTOM],[LEFT_RIGHT]")
          margin3 = _("[TOP],[LEFT_RIGHT],[BOTTOM]")
          margin4 = _("[TOP],[RIGHT],[BOTTOM],[LEFT]")
          parser.on("--margin={#{margin1}|#{margin2}|#{margin3}|#{margin4}}",
                    Array,
                    _("Set margin for slides per page mode print.")) do |margins|
            begin
              top, right, bottom, left = Utils.parse_four_way(margins)
              options.margin_top = top
              options.margin_right = right
              options.margin_bottom = bottom
              options.margin_left = left
            rescue ArgumentError
              raise OptionParser::InvalidArgument.new(margins)
            end
          end

          parser.on("--page-margin-left=MARGIN",
                    Integer,
                    _("Set left page margin."),
                    _("(auto)")) do |margin|
            options.page_margin_left = margin
          end

          parser.on("--page-margin-right=MARGIN",
                    Integer,
                    _("Set right page margin."),
                    _("(auto)")) do |margin|
            options.page_margin_right = margin
          end

          parser.on("--page-margin-top=MARGIN",
                    Integer,
                    _("Set top page margin."),
                    _("(auto)")) do |margin|
            options.page_margin_top = margin
          end

          parser.on("--page-margin-bottom=MARGIN",
                    Integer,
                    _("Set bottom page margin."),
                    _("(auto)")) do |margin|
            options.page_margin_bottom = margin
          end

          parser.on("--page-margin={#{margin1}|#{margin2}|#{margin3}|#{margin4}}",
                    Array,
                    _("Set page margin.")) do |margins|
            begin
              top, right, bottom, left = Utils.parse_four_way(margins)
              options.page_margin_top = top
              options.page_margin_right = right
              options.page_margin_bottom = bottom
              options.page_margin_left = left
            rescue ArgumentError
              raise OptionParser::InvalidArgument.new(margins)
            end
          end

          parser.category _("dRuby")

          parser.on("--[no-]use-druby",
                    _("Specify whether to use dRuby."),
                    "(#{options.use_druby})") do |bool|
            options.use_druby = bool
          end

          parser.on("--druby-uri=URI",
                    _("Specify dRuby URI."),
                    "(#{options.druby_uri})") do |uri|
            options.druby_uri = uri if uri
          end

          parser.on("--[no-]output-druby-uri",
                    _("Specify whether to output dRuby URI."),
                    "(#{options.output_druby_uri})") do |bool|
            options.output_druby_uri = bool
          end

          parser.category _("SOAP")

          parser.on("--[no-]use-soap",
                    _("Specify whether to use SOAP."),
                    "(#{options.use_soap})") do |bool|
            options.use_soap = bool
          end

          parser.on("--soap-host=HOST",
                    _("Specify SOAP host as [HOST]."),
                    "(#{options.soap_host})") do |port|
            options.soap_host = host
          end

          parser.on("--soap-port=PORT",
                    Integer,
                    _("Specify SOAP port as [PORT]."),
                    "(#{options.soap_port})") do |port|
            options.soap_port = port
          end

          parser.category _("XML-RPC")

          parser.on("--[no-]use-xmlrpc",
                    _("Specify whether to use XML-RPC."),
                    "(#{options.use_xmlrpc})") do |bool|
            options.use_xmlrpc = bool
          end

          parser.on("--xmlrpc-host=HOST",
                    _("Specify XML-RPC host as [HOST]."),
                    "(#{options.xmlrpc_host})") do |port|
            options.xmlrpc_host = host
          end

          parser.on("--xmlrpc-port=PORT",
                    Integer,
                    _("Specify XML-RPC port as [PORT]."),
                    "(#{options.xmlrpc_port})") do |port|
            options.xmlrpc_port = port
          end

          parser.category _("Public level")

          levels = Front::PublicLevel.constants.sort_by do |const|
              Front::PublicLevel.const_get(const)
          end.collect do |const|
            const.to_s.downcase.gsub(/_/, "-")
          end
          messages = [_("Specify public level.")]
          messages << _("Select from the following:")
          messages << "["
          messages << "  "
          levels[0..-2].each do |level|
            messages.last << "#{level}, "
            messages << "  " if messages.last.size > 30
          end
          messages.last << levels.last
          messages << "]"
          messages << (_("(%s)") % options.default_public_level)
          parser.on("--public-level=LEVEL", levels, *messages) do |level|
            options.public_level = level
          end

          parser.category _("Comment")

          parser.on("--comment-source=FILE",
                    _("Deprecated. Just ignored."),
                    _("Specify initial comment source."),
                    _("(default source)")) do |name|
          end

          parser.on("--comment-encoding=ENCODING",
                    _("Deprecated. Just ignored."),
                    _("Specify comment source encoding."),
                    _("(auto)")) do |encoding|
          end

          parser.category _("Migemo")

          search_path = options.migemo_dictionary_search_path.join(', ')
          parser.on("--migemo-dictionary-search-path=PATH1,PATH2,...",
                    Array,
                    _("Specify search paths for Migemo static dictionary."),
                    _("(%s)") % search_path) do |path|
            options.migemo_dictionary_search_path = path
          end

          parser.on("--migemo-dictionary-name=NAME",
                    Array,
                    _("Specify static dictionary name for Migemo."),
                    _("(%s)") % options.migemo_dictionary_name) do |name|
            options.migemo_dictionary_name = name
          end

          parser.category _("3D")

          parser.on("--[no-]use-gl",
                    _("Specify whether to use OpenGL if available."),
                    "(#{options.use_gl})") do |bool|
            options.use_gl = bool
          end

          parser.category _("Others")

          parser.on("--check-syntax",
                    _("Check slide source syntax and exit.")) do
            options.action = :check_syntax
          end

          parser.on("--[no-]show-native-window-id",
                    _("Show a native window ID of the Rabbit window if available."),
                    _("e.g. The ID is the ID of X resource on X window system."),
                    "(#{options.show_native_window_id})") do |bool|
            options.show_native_window_id = bool
          end
        end
      end

      def adjust_rest_arguments(console, parser, options)
        return unless options.rest.size == 1

        source = options.rest[0]
        if /\.gem\z/i =~ source
          gem_name = $PREMATCH
          require_relative "../gem-finder"
          finder = GemFinder.new
          spec = finder.find(gem_name, "#{SlideConfiguration::GEM_NAME_PREFIX}-")
          source = spec.gem_dir if spec
        end

        options_file = File.join(source, options.options_file)
        if File.file?(options_file)
          options.rest.clear
          console.read_options_file(parser, options, options_file)
        end
      end

      def make_canvas(renderer)
        canvas = Canvas.new(renderer)
        canvas.comment_theme = @options.comment_theme
        canvas.allotted_time = @options.allotted_time
        canvas
      end

      def add_source_dialog_filter(dialog, name, pattern)
        filter = Gtk::FileFilter.new
        filter.name = "#{name} (#{pattern})"
        filter.add_pattern(pattern)
        dialog.add_filter(filter)
      end

      def choose_source_file_by_dialog
        dialog = Gtk::FileChooserDialog.new(:title => _("Choose a Rabbit source file"),
                                            :action => :open,
                                            :buttons => [[Gtk::Stock::CANCEL, :cancel],
                                                        [Gtk::Stock::OPEN, :accept]])
        dialog.current_folder = @options.base if @options.base
        add_source_dialog_filter(dialog, "Rabbit files", "*.rab")
        add_source_dialog_filter(dialog, "RD files", "*.rd")
        add_source_dialog_filter(dialog, "Hiki files", "*.hiki")
        add_source_dialog_filter(dialog, "PDF files", "*.pdf")
        add_source_dialog_filter(dialog, "Markdown files", "*.md")
        add_source_dialog_filter(dialog, "All files", "*")
        file_name = nil
        if dialog.run == Gtk::ResponseType::ACCEPT
          file_name = dialog.filename
          dialog.destroy
        end
        file_name
      end

      def make_source
        rest_arguments = @options.rest
        if @options.source_type == :auto
          if rest_arguments.empty?
            file_name = choose_source_file_by_dialog
            throw(@abort_tag, true) if file_name.nil?
            rest_arguments = [file_name]
            @options.source_type = Source::File
          elsif rest_arguments.size == 1 and
               /\A(?:https?|file):\/\//i =~ rest_arguments[0]
            @options.source_type = Source::URI
          else
            @options.source_type = Source::File
          end
        end
        if @options.source_type == Source::ARGF
          rest_arguments = [ARGF]
        end
        source = @options.source_type.new(@options.encoding, *rest_arguments)
        source.base = @options.base if @options.base
        source
      end

      def make_front(canvas)
        level = @options.public_level
        level ||= @options.default_public_level
        level = level.gsub(/-/, "_").upcase
        canvas.front(Front::PublicLevel.const_get(level))
      end

      def apply_theme_if_need(target)
        target.apply_theme(@options.theme) if @options.theme_specified
      end

      def parse(target, source, background=false)
        if background
          callback = Utils.process_pending_events_proc
        else
          callback = nil
        end
        target.parse(source, callback)
      end

      def setup_image_info(target)
        target.saved_image_type = @options.saved_image_type
        target.saved_image_base_name = @options.saved_image_base_name
        target.output_html = @options.output_html
        target.output_index_html = @options.output_index_html
        target.rss_base_uri = @options.rss_base_uri
        target.source_filename = @options.source_filename
      end

      def setup_print_info(target)
        target.filename = @options.print_out_filename
        target.slides_per_page = @options.slides_per_page
        target.draw_scaled_image = @options.draw_scaled_image
      end

      def setup_base_size(target)
        target.base_width = @options.width
        target.base_height = @options.height
      end

      def setup_paper_size(target)
        target.paper_width = @options.paper_width
        target.paper_height = @options.paper_height
        target.page_margin_left = @options.page_margin_left
        target.page_margin_right = @options.page_margin_right
        target.page_margin_top = @options.page_margin_top
        target.page_margin_bottom = @options.page_margin_bottom
        target.margin_left = @options.margin_left
        target.margin_right = @options.margin_right
        target.margin_top = @options.margin_top
        target.margin_bottom = @options.margin_bottom
      end

      def setup_migemo_info(target)
        target.migemo_dictionary_search_path =
          @options.migemo_dictionary_search_path
        target.migemo_dictionary_name = @options.migemo_dictionary_name
      end

      def setup_3d_info(target)
        target.use_gl = @options.use_gl
      end

      def setup_druby(front)
        require "drb/drb"
        begin
          DRb.start_service(@options.druby_uri, front)
          ::Rabbit.logger.info(DRb.uri) if @options.output_druby_uri
        rescue SocketError
          ::Rabbit.logger.error($!)
        rescue Errno::EADDRINUSE
          ::Rabbit.logger.error(_("dRuby URI <%s> is in use.") % @options.druby_uri)
        end
      end

      def setup_soap(front)
        require_relative "../soap/server"
        thread = nil

        begin
          config = {
            :BindAddress => @options.soap_host,
            :Port => @options.soap_port,
            :AddressFamily => Socket::AF_INET,
            :Logger => ::Rabbit.logger,
          }
          server = Rabbit::SOAP::Server.new(front, config)
          prev = trap(:INT) {server.shutdown; trap(:INT, prev)}
          thread = Thread.new {server.start}
        rescue Errno::EADDRINUSE
          ::Rabbit.logger.error(_("port <%s> for SOAP is in use.") % @options.soap_port)
        end

        thread
      end

      def setup_xmlrpc(front)
        require_relative "../xmlrpc/server"
        thread = nil

        begin
          config = {
            :BindAddress => @options.xmlrpc_host,
            :Port => @options.xmlrpc_port,
            :AddressFamily => Socket::AF_INET,
            :Logger => ::Rabbit.logger,
          }
          server = Rabbit::XMLRPC::Server.new(front, config)
          prev = trap(:INT) {server.shutdown; trap(:INT, prev)}
          thread = Thread.new {server.start}
        rescue Errno::EADDRINUSE
          ::Rabbit.logger.error(_("port <%s> for XML-RPC is in use.") % @options.xmlrpc_port)
        end

        thread
      end

      def do_print
        source = make_source
        canvas = make_canvas(Renderer::Printer)
        setup_base_size(canvas)
        setup_paper_size(canvas)
        setup_print_info(canvas)
        setup_3d_info(canvas)
        apply_theme_if_need(canvas)
        parse(canvas, source)
        canvas.print
        canvas.quit
        true
      rescue ::Rabbit::NoPrintSupportError
        ::Rabbit.logger.error($!.message)
        false
      end

      def do_save_as_image
        source = make_source
        canvas = make_canvas(Renderer::Offscreen)
        setup_base_size(canvas)
        setup_image_info(canvas)
        setup_print_info(canvas)
        setup_paper_size(canvas)
        setup_3d_info(canvas)
        apply_theme_if_need(canvas)
        parse(canvas, source)
        canvas.activate("ToggleIndexMode") if @options.index_mode
        canvas.save_as_image
        canvas.quit

        true
      end

      def do_display
        source = make_source
        if ENV["RABBIT_RENDERER"] == "scene"
          canvas = make_canvas(Renderer::Scene)
        else
          canvas = make_canvas(Renderer::Display::DrawingArea)
        end
        frame = Frame.new(canvas)
        frame.geometry = @options.geometry
        setup_base_size(canvas)
        setup_paper_size(canvas)
        setup_image_info(canvas)
        setup_print_info(canvas)
        setup_migemo_info(canvas)
        setup_3d_info(canvas)
        frame.init_gui(@options.width, @options.height, true)
        frame.fullscreen if @options.full_screen
        if @options.show_native_window_id
          native_surface = frame.window.surface
          if native_surface.respond_to?(:xid)
            ::Rabbit.logger.info(_("Window ID: %d") % native_surface.xid)
          end
        end
        apply_theme_if_need(frame)
        parse(frame, source, !Utils.windows?)
        canvas.move_to_if_can(@options.initial_slide)
        canvas.activate("ToggleIndexMode") if @options.index_mode

        front = make_front(canvas)
        setup_druby(front) if @options.use_druby
        setup_soap(front) if @options.use_soap
        setup_xmlrpc(front) if @options.use_xmlrpc

        true
      end

      def do_check_syntax
        source = make_source
        canvas = make_canvas(Renderer::Printer)
        exception = nil
        begin
          canvas.parse(source) do |_exception|
            exception = _exception
          end
        rescue
          exception = $!
        end

        if exception
          ::Rabbit.logger.info(exception.message)
          false
        else
          true
        end
      end
    end
  end
end
