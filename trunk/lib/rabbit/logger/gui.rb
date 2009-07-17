require "gtk2"

require "rabbit/keys"
require "rabbit/logger/base"

module Rabbit
  module Logger

    class GUI
      include Base

      attr_accessor :start_gui_main_loop_automatically
      def initialize(level=nil, width=450, height=400)
        Gtk.init
        super(*[level].compact)
        @width = width
        @height = height
        @start_gui_main_loop_automatically = false
        init_dialog
      end

      def clear_buffer
        @buffer.text = ""
      end

      def quit
        @dialog.destroy
      end

      private
      def do_log(severity, prog_name, message)
        # ::STDERR.puts(format_severity(severity))
        # ::STDERR.puts(GLib.filename_from_utf8(message))
        @current_severity = severity
        init_dialog if @dialog.destroyed?
        @dialog.show_all
        log_severity(severity)
        log_prog_name(prog_name)
        log_message(message)
        if @start_gui_main_loop_automatically and Gtk.main_level.zero?
          Thread.new{Gtk.main}
        end
      end

      def log_severity(severity)
        append("[")
        name = severity_name(severity)
        append(name, name)
        append("]\n")
      end

      def log_prog_name(prog_name)
        if prog_name
          append("#{prog_name}: ", "prog_name")
        end
      end
      
      def log_message(message)
        unless GLib.utf8_validate(message)
          message = GLib.filename_to_utf8(message)
        end
        append(message, "message")
        append("\n")
      end

      def append(text, *tags)
        iter = @buffer.get_iter_at_offset(-1)
        @buffer.insert_with_tags(iter, text, *tags)
      end

      def title
        _("Rabbit Error Dialog")
      end
      
      def init_dialog(width=@width, height=@height)
        flags = 0
        buttons = [
          [Gtk::Stock::CLEAR, Gtk::Dialog::RESPONSE_CANCEL],
          [Gtk::Stock::CLOSE, Gtk::Dialog::RESPONSE_CLOSE],
        ]
        @dialog = Gtk::Dialog.new(title, nil, flags, *buttons)
        @dialog.vbox.add(init_buffer)
        @dialog.set_default_size(width, height)
        set_dialog_delete
        set_dialog_response
        set_dialog_expose_event
        set_dialog_accel_group
      end

      def set_dialog_delete
        @dialog.signal_connect("destroy") do |widget, event|
          exit if @current_severity >= FATAL
          true
        end
      end
      
      def set_dialog_response
        @dialog.signal_connect("response") do |widget, event|
          case event
          when Gtk::Dialog::RESPONSE_CANCEL
            clear_buffer
          when Gtk::Dialog::RESPONSE_CLOSE
            quit
          end
          true
        end
      end

      def set_dialog_expose_event
        @dialog.signal_connect("expose_event") do |widget, event|
          @dialog.title = title
          false
        end
      end
      
      def set_dialog_accel_group
        accel_group = Gtk::AccelGroup.new
        mod = Gdk::Window::ModifierType.new
        flags = Gtk::AccelFlags::VISIBLE
        Keys::QUIT_KEYS.each do |val|
          accel_group.connect(val, mod, flags) do
            @dialog.signal_emit("response", Gtk::Dialog::RESPONSE_CLOSE)
            true
          end
        end
        @dialog.add_accel_group(accel_group)
      end

      def init_buffer
        textview = Gtk::TextView.new
        textview.set_editable(false)
        textview.set_wrap_mode(Gtk::TextTag::WrapMode::WORD)
        @buffer = textview.buffer
        create_tags
        scrolled_window = Gtk::ScrolledWindow.new
        scrolled_window.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC)
        scrolled_window.add(textview)
        scrolled_window
      end

      def create_tags
        @buffer.create_tag("DEBUG",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "foreground" => "blue")
        @buffer.create_tag("INFO", "foreground" => "blue")
        @buffer.create_tag("WARN", "foreground" => "red")
        @buffer.create_tag("ERROR",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "foreground" => "red")
        @buffer.create_tag("FATAL",
                           "foreground" => "yellow",
                           "background" => "black")
        @buffer.create_tag("UNKNOWN",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "foreground" => "yellow",
                           "background" => "black")
        @buffer.create_tag("ANY",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD)
        @buffer.create_tag("prog_name",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "foreground" => "blue",
                           "left_margin" => 10)
        @buffer.create_tag("message",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "left_margin" => 10,
                           "right_margin" => 10)
      end
      
    end
    
  end
end
