require "gtk2"

require "rabbit/keys"
require "rabbit/logger/base"

module Rabbit
  module Logger

    class GUI
      include Base
      include Keys

      def initialize(level=nil, width=450, height=400)
        super(*[level].compact)
        @have_done_gtk_main = false
        init_dialog(width, height)
      end

      def clear_buffer
        @buffer.text = ""
      end

      def quit
        @dialog.destroy
      end
      
      private
      def do_log(severity, message)
        # ::STDERR.puts(format_severity(severity))
        # ::STDERR.puts(GLib.filename_from_utf8(message))
        @current_severity = severity
        @dialog.show_all
        log_severity(severity)
        log_message(message)
        Gtk.main if severity >= FATAL and Gtk.main_level.zero?
      end

      def log_severity(severity)
        append("[")
        name = severity_name(severity)
        append(name, name)
        append("]\n")
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
      
      def init_dialog(width, height)
        title = _("Rabbit Error Dialog")
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
        set_dialog_key_press_event
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

      def set_dialog_key_press_event
        @dialog.signal_connect("key_press_event") do |widget, event|
          case event.keyval
          when *QUIT_KEYS
            quit
          end
          true
        end
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
        @buffer.create_tag("message",
                           "weight" => Pango::FontDescription::WEIGHT_BOLD,
                           "left_margin" => 10,
                           "right_margin" => 10)
      end
      
    end
    
  end
end
