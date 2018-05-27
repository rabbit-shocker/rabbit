require "rabbit/gtk"

require "rabbit/keys"
require "rabbit/logger/base"

module Rabbit
  module Logger

    class GUI
      include Base

      attr_accessor :start_gui_main_loop_automatically
      def initialize(level=nil, width=450, height=400)
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
          Thread.new {Gtk.main}
        end
      end

      def log_severity(severity)
        append("[")
        name = Severity::MARK_TABLE[severity]
        append(_(name), name)
        append("]\n")
      end

      def log_prog_name(prog_name)
        if prog_name
          append("#{prog_name}: ", "prog_name")
        end
      end

      def log_message(message)
        append(message, "message")
        append("\n")
      end

      def append(text, *tags)
        iter = @buffer.get_iter_at(offset: -1)
        @buffer.insert_with_tags(iter, text.encode("UTF-8"), *tags)
      end

      def title
        _("Rabbit Error Dialog")
      end

      def init_dialog(width=@width, height=@height)
        @dialog = Gtk::Dialog.new(title: title,
                                  buttons: [
                                    [Gtk::Stock::CLEAR, :cancel],
                                    [Gtk::Stock::CLOSE, :close],
                                  ])
        @dialog.child.add(init_buffer)
        @dialog.set_default_size(width, height)
        @dialog.title = title
        set_dialog_delete
        set_dialog_response
        set_dialog_accel_group
      end

      def set_dialog_delete
        @dialog.signal_connect("destroy") do |widget, event|
          exit if @current_severity >= Severity::FATAL
          true
        end
      end

      def set_dialog_response
        @dialog.signal_connect("response") do |widget, event|
          case event
          when Gtk::ResponseType::CANCEL
            clear_buffer
          when Gtk::ResponseType::CLOSE
            quit
          end
          true
        end
      end

      def set_dialog_accel_group
        accel_group = Gtk::AccelGroup.new
        mod = Gdk::ModifierType.new
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
        textview.set_wrap_mode(:word)
        @buffer = textview.buffer
        create_tags
        scrolled_window = Gtk::ScrolledWindow.new
        scrolled_window.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC)
        scrolled_window.add(textview)
        scrolled_window
      end

      def create_tags
        @buffer.create_tag("DEBUG",
                           "weight" => :bold,
                           "foreground" => "blue")
        @buffer.create_tag("INFO", "foreground" => "blue")
        @buffer.create_tag("WARNING", "foreground" => "red")
        @buffer.create_tag("ERROR",
                           "weight" => :bold,
                           "foreground" => "red")
        @buffer.create_tag("FATAL",
                           "foreground" => "yellow",
                           "background" => "black")
        @buffer.create_tag("UNKNOWN",
                           "weight" => :bold,
                           "foreground" => "yellow",
                           "background" => "black")
        @buffer.create_tag("ANY",
                           "weight" => :bold)
        @buffer.create_tag("prog_name",
                           "weight" => :bold,
                           "foreground" => "blue",
                           "left_margin" => 10)
        @buffer.create_tag("message",
                           "weight" => :bold,
                           "left_margin" => 10,
                           "right_margin" => 10)
      end
    end
  end
end
