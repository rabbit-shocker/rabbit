require 'gst'
require 'rabbit/gtk'

module Rabbit
  class VideoWindow
    attr_accessor :window, :direction, :entry
    def initialize(element)
      @element = element
      @video = VideoWidget.new(@element.filename)
    end

    def setup(window)
      if not @completed
        @window = window
        init_window
        init_keys
        @completed = true
      end
    end

    def show(window)
      setup(window) if not window.nil?
      Utils.move_to(window, @video_window) do |bx, by, bw, bh, tw, th, sw, sh|
        [bx+@element.x, by+@element.y]
      end
      @video_window.resize(@element.width, @element.height)
      @video_window.show_all
      @video.play
    end

    def hide
      @video.pause
      @video_window.hide
    end

    private
    def init_window
      @video_window = Gtk::Window.new(Gtk::Window::POPUP)
      @video_window.modal = true
      @video_window.set_transient_for(window)

      vbox = Gtk::VBox.new
      vbox.pack_start(@video)
      @video_window.add(vbox)
      @video_window.signal_connect('frame-event') do |widget, event|
        if event.event_type == Gdk::EventType::BUTTON_PRESS
          @video.toggle
        end
      end
      @video_window.signal_connect('destroy') do
        @video.stop
      end
    end

    def init_keys
      @video_window.signal_connect("key_press_event") do |widget, key|
        case key.keyval
        when Gdk::Keyval::GDK_KEY_space
          @video.toggle
        when Gdk::Keyval::GDK_KEY_plus
          @video.seek(10)
        when Gdk::Keyval::GDK_KEY_minus
          @video.seek(-10)
        when *[
            Keys::MOVE_TO_NEXT_KEYS, Keys::MOVE_TO_PREVIOUS_KEYS,
            Keys::MOVE_TO_LAST_KEYS, Keys::MOVE_TO_LAST_KEYS,
          ].flatten
          hide
          Gtk::AccelGroup.activate(window, key.keyval, key.state)
        else
          Gtk::AccelGroup.activate(window, key.keyval, key.state)
        end
      end
    end

    class VideoWidget < Gtk::DrawingArea
      def initialize(file)
        super()

        @playbin = Gst::ElementFactory.make('playbin2')

        @video = Gst::ElementFactory.make('xvimagesink')
        @video.force_aspect_ratio = true

        @playbin.video_sink = @video
        @playbin.audio_sink = Gst::ElementFactory.make('autoaudiosink')
        @playbin.signal_connect('notify') do
          @playbin.video_sink.xwindow_id = self.window.xid if self.window
          @playbin.video_sink.expose
        end
        @playbin.uri = "file://#{File.absolute_path(file)}"
        @playbin.ready
      end

      def play
        @playbin.play
        @playing = true
      end

      def pause
        @playbin.pause
        @playing = false
      end

      def stop
        @playbin.stop
        @playing = false
      end

      def seek(time)
        @playbin.seek(1.0, Gst::Format::TIME,
          Gst::Seek::FLAG_FLUSH | Gst::Seek::FLAG_KEY_UNIT,
          Gst::Seek::TYPE_CUR, time * Gst::SECOND,
          Gst::Seek::TYPE_NONE, -1);
      end

      def toggle
        @playing ? pause : play
      end
    end
  end
end
