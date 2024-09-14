# Copyright (C) 2012  Narihiro Nakamura <authornari@gmail.com>
# Copyright (C) 2015-2018  Kouhei Sutou <kou@cozmixng.org>
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

case ENV["RABBIT_GTK"]
when "4"
else
  require "clutter-gtk"
  require "clutter-gst"
end

require "rabbit/gtk"

module Rabbit
  class VideoWindow
    # TODO: Implemented in renderer or something
    class << self
      def show(window, element)
        @instance ||= VideoWindow.new(window)
        @instance.show(element)
      end

      def unset_instance
        @instance = nil
      end
    end

    def initialize(window)
      @parent_window = window
      init_window
      init_keys
    end

    def show(element)
      @player.filename = element.filename
      @window.resize(element.width, element.height)
      @window.show_all
      @player.playing = true
    end

    private
    def init_window
      @window = Gtk::ApplicationWindow.new(Rabbit.application)
      @window.modal = true
      @window.set_transient_for(@parent_window)

      @embed = ClutterGtk::Embed.new
      @window.add(@embed)

      @stage = @embed.stage
      @player = ClutterGst::Playback.new
      @player.seek_flags = :accurate
      @player.signal_connect(:eos) do
        @player.progress = 0.0
        @player.playing = true
      end

      @aspect_ratio = ClutterGst::Aspectratio.new
      @aspect_ratio.player = @player
      @video = Clutter::Actor.new
      @video.width = @stage.width
      @video.height = @stage.height
      @video.content = @aspect_ratio

      @stage.add_child(@video)

      @window.signal_connect(:button_press_event) do |widget, event|
        @player.playing = !@player.playing?
      end

      @window.signal_connect(:destroy) do
        @player.playing = false
        self.class.unset_instance
      end
    end

    def init_keys
      @window.signal_connect(:key_press_event) do |widget, key|
        case key.keyval
        when Gdk::Keyval::KEY_space
          @player.playing = !@player.playing?
        when Gdk::Keyval::KEY_plus
          seek(10)
        when Gdk::Keyval::KEY_minus
          seek(-10)
        when *[
            Keys::MOVE_TO_NEXT_KEYS, Keys::MOVE_TO_PREVIOUS_KEYS,
            Keys::MOVE_TO_LAST_KEYS, Keys::MOVE_TO_LAST_KEYS,
          ].flatten
          @window.destroy
          Gtk::AccelGroup.activate(@parent_window, key.keyval, key.state)
        else
          Gtk::AccelGroup.activate(@parent_window, key.keyval, key.state)
        end
        true
      end
    end

    def seek(second)
      duration = @player.duration
      progress = @player.progress + (second / duration)
      if progress < 0.0
        progress = 0.0
      elsif progress > 1.0
        progress = 1.0
      end
      @player.progress = progress
    end
  end
end
