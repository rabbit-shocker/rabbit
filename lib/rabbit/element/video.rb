# Copyright (C) 2012  Narihiro Nakamura <authornari@gmail.com>
# Copyright (C) 2013-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../image"
require_relative "../parser/base"
require_relative "../properties"
require_relative "block-element"

module Rabbit
  module Element
    class Video
      include Base
      include BlockElement
      include BlockHorizontalCentering

      attr_reader :filename
      attr_reader :relative_width, :relative_height
      attr_reader :relative_margin_top, :relative_margin_bottom
      attr_reader :relative_margin_left, :relative_margin_right
      attr_reader :relative_padding_top, :relative_padding_bottom
      attr_reader :relative_padding_left, :relative_padding_right

      def initialize(filename, props)
        @filename = filename
        @thumbnail = nil
        super()
        @properties = Properties.new(props)
        width, height = detect_size
        @original_width = width
        @original_height = height
        %w(relative_width relative_height
           relative_margin_top relative_margin_bottom
           relative_margin_left relative_margin_right
           relative_padding_top relative_padding_bottom
           relative_padding_left relative_padding_right
          ).each do |name|
          instance_variable_set("@#{name}",
                                @properties.get_relative_size(name, @filename))
        end
        resize(width, height)
      end

      alias _compile compile
      def compile_for_horizontal_centering(canvas, x, y, w, h)
        _compile(canvas, x, y, w, h)
      end

      def compile(canvas, x, y, w, h)
        super
        adjust_size(canvas, @x, @y, @w, @h)
      end

      def setup_scene_element(canvas, scene_widget, x, y, w, h)
        x, y, w, h = super

        video = Gtk::Video.new(@filename)
        scene_widget.put(video, x, y, @width || w, @height || h)
        y += @height
        h -= @height

        [x, y, w, h]
      end

      def scene_snapshot_element(widget, snapshot, canvas, x, y, w, h)
        y += @height
        h -= @height
        [x, y, w, h]
      end

      def width
        @width.to_i + @padding_left + @padding_right
      end

      def height
        @height.to_i + @padding_top + @padding_bottom
      end

      def as_large_as_possible?
        @properties.get_boolean("as_large_as_possible", false)
      end

      def keep_ratio?
        @properties.get_boolean("keep_ratio", true) and
          @original_width and
          @original_height
      end

      def draw_element(canvas, x, y, w, h, simulation)
        unless simulation
          @thumbnail.draw(canvas, x, y) if @thumbnail
        end
        [x, y + height, w, h - height]
      end

      def text
        "video: #{File.basename(@filename)}"
      end

      def to_rd
        text
      end

      private
      def detect_size
        width = @properties.get_size("width", @filename)
        height = @properties.get_size("height", @filename)
        if (width.nil? or height.nil?) and Gtk.const_defined?(:MediaFile)
          w, h = detect_size_media_file
          width ||= w
          height ||= h
        end
        if (width.nil? or height.nil?) and defined?(::Gst)
          w, h = detect_size_gstreamer
          width ||= w
          height ||= h
        end
        [width, height]
      end

      def detect_size_media_file
        Gio::File.open(path: @filename) do |gio_file|
          media_file = Gtk::MediaFile.new(gio_file)
          begin
            # We want to mute here to avoid playing audio but if we
            # mute, media_file isn't prepared...
            # media_file.muted = true
            media_file.play
            until media_file.prepared?
              GLib::MainContext.default.iteration(true)
            end
            width = media_file.intrinsic_width
            width = nil if width.zero?
            height = media_file.intrinsic_height
            height = nil if height.zero?
            [width, height]
          ensure
            media_file.pause
            media_file.clear
          end
        end
      end

      def detect_size_gstreamer
        pipeline = Gst::Pipeline.new
        filesrc = Gst::ElementFactory.make("filesrc")
        filesrc.location = @filename
        decodebin = Gst::ElementFactory.make("decodebin")
        return [nil, nil] if decodebin.nil?
        videoconvert = Gst::ElementFactory.make("videoconvert")
        return [nil, nil] if videoconvert.nil?
        jpegenc = Gst::ElementFactory.make("jpegenc")
        return [nil, nil] if jpegenc.nil?
        filesink = Gst::ElementFactory.make("filesink")
        @thumbnail_file = Tempfile.new(["rabbit-video", ".jpeg"])
        filesink.location = @thumbnail_file.path

        pipeline << filesrc << decodebin << videoconvert << jpegenc << filesink
        filesrc >> decodebin
        decodebin.signal_connect(:pad_added) do |_, pad|
          sink_pad = videoconvert.get_static_pad("sink")
          pad.link(sink_pad)
        end
        videoconvert >> jpegenc >> filesink

        loop = GLib::MainLoop.new

        bus = pipeline.bus
        bus.add_watch do |bus, message|
          case message.type
          when Gst::MessageType::EOS
            loop.quit
          when Gst::MessageType::ERROR
            loop.quit
          end
          true
        end

        pipeline.play
        begin
          loop.run
        ensure
          pipeline.stop
        end

        @thumbnail = ImageLoader.new(@thumbnail_file.path)
        [@thumbnail.width, @thumbnail.height]
      end

      def adjust_margin(w, h)
        @margin_top =
          @relative_margin_top&.resolve(h) || @margin_top
        @margin_bottom =
          @relative_margin_bottom&.resolve(h) || @margin_bottom
        @margin_left =
          @relative_margin_left&.resolve(w) || @margin_left
        @margin_right =
          @relative_margin_right&.resolve(w) || @margin_right
      end

      def adjust_padding(w, h)
        @padding_top =
          @relative_padding_top&.resolve(h) || @padding_top
        @padding_bottom =
          @relative_padding_bottom&.resolve(h) || @padding_bottom
        @padding_left =
          @relative_padding_left&.resolve(w) || @padding_left
        @padding_right =
          @relative_padding_right&.resolve(w) || @padding_right
      end

      def adjust_size(canvas, x, y, w, h)
        base_w = w
        base_h = h
        adjust_margin(base_w, base_h)
        adjust_padding(base_w, base_h)
        base_h = base_h - @padding_top - @padding_bottom
        if as_large_as_possible?
          iw = base_w
          ih = base_h
        else
          iw = @relative_width&.resolve(base_w)
          ih = @relative_height&.resolve(base_h)
        end
        resize(iw, ih)
      end

      def resize(w, h)
        return if w.nil? and h.nil?

        if keep_ratio?
          if w and h.nil?
            h = (@original_height * w.to_f / @original_width).ceil
          elsif w.nil? and h
            w = (@original_width * h.to_f / @original_height).ceil
          end
        else
          w ||= width
          h ||= height
        end
        w = w.ceil if w
        h = h.ceil if h
        if w and w > 0 and h and h > 0 and [w, h] != [width, height]
          @width = w
          @height = h
          @thumbnail&.resize(@width, @height)
        end
      end
    end
  end
end
