# Copyright (C) 2004-2020  Sutou Kouhei <kou@cozmixng.org>
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

require "gdk_pixbuf2"

require "rabbit/image-data-loader"
require "rabbit/properties"

module Rabbit
  module ImageManipulable

    class Base
      extend ModuleLoader

      attr_reader :filename
      attr_reader :properties
      attr_reader :original_width
      attr_reader :original_height
      attr_reader :animation

      def initialize(filename, props)
        @filename = filename
        @properties = Properties.new(props)
        initialize_keep_ratio
        @animation = nil
        @animation_iterator = nil
        @animation_timeout = nil
        update_size
        @original_width = @width
        @original_height = @height
      end

      def [](key)
        @properties[key]
      end

      def []=(key, value)
        @properties[key] = value
      end

      def keep_ratio?
        @properties.keep_ratio
      end
      # For backward compatibility
      alias_method :keep_ratio, :keep_ratio?

      def keep_ratio=(value)
        @properties.keep_ratio = value
      end

      def pixbuf
        @pixbuf
      end

      def width
        (relative_clip_width&.resolve(@width) || @width) -
          (relative_clip_x&.resolve(@width) || 0)
      end

      def height
        (relative_clip_height&.resolve(@height) || @height) -
          (relative_clip_y&.resolve(@height) || 0)
      end

      def relative_clip_x
        @properties.get_relative_size("relative_clip_x", @filename)
      end

      def relative_clip_y
        @properties.get_relative_size("relative_clip_y", @filename)
      end

      def relative_clip_width
        @properties.get_relative_size("relative_clip_width", @filename)
      end

      def relative_clip_height
        @properties.get_relative_size("relative_clip_height", @filename)
      end

      def resize(w, h)
        if w.nil? and h.nil?
          return
        elsif keep_ratio?
          if w and h.nil?
            h = (original_height * w.to_f / original_width).ceil
          elsif w.nil? and h
            w = (original_width * h.to_f / original_height).ceil
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
        end
      end

      def draw(canvas, x, y, params={})
        default_params = default_draw_params(x, y)
        target_pixbuf = pixbuf
        if @animation_iterator
          @animation_iterator.advance
          target_pixbuf = @animation_iterator.pixbuf
          update_animation_timeout(canvas)
        end
        canvas.draw_pixbuf(target_pixbuf, x, y, default_params.merge(params))
      end

      private
      def initialize_keep_ratio
        return unless @properties["keep_ratio"].nil?
        # For backward compatibility
        keep_scale = @properties["keep_scale"]
        if keep_scale.nil?
          @properties["keep_ratio"] = true
        else
          @properties["keep_ratio"] = keep_scale
        end
      end

      def load_data(data)
        loader = ImageDataLoader.new(data)
        begin
          loader.load
        rescue ImageLoadError
          raise ImageLoadError.new("#{@filename}: #{$!.message}")
        end

        @width = loader.width
        @height = loader.height
        @pixbuf = loader.pixbuf
        @animation = loader.animation
        if @animation and not @animation.static_image?
          @animation_iterator = @animation.get_iter
        else
          @animation_iterator = nil
        end
        if @animation_timeout
          GLib::Source.remove(@animation_timeout)
          @animation_timeout = nil
        end
      end

      def update_animation_timeout(canvas)
        delay_time = @animation_iterator.delay_time
        if delay_time > 0 and @animation_timeout.nil?
          @animation_timeout = GLib::Timeout.add(delay_time) do
            canvas.redraw
            @animation_timeout = nil
            # update_animation_timeout(canvas)
            GLib::Source::REMOVE
          end
        end
      end

      def default_draw_params(x, y)
        _relative_clip_x = relative_clip_x
        _relative_clip_y = relative_clip_y
        _relative_clip_width = relative_clip_width
        _relative_clip_height = relative_clip_height
        if _relative_clip_x or
           _relative_clip_y or
           _relative_clip_width or
           _relative_clip_height
          clip_x = _relative_clip_x&.resolve(@width) || 0
          clip_y = _relative_clip_y&.resolve(@height) || 0
          clip_width = _relative_clip_width&.resolve(@width) || @width
          clip_height = _relative_clip_height&.resolve(@height) || @height
          uncliped_width = width - (clip_width - clip_x) + @width
          uncliped_height = height - (clip_height - clip_y) + @height
          {
            width: uncliped_width,
            height: uncliped_height,
            clip_x: x + clip_x,
            clip_y: y + clip_y,
            clip_width: clip_width,
            clip_height: clip_height,
          }
        else
          {
            width: width,
            height: height,
          }
        end
      end
    end
  end
end
