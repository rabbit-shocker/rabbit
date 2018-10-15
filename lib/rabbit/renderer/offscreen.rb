# Copyright (C) 2016-2018  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/image-data-loader"
require "rabbit/renderer/base"
require "rabbit/renderer/engine/cairo"
require "rabbit/utils"

module Rabbit
  module Renderer
    class Offscreen
      include Renderer::Base
      include Engine::Cairo

      attr_accessor :filename
      attr_accessor :width, :height, :pango_context

      def initialize(canvas, width=nil, height=nil)
        super(canvas)
        @filename = nil
        @width = width
        @height = height
        @pango_context = nil
      end

      def post_apply_theme
      end

      def post_move(old_index, index)
      end

      def post_move_in_slide(old_index, index)
      end

      def pre_parse
      end

      def post_parse
      end

      def index_mode_on
      end

      def index_mode_off
      end

      def pre_toggle_index_mode
      end

      def post_toggle_index_mode
      end

      def make_layout(text)
        attrs, text = Pango.parse_markup(text)
        layout = Pango::Layout.new(create_pango_context)
        layout.text = text
        layout.set_attributes(attrs)
        layout
      end

      def to_pixbuf(slide)
        pixbuf = nil
        ::Cairo::ImageSurface.new(@width, @height) do |surface|
          context = ::Cairo::Context.new(surface)
          init_context(context)
          slide.draw(@canvas)
          png = StringIO.new
          surface.write_to_png(png)
          loader = ImageDataLoader.new(png.string)
          pixbuf = loader.load
          finish_context
        end
        pixbuf
      end

      def create_pango_context
        context = Gtk::Invisible.new.create_pango_context
        set_font_resolution(context)
        context
      end

      def pre_to_pixbuf(slide_size)
      end

      def to_pixbufing(i)
        Utils.process_pending_events
        true
      end

      def post_to_pixbuf(canceled)
      end

      private
      def init_color
        super
        init_engine_color
      end

      def offscreen_renderer?
        true
      end

      def init_dpi
        @x_dpi = ScreenInfo.screen_x_resolution
        @y_dpi = ScreenInfo.screen_y_resolution
      end
    end
  end
end
