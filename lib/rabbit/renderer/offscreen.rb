# Copyright (C) 2016-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../image-data-loader"
require_relative "base"
require_relative "engine/cairo"
require_relative "../utils"

module Rabbit
  module Renderer
    class Offscreen < Base
      include Engine::Cairo

      attr_accessor :filename
      attr_accessor :pango_context

      def initialize(canvas)
        super(canvas)
        @filename = nil
        @pango_context = nil
      end

      def width
        @base_width
      end

      def height
        @base_height
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
        ::Cairo::ImageSurface.new(@base_width, @base_height) do |surface|
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
        context = Gtk::Label.new.create_pango_context
        set_font_resolution(context)
        context
      end

      def pre_to_pixbuf(n_slides)
      end

      def to_pixbufing(i)
        Utils.process_pending_events
        true
      end

      def post_to_pixbuf(canceled)
      end

      private
      def offscreen_renderer?
        true
      end
    end
  end
end
