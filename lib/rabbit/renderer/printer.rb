# Copyright (C) 2005-2018  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/rabbit"

require "rabbit/renderer/base"
require "rabbit/renderer/engine/cairo"
require "rabbit/renderer/print-layout"

module Rabbit
  module Renderer
    class Printer
      A4_WIDTH = 596
      A4_HEIGHT = 842

      include Renderer::Base
      include Engine::Cairo

      attr_writer :filename
      attr_accessor :width
      attr_accessor :height

      def initialize(canvas)
        super
        @filename = nil
        @width = nil
        @height = nil
        init_paper
        init_color
        update_layout
      end

      def page_width
        @page_width - page_margin_left - page_margin_right
      end

      def page_height
        @page_height - page_margin_top - page_margin_bottom
      end

      def paper_width=(value)
        super
        init_paper
      end

      def paper_height=(value)
        super
        init_paper
      end

      def slides_per_page=(slides)
        super
        init_paper
        update_layout
      end

      def printable?
        true
      end

      def filename
        @filename ||= default_filename
      end

      def pre_print(slide_size)
        init_context(create_context)
      end

      def post_print(canceled)
        return if canceled
        @context.target.finish
      end

      def pre_parse
      end

      def post_parse
      end

      def post_apply_theme
      end

      def post_move(old_index, index)
      end

      def post_move_in_slide(old_index, index)
      end

      def draw_slide(slide, simulation)
        if simulation
          yield
        else
          slide_width = @layout.slide_width
          slide_height = @layout.slide_height
          size = Size.new(slide_width, slide_height, width.to_f / height.to_f)
          x = @layout.normalize_x(0)
          y = @layout.normalize_y(0)
          save_context do
            translate_context(x, y)
            clip_slide(0, 0, slide_width, slide_height)
            draw_background(0, 0, slide_width, slide_height)
            translate_context(size.logical_margin_left,
                              size.logical_margin_top)
            scale_context(size.logical_width / width.to_f,
                          size.logical_height / height.to_f)
            yield
            if @slides_per_page > 1
              draw_rectangle(false, 0, 0, width, height, @black)
            end
          end
          @context.show_page if need_show_page?
        end
      end

      def clip_slide(x, y, w, h)
        x, y = from_screen(x, y)
        @context.rectangle(x, y, w, h)
        @context.clip
      end

      def draw_background(x, y, w, h)
        draw_rectangle(true, x, y, w, h, @background)
      end

      private
      def default_filename
        "#{GLib.filename_from_utf8(@canvas.title.gsub(/\n/, ''))}.pdf"
      end

      def init_paper
        if @slides_per_page > 1
          @paper_width = A4_WIDTH
          @paper_height = A4_HEIGHT
        end

        default_width_mm = 360
        default_height_mm = 270
        if @paper_width.nil? and @paper_height.nil?
          size = Size.new(default_width_mm,
                          default_height_mm,
                          width.to_f / height.to_f)
          @page_width = size.logical_width
          @page_height = size.logical_height
        else
          @page_width = @paper_width || default_width_mm
          @page_height = @paper_height || default_height_mm
        end
      end

      def init_color
        super
        @foreground = make_color("black")
        @background = make_color(@background_color)
      end

      def init_dpi
        super
        @x_dpi = 300
        @y_dpi = 300
      end

      def update_layout
        @layout = PrintLayout.create(self, @canvas)
      end

      def create_context(output=nil)
        surface = find_surface(filename, output)
        surface.set_fallback_resolution(@x_dpi, @y_dpi)
        ::Cairo::Context.new(surface)
      end

      def create_pango_context
        context = create_context(StringIO.new).create_pango_layout.context
        set_font_resolution(context)
        context
      end

      def find_surface(filename, output=nil)
        args = [output || filename, @page_width, @page_height]
        case File.extname(filename)
        when /\.ps/i
          ::Cairo::PSSurface.new(*args)
        when /\.pdf/i
          ::Cairo::PDFSurface.new(*args)
        when /\.svg/i
          surface = ::Cairo::SVGSurface.new(*args)
          surface.restrict_to_version(::Cairo::SVG_VERSION_1_2)
          surface
        when /\.cs/i
          args[0] = ::Cairo::ScriptDevice.new(args[0])
          ::Cairo::ScriptSurface.new(*args)
        else
          @canvas.logger.warn(_("can't find printer for %s") % filename)
          args[0] = "default.ps"
          ::Cairo::PSSurface.new(*args)
        end
      end

      def need_show_page?
        @slides_per_page == 1 or
          @canvas.last_slide? or
          ((@canvas.current_index + 1) % @slides_per_page).zero?
      end
    end
  end
end
