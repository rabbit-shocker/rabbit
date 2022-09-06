# Copyright (C) 2006-2022  Sutou Kouhei <kou@cozmixng.org>
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

require "poppler"

require "rabbit/gettext"
require "rabbit/image/base"

module Rabbit
  module ImageManipulable
    class PDF < Base
      include GetText

      unshift_loader(self)

      class << self
        def match?(filename)
          return true if File.extname(filename) == ".pdf"

          File.open(filename, "rb") do |file|
            data = file.read(10)
            return false if data.nil?

            data.start_with?("%PDF-1.")
          end
        end
      end

      def draw(canvas, x, y, params={})
        if @document
          default_params = default_draw_params(x, y)
          canvas.draw_poppler_page(page, x, y, default_params.merge(params))
        else
          super
        end
      end

      def pixbuf
        @pixbuf ||= to_pixbuf
      end

      def update_size
        @document = Poppler::Document.new(uri)
        @width, @height = page.size
      end

      private
      def page
        index = self["page"] || 1
        begin
          index = Integer(index)
        rescue ArgumentError
          message = _("invalid PDF page number: <%s>") % index.inspect
          raise ImageLoadError.new("#{@filename}: #{message}")
        end
        _page = @document[index - 1]
        if _page.nil?
          message = _("%s page doesn't exist in PDF") % index
          raise ImageLoadError.new("#{@filename}: #{message}")
        end
        _page
      end

      def filename
        File.expand_path(@filename)
      end

      def uri
        GLib.filename_to_uri(filename)
      end

      def to_pixbuf
        w = original_width
        h = original_height
        pixbuf = GdkPixbuf::Pixbuf.new(:rgb, true, 8, w, h)
        page.render(0, 0, w, h, 1.0, 0, pixbuf)
        pixbuf
      end
    end
  end
end
