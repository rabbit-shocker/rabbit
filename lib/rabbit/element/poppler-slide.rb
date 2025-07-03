# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "slide"
require_relative "poppler-page"

module Rabbit
  module Element
    class PopplerSlide < Slide
      def initialize(page)
        @raw_page = page
        @page = PopplerPage.new(page)
        super(@page)
      end

      def headline
        @page
      end

      def body
        @page
      end

      def title
        (@page.text.split(/\r?\n/, 2)[0] || super).chomp
      end

      def size_ratio
        w, h = @raw_page.size
        w.to_f / h.to_f
      end
    end
  end
end
