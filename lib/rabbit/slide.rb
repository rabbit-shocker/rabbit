# Copyright (C) 2014  Kouhei Sutou <kou@cozmixng.org>
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

# NOTE: This class isn't used yet because it breaks the current API.
# TODO: Rename Rabbit::Element::Slide to Rabbit::Element::Page.

module Rabbit
  class Slide
    include Enumerable

    def initialize
      @pages = []
    end

    def <<(page)
      @pages << page
    end

    def size
      @pages.size
    end

    def each(&block)
      @pages.each(&block)
    end
  end
end
