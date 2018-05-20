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

require "gdk_pixbuf2"

module Rabbit
  class ImageDataLoader
    attr_reader :width
    attr_reader :height
    attr_reader :pixbuf
    attr_reader :animation
    def initialize(data)
      @width = 0
      @height = 0
      @pixbuf = nil
      @animation = nil
      @data = data
    end

    def load
      loader = GdkPixbuf::PixbufLoader.new
      id = loader.signal_connect("size_prepared") do |l, width, height|
        @width = width
        @height = height
      end
      begin
        loader.last_write(@data)
      rescue GdkPixbuf::PixbufError => error
        loader.close rescue GdkPixbuf::PixbufError
        raise ImageLoadError.new(error.message)
      end
      loader.signal_handler_disconnect(id)
      @pixbuf = loader.pixbuf
      @animation = loader.animation
      @pixbuf
    end
  end
end
