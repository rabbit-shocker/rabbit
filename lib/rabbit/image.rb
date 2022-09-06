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

require "forwardable"
require "tempfile"

require "rabbit/image/default"
require "rabbit/image/dia"
require "rabbit/image/eps"
require "rabbit/image/gimp"
require "rabbit/image/mermaid"
require "rabbit/image/pdf"
require "rabbit/image/svg"

module Rabbit
  module ImageManipulable
    extend Forwardable

    def_delegators(:@loader, :keep_ratio, :keep_ratio?, :keep_ratio=)
    def_delegators(:@loader, :x_aspect_ratio, :y_aspect_ratio)
    def_delegators(:@loader, :pixbuf, :width, :height)
    def_delegators(:@loader, :original_width, :original_height)
    def_delegators(:@loader, :resize, :draw)
    def_delegators(:@loader, :[], :[]=)
    def_delegators(:@loader, :properties)
    alias_method :scale, :resize

    def initialize(filename, props=nil, *args, canvas: nil, &block)
      unless File.exist?(filename)
        raise ImageFileDoesNotExistError.new(filename)
      end
      super(*args, &block)
      @loader = Base.find_loader(filename).new(filename, props, canvas: canvas)
    end
  end

  class ImageLoader
    include ImageManipulable
  end
end
