# Copyright (C) 2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "gtk"

require_relative "image"
require_relative "theme/searcher"

module Rabbit
  module Icon
    module_function
    def rabbit
      image_theme = Theme::Searcher.find_theme("rabbit-images", true)
      file = Theme::Searcher.find_file("lavie-icon.png", [image_theme])
      loader = ImageLoader.new(file)
      loader.resize(32, 32)
      bytes = GLib::Bytes.new(loader.pixbuf.save("png"))
      Gio::BytesIcon.new(bytes)
    end
  end
end
