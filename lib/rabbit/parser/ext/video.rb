# Copyright (C) 2012  Narihiro Nakamura <authornari@gmail.com>
# Copyright (C) 2018-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../../element"
require_relative "image"

module Rabbit
  module Parser
    module Ext
      module Video
        def make_video(canvas, uri_str, prop)
          path = Image::Private.uri_string_to_image_filename(canvas, uri_str)
          begin
            Element::Video.new(path, prop)
          rescue Error
            Rabbit.logger.warn($!.message)
            nil
          end
        end
      end
    end
  end
end
