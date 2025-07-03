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

require_relative "../utils"

require_relative "../image"
require_relative "../element"
require_relative "base"

module Rabbit
  module Parser
    class Image < Base
      push_loader(self)

      class << self
        def format_name
          "image"
        end

        def match?(source)
          options = {
            :prefix => "image-parser-match",
            :source  => source,
          }
          Rabbit::TemporaryFile.create(options) do |input|
            begin
              Rabbit::ImageLoader.new(input.path)
              true
            rescue Rabbit::ImageLoadError
              false
            end
          end
        end
      end

      include Element
      def parse
        options = {
          :prefix => "image-parser-parse",
          :source => @source,
        }
        TemporaryFile.create(options) do |image|
          @image = image
          @canvas << ImageTitleSlide.new(@image.path)
        end
      end
    end
  end
end
