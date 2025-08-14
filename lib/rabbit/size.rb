# Copyright (C) 2015-2025  Kouhei Sutou <kou@cozmixng.org>
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

module Rabbit
  class Size
    attr_reader :base_width
    attr_reader :base_height
    attr_reader :real_width
    attr_reader :real_height
    attr_reader :real_content_width
    attr_reader :real_content_height
    attr_reader :logical_width
    attr_reader :logical_height
    attr_reader :logical_margin_left
    attr_reader :logical_margin_right
    attr_reader :logical_margin_top
    attr_reader :logical_margin_bottom
    attr_reader :ratio
    def initialize(base_width, base_height, width, height, ratio)
      @base_width = base_width
      @base_height = base_height
      @real_width = width
      @real_height = height
      @ratio = ratio
      compute_logical_size
    end

    def have_logical_margin_x?
      @logical_margin_left > 0 or
        @logical_margin_right > 0
    end

    def have_logical_margin_y?
      @logical_margin_top > 0 or
        @logical_margin_bottom > 0
    end

    def have_logical_margin?
      have_logical_margin_x? or have_logical_margin_y?
    end

    def logical_scale
      @logical_scale
    end

    def resolve_logical_x(x)
      x * @logical_scale[0]
    end

    def resolve_logical_y(y)
      y * @logical_scale[0]
    end

    private
    def compute_logical_size
      @logical_width = @base_width
      @logical_height = @base_height

      real_ratio = @real_width.to_f / @real_height.to_f
      if real_ratio == @ratio
        @real_content_width = @real_width
        @real_content_height = @real_height
      elsif real_ratio > @ratio
        @real_content_width = @real_width * (@ratio / real_ratio)
        @real_content_height = @real_height
      else
        @real_content_width = @real_width
        @real_content_height = @real_height * (real_ratio / @ratio)
      end

      logical_scale_x = @real_content_width.to_f / @logical_width.to_f
      logical_scale_y = @real_content_height.to_f / @logical_height.to_f

      real_margin_width = @real_width - @real_content_width
      logical_margin_width = real_margin_width / logical_scale_x
      @logical_margin_left = logical_margin_width / 2
      @logical_margin_right = logical_margin_width / 2

      real_margin_height = @real_height - @real_content_height
      logical_margin_height = real_margin_height / logical_scale_y
      @logical_margin_top = logical_margin_height / 2
      @logical_margin_bottom = logical_margin_height / 2

      @logical_scale = [logical_scale_x, logical_scale_y]
    end
  end
end
