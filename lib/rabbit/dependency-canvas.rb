# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "canvas"

module Rabbit
  class DependencyCanvas < Canvas
    extend Forwardable

    def_delegators(:@parent,
                   :theme_name, :allotted_time, :rest_time, :activate)

    def initialize(parent, *rest, &block)
      @parent = parent
      super(*rest, &block)
      self.base_width = @parent.base_width
      self.base_height = @parent.base_height
    end
  end
end
