# Copyright (C) 2016-2021  Sutou Kouhei <kou@cozmixng.org>
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

require "rake"
require "open-uri"

require "rabbit/gettext"
require "rabbit/password-reader"
require "rabbit/yaml-loader"

module Rabbit
  class GemPusher
    include GetText
    include Rake::DSL

    def initialize(gem_path, user)
      @gem_path = gem_path
      @user = user
    end

    def push
      system("gem push #{@gem_path}")
    end
  end
end
