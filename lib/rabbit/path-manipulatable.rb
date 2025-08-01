# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "fileutils"
require "pathname"

require_relative "rabbit"

module Rabbit
  module PathManipulatable
    include GetText

    private
    def create_directory(path)
      Rabbit.logger.info(_("Creating directory: %s") % path)
      FileUtils.mkdir_p(path)
    end

    def create_file(path, &block)
      Rabbit.logger.info(_("Creating file:      %s") % path)
      File.open(path, "w", &block)
    end
  end
end
