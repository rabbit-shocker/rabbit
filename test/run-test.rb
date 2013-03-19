#!/usr/bin/env ruby
#
# Copyright (C) 2004-2012  Kouhei Sutou <kou@cozmixng.org>
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

base_dir = File.expand_path(".")
$LOAD_PATH.unshift(base_dir)
$LOAD_PATH.unshift(File.join(base_dir, "lib"))
$LOAD_PATH.unshift(File.join(base_dir, "test"))

require "rabbit-test-utils"

test_file = "test/**/test-*.rb"

Dir.glob(test_file) do |file|
  require file.gsub(/\.rb$/, '')
end
