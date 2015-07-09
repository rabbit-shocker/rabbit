#!/usr/bin/env ruby
#
# Copyright (C) 2004-2019  Kouhei Sutou <kou@cozmixng.org>
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

if ENV['CI']=='true'
  require 'simplecov'
  SimpleCov.start do
    add_filter 'test/'
  end
  require 'codecov'
  SimpleCov.formatter = SimpleCov::Formatter::Codecov
end

base_dir = File.expand_path(File.join(File.dirname(__FILE__), ".."))
test_dir = File.join(base_dir, "test")

$LOAD_PATH.unshift(File.join(base_dir, "lib"))
$LOAD_PATH.unshift(test_dir)

require_relative "helper"

exit(Test::Unit::AutoRunner.run(true, test_dir))
