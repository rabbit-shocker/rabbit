# Copyright (C) 2026  Shugo Maeda <shugo@ruby-lang.org>
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

require "rabbit/gettext"
require "rabbit/error"
require "rabbit/properties"

class RabbitPropertiesTest < Test::Unit::TestCase
  sub_test_case("#get_size") do
    def get_size(value)
      properties = Rabbit::Properties.new({"width" => value})
      properties.get_size("width", "image.png")
    end

    test "nil" do
      assert_nil(get_size(nil))
    end

    test "String" do
      assert_equal(80, get_size("80"))
    end

    test "Integer" do
      assert_equal(80, get_size(80))
    end

    test "Float" do
      assert_equal(80, get_size(80.0))
    end

    test "invalid String" do
      assert_raise(Rabbit::InvalidSizeError) do
        get_size("80px")
      end
    end

    test "invalid Object" do
      assert_raise(Rabbit::InvalidSizeError) do
        get_size([80])
      end
    end
  end
end
