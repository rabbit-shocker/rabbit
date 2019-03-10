# Copyright (C) 2005-2019  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/renderer/color"

class RabbitColorTest < Test::Unit::TestCase
  def test_parse
    assert_color_parse(0, 0, 0, nil, "#000")
    assert_color_parse(0, 0, 0, nil, "#000000")
    assert_color_parse(0, 0, 0, nil, "#000000000000")
    assert_color_parse(0, 0, 0, nil, "black")

    assert_color_parse(65535, 65535, 65535, nil, "#fff")
    assert_color_parse(65535, 65535, 65535, nil, "#ffffff")
    assert_color_parse(65535, 65535, 65535, nil, "#ffffffffffff")
    assert_color_parse(65535, 65535, 65535, nil, "#FFF")
    assert_color_parse(65535, 65535, 65535, nil, "#FFFFFF")
    assert_color_parse(65535, 65535, 65535, nil, "#FFFFFFFFFFFF")
    assert_color_parse(65535, 65535, 65535, nil, "#FfF")
    assert_color_parse(65535, 65535, 65535, nil, "#FFffFF")
    assert_color_parse(65535, 65535, 65535, nil, "#FFFFffffFFFF")
    assert_color_parse(65535, 65535, 65535, nil, "white")

    assert_color_parse(62720, 61904, 50688, nil, "#f500f1d0c600")
  end

  def parse(spec)
    Rabbit::Renderer::Color.parse(spec)
  end

  def assert_color_parse(r, g, b, a, spec, message=nil)
    _wrap_assertion do
      color = parse(spec)
      assert_equal([r, g, b, a || 65535],
                   color.to_a.collect {|x| (x * 65535).ceil})
    end
  end
end
