# Copyright (C) 2015-2019  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit/size"

class RabbitSizeTest < Test::Unit::TestCase
  sub_test_case("width > height") do
    setup do
      @size = Rabbit::Size.new(800, 600, 1920, 1018, 4.0 / 3.0)
    end

    test "#logical_width" do
      assert_equal(800, @size.logical_width.round)
    end

    test "#logical_height" do
      assert_equal(600, @size.logical_height)
    end

    test "#logical_margin_left" do
      assert_equal(166, @size.logical_margin_left.round)
    end

    test "#logical_margin_right" do
      assert_equal(166, @size.logical_margin_right.round)
    end

    test "#logical_margin_top" do
      assert_equal(0, @size.logical_margin_top)
    end

    test "#logical_margin_bottom" do
      assert_equal(0, @size.logical_margin_bottom)
    end
  end

  sub_test_case("width < height") do
    setup do
      @size = Rabbit::Size.new(800, 600, 1018, 1920, 4.0 / 3.0)
    end

    test "#logical_width" do
      assert_equal(800, @size.logical_width)
    end

    test "#logical_height" do
      assert_equal(600, @size.logical_height.round)
    end

    test "#logical_margin_left" do
      assert_equal(0, @size.logical_margin_left)
    end

    test "#logical_margin_right" do
      assert_equal(0, @size.logical_margin_right)
    end

    test "#logical_margin_top" do
      assert_equal(454, @size.logical_margin_top.round)
    end

    test "#logical_margin_bottom" do
      assert_equal(454, @size.logical_margin_bottom.round)
    end

    test "#logical_scale" do
      assert_equal([1.27, 1.27],
                   @size.logical_scale.collect {|scale| scale.round(2)})
    end
  end

  sub_test_case("predicates") do
    sub_test_case("#have_logical_margin_x?") do
      test "width > height" do
        size = Rabbit::Size.new(800, 600, 1920, 1018, 4.0 / 3.0)
        assert do
          size.have_logical_margin_x?
        end
      end

      test "width < height" do
        size = Rabbit::Size.new(800, 600, 1018, 1920, 4.0 / 3.0)
        assert do
          not size.have_logical_margin_x?
        end
      end

      test "width / height == ratio" do
        size = Rabbit::Size.new(800, 600, 1600, 1200, 4.0 / 3.0)
        assert do
          not size.have_logical_margin_x?
        end
      end
    end

    sub_test_case("#have_logical_margin_y?") do
      test "width > height" do
        size = Rabbit::Size.new(800, 600, 1920, 1018, 4.0 / 3.0)
        assert do
          not size.have_logical_margin_y?
        end
      end

      test "width < height" do
        size = Rabbit::Size.new(800, 600, 1018, 1920, 4.0 / 3.0)
        assert do
          size.have_logical_margin_y?
        end
      end

      test "width / height == ratio" do
        size = Rabbit::Size.new(800, 600, 1600, 1200, 4.0 / 3.0)
        assert do
          not size.have_logical_margin_y?
        end
      end
    end

    sub_test_case("#have_logical_margin?") do
      test "width > height" do
        size = Rabbit::Size.new(800, 600, 1920, 1018, 4.0 / 3.0)
        assert do
          size.have_logical_margin?
        end
      end

      test "width < height" do
        size = Rabbit::Size.new(800, 600, 1018, 1920, 4.0 / 3.0)
        assert do
          size.have_logical_margin?
        end
      end

      test "width / height == ratio" do
        size = Rabbit::Size.new(800, 600, 1600, 1200, 4.0 / 3.0)
        assert do
          not size.have_logical_margin?
        end
      end
    end
  end
end
