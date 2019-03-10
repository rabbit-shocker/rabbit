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

require "rabbit/utils"

class RabbitUtilsTest < Test::Unit::TestCase
  def test_combination
    assert_combination([], [])
    assert_combination([[], [1]], [1])
    assert_combination([[], [1], [2], [1, 2]], [1, 2])
    assert_combination([[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]],
                       [1, 2, 3])
  end

  def test_parse_four_way
    assert_four_way([1, 1, 1, 1], %w(1))
    assert_four_way([1, 2, 1, 2], %w(1 2))
    assert_four_way([1, 2, 3, 2], %w(1 2 3))
    assert_four_way([1, 2, 3, 4], %w(1 2 3 4))

    assert_invalid_four_way([])
    assert_invalid_four_way(%w(1 2 3 4 5))
    assert_invalid_four_way(%w(a))
    assert_invalid_four_way(%w(1.0))
  end

  private
  def assert_combination(expected, elements)
    _wrap_assertion do
      results = Rabbit::Utils.combination(elements)
      assert_equal(normalize_combination_results(expected),
                   normalize_combination_results(results))
    end
  end

  def normalize_combination_results(results)
    results.collect {|result| result.sort}.sort
  end

  def assert_four_way(expected, input)
    _wrap_assertion do
      assert_equal(expected, Rabbit::Utils.parse_four_way(input))
    end
  end

  def assert_invalid_four_way(input)
    _wrap_assertion do
      assert_raises(ArgumentError) {Rabbit::Utils.parse_four_way(input)}
    end
  end
end
