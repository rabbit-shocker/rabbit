require "rabbit-test-utils"

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
