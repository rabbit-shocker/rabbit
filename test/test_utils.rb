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
end
