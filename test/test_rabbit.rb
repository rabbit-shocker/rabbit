require "rabbit-test-utils"

load "bin/rabbit"

class RabbitTest < Test::Unit::TestCase

  def test_parse_margins
    assert_equal([1, 1, 1, 1], parse_margins(%w(1)))
    assert_equal([1, 2, 1, 2], parse_margins(%w(1 2)))
    assert_equal([1, 2, 3, 2], parse_margins(%w(1 2 3)))
    assert_equal([1, 2, 3, 4], parse_margins(%w(1 2 3 4)))

    assert_raises(ArgumentError){parse_margins([])}
    assert_raises(ArgumentError){parse_margins(%w(1 2 3 4 5))}
    assert_raises(ArgumentError){parse_margins(%w(a))}
    assert_raises(ArgumentError){parse_margins(%w(1.0))}
  end
  
end
