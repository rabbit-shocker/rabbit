require 'fileutils'

require "rabbit-test-utils"

require "rabbit/theme/applier"

class RabbitApplierTest < Test::Unit::TestCase
  def test_normalize_source
    assert_normalize_source_ivar("x_large_font_size", "huge_font_size")
    assert_normalize_source_ivar("xx_large_font_size", "very_huge_font_size")
    assert_normalize_source_ivar("x_large_script_font_size",
                                 "huge_script_font_size")
  end

  def normalize_source(src)
    dummy = Object.new
    def dummy.slides
      Object.new
    end
    Rabbit::Theme::Applier.new(dummy).__send__(:normalize_source, src)
  end

  def assert_normalize_source(expected, src)
    _wrap_assertion do
      assert_equal(expected, normalize_source(src))
    end
  end

  def assert_normalize_source_ivar(after, before)
    _wrap_assertion do
      assert_normalize_source("@#{after}", "@#{before}")
      assert_normalize_source("@#{after}=111",
                              "@#{before}=111")
      assert_normalize_source("@#{after} = 111",
                              "@#{before} = 111")
      assert_normalize_source("(@#{after})",
                              "(@#{before})")
      assert_normalize_source("(xxx, @#{after}, xxx)",
                              "(xxx, @#{before}, xxx)")
    end
  end
end
