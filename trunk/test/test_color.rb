require 'fileutils'

require "rabbit-test-utils"

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
      assert_equal([r, g, b, a || 65535], color.to_gdk_rgba)
      assert_equal(!a.nil?, color.have_alpha?, "have_alpha?")
      gdk_color = color.to_gdk_color
      assert_equal([r, g, b],
                   [gdk_color.red, gdk_color.green, gdk_color.blue])
    end
  end
end
