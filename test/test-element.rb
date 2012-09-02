require 'fileutils'

require "rabbit-test-utils"

require "rabbit/element"

class RabbitElementTest < Test::Unit::TestCase
  def test_normalize_font_property
    assert_normalize_font_property(["font_desc", "Sans Italic 12"],
                                   [:desc, "Sans Italic 12"])
    assert_normalize_font_property(["font_desc", "Sans Italic 12"],
                                   [:description, "Sans Italic 12"])

    assert_normalize_font_property(["font_family", "Sans"],
                                   [:family, "Sans"])

    assert_normalize_font_property(["size", 12800],
                                   [:size, 12800])
    assert_normalize_font_property(["size", "small"],
                                   [:size, "small"])

    assert_normalize_font_property(["style", "oblique"],
                                   [:style, "oblique"])

    assert_normalize_font_property(["weight", "bold"],
                                   [:weight, "bold"])

    assert_normalize_font_property(["variant", "smallcaps"],
                                   [:variant, "smallcaps"])

    assert_normalize_font_property(["stretch", "ultracondensed"],
                                   [:stretch, "ultracondensed"])
    
    assert_normalize_font_property(["foreground", "#ff00cc"],
                                   [:foreground, "#ff00cc"])
    assert_normalize_font_property(["foreground", "#ff00cc"],
                                   [:color, "#ff00cc"])
    assert_normalize_font_property(["foreground", "#ff00cc"],
                                   [:fg_color, "#ff00cc"])
    assert_normalize_font_property(["foreground", "#ff00cc"],
                                   [:fg, "#ff00cc"])

    assert_normalize_font_property(["background", "#ff00cc"],
                                   [:background, "#ff00cc"])
    assert_normalize_font_property(["background", "#ff00cc"],
                                   [:bg_color, "#ff00cc"])
    assert_normalize_font_property(["background", "#ff00cc"],
                                   [:bg, "#ff00cc"])

    assert_normalize_font_property(["underline", "low"],
                                   [:underline, "low"])
    assert_normalize_font_property(["underline", "low"],
                                   [:ul, "low"])

    assert_normalize_font_property(["underline_color", "red"],
                                   [:underline_color, "red"])
    assert_normalize_font_property(["underline_color", "red"],
                                   [:ul_color, "red"])

    assert_normalize_font_property(["rise", 12800],
                                   [:rise, 12800])
    assert_normalize_font_property(["rise", 12800],
                                   [:superscript, 12800])
    assert_normalize_font_property(["rise", -12800],
                                   [:subscript, 12800])

    assert_normalize_font_property(["strikethrough", "true"],
                                   [:strikethrough, true])
    assert_normalize_font_property(["strikethrough", "false"],
                                   [:strike_through, false])

    assert_normalize_font_property(["strikethrough_color", "red"],
                                   [:strikethrough_color, "red"])
    assert_normalize_font_property(["strikethrough_color", "red"],
                                   [:strike_through_color, "red"])

    assert_normalize_font_property(["fallback", "false"],
                                   [:fallback, false])
  end

  def normalize_font_property(key, value)
    elem = Rabbit::Element::Text.new("XXX")
    elem.__send__(:normalize_font_property, key, value)
  end

  def assert_normalize_font_property(expected, actual)
    _wrap_assertion do
      assert_equal(expected, normalize_font_property(*actual))
    end
  end
end
