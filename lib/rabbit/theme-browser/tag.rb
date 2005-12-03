require 'gtk2'

require 'rabbit/rabbit'

module Rabbit
  class ThemeBrowser
    module Tag
      INFOS = [
        [
          "heading1",
          {
            "weight" => Pango::FontDescription::WEIGHT_BOLD,
            "pixels-above-lines" => 10,
            "pixels-below-lines" => 10,
            "left-margin" => 5,
            "size" => 18 * Pango::SCALE,
          }
        ],
        [
          "heading2",
          {
            "pixels-above-lines" => 10,
            "pixels-below-lines" => 5,
            "left-margin" => 5,
            "size" => 17 * Pango::SCALE,
          }
        ],
        [
          "item",
          {
            "left-margin" => 15,
            "pixels-above-lines" => 2,
            "pixels-below-lines" => 2,
          }
        ],
        [
          "item-content",
          {
          }
        ],
        [
          "link",
          {
            "foreground" => "blue",
            "underline" => Pango::AttrUnderline::SINGLE,
          }
        ],
        [
          "description",
          {
            "left-margin" => 20,
            "right-margin" => 20,
          }
        ],
        [
          "parameter",
          {
            "left-margin" => 20,
            "right-margin" => 20,
            "pixels-below-lines" => 4,
          }
        ],
        [
          "parameter-name",
          {
            "foreground" => "red",
            "family" => "Monospace",
          }
        ],
        [
          "parameter-default",
          {
            "foreground" => "gray",
            "family" => "Monospace",
            "style" => Pango::FontDescription::STYLE_ITALIC,
          }
        ],
        [
          "parameter-description",
          {
            "left-margin" => 40,
            "right-margin" => 40,
          }
        ],
      ]
    end
  end
end
