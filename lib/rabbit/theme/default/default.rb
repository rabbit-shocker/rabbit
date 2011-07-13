@default_foreground ||= @foreground
@default_background ||= @background
@default_shadow_color ||= @shadow_color

set_foreground(@default_foreground)
set_background(@default_background)

add_image_path("ruby-images")
include_theme("default-icon")
include_theme("default-title-text")
include_theme("default-text")
include_theme("default-title-slide")
include_theme("default-slide")
include_theme("default-item-mark")
include_theme("default-method-list")
include_theme("default-preformatted")
include_theme("default-block-quote")
include_theme("default-foot-text")
include_theme("default-description")
include_theme("image")
include_theme("table")
include_theme("newline-in-slides")
include_theme("slide-number")

include_theme("per-slide-background-color")

include_theme("background-image-toolkit")
include_theme("per-slide-background-image")
include_theme("body-background-image")

include_theme("tag")

include_theme("syntax-highlighting")

if display? and canvas.renderer.widget.class.name == "Clutter::GtkEmbed"
  include_theme("clutter-comment")
else
  include_theme("footer-comment")
end
