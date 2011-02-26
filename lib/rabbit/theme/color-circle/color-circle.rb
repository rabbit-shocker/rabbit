must_set_variables = %w(foreground background color light_color bright_color
                        graffiti_color)

@color_circle_open_quote_image ||= nil
@color_circle_close_quote_image ||= nil

not_set_variables = []
must_set_variables.each do |name|
  variable_name = "@color_circle_#{name}"
  if instance_variable_get(variable_name).nil?
    not_set_variables << variable_name
  end
end

unless not_set_variables.empty?
  format = _("required variables aren't set: %s")
  canvas.logger.error(format % not_set_variables.inspect)
  theme_exit
end

set_graffiti_color(@color_circle_graffiti_color)

set_progress_foreground(@color_circle_color)
set_progress_background(@color_circle_bright_color)

include_theme("default-icon")

include_theme("newline-in-slides")
include_theme("tag")

include_theme("image")
include_theme("table")

include_theme("color-circle-title-slide")
include_theme("color-circle-title-text")
include_theme("color-circle-slide")
include_theme("color-circle-text")
include_theme("color-circle-item-mark")
include_theme("color-circle-preformatted")
include_theme("color-circle-block-quote")
include_theme("color-circle-foot-text")
include_theme("color-circle-description")
include_theme("color-circle-method-list")
