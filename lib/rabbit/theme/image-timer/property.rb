@category = N_("Time")
@name = N_("ImageTimer")
@abstract = N_("Timer, an image version")
@description = N_("Display the progress of time with position of an image. " \
                  "This is useful both for speakers and for listeners to " \
                  "know elapsed time and time left. " \
                  "By default, a tortoise image walks slowly step by step.")
@dependencies = %w(rabbit-images)
@parameters = {
  "@image_timer_limit" => {
    :default => "canvas.title_slide.allotted_time",
    :description => N_("Limit time by second."),
  },
  "@image_timer_auto_update" => {
    :default => "true",
    :description => N_("Whether update image position automatically or not."),
  },
  "@image_timer_auto_scroll" => {
    :default => "false",
    :description => N_("Whether scroll automatically or not."),
  },
  "@image_timer_auto_scroll_direction" => {
    :default => ":left",
    :description => N_("Direction of automatic scrolling."),
  },
  "@image_timer_image" => {
    :default => "'kame.png'",
    :description => N_("File name of the image that moves along. " \
                       "A tortoise image in the 'rabbit-image' theme is " \
                       "used by default."),
  },
  "@image_timer_interval" => {
    :default => "3",
    :description => N_("Time interval between automatic update."),
  },
}
