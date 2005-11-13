@category = N_("Time")
@name = N_("ImageTimer")
@abstract = N_("Timer, an image version")
@description = N_("Displays the progress of time with position of an image. " \
                  "This is useful both for speakers and for listeners to " \
                  "know elapsed time and time left. " \
                  "By default, a tortoise image walks along the bottom of " \
                  "slides slowly step by step.\n" \
                  "\n" \
                  "Using with another theme, 'image-slide-number', you can " \
                  "make a hare and a tortoise race like the fable of the " \
                  "hare and the tortoise. " \
                  "When doing so, however, note that you should make your " \
                  "presentation not so slowly that the hare will lose to " \
                  "the tortoise.")
@dependencies = %w(rabbit-images)
@parameters = {
  "@image_timer_limit" => {
    :default => "canvas.title_slide.allotted_time",
    :description => N_("Limit time by second."),
  },
  "@image_timer_auto_update" => {
    :default => "true",
    :description => N_("Whether updating image position automatically or " \
                       "not."),
  },
  "@image_timer_auto_scroll" => {
    :default => "false",
    :description => N_("Whether scrolling automatically or not."),
  },
  "@image_timer_auto_scroll_direction" => {
    :default => ":left",
    :description => N_("Direction of automatic scrolling."),
  },
  "@image_timer_image" => {
    :default => "'kame.png'",
    :description => N_("File name of an image that moves. " \
                       "A tortoise image in the 'rabbit-image' theme is " \
                       "used by default."),
  },
  "@image_timer_interval" => {
    :default => "3",
    :description => N_("Time interval between automatic update."),
  },
}
