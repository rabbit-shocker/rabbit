@category = N_("Toolkit")
@title = N_("ImageSlideNumber")
@abstract = N_("Toolkit to display slide numbers, an image version")
@description = N_("Displays the progress of presentation with position of " \
                  "an image. " \
                  "This is useful both for speakers and for listeners to " \
                  "know elapsed slide numbers and slide numbers left. " \
                  "By default, an image of a hare, which is a family of " \
                  "rabbit, jumps along the bottom of slides between two " \
                  "flags.\n" \
                  "\n" \
                  "Using together with another theme, 'image-timer', you " \
                  "can make a hare and a tortoise race like the fable of " \
                  "the hare and the tortoise. " \
                  "When doing so, however, note that you should make your " \
                  "presentation not so slowly that the hare will lose to " \
                  "the tortoise.")
@dependencies = %w(rabbit-images)
@parameters = {
  "@image_slide_number_image" => {
    :default => "'mini-usagi.png'",
    :description => N_("File name of an image that moves. " \
                       "An image of a hare in the 'rabbit-image' theme is " \
                       "used by default."),
  },
  "@image_slide_number_show_text" => {
    :default => "false",
    :description => N_("Whether drawing start and goal flags with text or " \
                       "not. " \
                       "When '(({true}))', flags are drawn with text with a " \
                       "form of @image_slide_number_flag_type and on which " \
                       "slide numbers are also drawn with color " \
                       "@image_slide_number_text_color. " \
                       "Otherwise, image files specified with " \
                       "@image_slide_number_start_image and " \
                       "@image_slide_number_goal_image are used as flags.")
  },
  "@image_slide_number_text_color" => {
    :default => "'white'",
    :description => N_("Color of numbers displayed on the start and goal " \
                       "flags.")
  },
  "@image_slide_number_flag_type" => {
    :default => "'rectangle'",
    :description => N_("Form of the start and goal flags. " \
                       "Avaiable forms are 'triangle' and 'rectangle'.")
  },
  "@image_slide_number_start_image" => {
    :default => "'start-flag.png'",
    :description => N_("File name of an image used as the start flag.")
  },
  "@image_slide_number_goal_image" => {
    :default => "'goal-flag.png'",
    :description => N_("File name of an image used as the goal flag.")
  },
  "@image_slide_number_uninstall" => {
    :default => "nil",
    :description => N_("Whether uninstalling this toolkit or not. " \
                       "This option is useful for cases you do not want to " \
                       "make the toolkit work for certain slides."),
  },
}
