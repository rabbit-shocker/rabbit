@category = N_("Toolkit")
@title = N_("Icon")
@abstract = N_("Toolkit to specify an image or images as icons")
@description = N_("Specifies an image or images as icons of the window, " \
                  "which will be used by some window managers and desktop " \
                  "environments, for example when the window is minimized " \
                  "(or 'iconified'), in the window frame, or when windows " \
                  "are switched. " \
                  "The specified image or images are automatically scaled " \
                  "to the icon sizes case by case. " \
                  "When several images are specified and they have " \
                  "different sizes, an image with the most similar size to " \
                  "that of icon among them is chosen and scaled in order to " \
                  "improve the quality of image finally displayed.")
@parameters = {
  "@icon_image" => {
    :default => "[]",
    :description => N_("List of image file names."),
  },
  "@icon_images" => {
    :default => "nil",
    :description => N_("Image file name."),
  },
}
