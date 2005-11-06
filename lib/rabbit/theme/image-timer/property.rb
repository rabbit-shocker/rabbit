@category = N_("Time")
@name = N_("ImageTimer")
@abstract = N_("image version timer")
@description = N_("...")
@dependencies = %w(rabbit-images)
@parameters = {
  "@image_timer_limit" => {
    :default => N_("must specify"),
    :description => N_("limit time. (sec)"),
  },
  "@image_timer_auto_update" => {
    :default => "true",
    :description => N_("update image position automatically or not."),
  },
}
