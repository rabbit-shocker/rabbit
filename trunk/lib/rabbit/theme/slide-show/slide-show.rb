theme_exit unless display?

if canvas.allotted_time
  default_slide_show_span = (canvas.allotted_time / canvas.slide_size.to_f).round
else
  default_slide_show_span = 60
end
default_slide_show_span *= 1000 # milliseconds -> seconds
@slide_show_span ||= default_slide_show_span
@slide_show_loop ||= false

@@slide_show_timeout_id ||= nil

unless @@slide_show_timeout_id.nil?
  Gtk.timeout_remove(@@slide_show_timeout_id)
end

@@slide_show_timeout_id = Gtk.timeout_add(@slide_show_span) do
  if canvas.last_slide?
    if @slide_show_loop
      canvas.reset_timer
      canvas.activate("FirstSlide")
      true
    else
      false
    end
  else
    canvas.activate("NextSlide")
    true
  end
end
