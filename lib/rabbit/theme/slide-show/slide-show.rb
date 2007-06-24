theme_exit unless display?

@slide_show_span ||= 1000 * 60 # a minute
@slide_show_loop ||= false

@@slide_show_timeout_id ||= nil

unless @@slide_show_timeout_id.nil?
  Gtk.timeout_remove(@@slide_show_timeout_id)
end

@@slide_show_timeout_id = Gtk.timeout_add(@slide_show_span) do
  if canvas.last_slide?
    if @slide_show_loop
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
