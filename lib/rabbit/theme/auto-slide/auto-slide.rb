theme_exit if print?

@auto_slide_span ||= 1000 * 60 # a minute
@auto_slide_loop ||= false

@@auto_slide_timeout_id ||= nil

unless @@auto_slide_timeout_id.nil?
  Gtk.timeout_remove(@@auto_slide_timeout_id)
end

@@auto_slide_timeout_id = Gtk.timeout_add(@auto_slide_span) do
  if canvas.last_slide?
    if @auto_slide_loop
      canvas.move_to_first
      true
    else
      false
    end
  else
    canvas.move_to_next_if_can
    true
  end
end
