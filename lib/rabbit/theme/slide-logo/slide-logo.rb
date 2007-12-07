proc_name = "slide-logo"

if @slide_logo_image.nil?
  theme_exit(_("must specify %s!!!") % "@slide_logo_image")
end

@slide_logo_position ||= :right
@slide_logo_width ||= canvas.width / 3.0
@slide_logo_height ||= nil

match(SlideElement) do
  delete_pre_draw_proc_by_name(proc_name)

  break if @slide_logo_image_uninstall

  loader = ImageLoader.new(find_file(@slide_logo_image))
  loader.resize(@slide_logo_width, @slide_logo_height)

  delete_pre_draw_proc_by_name(name)
  add_pre_draw_proc(name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      if @slide_logo_position == :right
        logo_x = canvas.width - loader.width
      else
        logo_x = 0
      end
      loader.draw(canvas, logo_x, 0)
    end
    [x, y, w, h]
  end
end
