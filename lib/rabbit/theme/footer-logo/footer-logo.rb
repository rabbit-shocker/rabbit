proc_name = "footer-logo"

if @footer_logo_image.nil?
  theme_exit("must specify @footer_logo_image!!")
end
@footer_logo_keep_ratio ||= nil
@footer_logo_keep_ratio = true if @footer_logo_keep_ratio.nil?
@footer_logo_margin_right ||= @margin_right
@footer_logo_margin_bottom ||= @margin_bottom + screen_y(1)
@footer_logo_uninstall ||= false

match(SlideElement) do
  delete_pre_draw_proc_by_name(proc_name)

  break if @footer_logo_uninstall

  loader = ImageLoader.new(find_file(@footer_logo_image))
  loader.keep_ratio = @footer_logo_keep_ratio
  available_w = canvas.width - @footer_logo_margin_right
  available_h = canvas.height - @footer_logo_margin_bottom
  add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    if simulation
      loader.resize(available_w * 0.1, available_h * 0.1)
    else
      loader.draw(canvas,
                  available_w - loader.width,
                  available_h - loader.height)
    end
    [x, y, w, h]
  end
end
