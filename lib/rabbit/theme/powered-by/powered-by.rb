proc_name = "powered-by"

@powered_by_props ||= {
  "size" => screen_size(1.5 * Pango::SCALE),
  "font_family" => @font_family,
}
@powered_by_props.delete("font_family") unless @powered_by_props["font_family"]

loader = nil
if @powered_by_image
  loader = ImageLoader.new(search_file(@powered_by_image))
end

add_powered_by = proc do |slide|
  space = screen_x(1)
  layout = nil
  tw, th = nil

  slide.delete_post_draw_proc_by_name(proc_name)

  slide.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    unless simulation
      if layout.nil?
        text = "Powered by #{@powered_by_text}"
        text = %Q[<span #{to_attrs(@powered_by_props)}>#{text}</span>]
        layout, tw, th = canvas.make_layout(text)
      end
      
      new_x = slide.left_margin
      new_y = canvas.height - slide.bottom_margin
      
      canvas.draw_layout(layout, new_x, new_y - th)
      
      unless loader.nil?
        slide_space = canvas.height - y - slide.bottom_margin
        loader.resize(nil, slide_space) if loader.height > slide_space
        px = new_x + tw + space
        py = new_y - loader.height
        canvas.draw_pixbuf(loader.pixbuf, px, py)
      end
    end
    [x, y, w, h]
  end
end

match(TitleSlide) do |slides|
  add_powered_by.call(slides.first)
end

match(Slide) do |slides|
  add_powered_by.call(slides.last)
end
