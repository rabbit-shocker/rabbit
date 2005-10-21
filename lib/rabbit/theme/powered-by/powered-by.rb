proc_name = "powered-by"

@powered_by_props ||= {
  "size" => screen_size(1.5 * Pango::SCALE),
  "font_family" => @font_family,
}

@powered_by_images ||= []

loaders = @powered_by_images.collect do |image|
  ImageLoader.new(find_file(image))
end

add_powered_by = proc do |slide|
  space = screen_x(1)
  layout = nil
  tw = th = 0

  slide.delete_post_draw_proc_by_name(proc_name)

  slide.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    unless simulation
      if @powered_by_text and layout.nil?
        text = @powered_by_text
        text = %Q[<span #{to_attrs(@powered_by_props)}>#{text}</span>]
        layout = canvas.make_layout(text)
        tw, th = layout.pixel_size
      end
      
      new_x = slide.margin_left
      new_y = canvas.height - slide.margin_bottom
      
      canvas.draw_layout(layout, new_x, new_y - th) if layout

      new_x += tw
      loaders.each do |loader|
        slide_space = canvas.height - y - slide.margin_bottom
        loader.resize(nil, slide_space) if loader.height > slide_space
        px = new_x + space
        py = new_y - loader.height
        canvas.draw_pixbuf(loader.pixbuf, px, py)
        new_x = px + loader.width 
      end
    end
    [x, y, w, h]
  end
end

match(TitleSlide) do |slides|
  add_powered_by.call(slides.first) unless slides.empty?
end

match(Slide) do |slides|
  add_powered_by.call(slides.last) unless slides.empty?
end
