proc_name = "powered-by"

loader = nil
if @powered_by_image
  loader = ImageLoader.new(search_file(@powered_by_image))
end

add_powered_by = proc do |page|
  space = screen_x(1)
  layout = nil
  tw, th = nil

  page.delete_post_draw_proc_by_name(proc_name)

  page.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
    unless simulation
      if layout.nil?
        layout, tw, th = make_layout(canvas, "Powered by #{@powered_by_text}")
        page_space = (canvas.height - y - page.bottom_margin) / 2.0
      end
      new_x = page.left_margin
      new_y = canvas.height - page.bottom_margin
      
      draw_layout(canvas, layout, new_x, new_y - th)
      
      unless loader.nil?
        px = new_x + tw + space
        py = new_y - loader.height
        draw_pixbuf(canvas, loader.pixbuf, px, py)
      end
    end
    [x, y, w, h]
  end
end

match(TitlePage) do |pages|
  add_powered_by.call(pages.first)
end

match(Page) do |pages|
  add_powered_by.call(pages.last)
end
