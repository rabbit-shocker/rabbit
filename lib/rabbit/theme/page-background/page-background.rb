proc_name = "page-background"

if @page_background_image.nil?
  raise "must specify @page_background_image!!"
end

match(Page) do |pages|
  loader = ImageLoader.new(search_file(@page_background_image))
  resized = false

  pages.delete_pre_draw_proc_by_name(proc_name)

  pages.add_pre_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      unless loader.nil?
        unless resized
          loader.resize(canvas.width, canvas.height)
          resized = true
        end
        canvas.draw_pixbuf(loader.pixbuf, 0, 0)
      end
    end
    [x, y, w, h]
  end
end
