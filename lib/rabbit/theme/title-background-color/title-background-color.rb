proc_name = "title-background-color"

if @title_background_color.nil?
  raise "must specify @title_background_color!!"
end

match(TitlePage) do |pages|
  pages.delete_pre_draw_proc_by_name(proc_name)

  pages.add_pre_draw_proc(proc_name) do |page, canvas, x, y, w, h, simulation|
    unless simulation
      args = [0, 0, canvas.width, canvas.height, @title_background_color]
      canvas.draw_rectangle(true, *args)
    end
    [x, y, w, h]
  end
end
