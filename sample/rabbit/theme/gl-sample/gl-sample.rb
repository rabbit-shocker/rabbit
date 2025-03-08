include_theme("default")

proc_name = "gl-sample"

match(Slide) do |slides|

  slides.delete_post_draw_proc_by_name(proc_name)

  list_id = nil

  slides.each do |slide|
    break if not(slide.body.empty? and slide.headline.text == "GL")

    head = slide.headline
    head.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      list_id ||= canvas.new_list_id
      if simulation
        canvas.gl_compile(list_id) do
          qobj = GLU.NewQuadric
          GLU.Sphere(qobj, 1.0, 50, 20)
        end
      else
        canvas.gl_call_list(list_id, x, y, 0, "blue")
        canvas.draw_teapot(true, w - x, y, 0, 1.0, "red")
      end
      [x, y, w, h]
    end
  end
end
