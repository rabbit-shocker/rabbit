proc_name = "gl-sample"

match(TitleSlide, Title) do |titles|

  titles.delete_pre_draw_proc_by_name(proc_name)

  break if @gl_sample_uninstall
  
  list_id = nil
  
  titles.add_pre_draw_proc(proc_name) do |title, canvas, x, y, w, h, simulation|
    list_id ||= canvas.new_list_id
    if simulation
      qobj = GLU.NewQuadric
      canvas.gl_compile(list_id) do
        GLU.Sphere(qobj, 1.0, 50, 20)
      end
    else
      canvas.gl_call_list(list_id, -1.0, 1.0, -3.0)
      canvas.draw_teapot(true, -1.0, -1.0, -3.0, 0.7)
    end
    [x, y, w, h]
  end
end
