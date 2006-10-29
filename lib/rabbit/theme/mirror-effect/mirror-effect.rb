def mirror_effect(target, options={})
  proc_name = options[:proc_name] || "mirror-effect"

  target.delete_post_draw_proc_by_name(proc_name)

  return if options[:uninstall]

  scale_y = 0.4
  shear_x = 0.5
  target.add_around_draw_proc(proc_name) do |canvas, x, y, w, h, simulation,
                                             next_proc|
    rx, ry, rw, rh = next_proc.call(canvas, x, y, w, h, simulation)
    unless simulation
      canvas.save_context do
        height = ry - y
        base = ry + target.margin_bottom
        canvas.translate_context(0, base)
        canvas.reflect_context(:x)
        canvas.shear_context(shear_x, 0)
        canvas.scale_context(1, scale_y)
        canvas.translate_context(0, -base)
        next_proc.call(canvas, x, y, w, height, simulation)
      end
    end
    [rx, ry, rw, rh]
  end
end
