def scroll_effect(target, options={})
  proc_name = options[:proc_name] || "scroll-effect"

  target.delete_post_draw_proc_by_name(proc_name)

  return if options[:uninstall]

  idle_id = nil
  effected = false
  slided_x = nil
  target.add_around_draw_proc(proc_name) do |canvas, x, y, w, h, simulation,
                                             next_proc|
    canvas.save_context do
      if !simulation and !effected
        slided_x ||= w
        slided_x = [slided_x - (w / 10), 0].max
        canvas.translate(slided_x, 0)
        idle_id ||= Gtk.idle_add do
          continue = !slided_x.zero?
          unless continue
            idle_id = nil
            slided_x = nil
            effected = true
          end
          canvas.activate("Redraw")
          continue
        end
      end
      next_proc.call(canvas, x, y, w, h, simulation)
    end
  end
end
