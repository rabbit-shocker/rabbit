def setup_scroll_text(text)
  class << text
    def scroll_text(option={})
      idle_id = nil
      effected = false
      slided_x = nil
      add_around_draw_proc(nil) do |canvas, x, y, w, h, simulation, next_proc|
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
  end
end

match("**", TextRenderer) do |texts|
  texts.each do |text|
    setup_scroll_text(text)
  end
end
