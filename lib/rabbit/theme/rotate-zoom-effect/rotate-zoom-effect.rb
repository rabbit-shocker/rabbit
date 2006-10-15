def setup_rotate_zoom_effect_slide(slide)
  class << slide
    def rotate_zoom_effect
      angle = 0
      last_angle = 2 * Math::PI
      last_scale_x = 1
      last_scale_y = 1
      idle_id = nil
      effected = false
      add_around_draw_proc(nil) do |canvas, x, y, w, h, simulation, next_proc|
        canvas.save_context do
          if !simulation and !effected
            angle = [angle + 0.05 * Math::PI, last_angle].min
            ratio = angle / last_angle
            scale_x = last_scale_x * ratio
            scale_y = last_scale_y * ratio
            translate(canvas, angle, scale_x, scale_y)
            idle_id ||= Gtk.idle_add do
              continue = (angle != last_angle and
                          canvas.current_slide == self)
              unless continue
                translate(canvas, last_angle, last_scale_x, last_scale_y)
                idle_id = nil
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

    private
    def translate(canvas, angle, scale_x, scale_y)
      center_x = canvas.width / 2
      center_y = canvas.height / 2
      canvas.translate(center_x, center_y)
      canvas.rotate(angle)
      canvas.translate(-center_x, -center_y)
      canvas.scale(scale_x, scale_y)
    end
  end
end


match(Slide) do |slides|
  slides.each do |slide|
    setup_rotate_zoom_effect_slide(slide)
  end
end
