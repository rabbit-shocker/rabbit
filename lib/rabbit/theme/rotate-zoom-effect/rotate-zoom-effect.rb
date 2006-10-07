def setup_rotate_zoom_effect_slide(slide)
  class << slide
    def rotate_zoom_effect
      angle = 0
      last_angle = 2 * Math::PI
      scale_x = 0
      scale_y = 0
      last_scale = 1
      idle_id = nil
      effected = false
      add_pre_draw_proc(nil) do |canvas, x, y, w, h, simulation|
        if !simulation and !effected
          angle = [angle + 0.05 * Math::PI, last_angle].min
          scale_x = [scale_x + rand * 0.05, last_scale].min
          scale_y = [scale_y + rand * 0.05, last_scale].min
          translate(canvas, angle, scale_x, scale_y)
          idle_id ||= Gtk.idle_add do
            continue = ([angle, scale_x, scale_y] !=
                        [last_angle, last_scale, last_scale] and
                        canvas.current_slide == self)
            unless continue
              translate(canvas, angle, scale_x, scale_y)
              idle_id = nil
              effected = true
            end
            canvas.activate("Redraw")
            continue
          end
        end
        [x, y, w, h]
      end
    end

    def translate(canvas, angle, scale_x, scale_y)
      canvas.reset_matrix
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
