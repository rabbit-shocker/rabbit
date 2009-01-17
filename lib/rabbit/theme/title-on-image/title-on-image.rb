@title_on_image_proc_name ||= "title-on-image-slide"
@title_on_image_color ||= "white"
@title_on_image_shadow_color ||= "#0009"
@title_on_image_background_color ||= "black"
@title_on_image_params = {
  :proc_name => @title_on_image_proc_name,
  :color => @title_on_image_color,
  :shadow_color => @title_on_image_shadow_color,
  :background_color => @title_on_image_background_color,
}

def setup_title_on_image_slide(slide)
  class << slide
    attr_accessor :applier

    attr_writer :title_on_image_default_params
    def title_on_image_default_params
      @title_on_image_default_params ||= {}
    end

    def title_on_image(params={}, &block)
      params = title_on_image_default_params.merge(params)

      clear_pre_draw_procs
      clear_post_draw_procs

      self.vertical_centering = true
      self.horizontal_centering = true

      proc_name = params[:proc_name]
      shadow_color = params[:shadow_color]
      background_color = params[:background_color]

      orig_x = orig_y = orig_w = orig_h = nil
      add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        orig_x, orig_y, orig_w, orig_h = x, y, w, h
        unless simulation
          args = [
            true,
            x - margin_left,
            y - margin_top,
            w + margin_left + margin_right,
            h + margin_top + margin_bottom,
            background_color,
          ]
          canvas.draw_rectangle(*args)
        end
        [x, y, w, h]
      end

      add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        base_h = orig_h / 2
        base_y = orig_y + base_h
        unless simulation
          shadow_headline = headline.clone
          shadow_headline.font(:color => nil)
          shadow_layout = canvas.make_layout(shadow_headline.markuped_text)
          shadow_layout.set_width(w * Pango::SCALE)
          shadow_layout.set_alignment(Pango::Layout::ALIGN_CENTER)

          font_size = headline.pixel_font_size
          move_x = @applier.screen_x(font_size.to_f / @applier.screen_size(10))
          move_y = @applier.screen_y(font_size.to_f / @applier.screen_size(20))
          canvas.draw_layout(shadow_layout, orig_x + move_x, base_y + move_y,
                             shadow_color)
        end
        headline.show do
          headline.draw_element(canvas,
                                orig_x, base_y, orig_w, base_h,
                                simulation)
        end
        [x, y, w, h]
      end

      headline.title_on_image(params)
    end

    def title_on_image?
      !headline.empty? and
        body.elements.all? {|elem| elem.is_a?(Element::Image)}
    end
  end
end

def setup_title_on_image_headline(head)
  class << head
    def title_on_image(params)
      proc_name = params[:proc_name]
      color = params[:color]

      clear_pre_draw_procs
      clear_post_draw_procs

      prop_set("foreground", color)
      hide

      orig_x = orig_y = orig_w = orig_h = nil
      add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        orig_x, orig_y, orig_w, orig_h = x, y, w, h
        [x, y, w, h]
      end

      add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        [orig_x, orig_y, orig_w, orig_h]
      end
    end
  end
end

match(Slide) do |slides|
  slides.each do |slide|
    setup_title_on_image_slide(slide)
    setup_title_on_image_headline(slide.headline)
    slide.applier = self
    slide.title_on_image_default_params = @title_on_image_params
  end
end
