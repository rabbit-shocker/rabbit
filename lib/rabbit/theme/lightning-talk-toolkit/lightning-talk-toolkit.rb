@lightning_talk_color ||= "black"
@lightning_talk_background_color ||= "white"
@lightning_talk_contact_information ||= nil
@lightning_talk_contact_information_font_size ||= @x_small_font_size
@lightning_talk_contact_information_color ||= nil
@lightning_talk_contact_information_font_family ||= @font_family
@lightning_talk_as_large_as_possible ||= false
@lightning_talk_wrap_mode ||= Pango::Layout::WRAP_WORD
@lightning_talk_props = {
  :size => @xx_large_font_size,
  :color => @lightning_talk_color,
  :background_color => @lightning_talk_background_color,
  :contact_information => @lightning_talk_contact_information,
  :contact_information_size => @lightning_talk_contact_information_font_size,
  :contact_information_color => @lightning_talk_contact_information_color,
  :contact_information_family => @lightning_talk_contact_information_font_family,
  :as_large_as_possible => @lightning_talk_as_large_as_possible,
  :wrap_mode => @lightning_talk_wrap_mode,
}

def setup_lightning_talk_slide(slide)
  class << slide
    def lightning_talk(props={}, &block)
      if lightning_talk?
        clear_pre_draw_procs
        clear_post_draw_procs
        
        self.vertical_centering = true
        self.horizontal_centering = true
        
        lightning_talk_setup_background(props)
        lightning_talk_setup_contact_information(props)
        
        headline.lightning_talk(props)
        block.call(self, headline) if block
      end
    end
    alias takahashi lightning_talk

    def lightning_talk?
      body.empty? or
        (headline.empty? and
         body.elements.all? {|elem| elem.is_a?(Element::Image)})
    end
    alias takahashi? lightning_talk?
    
    private
    def lightning_talk_setup_background(props)
      proc_name = props[:proc_name] || "lightning-talk"
      background_color = props[:background_color] || "white"
      add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
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
    end

    def lightning_talk_setup_contact_information(props)
      proc_name = props[:proc_name] || "lightning-talk"
      contact_information = props[:contact_information]
      contact_information_size = props[:contact_information_size]
      contact_information_family = props[:contact_information_family]
      contact_information_color = props[:contact_information_color]
      
      if contact_information
        add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
          unless simulation
            text = Text.new(contact_information)
            props = {
              :size => contact_information_size,
              :family => contact_information_family,
            }
            text.font(props)
            text.align = Pango::Layout::ALIGN_RIGHT
            text.compile(canvas, x, y, w, h)
            text.layout.set_width(width * Pango::SCALE)
            text_x = margin_left
            text_y = canvas.height - margin_bottom - text.height
            canvas.draw_layout(text.layout, text_x, text_y,
                               contact_information_color)
          end
          [x, y, w, h]
        end
      end
    end
  end
end

def setup_lightning_talk_headline(head)
  class << head
    def lightning_talk(props={})
      proc_name = props[:proc_name] || "lightning-talk"
      wrap_mode = props[:wrap_mode] || Pango::Layout::WRAP_WORD
      as_large_as_possible = props[:as_large_as_possible] || false
      color = props[:color] || "black"
      
      clear_pre_draw_procs
      clear_post_draw_procs
      
      font :size => props[:size], :color => color
      self.wrap_mode = wrap_mode
      
      orig_x = orig_y = orig_w = orig_h = nil
      add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        orig_x, orig_y, orig_w, orig_h = x, y, w, h
        [x, y, w, h]
      end
      
      add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        if empty?
          [orig_x, orig_y, orig_w, orig_h]
        else
          [x, y, w, h]
        end
      end
      
      if as_large_as_possible
        lightning_talk_as_large_as_possible(props)
      end
    end
    alias takahashi lightning_talk

    private
    def lightning_talk_as_large_as_possible(props={})
      slide = parent
      proc_name = props[:proc_name] || "lightning-talk"
      wrap_mode = props[:wrap_mode] || Pango::Layout::WRAP_WORD
      
      computed = false
      add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
        if simulation and !computed
          max_width = canvas.width - slide.margin_left - slide.margin_right
          max_height = canvas.height - slide.margin_top - slide.margin_bottom
          max_width *= Pango::SCALE
          max_height *= Pango::SCALE
          size = props[:size] || @first_line_height * Pango::SCALE
          
          computed = true
          loop do
            new_size = (size * 1.05).ceil
            font :size => new_size
            text = markuped_text
            layout, text_width, text_height = canvas.make_layout(text)
            layout.width = max_width
            layout.wrap = wrap_mode
            current_width, current_height = layout.size
            if current_width > max_width or current_height > max_height
              break
            end
            size = new_size
          end
          font :size => size
        end
        [x, y, w, h]
      end
    end
  end
end

match(Slide) do |slides|
  slides.each do |slide|
    setup_lightning_talk_slide(slide)
    setup_lightning_talk_headline(slide.headline)
  end
end
