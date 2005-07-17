proc_name = "image"

if @image_with_frame.nil?
#  @image_with_frame = true
end

@image_caption_space ||= 5

@image_top_margin ||= screen_size(0)
@image_bottom_margin ||= screen_size(0)

@image_frame_color ||= "black"
@image_frame_shadow_color ||= "gray"
@image_frame_padding ||= screen_size(1)
@image_frame_shadow_width ||= 4
@image_frame_shadow_offset ||= 2
@image_frame_width = 1

match("**", Image) do |images|

  images.delete_pre_draw_proc_by_name(proc_name)
  images.delete_post_draw_proc_by_name(proc_name)
  
  images.horizontal_centering = true

  left_margin = 0
  right_margin = 0
  top_margin = @image_top_margin
  bottom_margin = @image_bottom_margin
  
  if @image_with_frame
    left_margin += @image_frame_padding + @image_frame_shadow_width
    right_margin += @image_frame_padding + @image_frame_shadow_width
    top_margin += @image_frame_padding + @image_frame_width
    bottom_margin += @image_frame_padding + @image_frame_width
    bottom_margin += @image_frame_shadow_width
  end

  images.left_margin = left_margin
  images.right_margin = right_margin
  images.top_margin = top_margin
  images.bottom_margin = bottom_margin
  
  space = screen_size(3)

  images.each do |image|
    image.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if @image_with_frame and !simulation
        # Frame
        f_sx = x - @image_frame_padding - @image_frame_width
        f_sy = y - @image_frame_padding - @image_frame_width
        f_ex = image.width
        f_ex += 2 * (@image_frame_padding + @image_frame_width)
        f_ey = image.height + 2 * (@image_frame_padding + @image_frame_width)
        canvas.draw_rectangle(false, f_sx, f_sy, f_ex, f_ey, @image_frame_color)

        # Under Shadow
        us_sx = f_sx + @image_frame_shadow_offset
        us_sy = f_sy + f_ey + @image_frame_width
        us_ex = f_ex + @image_frame_shadow_width - @image_frame_shadow_offset
        us_ey = @image_frame_shadow_width
        canvas.draw_rectangle(true, us_sx, us_sy, us_ex, us_ey,
                              @image_frame_shadow_color)
        
        # Right Shadow
        rs_sx = f_sx + f_ex + @image_frame_width
        rs_sy = f_sy + @image_frame_shadow_offset
        rs_ex = @image_frame_shadow_width
        rs_ey = f_ey + @image_frame_shadow_width - @image_frame_shadow_offset
        canvas.draw_rectangle(true, rs_sx, rs_sy, rs_ex, rs_ey,
                              @image_frame_shadow_color)
      end

      [x, y, w, h]
    end
  end

  images.each do |image|
    layout = nil
    th = 0
    image.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if image.caption and simulation
        caption = NormalText.new(image.caption)
        caption.prop_set("size", @normal_font_size)
        set_font_family(caption)
        new_w = w + image.left_margin + image.right_margin
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, new_w, h)
        end
        caption.compile(canvas, x, y, new_w, h)
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        base_x = (image.ox || x) + image.left_margin
        base_y = y + @image_caption_space
        if @image_with_frame
          base_y += @image_frame_width
          base_y += @image_frame_shadow_width + @image_frame_padding
        end
        canvas.draw_layout(layout, base_x, base_y) # dirty!!!
      end
      new_y = y + space + th
      new_h = h - space - th
      if @image_with_frame
        new_y += @image_frame_width
        new_y += @image_frame_shadow_width + @image_frame_padding
        new_h -= @image_frame_width
        new_h -= @image_frame_shadow_width + @image_frame_padding
      end
      [x, new_y, w, new_h]
    end
  end
end
