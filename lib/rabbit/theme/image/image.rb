proc_name = "image"

@image_frame_width = 1

match("**", Image) do |images|

  images.delete_pre_draw_proc_by_name(proc_name)
  images.delete_post_draw_proc_by_name(proc_name)
  
  images.horizontal_centering = true

  params = {
    :proc_name => proc_name,
    :frame_color => @image_frame_color,
    :shadow_color => @image_frame_shadow_color,
    :shadow_width => @image_frame_shadow_width,
    :shadow_offset => @image_frame_shadow_offset,
  }
  
  left_padding = 0
  right_padding = 0
  top_padding = 0
  bottom_padding = 0
  
  if @image_with_frame
    left_padding += @image_frame_padding + @image_frame_shadow_width
    right_padding += @image_frame_padding + @image_frame_shadow_width
    top_padding += @image_frame_padding + @image_frame_width
    bottom_padding += @image_frame_padding + @image_frame_width
    bottom_padding += @image_frame_shadow_width
  end

  images.left_padding = left_padding
  images.right_padding = right_padding
  images.top_padding = top_padding
  images.bottom_padding = bottom_padding

  draw_frame(images, params) if @image_with_frame
  
  images.each do |image|
    layout = nil
    th = 0
    image.add_post_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      if image.caption and simulation
        caption = NormalText.new(image.caption)
        caption.prop_set("size", @normal_font_size)
        set_font_family(caption)
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, w, h)
        end
        caption.compile(canvas, x, y, w, h)
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        base_x = image.ox || x
        base_y = y + image.bottom_padding + @image_caption_space
        canvas.draw_layout(layout, base_x, base_y)
      end
      new_y = y + @space + th
      new_h = h - @space - th
      [x, new_y, w, new_h]
    end
  end
end
