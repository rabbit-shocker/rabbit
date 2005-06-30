proc_name = "image"

if @image_with_frame.nil?
#  @image_with_frame = true
end

@image_caption_space ||= 5

@image_frame_color ||= "black"
@image_frame_shadow_color ||= "gray"
@image_frame_padding ||= 3
@image_frame_shadow_width ||= 4
@image_frame_shadow_offset ||= 2
@image_frame_width = 1

match("**", Image) do |images|

  images.delete_pre_draw_proc_by_name(proc_name)
  images.delete_post_draw_proc_by_name(proc_name)
  
  images.horizontal_centering = true

  space = screen_size(3)

  make_normalized_size = Proc.new do |size|
    size && screen_size(size)
  end

  make_relative_size = Proc.new do |size, parent_size|
    size && parent_size && ((size / 100.0) * parent_size).ceil
  end

  images.each do |image|
    image.add_pre_draw_proc(proc_name) do |canvas, x, y, w, h, simulation|
      nw = make_normalized_size.call(image.normalized_width)
      nh = make_normalized_size.call(image.normalized_height)
      rw = make_relative_size.call(image.relative_width, w)
      rh = make_relative_size.call(image.relative_height, h)
      iw = nw || rw
      ih = nh || rh
      image.resize(iw, ih)

      if @image_with_frame and !simulation
        # Frame
        f_sx = x - @image_frame_padding
        f_sy = y - @image_frame_padding
        f_ex = image.width + 2 * @image_frame_padding
        f_ey = image.height + 2 * @image_frame_padding
        canvas.draw_rectangle(false, f_sx, f_sy, f_ex, f_ey, "black")

        # Under Shadow
        us_sx = f_sx + @image_frame_shadow_offset
        us_sy = f_sy + f_ey + @image_frame_width
        us_ex = f_ex + @image_frame_shadow_width - @image_frame_shadow_offset
        us_ey = @image_frame_shadow_width
        canvas.draw_rectangle(true, us_sx, us_sy, us_ex, us_ey, "gray")
        
        # Right Shadow
        rs_sx = f_sx + f_ex + @image_frame_width
        rs_sy = f_sy + @image_frame_shadow_offset
        rs_ex = @image_frame_shadow_width
        rs_ey = f_ey + @image_frame_shadow_width - @image_frame_shadow_offset
        canvas.draw_rectangle(true, rs_sx, rs_sy, rs_ex, rs_ey, "gray")
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
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, w, h)
        end
        caption.compile(canvas, x, y, w, h)
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        base_y = y + @image_caption_space
        if @image_with_frame
          base_y += @image_frame_width
          base_y += @image_frame_shadow_width + @image_frame_padding
        end
        canvas.draw_layout(layout, image.ox || x, base_y) # dirty!!!
      end
      [x, y + space + th, w, h - space - th]
    end
  end
end
