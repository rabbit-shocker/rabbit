@preformatted_frame_color ||= "#55003dff0eff"
@preformatted_frame_width ||= 2
@preformatted_fill_color ||= "#fcfae2"
@preformatted_shadow_color ||= nil

@preformatted_padding_left ||= screen_x(5)
@preformatted_padding_right ||= screen_x(5)
@preformatted_padding_top ||= screen_y(2)
@preformatted_padding_bottom ||= screen_y(2)

match("**", PreformattedBlock) do |blocks|
  name = "preformatted-block"
  
  blocks.horizontal_centering = true

  params = {
    :proc_name => name,
    :frame_color => @preformatted_frame_color,
    :frame_width =>  @preformatted_frame_width,
    :fill_color => @preformatted_fill_color,
    :shadow_color => @preformatted_shadow_color,
  }

  padding_set(:left => @preformatted_padding_left,
              :right => @preformatted_padding_right,
              :top => @preformatted_padding_top,
              :bottom => @preformatted_padding_bottom)

  blocks.wrap_mode = false

  blocks.margin_bottom = @space

  blocks.keep_in_size

  draw_frame(blocks, params)
end
