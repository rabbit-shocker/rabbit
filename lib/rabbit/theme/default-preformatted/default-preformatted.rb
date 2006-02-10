match("**", PreformattedBlock) do |blocks|
  name = "preformatted-block"
  
  blocks.horizontal_centering = true

  params = {
    :proc_name => name,
    :frame_color => @preformatted_frame_color,
    :frame_width =>  @preformatted_frame_width,
    :fill_color => @preformatted_fill_color,
  }

  padding_set(:left => @preformatted_padding_left,
              :right => @preformatted_padding_right,
              :top => @preformatted_padding_top,
              :bottom => @preformatted_padding_bottom)

  blocks.wrap_mode = false

  blocks.margin_bottom = @space
    
  draw_frame(blocks, params)
end
