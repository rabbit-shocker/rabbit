@block_quote_frame_color ||= "#55003dff0eff"
@block_quote_frame_width ||= 2
@block_quote_fill_color ||= "#fcfae2"

@block_quote_padding_left ||= screen_x(5)
@block_quote_padding_right ||= screen_x(5)
@block_quote_padding_top ||= screen_y(2)
@block_quote_padding_bottom ||= screen_y(2)

match("**", BlockQuote) do |blocks|
  name = "block-quote"

  params = {
    :proc_name => name,
    :frame_color => @block_quote_frame_color,
    :frame_width =>  @block_quote_frame_width,
    :fill_color => @block_quote_fill_color,
  }

  padding_set(:left => @block_quote_padding_left,
              :right => @block_quote_padding_right,
              :top => @block_quote_padding_top,
              :bottom => @block_quote_padding_bottom)

  blocks.margin_bottom = @space

  draw_frame(blocks, params)
end
