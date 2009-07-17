@block_quote_fill_color = @color_circle_background
@block_quote_frame_color = @color_circle_color
@block_quote_frame_width = 4

@block_quote_open_quote_image = @color_circle_open_quote_image
@block_quote_close_quote_image = @color_circle_close_quote_image

include_theme("default-block-quote")

block_quote = [Slide, Body, BlockQuote]

item_list_item = [ItemList, ItemListItem]

match(*(block_quote + (item_list_item * 1))) do |items|
  compute_mark_space = Proc.new do |mark_width|
    mark_width * 0.3
  end
  compute_indent_width = Proc.new do |mark_width, mark_space|
    mark_width + mark_space
  end
  setup_default_item_mark(items, "block-quote-item1", 4, 4, (3 / 4.0),
                          @color_circle_color,
                          "mark_space" => compute_mark_space,
                          "indent_width" => compute_indent_width,
                          "type" => "circle")
end
