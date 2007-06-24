add_image_path("rabbit-images")

include_theme("blue-circle-config")

@block_quote_fill_color = "white"
@block_quote_frame_color = @blue_circle_blue
@block_quote_frame_width = 4

@block_quote_open_quote_image = "open-quote-blue.png"
@block_quote_close_quote_image = "close-quote-blue.png"

include_theme("default-block-quote")

block_quote = [Slide, Body, BlockQuote]

item_list_item = [ItemList, ItemListItem]

match(*(block_quote + (item_list_item * 1))) do |items|
  name = "block-quote-item1"

  mark_width = screen_x(4)
  mark_height = screen_y(4)
  mark_space = mark_width * 0.3
  indent_width = mark_width + mark_space

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, x, y, w, h|
    x -= mark_space
    canvas.draw_circle(true, x, y, w, h, @blue_circle_blue)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end
