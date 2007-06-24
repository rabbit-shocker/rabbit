@block_quote_fill_color = @color_circle_background
@block_quote_frame_color = @color_circle_color
@block_quote_frame_width = 4

@block_quote_open_quote_image = @color_circle_open_quote_image
@block_quote_close_quote_image = @color_circle_close_quote_image

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

  args = [items, indent_width, mark_width, mark_height, name]
  draw_mark(*args) do |item, canvas, x, y, w, h|
    x -= mark_space
    canvas.draw_circle(true, x, y, w, h, @color_circle_color)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end
