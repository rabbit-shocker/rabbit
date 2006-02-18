@block_quote_open_quote_image = "open-quote-brown.png"
@block_quote_close_quote_image = "close-quote-brown.png"

include_theme("default-block-quote")

block_quote = [Slide, Body, BlockQuote]
item_list_item = [ItemList, ItemListItem]

match(*(block_quote + (item_list_item * 1))) do |items|
  name = "block-quote-item1"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "red-item.png", name)
end

