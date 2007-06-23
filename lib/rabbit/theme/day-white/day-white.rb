add_image_path("ruby-images")
include_theme("image")
include_theme("table")
include_theme("default-icon")
include_theme("default-title-text")
include_theme("default-text")
include_theme("default-title-slide")
include_theme("default-slide")
include_theme("default-item-mark")
include_theme("default-method-list")
include_theme("default-preformatted")
include_theme("default-block-quote")
include_theme("default-foot-text")
include_theme("default-description")

set_foreground("#000000")
set_background("#ffffff")

@image_slide_number_show_text = true
include_theme("image-slide-number")
include_theme("image-timer")

slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"

  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "#a00000"

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  args = [items, indent_width, mark_width, mark_height, name]
  draw_mark(*args) do |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (3 / 4.0)
  margin_with(:bottom => space)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"

  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "#a00000"

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  args = [items, indent_width, mark_width, mark_height, name]
  draw_mark(*args) do |item, canvas, x, y, w, h|
    x -= mark_width
    canvas.draw_circle(true, x, y, w, h, color)
  end

  space = @space * (2 / 4.0)
  margin_with(:bottom => space)
end

include_theme("windows-adjust")
