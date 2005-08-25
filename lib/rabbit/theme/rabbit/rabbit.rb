set_foreground("black")
if print? and slides_per_page > 1
  set_background("white")
else
  set_background("#f500f1d0c600")
end
#set_background_image("lavie.png")

include_theme("default")

## pink base
# set_progress_foreground("#ff00da00d200")
set_progress_foreground("#fffff3f3f711")
set_progress_background("#ff00cc00ff00")
# set_progress_background("#fd05f3f3fa0b")

## green base
# set_progress_foreground("#eb29f6f6df41")
# set_progress_background("#eb29f6f6e535")

@image_with_frame = true
include_theme("image")

@title_logo_image = "lavie_with_logo.png"
include_theme("title-logo")

@headline_logo_image = "lavie.png"
include_theme("headline-logo")

@title_shadow_color = "#c09090"
include_theme("title-shadow")

@slide_number_uninstall = true
include_theme("slide-number")

@image_slide_number_show_text = true
include_theme("image-slide-number")

@powered_by_image = "rabbit_banner.png"
@powered_by_text = "Rabbit #{VERSION} and COZMIXNG"
include_theme("powered-by")

@icon_images = ["lavie_icon.png"]
include_theme("icon")


match(TitleSlide, Title) do |titles|
  titles.prop_set("foreground", "red")
  titles.prop_set("style", "italic")
end

match(Slide, HeadLine) do |heads|
  heads.horizontal_centering = true
end

slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]

# match(*slide_body) do |bodies|
#   bodies.vertical_centering = true
# end

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "red_item.png", name)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "blue_item.png", name)
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "green_item.png", name)
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item1"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "red_item2.png", name)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "green_item.png", name)
end

if windows?
  match("**") do |elems|
    elems.prop_delete("style")
  end
end
