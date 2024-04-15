add_image_path("rubykaigi2024-images")

# puts font_families.sort
@xxx_large_font_size = screen_size(8 * Pango::SCALE)
@xx_large_font_size = screen_size(7 * Pango::SCALE)
@x_large_font_size = screen_size(6 * Pango::SCALE)
@large_font_size = screen_size(5 * Pango::SCALE)
@normal_font_size = screen_size(4 * Pango::SCALE)
@small_font_size = screen_size(3 * Pango::SCALE)
@x_small_font_size = screen_size(2.5 * Pango::SCALE)
@xx_small_font_size = screen_size(2 * Pango::SCALE)

@font_family = find_font_family('Poppins')
@monospace_font_family = 'Courier Prime'
@title_font_family = find_font_family('Dela Gothic One')

# colors
yellow       = "#ffeb00"
light_yellow = "#fffef5"
red          = "#ff3f46"
pink         = "#ffc2de"
blue         = "#2994ff"
sky_blue     = "#98ffff"
green        = "#80f142"
yellow_green = "#ccff00"
black        = "#000000"
white        = "#ffffff"

@default_headline_line_color = red
@default_headline_line_width = 2
@default_headline_line_expand = true

@default_emphasis_color = red
@default_emphasis_level2_color = "#ffffff"

set_graffiti_color red
set_graffiti_line_width 5

@space = screen_y(1)

@image_slide_number_start_flag_color = blue
@image_slide_number_goal_flag_color = red

@preformatted_fill_color = light_yellow
@preformatted_frame_color = sky_blue
@preformatted_frame_width = 3
@slide_background_image = 'slide_background.png'
include_theme("slide-background-image")

@title_slide_background_image = 'title_background.png'
include_theme("title-slide-background-image")

include_theme('default')

match TitleSlide do |slides|
  slides.margin_left = 850
  slides.margin_right = 50
  slides.prop_set("foreground", black)
  slides.prop_set "style", "normal"
end
match TitleSlide, Title do |title|
  title.margin_top = -10
  title.prop_set "size", @xxx_large_font_size
  title.prop_set "font-family", @title_font_family
  title.prop_set "weight", "normal"
  title.prop_set "foreground", red
end
match TitleSlide, Subtitle do |subtitle|
  subtitle.margin_top = 50
  subtitle.prop_set "size", @large_font_size
  subtitle.prop_set "font-family", @title_font_family
  subtitle.prop_set "weight", "normal"
  subtitle.prop_set "foreground", blue
end
match TitleSlide, Author do |author|
  author.margin_top = 50
  author.prop_set "size", @normal_font_size
  author.prop_set "font-family", @title_font_family
  author.prop_set "weight", "normal"
  author.prop_set "foreground", black
end
match TitleSlide, Institution do |i|
  i.prop_set "size", @small_font_size
end

match(Slide, HeadLine) do |heads|
  heads.margin_top = 0
  heads.prop_set "size", @large_font_size
  set_font_family(heads)
end

match(Slide) do |slides|
  slides.margin_left = 50
  slides.margin_right = 50
  slides.prop_set("foreground", black)
end

@item_image_1 = 'bullet-point-36.png'
@item_image_2 = 'bullet-point-32.png'
@item_image_3 = 'bullet-point-28.png'

include_theme("default-item-mark")

slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]

indent = 50

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, @item_image_1, name, indent: indent)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, @item_image_2, name, indent: indent)
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, @item_image_3, name, indent: indent)
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item1"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, @item_image_1, name, indent: indent)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, @item_image_2, name, indent: indent)
end

# Chapter
match Slide do |slides|
  slides.each do |slide|
    if slide.match?(/sub_chapter/)
      slide.horizontal_centering = true
    elsif slide.match?(/chapter/)
      set_font_family slide, @title_font_family
      slide.horizontal_centering = true
    elsif slide.match?(/cite/)
      slide.prop_set "foreground", black
      slide.horizontal_centering = true
    end
  end
end

match("**", PreformattedBlock) do |block|
  block.prop_set("foreground", black)
end

