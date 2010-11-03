# #--------------------------------------
# # font size setup
# #--------------------------------------
# @xxxx_large_font_size = screen_size(6.5 * Pango::SCALE)
# @xxx_large_font_size = screen_size(5 * Pango::SCALE)
# @xx_large_font_size = screen_size(4.5.* Pango::SCALE)
# @x_large_font_size = screen_size(4 * Pango::SCALE)
# @large_font_size = screen_size(3.5 * Pango::SCALE)
# @normal_font_size = screen_size(3 * Pango::SCALE)
# @small_font_size = screen_size(2.8  * Pango::SCALE)
# @x_small_font_size = screen_size(2.5 * Pango::SCALE)
# @xx_small_font_size = screen_size(2.2 * Pango::SCALE)
# @xxx_small_font_size = screen_size(2 * Pango::SCALE)
# @script_font_size = @xxx_small_font_size
# @large_script_font_size = @xx_small_font_size
# @x_large_script_font_size = @xsmall_font_size
# #--------------------------------------
# @title_slide_font_size = @xx_large_font_size
# @title_slide_title_font_size = @xxxx_large_font_size 
# @title_slide_subtitle_font_size = @normal_font_size 
# @title_slide_author_font_size = @xx_large_font_size 
# @title_slide_content_source_font_size = @_small_font_size
# @title_slide_institution_font_size = @small_font_size
# @title_slide_place_font_size = @xx_small_font_size
# @title_slide_date_font_size = @xx_small_font_size
# @title_slide_note_font_size = @xxx_small_font_size
#--------------------------------------
# font family setup
#--------------------------------------
# using font name in `fc-list -v`
@default_font = "Sans" 
@font_family = find_font_family(@default_font)
@bold_font = "Sans" 
@bold_font_family = find_font_family(@bold_font)
@monospace_font = "Monospace"
@monospace_font_family = find_font_family(@monospace_font)

#--------------------------------------
# using default
#--------------------------------------
add_image_path("debian-images")
#include_theme("default")
# not need slide-number...
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
include_theme("image")
include_theme("table")
include_theme("newline-in-slides")
include_theme("per-slide-background-color")
include_theme("background-image-toolkit")
include_theme("per-slide-background-image")
include_theme("body-background-image")
include_theme("tag")

@preformatted_frame_color = "#242424"
@preformatted_frame_width = 0.5
@preformatted_fill_color  = "#fff"
@preformatted_shadow_color = "#242424"
@centering_preformatted_block = true
@description_term_line_color ||= "#ff9900"


#--------------------------------------
# title style changed
#--------------------------------------
@title_background_image = "debian-logo-pink.png"
include_theme("title-background-image")
match(TitleSlide) do |contents|
 contents.prop_set("style", "normal")
end
match(TitleSlide, ContentSource) do |sources|
  sources.margin_bottom = 0
end
match(TitleSlide, Date) do |dates|
  dates.margin_top = @space 
end
match(TitleSlide, Place) do |places|
end
#--------------------------------------
# title logo position changed
#--------------------------------------
@title_logo ||= "debian-whirl.png"
logo_loader = ImageLoader.new(find_file(@title_logo))
title_logo = Proc.new do |slide, canvas, x, y, w, h, simulation|
  unless simulation
    draw_x = canvas.width - logo_loader.width
    draw_y = canvas.height - logo_loader.height
    logo_loader.draw(canvas, draw_x, draw_y)
  end
  [x, y, w, h]
end
match(TitleSlide) do |slides|
  name2 = "title-logo"
  slides.delete_pre_draw_proc_by_name(name2)
  slides.add_pre_draw_proc(name2, &title_logo)
end

## header/footer settings
# image loader
@slide_header ||= "top.png"
@slide_footer ||= "bottom.png"
@slide_banner ||= "debian-logo.png"
# Headline Settings
loader_head = ImageLoader.new(find_file(@slide_header))
match(Slide, HeadLine) do |heads|
  heads.each do |head|
    head.add_pre_draw_proc("header_bg") do |canvas, x, y, w, h, simulation|
      unless simulation
        loader_head.resize(canvas.width, nil)
        loader_head.draw(canvas, 
          x + w - canvas.width + @margin_left + @margin_right, 0)
      end
      [x, y, w, h]
    end
  end
  name = "head-line"
  delete_post_draw_proc_by_name(name)
  heads.margin_left = (canvas.width + @margin_left + @margin_right)/6.0
  heads.margin_right = @margin_right
  heads.margin_top = @margin_top * 1.3
  heads.prop_set("size", @xx_large_font_size)
  heads.prop_set("foreground", "#fff")
  loader_head.resize(canvas.width, nil)
  static_y = loader_head.height - @margin_top
  original_h = nil
  heads.add_pre_draw_proc do |head, canvas, x, y, w, h, simulation|
    original_h = h
    [x, y, w, h]
  end
  heads.add_post_draw_proc do |head, canvas, x, y, w, h, simulation|
    [x, static_y, w, original_h - static_y]
  end
end
# Slide number settings
@slide_number_props = {
  "size" => @x_small_font_size,
  "font_family" => @bold_font
}
@slide_number_color = "#fff"
match(Slide) do |slides|
  break if @slide_number_uninstall
  unless @not_use_slide_number
    slides.add_post_draw_proc("slide_number") do |slide, canvas, x, y, w, h, simulation|
      unless simulation
        text = Text.new("#{canvas.current_index}/#{canvas.slide_size - 1}")
        text.font @slide_number_props
        text.align = Pango::Layout::ALIGN_RIGHT
        text.compile(canvas, x, y, w, h)
        layout = text.layout
        layout.set_width(w * Pango::SCALE)
        num_y = @margin_top
        canvas.draw_layout(text.layout, x, num_y, @slide_number_color)
      end
    [x, y, w, h]
    end
  end
end
# Footer settings
loader_foot = ImageLoader.new(find_file(@slide_footer))
loader_banner = ImageLoader.new(find_file(@slide_banner))
match(Slide) do |slides|
  slides.each do |slide|
    slide.add_post_draw_proc("footer") do |canvas, x, y, w, h, simulation|
      unless simulation
        footer_height = canvas.height/26.0 + screen_y(1) * 0.2
        loader_foot.resize(canvas.width, footer_height)
        loader_foot.draw(canvas, 0, canvas.height - footer_height)
        banner_height = canvas.height/26.0
        loader_banner.resize(nil, banner_height)
        banner_pos_x = canvas.width - loader_banner.width 
        banner_pos_y = canvas.height - loader_banner.height - footer_height
        loader_banner.draw(canvas, banner_pos_x, banner_pos_y)
      end
      [x, y, w, h]
    end
  end
end

# item setup need update!!
slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]
match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end

enum_list_item = [EnumList, EnumListItem]
match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item1"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end

# text changed
match("**", ReferText) do |texts|
  texts.prop_set("underline", "none")
  texts.prop_set("foreground", "blue")
end

# 高橋メソッド
@lightning_talk_proc_name = "lightning-debian"
@lightning_talk_as_large_as_possible = true
include_theme("title-on-image-toolkit")
include_theme("lightning-talk-toolkit")
match(Slide) do |slides|
  slides.each do |slide|
    slide.title_on_image if slide.title_on_image?
  end
end
match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      # reset header.margin_left
      slide.headline.margin_left = @margin_left
      slide.lightning_talk
    end
  end
end


