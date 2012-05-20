# -*- coding: utf-8 -*-
#--------------------------------------
# font size setup
#--------------------------------------
@xxxx_large_font_size = screen_size(10 * Pango::SCALE)
@xxx_large_font_size = screen_size(8 * Pango::SCALE)
@xx_large_font_size = screen_size(6 * Pango::SCALE)
@x_large_font_size = screen_size(5 * Pango::SCALE)
@large_font_size = screen_size(4.5 * Pango::SCALE)
@normal_font_size = screen_size(3.5 * Pango::SCALE)
@small_font_size = screen_size(3.2  * Pango::SCALE)
@x_small_font_size = screen_size(3 * Pango::SCALE)
@xx_small_font_size = screen_size(2.8 * Pango::SCALE)
@xxx_small_font_size = screen_size(2.5 * Pango::SCALE)
@script_font_size = @xxx_small_font_size
@large_script_font_size = @xx_small_font_size
@x_large_script_font_size = @xsmall_font_size
#--------------------------------------
@title_slide_title_font_size = @xxx_large_font_size
#--------------------------------------
# font family setup
#--------------------------------------
@default_font ||= "Sans"
@font_family = find_font_family(@default_font)
@bold_font ||= "Sans"
@bold_font_family = find_font_family(@bold_font)
@monospace_font ||= "Monospace"
@monospace_font_family = find_font_family(@monospace_font)
#--------------------------------------
# add image path
#--------------------------------------
add_image_path("debian-images")
add_image_path("ruby-images")
add_image_path("rabbit-images")
#--------------------------------------
# set preformatted text area
#--------------------------------------
@preformatted_frame_color = "#242424"
@preformatted_frame_width = 0.5
@preformatted_fill_color  = "#fff"
@preformatted_shadow_color = "#242424"
@centering_preformatted_block = true
#--------------------------------------
# set blockqoute texts area
#--------------------------------------
@block_quote_frame_width = 1
@block_quote_title_font_size = @x_small_font_size * 0.8
@block_quote_padding_left = screen_x(2)
@block_quote_padding_right = screen_x(2)
#--------------------------------------
# set color of description term underline
#--------------------------------------
@description_term_line_color ||= "#ff9900"
#--------------------------------------
# set image caption size
#--------------------------------------
# @image_frame_width = 0.0
@image_caption_font_size = @xx_small_font_size * 0.8
#----------------------------------------
# set slide number at top-right
#----------------------------------------
@slide_number_props = {
  "size" => @xx_small_font_size * 0.8,
  "font_family" => @bold_font
}
@slide_number_position = :top
@slide_number_color = "#fff"
#--------------------------------------
# use default theme
#--------------------------------------
include_theme("default")
include_theme("title-shadow")
include_theme("image-timer")
#----------------------------------------
# change reference text style
#----------------------------------------
match("**", ReferText) do |texts|
  texts.prop_set("underline", "none")
  texts.prop_set("foreground", "blue")
end
#----------------------------------------
# change blockqoute text style
#----------------------------------------
block_quote = [Slide, Body, BlockQuote]
item_list_item = [ItemList, ItemListItem]

match(*(block_quote + (item_list_item * 1))) do |items|
  name = "block-quote-item1"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "item1.png", name)
end
#----------------------------------------
# item mark setup
#----------------------------------------
# item setup need update!!
slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]
match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  draw_image_mark(items, "item1.png", name)
end
#--------------------------------------
# set title background-image
#--------------------------------------
@title_background_image = "debian-logo-pink.png"
include_theme("title-background-image")
#--------------------------------------
# override title slide font props
#--------------------------------------
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
# set debian logo in title slide
#--------------------------------------
@title_logo = "debian-whirl.png"
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
#----------------------------------------
# per slide header/footer style settings
#----------------------------------------
@slide_header = "top.png"
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
  heads.margin_bottom = @margin_bottom * 0.5
  heads.margin_top = @margin_top
  heads.each do |head|
    headline_length = 0
    head.text.split(//u).each_with_index do |chr, idx|
      headline_length += chr.size > 1 ? 2 : 1
    end
    head.margin_top = @margin_top * 0.20 if headline_length >= 26
  end
  heads.prop_set("size", @xx_large_font_size )
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
# Footer settings
@slide_footer = "bottom.png"
@slide_banner = "debian-logo.png"
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
#----------------------------------------
# slide centering?
#----------------------------------------
@slide_centering = true
slide_body = [Slide, Body]
match(*slide_body) do |bodies|
  bodies.vertical_centering = @slide_centering
end
#----------------------------------------
# TAKAHASHI method !!
#----------------------------------------
@lightning_talk_proc_name = "lightning-debian"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")
match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.headline.margin_left = @margin_left
      slide.lightning_talk
    end
  end
end
include_theme("per-slide-background-image")

