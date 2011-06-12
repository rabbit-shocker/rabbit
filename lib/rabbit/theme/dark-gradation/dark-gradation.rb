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
#-------------------------
# forground color
#-------------------------
@default_foreground = "#ffeeee"
set_foreground(@default_foreground)
@default_emphasis_color = "#00ffff"
@default_link_color = "#00ffff"
@slide_headline_hide = true
@default_headline_line_width = 0
#-------------------------
# slide margin
#-------------------------
@space = screen_y(1)
@margin_top = screen_x(1)
@margin_right = screen_y(1)
@margin_bottom = screen_x(1)
@margin_left = screen_y(1)
#-------------------------
# image
#-------------------------
@image_with_frame = nil
@image_caption_space = screen_y(1)
@image_caption_color = "#ffffff"
@image_frame_color = "black"
@image_frame_shadow_color = "gray"
@image_frame_padding = screen_x(1)
@image_frame_shadow_width = 4
@image_frame_shadow_offset = 2
#-------------------------
# table
#-------------------------
@table_frame_color = "#ff00ff"
@table_fill_color = "#444444"
@table_padding_left = screen_x(5) * 0
@table_padding_right = screen_x(5) * 0
@table_padding_top = screen_y(2) * 0
@table_padding_bottom = screen_y(2) * 0
@table_head_frame_color = "#ff00ff"
@table_body_frame_color = "#ff00ff"
@table_head_fill_color = "#646464"
@table_body_fill_color = nil
@table_cell_padding_left = screen_x(2)
@table_cell_padding_right = screen_x(2)
@table_cell_padding_top = screen_y(0.5)
@table_cell_padding_bottom = screen_y(0.5)
@table_header_padding_left = screen_x(2) * 0
@table_header_padding_right = screen_x(2) * 0
@table_header_padding_top = screen_y(0.5)
@table_header_padding_bottom = screen_y(0.5)
@table_caption_space = screen_y(2)
@table_caption_color = "#ffffff"
#-------------------------
# preformatted config
#-------------------------
@preformatted_frame_color = "#0ff"
@preformatted_frame_width = 1
@preformatted_fill_color  = "#040404"
@preformatted_shadow_color = nil
@centering_preformatted_block = false
#-------------------------
@default_item1_mark_color = "#ff69b4"
@default_item2_mark_color = "#836fff"
@default_item3_mark_color = "#7ccd7c"
@default_enum_item1_mark_color = "#836fff"
@default_enum_item2_mark_color = "#7ccd7c"
@default_item1_mark_type = "circle"
@default_item2_mark_type = "circle"
@default_item3_mark_type = "circle"
@default_enum_item1_mark_type = "circle"
@default_enum_item2_mark_type = "circle"
@block_quote_frame_color = "#ff5300"
@block_quote_frame_width = 1
@block_quote_fill_color = "#222"
@block_quote_title_color = "#fff"
@block_quote_title_font_size = @xx_small_font_size
@block_quote_padding_left   = screen_x(4)
@block_quote_padding_right  = screen_x(4)
@block_quote_padding_top    = screen_y(4)
@block_quote_padding_bottom = screen_y(4)

@block_quote_margin_left    = screen_x(3)
@block_quote_margin_right   = screen_x(3)
@block_quote_open_quote_image = nil
@block_quote_close_quote_image = nil
@block_quote_image_max_width = canvas.width * 0.1
#-------------------------
# include defaul theme!
#-------------------------
include_theme("default")
#-------------------------
# title shadow
#-------------------------
include_theme("title-shadow")
@title_shadow_color = "#c09090"
#-------------------------
# background images
#-------------------------
add_image_path("dark-gradation-images")
@slide_background_image ||= "background-gradation.png"
@title_background_image ||= "background-gradation.png"
# title background grad
proc_name = "title-background-image"
match(TitleSlide) do |slides|
  loader = ImageLoader.new(find_file(@title_background_image))
  resized = false
  slides.delete_pre_draw_proc_by_name(proc_name)
  slides.add_pre_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless loader.nil?
        unless resized
          loader.resize(canvas.width, canvas.height)
          resized = true
        end
        loader.draw(canvas, 0, 0)
      end
    end
    [x, y, w, h]
  end
end
#---------------------------
# slide number props
#---------------------------
@slide_number_props = {
  "size" => @xx_small_font_size * 0.8,
  "font_family" => @font_family,
}
#-------------------------
# overwrite default settings
#-------------------------
match(TitleSlide, "*") do |texts|
  texts.prop_set("style", "normal")
end
match(TitleSlide, Title) do |titles|
  titles.margin_bottom = @space * 2
end

match("**", ReferText) do |texts|
  texts.prop_set("underline", "single")
  texts.prop_set("foreground", @default_link_color)
end

match("**", HeadLine) do |heads|
  heads.horizontal_centering = true
  heads.prop_set("size", @large_font_size * 1.25)
  heads.margin_bottom = @space
end

match("**", Emphasis) do |texts|
  texts.prop_set("foreground", @default_emphasis_color)
end

slide_body = [Slide, Body]
match(*slide_body) do |bodies|
  bodies.vertical_centering = true
end

# I don't like blodize, it's bit dirty...
match(Slide, "**") do |elems|
  elems.prop_set("weight", "normal")
end
#-------------------------
# lightning talk setting(Takahashi method)
#-------------------------
@lightning_talk_proc_name = "lightning-dark-gradation"
@lightning_talk_as_large_as_possible = true
@lightning_talk_color = "#ffffffffffff"
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end
# set background-image in slide using Takahashi method
include_theme("slide-background-image") # 2nd!
