add_image_path("rubykaigi2011-images")

line_color = "#999999"
dark_color = "#160F0D"
fill_color = "#ffffff4d"

@foreground = dark_color

@default_headline_line_color ||= line_color
@description_term_line_color ||= line_color

@default_description_item1_mark_color ||= dark_color
@default_block_quote_item1_mark_color ||= dark_color

@preformatted_frame_color ||= line_color
@preformatted_fill_color ||= fill_color

@block_quote_frame_color ||= line_color
@block_quote_fill_color ||= fill_color

@table_frame_color = line_color
@table_fill_color = fill_color
@table_head_frame_color = line_color
@table_head_fill_color = dark_color
@table_header_font_props[:color] = "white"

@image_slide_number_start_flag_color = dark_color
@image_slide_number_goal_flag_color = dark_color

include_theme("default")

@lightning_talk_proc_name = "lightning-rabbit"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end

background_image = "rubykaigi2011-background-white.jpg"
@title_slide_background_image = background_image
@slide_background_image = background_image
include_theme("title-slide-background-image")
include_theme("slide-background-image")
