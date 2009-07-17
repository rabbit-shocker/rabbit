include_theme("newline-in-slides")

@lightning_talk_proc_name = "lightning-blue-bar"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

@margin_top *= 1.5
@margin_bottom /= 2

match(Slide) do |slides|
  slides.each do |slide|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end

base_color = "black"
bar_gradation_light_rgb = [0xef, 0xff, 0xff].collect {|x| x / 255.0}
bar_gradation_middle_rgb = [0x3e, 0x74, 0xf4].collect {|x| x / 255.0}
bar_gradation_thick_rgb = [0x2d, 0x38, 0x8c].collect {|x| x / 255.0}

@default_item1_mark_color = base_color
@default_item1_mark_type = "circle"
@default_item2_mark_color = base_color
@default_item2_mark_type = "dash"
@default_item3_mark_color = base_color
@default_item3_mark_type = "circle"
@default_enum_item1_mark_color = base_color
@default_enum_item1_mark_type = "circle"
@default_enum_item2_mark_color = base_color
@default_enum_item2_mark_type = "dash"
@default_description_item1_mark_color = base_color
@description_term_line_color = base_color

@slide_number_uninstall = !print?
include_theme("default")

set_progress_foreground(bar_gradation_middle_rgb)
set_progress_background(bar_gradation_light_rgb)


unless print?
  @image_slide_number_image = "mini-usa-taro.png"
  @image_slide_number_show_text = true
  include_theme("image-slide-number")
  if canvas.allotted_time
    @image_timer_image = "mini-kame-taro.png"
    include_theme("image-timer")
  end
end

match(Slide, HeadLine) do |heads|
  name = "head-line"
  heads.delete_post_draw_proc_by_name(name)

  heads.margin_bottom = screen_y(5)
  heads.horizontal_centering = true
end

bar_line_width = @margin_bottom

@slide_header_info_line_width = bar_line_width
@slide_header_info_line_params = {
  :pattern => {
    :base => [0, 0, canvas.width, 0],
    :type => :linear,
    :color_stops => [
                     [0.0, *bar_gradation_light_rgb],
                     [0.5, *bar_gradation_middle_rgb],
                     [1.0, *bar_gradation_thick_rgb],
                    ],
  }
}
@slide_header_info_text_size = screen_size(1.2 * Pango::SCALE)
@slide_header_info_text_over_line = true
@slide_header_info_text_color = "white"
@slide_header_info_base_y = bar_line_width
include_theme("slide-header-info")

@slide_footer_info_line_width = bar_line_width
@slide_footer_info_line_params = {
  :pattern => {
    :base => [0, 0, canvas.width, 0],
    :type => :linear,
    :color_stops => [
                     [0.0, *bar_gradation_thick_rgb],
                     [0.5, *bar_gradation_middle_rgb],
                     [1.0, *bar_gradation_light_rgb],
                    ],
  }
}
include_theme("slide-footer-info")
