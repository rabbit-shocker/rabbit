# -*- coding: utf-8 -*-

@font_family =
  find_font_family("モトヤLマルベリ3等幅") ||
  find_font_family("ＤＦＰ中丸ゴシック体") ||
  @font_family

rroonga_color = "#ff5151"
ranguba_color = "#f57900"
background_color = "#ffffff"

set_graffiti_color("#{rroonga_color}99")
set_graffiti_line_width(30)

set_progress_foreground(ranguba_color)
set_progress_background(background_color)

@title_slide_title_font_size = @x_large_font_size * 1.3
@title_slide_content_source_font_size = @x_small_font_size
@title_slide_date_font_size = @x_small_font_size

@default_emphasis_color = rroonga_color

add_image_path("ranguba-images")

@block_quote_image_background_alpha = 0.3

bar_line_width = @margin_bottom
bar = ImageLoader.new(find_file("ranguba-bar.png"))
bar.keep_ratio = true
bar.resize(bar.width, bar_line_width)
bar_pixbuf = bar.pixbuf.scale(bar.width, bar.height)
@default_headline_line_width = bar_line_width
@default_headline_line_params = Proc.new do |headline, canvas, x, y, w, h|
  {
    :line_cap => :square,
    :pattern => {
      :type => :pixbuf,
      :pixbuf => bar_pixbuf,
      :extend => :repeat,
      :transformations => [
                           [:translate, -x, -(y + bar.height / 2)],
                          ],
    },
  }
end
@default_headline_line_expand = true

@description_term_line_color = ranguba_color
@default_description_item1_mark_color = ranguba_color
@default_block_quote_item1_mark_color = ranguba_color

@slide_number_uninstall = !print?

include_theme("default")

@icon_images = ["ranguba-mark.png"]
include_theme("icon")

@slide_logo_image = "ranguba-mark.png"
@slide_logo_position = Proc.new do |slide, canvas|
  [slide.margin_left, slide.margin_top]
end
@slide_logo_height = Proc.new do |slide, canvas|
  slide[0].first_line_height
end
include_theme("slide-logo")

unless print?
  @image_slide_number_image ||= "mini-usa-taro.png"
  @image_slide_number_show_text = true
  include_theme("image-slide-number")
  if canvas.allotted_time
    @image_timer_image ||= "mini-kame-taro.png"
    include_theme("image-timer")
  end
end

@slide_footer_info_left_text ||= canvas.title.gsub(/\n+/, ' ')
@slide_footer_info_right_text ||= "Powered by Rabbit #{Rabbit::VERSION}"
include_theme("slide-footer-info")

match(TitleSlide, Author) do |authors|
  authors.horizontal_centering = false
  authors.align = :left

  authors.add_post_draw_proc do |author, canvas, x, y, w, h, simulation|
    cancel_height = author.height + author.margin_bottom
    [x, y - cancel_height, w, h + cancel_height]
  end
end

match(TitleSlide, Institution) do |institutions|
  institutions.horizontal_centering = false
  institutions.align = :right
end

match(TitleSlide, Date) do |dates|
  dates.horizontal_centering = false
  dates.align = :right
end

match(TitleSlide, ContentSource) do |sources|
  sources.horizontal_centering = false
  sources.align = :right

  sources.margin_top = @space
  sources.margin_bottom = 0
end

match(Slide, HeadLine) do |heads|
  heads.horizontal_centering = true
end

match(Slide, Body) do |bodies|
  bodies.vertical_centering = true
  bodies.each do |body|
    next if body.elements.all? {|element| element.is_a?(Image)}
    next if body.elements.any? {|element| element.is_a?(BlockQuote)}
    next if body.elements.any? {|element| element.is_a?(PreformattedBlock)}
    next if body.elements.any? {|element| element.is_a?(Table)}
    body.margin_left = canvas.width * 0.01
    body.margin_right = canvas.width * 0.01
  end
end

slide_body = [Slide, Body]
item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "ranguba-item-mark.png", name)
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "ranguba-item-mark-mini.png", name)
end

# match(*(slide_body + (item_list_item * 3))) do |items|
#   name = "item3"

#   items.delete_pre_draw_proc_by_name(name)
#   items.delete_post_draw_proc_by_name(name)

#   draw_image_mark(items, "green-item.png", name)
# end

# enum_list_item = [EnumList, EnumListItem]

# match(*(slide_body + enum_list_item + item_list_item)) do |items|
#   name = "enum-item1"
  
#   items.delete_pre_draw_proc_by_name(name)
#   items.delete_post_draw_proc_by_name(name)

#   draw_image_mark(items, "red-item2.png", name)
# end

# match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
#   name = "enum-item2"
  
#   items.delete_pre_draw_proc_by_name(name)
#   items.delete_post_draw_proc_by_name(name)

#   draw_image_mark(items, "green-item.png", name)
# end

desc_list_content = [DescriptionList, DescriptionListItem, DescriptionContent]

match(*(slide_body + desc_list_content + item_list_item)) do |items|
  name = "description-item1"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "ranguba-item-mark.png", name,
                  :indent => Proc.new {|item, loader| loader.width})
end

match(*(slide_body + desc_list_content + item_list_item * 2)) do |items|
  name = "description-item2"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)

  draw_image_mark(items, "ranguba-item-mark-mini.png", name,
                  :indent => Proc.new {|item, loader| loader.width})
end

@lightning_talk_proc_name = "lightning-clear-code"
@lightning_talk_as_large_as_possible = true
include_theme("lightning-talk-toolkit")

match(Slide) do |slides|
  slides.each_with_index do |slide, i|
    if slide.lightning_talk?
      slide.lightning_talk
    end
  end
end

# match(Slide, Body) do |bodies|
#   bodies.each do |body|
#     if body.elements.collect {|element| element.class} == [Paragraph]
#       body.elements.each do |element|
#         element.horizontal_centering = true
#         if element.text.size > 50
#           element.prop_set("size", @large_font_size)
#         else
#           element.prop_set("size", @x_large_font_size)
#         end
#         # element.as_large_as_possible("one-paragraph")
#       end
#     end
#     body.margin_left *= 0.7
#     body.margin_right *= 0.7
#   end
# end

# match("**", Table, TableBody, TableRow, TableCell) do |cells|
#   cells.horizontal_centering = true
# end
