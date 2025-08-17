color = "red"
fill_color = "white"
shadow_color = "#c09090"
line_width = screen_size(0.8)
line_width += 1 if (line_width % 2).zero?

include_theme("default-title-text")
include_theme("default-text")
include_theme("default-title-slide")
include_theme("default-slide")
include_theme("default-method-list")
@foot_text_block_line_color = color
@foot_text_block_line_length_ratio = 0.8
include_theme("default-foot-text")
include_theme("default-description")
@preformatted_frame_color = color
@preformatted_fill_color = fill_color
@preformatted_shadow_color = shadow_color
include_theme("default-preformatted")
@block_quote_frame_color = color
@block_quote_fill_color = fill_color
include_theme("default-block-quote")
include_theme("simple-item-mark")
include_theme("rabbit-icon")

include_theme("image")
include_theme("table")
include_theme("tag")
include_theme("newline-in-slides")

set_progress_foreground("#ffff00000000")
set_progress_background("#ffffeb29ffff")

set_graffiti_color(color)

@title_shadow_color = shadow_color
include_theme("title-shadow")

draw_rounded_frame = Proc.new do |targets, name|
  padding_left = screen_x(5)
  padding_right = screen_x(5)
  padding_top = screen_y(0)
  padding_bottom = screen_y(0)
  
  targets.padding_left = padding_left
  targets.padding_right = padding_right
  targets.padding_top = padding_top
  targets.padding_bottom = padding_bottom
  
  targets.delete_pre_draw_proc_by_name(name)
  
  targets.add_pre_draw_proc(name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      rx = x - slide.padding_left
      ry = y - slide.padding_top
      rw = canvas.width - slide.margin_right - slide.margin_left
      rh = canvas.height - slide.margin_top - slide.margin_bottom
      radius = screen_x(3)
      canvas.draw_rounded_rectangle(false, rx, ry, rw, rh, radius, color,
                                    {:line_width => line_width})
    end
    [x, y, w, h]
  end
end

match(TitleSlide) do |slides|
  draw_rounded_frame.call(slides, "title-slide")
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("foreground", color)
end

match(Slide) do |slides|
  draw_rounded_frame.call(slides, "slide")
end

match(Slide, HeadLine) do |heads|
  name = "head-line"

  heads.delete_post_draw_proc_by_name(name)

  heads.prop_set("foreground", color)
  
  space = @space / 2.0
  heads.margin_top = space * 3
  heads.margin_bottom = space * 3
  heads.add_post_draw_proc(name) do |head, canvas, x, y, w, h, simulation|
    unless simulation
      slide = head.parent
      sx = x - slide.padding_left
      sy = y + space
      ex = x + w + slide.padding_right
      ey = sy
      canvas.draw_line(sx, sy, ex, ey, color, {:line_width => line_width})
    end
    [x, y, w, h]
  end
end
