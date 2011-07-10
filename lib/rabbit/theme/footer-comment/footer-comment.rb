theme_exit unless display?

proc_name = "footer-comment"

@footer_comment_color ||= @default_foreground
@footer_comment_shadow_color ||= @default_shadow_color
@footer_comment_props ||= {
  "size" => (@x_small_font_size * 0.5).ceil,
  "font_family" => @font_family,
  "color" => @footer_comment_color,
  "shadow-color" => @footer_comment_shadow_color,
}
@footer_comment_padding ||= {
  :left => 3 * @space,
  :right => 3 * @space,
  :bottom => (@space * 0.5).ceil,
}
@footer_comment_min_display_time ||= 1
@footer_comment_keep_last_comment ||= false

match(SlideElement) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  canvas.delete_on_comment_proc_by_name(proc_name)

  break if @footer_comment_uninstall

  comments = []
  canvas.on_comment(proc_name) do |comment|
    comments << comment
    canvas.activate("Redraw")
  end

  redraw_time = Time.now
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless comments.empty?
        content = comments.first
        text = Text.new(ERB::Util.h(content.strip.gsub("\n", " ")))
        if Time.now - redraw_time > @footer_comment_min_display_time
          redraw_time = Time.now
          if @footer_comment_keep_last_comment
            comments.shift if comments.size > 1
          else
            comments.shift
          end
        end
        text.font @footer_comment_props
        text.padding_with @footer_comment_padding
        set_font_family(text)
        text.compile(canvas, x, y, w, h)

        # set horizontal
        text_x = x
        text_w = w - text.padding_left - text.padding_right
        text.compile(canvas, text_x, y, text_w, h)

        # set vertical
        text_y = canvas.height - slide.margin_bottom - slide.padding_bottom
        text_y -= text.height
        text_h = h - text.height
        text.compile(canvas, text_x, text_y, text_w, text_h)

        text.draw
      end
    end
    [x, y, w, h]
  end
end
