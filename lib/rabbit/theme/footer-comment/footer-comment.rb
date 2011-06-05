theme_exit unless display?

proc_name = "footer-comment"

@footer_comment_props ||= {
  "size" => (@xx_small_font_size * 0.5).ceil,
  "font_family" => @font_family,
}
@footer_comment_color ||= @default_foreground
@footer_comment_min_display_time ||= 1

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
          comments.shift if comments.size > 1
        end
        text.font @footer_comment_props
        set_font_family(text)
        text.compile(canvas, x, y, w, h)
        text.layout.set_width(w * Pango::SCALE)
        text_x = x
        text_y = canvas.height - slide.margin_bottom - slide.padding_bottom
        text_y -= text.layout.pixel_size[1]
        canvas.draw_layout(text.layout, text_x, text_y, @footer_comment_color)
      end
    end
    [x, y, w, h]
  end
end
