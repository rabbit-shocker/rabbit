theme_exit unless display?

proc_name = "twitter-footer"

@twitter_footer_props ||= {
  "size" => (@xx_small_font_size * 0.5).ceil,
  "font_family" => @font_family,
}
@twitter_footer_color ||= "black"
@twitter_footer_filters ||= ['twitter']
@twitter_footer_min_display_time ||= 1

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  canvas.twitter.close

  break if @twitter_footer_uninstall

  twitter_stream_tweets = []
  redraw_time = Time.now
  canvas.twitter.start_stream(@twitter_footer_filters) do |status|
    tweet = "@#{status['user']['screen_name']}: #{status['text']}"
    twitter_stream_tweets << tweet
    if Time.now - redraw_time > @twitter_footer_min_display_time
      canvas.activate("Redraw")
      redraw_time = Time.now
    end
  end

  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      unless twitter_stream_tweets.empty?
        content = twitter_stream_tweets.first
        text = Text.new(ERB::Util.h(content.strip.gsub("\n", " ")))
        twitter_stream_tweets.shift if twitter_stream_tweets.size > 1
        text.font @twitter_footer_props
        set_font_family(text)
        text.compile(canvas, x, y, w, h)
        text.layout.set_width(w * Pango::SCALE)
        text_x = x
        text_y = canvas.height - slide.margin_bottom - slide.padding_bottom
        text_y -= text.layout.pixel_size[1]
        canvas.draw_layout(text.layout, text_x, text_y, @twitter_footer_color)
      end
    end
    [x, y, w, h]
  end
end
