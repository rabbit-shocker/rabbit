theme_exit unless display?

proc_name = "twitter-footer"

@twitter_footer_props ||= {
  "size" => screen_size(1.4 * Pango::SCALE),
  "font_family" => @font_family,
}
@twitter_footer_color ||= "black"
@twitter_stream_filters = ['twitter']

match(Slide) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  canvas.twitter.close

  break if @twitter_footer_uninstall

  twitter_stream_tweets = []
  canvas.twitter.start_stream(@twitter_stream_filters) do |status|
    tweet = "@#{status['user']['screen_name']}: #{status['text']}"
    twitter_stream_tweets << tweet
    canvas.activate("Redraw")
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
        num_x = x
        num_y = canvas.height - 20
        canvas.draw_layout(text.layout, num_x, num_y, @twitter_footer_color)
      end
    end
    [x, y, w, h]
  end
end
