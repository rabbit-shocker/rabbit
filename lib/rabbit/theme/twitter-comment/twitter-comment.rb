theme_exit unless display?

@twitter_footer_color ||= @foreground
if !defined?(@twitter_footer_filters) or @twitter_footer_filters.nil?
  theme_exit("must specify @twitter_footer_filters!! as " +
             "array of keyword: ['keyword1', 'keyword1', ...])")
end

canvas.twitter.close
theme_exit if @twitter_footer_uninstall

canvas.twitter.start(@twitter_footer_filters) do |status|
  tweet = "@#{status['user']['screen_name']}: #{status['text']}"
  canvas.append_comment(tweet)
end
