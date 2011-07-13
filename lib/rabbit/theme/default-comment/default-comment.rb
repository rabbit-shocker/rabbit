if canvas.comment_theme
  include_theme(canvas.comment_theme)
else
  if display? and canvas.renderer.widget.class.name == "Clutter::GtkEmbed"
    include_theme("clutter-comment")
  else
    include_theme("footer-comment")
  end
end
