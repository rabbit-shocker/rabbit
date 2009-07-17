format = _("%s is deprecated. Use %s instead.")
warning(format % ["auto-slide", "slide-show"])

@slide_show_span = @auto_slide_span
@slide_show_loop = @auto_slide_loop
include_theme("slide-show")
