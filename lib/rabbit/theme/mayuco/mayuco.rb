add_image_path("mayuco-images")

green_background_color = "#639943"
green_foreground_color = "#ffffff"

@title_slide_title_font_size = @large_font_size
@title_slide_subtitle_font_size = @x_small_font_size

include_theme("default")

match(TitleSlide) do |slides|
  slides.vertical_centering = false

  slides.margin_top = canvas.height * 0.1

  kinotan_4 = Image.new(find_file("kinotan-4.svg"),
                        :height => canvas.height * 0.25)
  slides.add_pre_draw_proc do |slide, canvas, x, y, w, h, simulation|
    if simulation
      ix = 0
      iy = canvas.height * 0.45
      iw = canvas.width
      ih = canvas.height - iy
      kinotan_4.compile(canvas, ix, iy, iw, ih)
      kinotan_4.do_horizontal_centering(canvas, ix, iy, iw, ih)
    else
      canvas.draw_rectangle(true, 0, 0, canvas.width, canvas.height,
                            green_background_color)
    end
    kinotan_4.draw(simulation)
    [x, y, w, h]
  end
end

match(TitleSlide, "*") do |elems|
  elems.horizontal_centering = false
  elems.prop_set("foreground", green_foreground_color)
end

match(TitleSlide, Title) do |titles|
  titles.spacing = canvas.height * 0.075
end

match(TitleSlide, Subtitle) do |subtitles|
  subtitles.align = :right

  subtitles.add_pre_draw_proc do |subtitle, canvas, x, y, w, h, simulation|
    y = canvas.height * 0.3
    h = canvas.height - y
    [x, y, w, h]
  end
end

match(TitleSlide, Author) do |authors|
  authors.align = :right
  authors.prop_set("size", @small_font_size)

  authors.add_pre_draw_proc do |author, canvas, x, y, w, h, simulation|
    y = canvas.height * 0.8
    h = canvas.height - y
    [x, y, w, h]
  end
end

match(TitleSlide, Institution) do |authors|
  authors.prop_set("size", @small_font_size)

  authors.add_pre_draw_proc do |author, canvas, x, y, w, h, simulation|
    y = canvas.height * 0.8
    h = canvas.height - y
    [x, y, w, h]
  end
end
