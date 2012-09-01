@title_slide_font_size ||= @large_font_size
@title_slide_title_font_size ||= @x_large_font_size
@title_slide_subtitle_font_size ||= @normal_font_size
@title_slide_content_source_font_size ||= @small_font_size
@title_slide_institution_font_size ||= @normal_font_size
@title_slide_place_font_size ||= @normal_font_size
@title_slide_date_font_size ||= @small_font_size
@title_slide_note_font_size ||= @small_font_size

match(TitleSlide, "*") do |elems|
  elems.horizontal_centering = true
  elems.prop_set("size", @title_slide_font_size)
  set_font_family(elems)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("size", @title_slide_title_font_size)
  titles.prop_set("weight", "heavy")
end

match(TitleSlide, Subtitle) do |titles|
  titles.prop_set("size", @title_slide_subtitle_font_size)
end

match(TitleSlide, ContentSource) do |sources|
  sources.prop_set("size", @title_slide_content_source_font_size)
  sources.prop_set("style", "italic")
end

match(TitleSlide, Institution) do |institutions|
  institutions.prop_set("size", @title_slide_institution_font_size)
  institutions.prop_set("style", "italic")

  institutions.margin_bottom = @space
end

match(TitleSlide, Place) do |places|
  places.prop_set("size", @title_slide_place_font_size)
end

match(TitleSlide, Date) do |dates|
  dates.prop_set("size", @title_slide_date_font_size)
  dates.prop_set("style", "italic")
end

match(TitleSlide, "**", Note) do |texts|
  texts.prop_set("size", @title_slide_note_font_size)
end
