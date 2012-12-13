proc_name = "title-shadow"

@title_shadow_color ||= "#6f6f6fcc"

match(TitleSlide, Title) do |titles|
  titles.font("shadow-color" => @title_shadow_color)
end
