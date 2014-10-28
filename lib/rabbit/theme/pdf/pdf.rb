set_background("black")

match(SlideElement) do |slides|
  slides.each do |slide|
    pdf_page = slide.pdf_page

    base_width  = pdf_page.width
    base_height = pdf_page.height

    width_ratio  = canvas.width.to_f  / base_width.to_f
    height_ratio = canvas.height.to_f / base_height.to_f

    unless width_ratio == height_ratio
      if width_ratio > height_ratio
        width_margin = canvas.width - (base_width * height_ratio)
        slide.margin_left  = width_margin / 2
        slide.margin_right = width_margin / 2
      else
        height_margin = canvas.height - (base_height * width_ratio)
        slide.margin_top    = height_margin / 2
        slide.margin_bottom = height_margin / 2
      end
    end
  end
end

begin
  include_theme(canvas.title)
rescue LoadError
end

if canvas.allotted_time
  margin_left   = @margin_left
  margin_right  = @margin_right
  margin_bottom = @margin_bottom

  title_slide = canvas.slides.first
  if title_slide
    margin_left   += title_slide.margin_left
    margin_right  += title_slide.margin_right
    margin_bottom += title_slide.margin_bottom
  end

  @image_slide_number_margin_left   = margin_left
  @image_slide_number_margin_right  = margin_right
  @image_slide_number_margin_bottom = margin_bottom
  include_theme("image-slide-number")

  @image_timer_margin_left   = margin_left
  @image_timer_margin_right  = margin_right
  @image_timer_margin_bottom = margin_bottom
  include_theme("image-timer")
end

include_theme("default-comment")
