set_background("black")

match(SlideElement) do |slides|
  slides.each do |slide|
    pdf_page = slide.pdf_page

    base_width  = pdf_page.width
    base_height = pdf_page.height
    ratio = base_width.to_f / base_height.to_f
    size = Size.new(canvas.width, canvas.height, ratio)

    slide.margin_left = size.logical_margin_left
    slide.margin_right = size.logical_margin_right
    slide.margin_top = size.logical_margin_top
    slide.margin_bottom = size.logical_margin_bottom
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
