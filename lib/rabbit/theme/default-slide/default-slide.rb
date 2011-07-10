@default_headline_line_color ||= "#ff9933"
@default_headline_line_params ||= {}
@default_headline_line_width ||= 1
unless defined?(@default_headline_line_expand)
  @default_headline_line_expand = false
end

match(Slide) do |slides|
  slides.each do |slide|
    slide.margin_set(@margin_top, @margin_right, @margin_bottom, @margin_left)
  end
end

match(Slide, HeadLine) do |headlines|
  name = "head-line"

  delete_post_draw_proc_by_name(name)

  space = @space / 2.0
  top_space = space + @default_headline_line_width / 2.0
  margin_with(:bottom => top_space * 2)
  add_post_draw_proc(name) do |headline, canvas, x, y, w, h, simulation|
    unless simulation
      if @default_headline_line_expand
        line_x, line_w = 0, canvas.width
      else
        line_x, line_w = x, w
      end
      if @default_headline_line_params.respond_to?(:call)
        params = @default_headline_line_params.call(headline, canvas,
                                                    line_x, y + top_space,
                                                    line_w, h - top_space)
      else
        params = @default_headline_line_params
      end
      canvas.draw_line(line_x, y + top_space, line_x + line_w, y + top_space,
                       @default_headline_line_color,
                       params.merge(:line_width => @default_headline_line_width))
    end
    [x, y, w, h]
  end

  headlines.each do |headline|
    slide = headline.slide
    headline.hide if slide.hide_title?
    color = slide["headline-color"]
    headline.font("color", color) if color
    shadow_color = slide["headline-shadow-color"]
    headline.font("shadow-color" => shadow_color) if shadow_color
  end
end

match(Slide, Body) do |bodies|
  bodies.each do |body|
    unless body.elements.any? {|element| element.is_a?(Image)}
      body.margin_with(:left => @body_margin_left,
                       :right => @body_margin_right)
    end
  end
end
