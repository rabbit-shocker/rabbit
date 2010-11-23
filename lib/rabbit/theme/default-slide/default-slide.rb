@default_headline_line_color ||= "#ff9933"
@default_headline_line_params ||= {}
@default_headline_line_width ||= 1

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
  margin_with(:bottom => top_space * 1.5)
  add_post_draw_proc(name) do |headline, canvas, x, y, w, h, simulation|
    unless simulation
      if @default_headline_line_params.respond_to?(:call)
        params = @default_headline_line_params.call(headline, canvas,
                                                    x, y + top_space,
                                                    w, h - top_space)
      else
        params = @default_headline_line_params
      end
      canvas.draw_line(x, y + top_space, x + w, y + top_space,
                       @default_headline_line_color,
                       params.merge(:line_width => @default_headline_line_width))
    end
    [x, y, w, h]
  end

  headlines.each do |headline|
    slide = headline.slide
    headline.hide if slide.hide_title?
    color = slide["headline-color"]
    headline.prop_set("foreground", color) if color
    shadow_color = slide["headline-shadow-color"]
    headline["shadow-color"] = shadow_color if shadow_color
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
