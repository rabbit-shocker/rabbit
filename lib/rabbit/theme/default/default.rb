match(TitleSlide) do |slides|
  slides.horizontal_centering = true
  slides.vertical_centering = true

  slides.left_margin = @left_margin
  slides.right_margin = @right_margin
  slides.top_margin = @top_margin
  slides.bottom_margin = @bottom_margin
end

match(TitleSlide, "*") do |elems|
  elems.prop_set("size", @large_font_size)
  elems.prop_set("font_family", @default_font_family)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("size", @huge_font_size)
  titles.prop_set("weight", "heavy")

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, Subtitle) do |titles|
  titles.prop_set("size", @normal_font_size)

  space = screen_size(5)
  titles.add_post_draw_proc do |title, canvas, x, y, w, h, simulation|
    if title.next_element.is_a?(Subtitle)
      [x, y, w, h]
    else
      [x, y + space, w, h - space]
    end
  end
end

match(TitleSlide, ContentSource) do |titles|
  titles.prop_set("size", @small_font_size)
  titles.prop_set("style", "italic")
end

match(TitleSlide, Institution) do |titles|
  titles.prop_set("size", @normal_font_size)
  titles.prop_set("style", "italic")
end


match(Slide) do |slides|
  slides.left_margin = @left_margin
  slides.right_margin = @right_margin
  slides.top_margin = @top_margin
  slides.bottom_margin = @bottom_margin
end

match(Slide, HeadLine) do |heads|
  heads.prop_set("size", @large_font_size)
  heads.prop_set("font_family", @default_font_family)
  heads.horizontal_centering = true

  space = screen_size(1)
  heads.add_post_draw_proc do |text, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, "red")
    end
    [x, y + space * 3, w, h - space * 3]
  end
end

match("**", Paragraph) do |texts|
  texts.prop_set("size", @normal_font_size)
  texts.prop_set("font_family", @default_font_family)

  space = screen_size(2.0)
  texts.add_post_draw_proc do |text, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match("**", Emphasis) do |texts|
  texts.prop_set("foreground", "red")
  texts.prop_set("style", "italic")
end

match("**", DeletedText) do |texts|
  texts.prop_set("strikethrough", "true")
end

match("**", ReferText) do |texts|
  texts.prop_set("underline", "single")
  texts.prop_set("foreground", "blue")
end

match("**", Subscript) do |texts|
  texts.prop_set("size", @script_font_size)
  texts.prop_set("rise", -(@script_font_size * 2 / 3.0).to_i)
end

match("**", Superscript) do |texts|
  texts.prop_set("size", @script_font_size)
  texts.prop_set("rise", (@script_font_size * 5 / 3.0).to_i)
end

match("**", HeadLine, "**", Subscript) do |texts|
  texts.prop_set("size", @large_script_font_size)
  texts.prop_set("rise", -(@large_script_font_size * 2 / 3.0).to_i)
end

match("**", HeadLine, "**", Superscript) do |texts|
  texts.prop_set("size", @large_script_font_size)
  texts.prop_set("rise", (@large_script_font_size * 5 / 3.0).to_i)
end

match("**", Title, "**", Subscript) do |texts|
  texts.prop_set("size", @huge_script_font_size)
  texts.prop_set("rise", -(@huge_script_font_size * 2 / 3.0).to_i)
end

match("**", Title, "**", Superscript) do |texts|
  texts.prop_set("size", @huge_script_font_size)
  texts.prop_set("rise", (@huge_script_font_size * 5 / 3.0).to_i)
end

match("**", PreformattedText) do |texts|
  texts.prop_set("size", @normal_font_size)
  texts.prop_set("font_family", "Monospace")
  texts.prop_set("weight", "bold")
end

match("**", DescriptionTerm) do |terms|
  terms.prop_set("size", @normal_font_size)
# terms.prop_set("underline", "double")

  color = "#ff9900"
  space = screen_size(1)
  terms.add_post_draw_proc do |term, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + term.width, y + space, color)
    end
    [x, y + space * 3, w, h - space * 3]
  end
end

match("**", PreformattedBlock) do |blocks|
  blocks.horizontal_centering = true

  border_color = "#55003dff0eff"
  fill_color = "#fcfae2"

  left = @preformatted_left_margin
  right = @preformatted_right_margin
  top = @preformatted_top_margin
  bottom = @preformatted_bottom_margin

  blocks.each do |block|
    orig_x = orig_y = orig_w = orig_h = nil
    new_x = new_y = new_w = new_h = nil

    block.wrap_mode = false

    block.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
      orig_x = x
      orig_y = y
      orig_w = w
      orig_h = h
      unless simulation
        canvas.draw_rectangle(true, new_x, new_y, new_w, new_h, fill_color)
        canvas.draw_rectangle(false, new_x, new_y, new_w, new_h, border_color)
      end
      [x, y + top, w, h - bottom]
    end
  
    block.add_post_draw_proc do |canvas, x, y, w, h, simulation|
      new_x = orig_x - left
      new_y = orig_y
      new_w = (block.width || w) + left + right
      new_h = orig_h - h
      [orig_x, y + top, orig_w, h - bottom]
    end
  end
end

match("**", MethodTerm) do |texts|
  texts.prop_set("size", @normal_font_size)
  texts.prop_set("font_family", "Monospace")
end

match("**", MethodKind) do |texts|
  texts.prop_set("foreground", "gray")
end

match("**", ClassName) do |texts|
  texts.prop_set("weight", "bold")
end

match("**", MethodName) do |texts|
  texts.prop_set("weight", "bold")
end

match("**", Code) do |texts|
  texts.prop_set("font_family", "Monospace")
end

match("**", FoottextBlock) do |blocks|
  space = screen_size(1)
  color = "#33ff33"
  blocks.add_pre_draw_proc do |block, canvas, x, y, w, h, simulation|
    if block.elements.empty?
      [x, y, w, h]
    else
      unless simulation
        canvas.draw_line(x, y + space, (x + w / 2.0).ceil, y + space, color)
      end
      [x, y + space * 3, w, h - space * 3]
    end
  end
end

match("**", Foottext) do |texts|
  texts.prop_set("size", @xx_small_font_size)
  texts.each do |text|
    if text.user_property["order_added"]
      order_text = text.elements.first
    else
      order_text = NormalText.new("(*#{text.order})")
      text.unshift(order_text)
      text.user_property["order_added"] = true
    end
    order_text.prop_set("foreground", "blue")
    order_text.prop_set("size", @script_font_size)
    order_text.prop_set("rise", (@script_font_size / 2.0).ceil)
  end
end

match("**", Footnote) do |notes|
  notes.prop_set("size", @script_font_size)
  notes.prop_set("rise", (@script_font_size * 3.0 / 2.0).ceil)
  notes.prop_set("foreground", "blue")
  notes.each do |note|
    note.text = "(*#{note.order})"
  end
end

match("**", Image) do |images|
  images.horizontal_centering = true

  space = screen_size(3)

  make_normalized_size = Proc.new do |size|
    size && screen_size(size)
  end

  make_relative_size = Proc.new do |size, parent_size|
    size && parent_size && ((size / 100.0) * parent_size).ceil
  end

  images.each do |image|
    pr = image.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
      if simulation
        nw = make_normalized_size.call(image.normalized_width)
        nh = make_normalized_size.call(image.normalized_height)
        rw = make_relative_size.call(image.relative_width, image.parent.w)
        rh = make_relative_size.call(image.relative_height, image.parent.h)
        iw = nw || rw
        ih = nh || rh
        image.resize(iw, ih)
        image.delete_pre_draw_proc(pr)
      end
      [x, y, w, h]
    end
  end

  images.each do |image|
    layout = nil
    th = 0
    image.add_post_draw_proc do |canvas, x, y, w, h, simulation|
      if image.caption and layout.nil?
        caption = NormalText.new(image.caption)
        caption.prop_set("size", @normal_font_size)
        caption.prop_set("font_family", @default_font_family)
        caption.compile(canvas, x, y, w, h)
        if image.horizontal_centering
          caption.do_horizontal_centering(canvas, x, y, w, h)
        end
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        canvas.draw_layout(layout, image.ox, y) # dirty!!!
      end
      [x, y + space + th, w, h - space - th]
    end
  end
end

match("**", MethodListItem, Paragraph) do |texts|
  space = @normal_font_size / Pango::SCALE
  indent(texts, space)
end

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "green"

  draw_mark(items, indent_width, mark_width, mark_height) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = screen_y(1.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (item_list_item * 2))) do |items|
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "blue"
  
  draw_mark(items, indent_width, mark_width, mark_height) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_circle(true, start_x, start_y, end_x, end_y, color)
  end

  space = screen_y(1.0)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (item_list_item * 3))) do |items|
  mark_width = screen_x(1.0)
  mark_height = screen_y(1.0)
  indent_width = mark_width * 3
  color = "red"
  
  draw_mark(items, indent_width, mark_width, mark_height) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = screen_y(0.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (item_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  indent_width = screen_x(2)
  props = {
    "size" => @normal_font_size,
    "font_family" => @default_font_family,
  }

  draw_order(items, indent_width) do |item|
    %Q[<span #{to_attrs(props)}>#{item.order}. </span>]
  end

  space = screen_y(1.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  indent_width = screen_x(1.5)
  props = {
    "size" => @small_font_size,
    "font_family" => @default_font_family,
  }

  draw_order(items, indent_width) do |item|
    %Q[<span #{to_attrs(props)}>#{(?a + item.order - 1).chr}. </span>]
  end

  space = screen_y(1.0)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  indent_width = screen_x(1)
  props = {
    "size" => @x_small_font_size,
    "font_family" => @default_font_family,
  }

  draw_order(items, indent_width) do |item|
    %Q[<span #{to_attrs(props)}>#{(?A + item.order - 1).chr}. </span>]
  end

  space = screen_y(1.0)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (enum_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (enum_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


match(*(slide_body + enum_list_item + item_list_item)) do |items|
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "#00ffff"

  draw_mark(items, indent_width, mark_width, mark_height) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = screen_y(1.0)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "#ff00ff"

  draw_mark(items, indent_width, mark_width, mark_height) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = screen_y(0.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + enum_list_item + item_list_item + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + enum_list_item + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


desc_list_item = [DescriptionList, DescriptionListItem]

match(*(slide_body + desc_list_item)) do |items|
  space = @normal_font_size / Pango::SCALE
  items.each do |item|
    indent(item[1..-1], space)
  end

  space = screen_y(1.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (desc_list_item * 2))) do |items|
  space = @small_font_size / Pango::SCALE
  items.each do |item|
    indent(item[1..-1], space)
  end

  space = screen_y(1.0)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (desc_list_item * 3))) do |items|
  space = @x_small_font_size / Pango::SCALE
  items.each do |item|
    indent(item[1..-1], space)
  end

  space = screen_y(0.5)
  items.add_post_draw_proc do |item, canvas, x, y, w, h, simulation|
    [x, y + space, w, h - space]
  end
end

match(*(slide_body + (desc_list_item * 1) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (desc_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end

match(*(slide_body + (desc_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @xx_small_font_size)
end

if windows?
  match("**") do |elems|
    elems.prop_delete("style")
  end
end
