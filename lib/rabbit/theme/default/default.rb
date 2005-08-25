include_theme("image")
include_theme("table")
include_theme("slide-number")

match(TitleSlide) do |slides|
  slides.horizontal_centering = true
  slides.vertical_centering = true

  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom
end

match(TitleSlide, "*") do |elems|
  elems.prop_set("size", @large_font_size)
  set_font_family(elems)
end

match(TitleSlide, Title) do |titles|
  titles.prop_set("size", @huge_font_size)
  titles.prop_set("weight", "heavy")
end

match(TitleSlide, Subtitle) do |titles|
  titles.prop_set("size", @normal_font_size)
end

match(TitleSlide, Author) do |authors|
  authors.margin_top = @space * 2
  authors.margin_bottom = @space
end

match(TitleSlide, ContentSource) do |sources|
  sources.prop_set("size", @small_font_size)
  sources.prop_set("style", "italic")

  sources.margin_bottom = @space
end

match(TitleSlide, Institution) do |institutions|
  institutions.prop_set("size", @normal_font_size)
  institutions.prop_set("style", "italic")
end


match(Slide) do |slides|
  slides.margin_left = @margin_left
  slides.margin_right = @margin_right
  slides.margin_top = @margin_top
  slides.margin_bottom = @margin_bottom
end

match(Slide, HeadLine) do |heads|
  name = "head-line"
  
  heads.prop_set("size", @large_font_size)
  set_font_family(heads)

  heads.delete_post_draw_proc_by_name(name)

  space = @space / 2.0
  heads.margin_bottom = space * 3
  heads.add_post_draw_proc(name) do |text, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + w, y + space, "red")
    end
    [x, y, w, h]
  end
end

match("**", Paragraph) do |texts|
  texts.prop_set("size", @normal_font_size)
  set_font_family(texts)

  texts.margin_top = @space / 2.0
  texts.margin_bottom = @space / 2.0
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
  texts.prop_set("weight", "bold")
  set_font_family(texts, @monospace_font_family)
end

match("**", DescriptionTerm) do |terms|
  name = "description-term"
  
  terms.prop_set("size", @normal_font_size)
# terms.prop_set("underline", "double")

  color = "#ff9900"
  space = @space / 2.0

  terms.margin_bottom = space * 3
  terms.delete_post_draw_proc_by_name(name)
  terms.add_post_draw_proc(name) do |term, canvas, x, y, w, h, simulation|
    unless simulation
      canvas.draw_line(x, y + space, x + term.width, y + space, color)
    end
    [x, y, w, h]
  end
end

match("**", PreformattedBlock) do |blocks|
  name = "preformatted-block"
  
  blocks.horizontal_centering = true

  params = {
    :proc_name => name,
    :frame_color => @preformatted_frame_color,
    :fill_color => @preformatted_fill_color,
  }

  blocks.padding_left = @preformatted_padding_left
  blocks.padding_right = @preformatted_padding_right
  blocks.padding_top = @preformatted_padding_top
  blocks.padding_bottom = @preformatted_padding_bottom

  blocks.wrap_mode = false

  blocks.margin_bottom = @space
    
  draw_frame(blocks, params)
end

match("**", MethodTerm) do |texts|
  texts.prop_set("size", @normal_font_size)
  set_font_family(texts, @monospace_font_family)
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
  set_font_family(texts, @monospace_font_family)
end

match("**", FoottextBlock) do |blocks|
  name = "foottext-block"
  space = @space / 2.0
  color = "#33ff33"

  blocks.delete_pre_draw_proc_by_name(name)
  blocks.each do |block|
    unless block.elements.empty?
      block.margin_top = space * 3

      block.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          args = [
            x, y - space * 2, (x + w / 2.0).ceil, y - space * 2,
            color
          ]
          canvas.draw_line(*args)
        end
        [x, y, w, h]
      end
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

match("**", MethodListItem, Paragraph) do |texts|
  name = "method-list-item-paragraph"

  texts.delete_pre_draw_proc_by_name(name)
  texts.delete_post_draw_proc_by_name(name)
  
  space = @normal_font_size / Pango::SCALE
  indent(texts, space, name)
end

slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 1))) do |items|
  name = "item1"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "green"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 2))) do |items|
  name = "item2"
  
  mark_width = screen_x(1.5)
  mark_height = screen_y(1.5)
  indent_width = mark_width * 3
  color = "blue"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_circle(true, start_x, start_y, end_x, end_y, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 3))) do |items|
  name = "item3"
  
  mark_width = screen_x(1.0)
  mark_height = screen_y(1.0)
  indent_width = mark_width * 3
  color = "red"
  
  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (item_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 1))) do |items|
  name = "enum-item1"
  
  indent_width = screen_x(2)
  props = {
    "size" => @normal_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{item.order}. </span>]
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (enum_list_item * 2))) do |items|
  name = "enum-item2"
  
  indent_width = screen_x(1.5)
  props = {
    "size" => @small_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{(?a + item.order - 1).chr}. </span>]
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (enum_list_item * 3))) do |items|
  name = "enum-item3"
  
  indent_width = screen_x(1)
  props = {
    "size" => @x_small_font_size,
    "font_family" => @font_family,
  }

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_order(items, indent_width, name) do |item|
    %Q[<span #{to_attrs(props)}>#{(?A + item.order - 1).chr}. </span>]
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (enum_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (enum_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


match(*(slide_body + enum_list_item + item_list_item)) do |items|
  name = "enum-item"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "#00ffff"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do |items|
  name = "enum-item2"
  
  mark_width = screen_x(2)
  mark_height = screen_y(2)
  indent_width = mark_width * 3
  color = "#ff00ff"

  items.delete_pre_draw_proc_by_name(name)
  items.delete_post_draw_proc_by_name(name)
  
  draw_mark(items, indent_width, mark_width, mark_height, name) do
    |item, canvas, start_x, start_y, end_x, end_y|
    canvas.draw_rectangle(true, start_x, start_y, end_x, end_y, color)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + enum_list_item + item_list_item + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + enum_list_item + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


desc_list_item = [DescriptionList, DescriptionListItem]

match(*(slide_body + desc_list_item)) do |items|
  name = "desc-item1"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @normal_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (3 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (desc_list_item * 2))) do |items|
  name = "desc-item2"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @small_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (2 / 4.0)
  items.margin_bottom = space
end

match(*(slide_body + (desc_list_item * 3))) do |items|
  name = "desc-item3"
  
  items.delete_post_draw_proc_by_name(name)
  
  space = @x_small_font_size / Pango::SCALE
  items.each do |item|
    term_items = ElementContainer.new(item[1..-1])
    term_items.delete_pre_draw_proc_by_name(name)
    term_items.delete_post_draw_proc_by_name(name)
    indent(term_items, space, name)
  end

  space = @space * (1 / 4.0)
  items.margin_bottom = space
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
