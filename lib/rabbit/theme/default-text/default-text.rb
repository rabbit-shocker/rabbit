include_theme("default-title-text")

match(Slide, HeadLine) do |heads|
  heads.prop_set("size", @large_font_size)
  heads.prop_set("weight", "bold")
  set_font_family(heads)
end

match("**", Paragraph) do |texts|
  texts.prop_set("size", @normal_font_size)
  set_font_family(texts)

  texts.margin_top = @space / 2.0
  texts.margin_bottom = @space / 2.0
end

match("**", Emphasis) do |texts|
  texts.prop_set("foreground", "red")
  texts.prop_set("weight", "bold")
end

match("**", DeletedText) do |texts|
  texts.prop_set("strikethrough", "true")
end

match(Slide, "**", Note) do |texts|
  texts.prop_set("size", @xx_small_font_size)
  texts.prop_set("foreground", "#666")
end

match("**", HeadLine, "**", Note) do |texts|
  texts.prop_set("size", @small_font_size)
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
  set_font_family(texts, @monospace_font_family)
end

match("**", Keyword) do |texts|
  texts.prop_set("weight", "bold")
end

match("**", Comment) do |texts|
  texts.prop_set("style", "italic")
end

match("**", DescriptionTerm) do |terms|
  terms.prop_set("size", @normal_font_size)
  terms.prop_set("weight", "bold")
  set_font_family(terms)
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

match("**", Foottext) do |texts|
  texts.prop_set("size", @xx_small_font_size)
end

match("**", Footnote) do |notes|
  notes.prop_set("size", @script_font_size)
  notes.prop_set("rise", (@script_font_size * 3.0 / 2.0).ceil)
end


slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (item_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (enum_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


match(*(slide_body + enum_list_item + item_list_item + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + enum_list_item + (item_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end


desc_list_item = [DescriptionList, DescriptionListItem]

match(*(slide_body + (desc_list_item * 1) + [Paragraph])) do |texts|
  texts.prop_set("size", @small_font_size)
end

match(*(slide_body + (desc_list_item * 2) + [Paragraph])) do |texts|
  texts.prop_set("size", @x_small_font_size)
end

match(*(slide_body + (desc_list_item * 3) + [Paragraph])) do |texts|
  texts.prop_set("size", @xx_small_font_size)
end
