@default_emphasis_color ||= "red"
@default_emphasis_level2_color ||= @default_emphasis_color

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
  texts.prop_set("foreground", @default_emphasis_color)
  texts.prop_set("weight", "bold")
end

match("**", Emphasis, Emphasis) do |texts|
  texts.prop_set("foreground", @default_emphasis_level2_color)
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
  texts.prop_set("size", @x_large_script_font_size)
  texts.prop_set("rise", -(@x_large_script_font_size * 2 / 3.0).to_i)
end

match("**", Title, "**", Superscript) do |texts|
  texts.prop_set("size", @x_large_script_font_size)
  texts.prop_set("rise", (@x_large_script_font_size * 5 / 3.0).to_i)
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

match("**", FootText) do |texts|
  texts.prop_set("size", @xx_small_font_size)
  set_font_family(texts)
end

match("**", Footnote) do |notes|
  notes.prop_set("size", @script_font_size)
  notes.prop_set("rise", (@script_font_size * 3.0 / 2.0).ceil)
end


slide_body = [Slide, Body]

item_list_item = [ItemList, ItemListItem]

match(*(slide_body + (item_list_item * 2))) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + (item_list_item * 3))) do
  prop_set("size", @x_small_font_size)
end

enum_list_item = [EnumList, EnumListItem]

match(*(slide_body + (enum_list_item * 2))) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + (enum_list_item * 3))) do
  prop_set("size", @x_small_font_size)
end


match(*(slide_body + item_list_item + enum_list_item)) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + (item_list_item * 2) + enum_list_item)) do
  prop_set("size", @x_small_font_size)
end

match(*(slide_body + enum_list_item + item_list_item)) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + enum_list_item + (item_list_item * 2))) do
  prop_set("size", @x_small_font_size)
end


desc_list_item = [DescriptionList, DescriptionListItem]
desc_term = desc_list_item + [DescriptionTerm]
desc_content = desc_list_item + [DescriptionContent]

match(*(slide_body + (desc_content * 1))) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + (desc_content * 2))) do
  prop_set("size", @x_small_font_size)
end

match(*(slide_body + (desc_content * 3))) do
  prop_set("size", @xx_small_font_size)
end

match("**", DescriptionTerm) do |terms|
  prop_set("weight", "bold")
  set_font_family(terms)
end

match(*(slide_body + desc_term)) do
  prop_set("size", @normal_font_size)
end

match(*(slide_body + (desc_content * 1) + desc_term)) do
  prop_set("size", @small_font_size)
end

match(*(slide_body + (desc_content * 2) + desc_term)) do
  prop_set("size", @x_small_font_size)
end
