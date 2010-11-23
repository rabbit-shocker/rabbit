# -*- coding: utf-8 -*-

@tag_uninstall ||= false

theme_exit if @tag_uninstall

match("**", CustomTag) do |tags|
  find_block_element = lambda do |tag|
    element = tag
    while element.inline_element?
      element = element.parent
    end
    element
  end

  find_markup_target = lambda do |tag|
    if tag.elements.empty?
      tag.parent
    else
      tag
    end
  end

  tags.each do |tag|
    case tag.name
    when "center"
      find_block_element.call(tag).horizontal_centering = true
    when "right"
      find_block_element.call(tag).align = "right"
    when "x-large"
      find_markup_target.call(tag).prop_set("size", @x_large_font_size)
    when "margin-top"
      find_block_element.call(tag).margin_top += @space
    when "margin-bottom"
      find_block_element.call(tag).margin_bottom += @space
    end
  end
end

