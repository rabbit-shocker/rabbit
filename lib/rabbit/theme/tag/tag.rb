# -*- coding: utf-8 -*-

@tag_uninstall ||= false
@tag_handlers ||= {}

theme_exit if @tag_uninstall

match("**", CustomTag) do |tags|
  find_outer_block = lambda do |tag|
    element = tag
    while element.inline_element?
      element = element.parent
    end
    element
  end

  find_target = lambda do |tag|
    if tag.elements.empty?
      tag.parent
    else
      tag
    end
  end

  find_handler = lambda do |tag|
    handler = @tag_handlers[tag.name]
    return handler if handler
    @tag_handlers.each do |key, value|
      return value if key === tag.name
    end
    nil
  end

  tags.each do |tag|
    case tag.name
    when "center"
      find_outer_block.call(tag).horizontal_centering = true
    when "right"
      find_outer_block.call(tag).align = "right"
    when "small"
      find_target.call(tag).prop_set("size", @small_font_size)
    when "x-small"
      find_target.call(tag).prop_set("size", @x_small_font_size)
    when "large"
      find_target.call(tag).prop_set("size", @large_font_size)
    when "x-large"
      find_target.call(tag).prop_set("size", @x_large_font_size)
    when "margin-top"
      find_outer_block.call(tag).margin_top += @space
    when "margin-bottom"
      find_outer_block.call(tag).margin_bottom += @space
    else
      handler = find_handler.call(tag)
      if handler
        handler.call(:target => find_target.call(tag),
                     :outer_block => find_outer_block.call(tag))
      end
    end
  end
end
