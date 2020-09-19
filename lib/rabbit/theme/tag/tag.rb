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

  find_align_target = lambda do |tag|
    element = tag
    while element.inline_element? and not element.is_a?(TableCell)
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
    when "left"
      find_align_target.call(tag).align = "left"
    when "center"
      find_align_target.call(tag).horizontal_centering = true
    when "right"
      find_align_target.call(tag).align = "right"
    when "small"
      find_target.call(tag).prop_set("size", @small_font_size)
    when "x-small"
      find_target.call(tag).prop_set("size", @x_small_font_size)
    when "xx-small"
      find_target.call(tag).prop_set("size", @xx_small_font_size)
    when "large"
      find_target.call(tag).prop_set("size", @large_font_size)
    when "x-large"
      find_target.call(tag).prop_set("size", @x_large_font_size)
    when "xx-large"
      find_target.call(tag).prop_set("size", @xx_large_font_size)
    when /\A(normal|oblique|italic)\z/
      find_target.call(tag).prop_set("style", $1)
    when /\Amargin-(top|bottom|left|right)(?:\s*\*\s*([-\d.]+))?\z/
      target = "margin_#{$1}"
      scale = Float($2 || 1)
      outer_block = find_outer_block.call(tag)
      current_value = outer_block.send(target)
      outer_block.send("#{target}=", current_value + (@space * scale))
    else
      handler = find_handler.call(tag)
      if handler
        handler.call(:target => find_target.call(tag),
                     :outer_block => find_outer_block.call(tag))
      end
    end
  end
end
