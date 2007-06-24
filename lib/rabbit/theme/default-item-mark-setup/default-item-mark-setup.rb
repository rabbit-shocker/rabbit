def setup_default_item_mark_get_option_value(options, name, *args)
  value = options[name]
  if value and value.respond_to?(:call)
    value.call(*args)
  else
    value
  end
end

def setup_default_item_mark_type(type, item, canvas, x, y, w, h, color)
  type ||= "rectangle"
  case type.to_s.downcase
  when "rectangle"
    canvas.draw_rectangle(true, x, y, w, h, color)
  when "circle"
    canvas.draw_circle(true, x, y, w, h, color)
  else
    format = _("unknown item mark type: %s\n" \
               "Rectangle type is used as fallback")
    canvas.logger.warn(format % type.inspect)
    setup_default_item_mark_type("rectangle")
  end
end

def setup_default_item_mark(items, name, width, height, space_ratio, color,
                            options={})
  option_value = Proc.new do |name, *args|
    setup_default_item_mark_get_option_value(options, name, *args)
  end

  mark_width = screen_x(width)
  mark_height = screen_y(height)
  mark_space = option_value.call("mark_space", mark_width) || mark_width
  indent_width = option_value.call("indent_width", mark_width, mark_space)
  indent_width ||= mark_width * 3

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  args = [items, indent_width, mark_width, mark_height, name]
  draw_mark(*args) do |item, canvas, x, y, w, h|
    x -= mark_space
    if block_given?
      yield(item, canvas, x, y, w, h, color)
    else
      type = options["type"]
      setup_default_item_mark_type(type, item, canvas, x, y, w, h, color)
    end
  end

  space = @space * space_ratio
  margin_with(:bottom => space)
end

def setup_default_enum_item_mark_type(type, item)
  type ||= "numeric"
  case type.to_s.downcase.gsub(/_/, '-')
  when "numeric"
    "#{item.order}. "
  when "lower-case"
    "#{(?a + item.order - 1).chr}. "
  when "upper-case"
    "#{(?A + item.order - 1).chr}. "
  else
    format = _("unknown enumeration item mark type: %s\n" \
               "Numeric type is used as fallback")
    setup_default_enum_item_mark_type("numeric", item)
  end
end

def setup_default_enum_item_mark(items, name, indent, space_ratio, props,
                                 options={})
  option_value = Proc.new do |name, *args|
    setup_default_item_mark_get_option_value(options, name, *args)
  end

  indent_width = screen_x(indent)
  indent_width = option_value.call("indent_width", indent_width) || indent_width
  default_props = {
    "font_family" => @font_family,
  }.merge(props)

  delete_pre_draw_proc_by_name(name)
  delete_post_draw_proc_by_name(name)

  draw_order(items, indent_width, name) do |item|
    props = default_props
    props = props.merge(item.first.text_props) unless item.empty?
    if block_given?
      enum_mark = yield(item)
    else
      type = option_value.call("type", item)
      enum_mark = setup_default_enum_item_mark_type(type, item)
    end
    %Q[<span #{to_attrs(props)}>#{enum_mark}</span>]
  end

  space = @space * space_ratio
  margin_with(:bottom => space)
end
