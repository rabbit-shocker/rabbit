# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

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
  normalized_type = type.to_s.downcase
  case normalized_type
  when "rectangle"
    canvas.draw_rectangle(true, x, y, w, h, color)
  when "circle"
    canvas.draw_circle(true, x, y, w, h, color)
  when "check", "dash"
    props = {
      "font-family" => @font_family,
      "weight" => "bold",
      "foreground" => canvas.make_color(color).to_gdk_format,
    }.merge(item.first.text_props)

    layout = make_layout(span(props, entity(normalized_type)))
    text_width, text_height = layout.pixel_size
    canvas.draw_layout(layout,
                       x - text_width / 2.0,
                       y - text_height / 3.0)
  else
    format = _("unknown item mark type: %s\n" \
               "Rectangle type is used as fallback")
    Rabbit.logger.warn(format % type.inspect)
    setup_default_item_mark_type("rectangle")
  end
end

def setup_default_item_mark(items, name, width, height, space_ratio, color,
                            options={})
  option_value = Proc.new do |option_name, *args|
    setup_default_item_mark_get_option_value(options, option_name, *args)
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
      yield(item, canvas, x, y, w, h, color, options)
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
  normalized_type = type.to_s.downcase.gsub(/_/, '-')
  case normalized_type
  when "numeric"
    "#{item.order}. "
  when "lower-case", "upper-case"
    mark = normalized_type == "lower-case" ? "a" : "A"
    (item.order - 1).times do
      mark = mark.succ
    end
    "#{mark}. "
  else
    format = _("unknown enumeration item mark type: %s\n" \
               "Numeric type is used as fallback")
    setup_default_enum_item_mark_type("numeric", item)
  end
end

def setup_default_enum_item_mark(items, name, indent, space_ratio, props,
                                 options={})
  option_value = Proc.new do |option_name, *args|
    setup_default_item_mark_get_option_value(options, option_name, *args)
  end

  indent_width = screen_x(indent)
  indent_width = option_value.call("indent_width", indent_width) || indent_width
  default_props = {
    "font-family" => @font_family,
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
    span(props, enum_mark)
  end

  space = @space * space_ratio
  margin_with(:bottom => space)
end
