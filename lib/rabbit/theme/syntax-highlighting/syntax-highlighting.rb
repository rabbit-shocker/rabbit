# -*- coding: utf-8 -*-

@syntax_highlighting_uninstall ||= false

theme_exit if @syntax_highlighting_uninstall

@syntax_highlighting_scheme ||= {}
@syntax_highlighting_scheme_default = {
  :comment => {
    :color => "#8f5902",
    :style => "italic",
  },
  :comment_delimiter => {
    :color => "#8f5902",
    :style => "italic",
  },
  :reserved => {
    :color => "#204a87",
  },
  :method => {
    :color => "#000000",
  },
  :string => {
    :color => "#4e9a06",
  },
  :variable => {
    :color => "#ce5c00",
  },
  :instance_variable => {
    :color => "#ce5c00",
  },
  :global_variable => {
    :color => "#8f5902",
  },
  :operator => {
    :color => "#ce5c00",
  },
  :ident => {
    # :color => "#f57900",
  },
  :pre_constant => {
    :color => "#204a87",
  },
}

@syntax_highlighting_frame_color ||= "#000000"
@syntax_highlighting_frame_width ||= 2
@syntax_highlighting_fill_color ||= "#f8f8f8"
@syntax_highlighting_shadow_color ||= nil

@syntax_highlighting_padding_left ||= screen_x(5)
@syntax_highlighting_padding_right ||= screen_x(5)
@syntax_highlighting_padding_top ||= screen_y(2)
@syntax_highlighting_padding_bottom ||= screen_y(2)

if @syntax_highlighting_keep_in_size.nil?
  @syntax_highlighting_keep_in_size = true
end

match("**", SyntaxHighlightingBlock) do |blocks|
  name = "syntax-highlighting-block"

  blocks.horizontal_centering = true

  params = {
    :proc_name => name,
    :frame_color => @syntax_highlighting_frame_color,
    :frame_width =>  @syntax_highlighting_frame_width,
    :fill_color => @syntax_highlighting_fill_color,
    :shadow_color => @syntax_highlighting_shadow_color,
  }

  padding_set(:left => @syntax_highlighting_padding_left,
              :right => @syntax_highlighting_padding_right,
              :top => @syntax_highlighting_padding_top,
              :bottom => @syntax_highlighting_padding_bottom)

  blocks.wrap_mode = false

  blocks.margin_top = @space
  blocks.margin_bottom = @space

  blocks.keep_in_size if @syntax_highlighting_keep_in_size

  draw_frame(blocks, params)
end

match("**", SyntaxHighlightingBlock, "**", CustomTag) do |tags|
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

  scheme = @syntax_highlighting_scheme_default.merge(@syntax_highlighting_scheme)
  tags.each do |tag|
    case tag.name
    when /\Asyntax-(.+)\z/
      style = (scheme[$1.gsub(/-/, '_').to_sym] || {})
      next if style.empty?
      find_markup_target.call(tag).font(style)
    end
  end
end

