# -*- coding: utf-8 -*-

@syntax_highlighting_uninstall ||= false

theme_exit if @syntax_highlighting_uninstall

# http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines
#
# Butter:     #fce94f     #edd400     #c4a000
# Orange:     #fcaf3e     #f57900     #ce5c00
# Chocolate:  #e9b96e     #c17d11     #8f5902
# Chameleon:  #8ae234     #73d216     #4e9a06
# Sky Blue:   #729fcf     #3465a4     #204a87
# Plum:       #ad7fa8     #75507b     #5c35cc
# Scarlet Red:#ef2929     #cc0000     #a40000
# Aluminium:  #eeeeec     #d3d7cf     #babdb6
#             #888a85     #555753     #2e3436

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
  :keyword => {
    :color => "#204a87",
  },
  :method => {
    :color => "#000000",
  },
  :constant => {
    :color => "#4e9a06",
  },
  :string => {
    :color => "#a40000",
  },
  :delimiter => {
    :color => "#ef2929",
  },
  :content => {
    :color => "#a40000",
  },
  :symbol => {
    :color => "#4e9a06",
  },
  :integer => {
    :color => "#c4a000",
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
  :include => {
    :color => "#73d216",
  },
  :pre_constant => {
    :color => "#5c35cc",
  },
  :preprocessor => {
    :color => "#ad7fa8",
  },
  :pre_type => {
    :color => "#5c35cc",
  },
  :directive => {
    :color => "#5c35cc",
  },
}

@syntax_highlighting_foreground ||= "#000000"
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
  blocks.prop_set("foreground", @syntax_highlighting_foreground)
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
      p tag.name if style.empty?
      next if style.empty?
      find_markup_target.call(tag).font(style)
    end
  end
end

