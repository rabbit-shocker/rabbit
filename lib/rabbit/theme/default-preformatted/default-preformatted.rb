@preformatted_frame_color ||= "#55003dff0eff"
@preformatted_frame_width ||= 2
@preformatted_fill_color ||= "#fcfae2"
@preformatted_shadow_color ||= nil

@preformatted_padding_left ||= screen_x(5)
@preformatted_padding_right ||= screen_x(5)
@preformatted_padding_top ||= screen_y(2)
@preformatted_padding_bottom ||= screen_y(2)

if @preformatted_keep_in_size.nil?
  @preformatted_keep_in_size = true
end

if @preformatted_centering.nil?
  @preformatted_centering = true
end

match("**", PreformattedBlock) do |blocks|
  name = "preformatted-block"

  blocks.prop_set("size", @normal_font_size)
  set_font_family(blocks, @monospace_font_family)

  blocks.horizontal_centering = @preformatted_centering

  params = {
    :proc_name => name,
    :frame_color => @preformatted_frame_color,
    :frame_width =>  @preformatted_frame_width,
    :fill_color => @preformatted_fill_color,
    :shadow_color => @preformatted_shadow_color,
  }

  padding_set(:left => @preformatted_padding_left,
              :right => @preformatted_padding_right,
              :top => @preformatted_padding_top,
              :bottom => @preformatted_padding_bottom)

  blocks.wrap_mode = false

  blocks.margin_top = @space
  blocks.margin_bottom = @space

  blocks.each do |block|
    keep_in_size = block.user_property["keep-in-size"]
    if keep_in_size.nil?
      keep_in_size = @preformatted_keep_in_size
    else
      keep_in_size = (keep_in_size == "true")
    end
    block.keep_in_size if keep_in_size
  end

  draw_frame(blocks, params)
end
