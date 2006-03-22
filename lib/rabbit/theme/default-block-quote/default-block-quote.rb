@block_quote_frame_color ||= "#55003dff0eff"
@block_quote_frame_width ||= 2
@block_quote_fill_color ||= "#fcfae2"

@block_quote_title_color ||= "#666"
@block_quote_title_font_size ||= @x_small_font_size

@block_quote_padding_left ||= screen_x(5)
@block_quote_padding_right ||= screen_x(5)
@block_quote_padding_top ||= screen_y(2)
@block_quote_padding_bottom ||= screen_y(2)

@block_quote_margin_left ||= screen_x(3)
@block_quote_margin_right ||= screen_x(3)

@block_quote_open_quote_image ||= nil
@block_quote_close_quote_image ||= nil
@block_quote_image_max_width ||= canvas.width * 0.1

match("**", BlockQuote) do
  name = "block-quote"

  prop_set("style", "italic")

  params = {
    :proc_name => name,
    :frame_color => @block_quote_frame_color,
    :frame_width =>  @block_quote_frame_width,
    :fill_color => @block_quote_fill_color,
  }

  draw_frame(params)

  each do |block|
    name = "block-quote-image"

    padding_left = @block_quote_padding_left
    padding_right = @block_quote_padding_right

    open_quote = nil
    close_quote = nil
    if @block_quote_open_quote_image
      open_quote = ImageLoader.new(find_file(@block_quote_open_quote_image))
      if open_quote.width > @block_quote_image_max_width
        open_quote.resize(@block_quote_image_max_width, nil)
      end
      padding_left += open_quote.width
    end
    if @block_quote_close_quote_image
      close_quote = ImageLoader.new(find_file(@block_quote_close_quote_image))
      if close_quote.width > @block_quote_image_max_width
        close_quote.resize(@block_quote_image_max_width, nil)
      end
      padding_right += close_quote.width
    end

    block.padding_with(:left => padding_left,
                       :right => padding_right,
                       :top => @block_quote_padding_top,
                       :bottom => @block_quote_padding_bottom)
    block.margin_with(:left => @block_quote_margin_left,
                      :right => @block_quote_margin_right,
                      :bottom => @space)

    block.delete_pre_draw_proc_by_name(name)
    block.delete_post_draw_proc_by_name(name)
    if open_quote
      adjust_open_quote_x = (@block_quote_padding_left / 2) + open_quote.width
      block.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          open_quote.draw(canvas, x - adjust_open_quote_x, y)
        end
        [x, y, w, h]
      end
    end
    if close_quote
      adjust_close_quote_x = (@block_quote_padding_bottom / 2)
      adjust_close_quote_x += close_quote.height
      block.add_post_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          close_quote.draw(x + w, y - adjust_close_quote_x)
        end
        [x, y, w, h]
      end
    end

    name = "block-quote-title"
    block.delete_post_draw_proc_by_name(name)

    if block.title
      layout = nil
      block.add_post_draw_proc(name) do |canvas, x, y, w, h, simulation|
        if layout.nil?
          title = Text.new(_("[cited from `%s']") % block.title)
          title.font(:size => @block_quote_title_font_size,
                     :style => "italic")
          title.align = Pango::Layout::ALIGN_RIGHT
          set_font_family(title)
          title_w = w + block.padding_left + block.padding_right
          title.compile(canvas, x, y, title_w, h)
          layout = title.layout
          block.margin_bottom += title.height + @block_quote_frame_width
        end
        unless simulation
          base_x = (block.ox || x) - block.padding_left
          base_y = y + block.padding_bottom + @block_quote_frame_width
          canvas.draw_layout(layout, base_x, base_y, @block_quote_title_color)
        end
        [x, y, w, h]
      end
    end
  end
end
