@block_quote_frame_color ||= "#55003dff0eff"
@block_quote_frame_width ||= 2
@block_quote_fill_color ||= "#fcfae2"

@block_quote_title_color ||= "#666"
@block_quote_title_font_size ||= @xx_small_font_size

@block_quote_padding_left ||= screen_x(5)
@block_quote_padding_right ||= screen_x(5)
@block_quote_padding_top ||= screen_y(2)
@block_quote_padding_bottom ||= screen_y(2)

@block_quote_margin_left ||= screen_x(3)
@block_quote_margin_right ||= screen_x(3)

@block_quote_open_quote_image ||= nil
@block_quote_close_quote_image ||= nil
@block_quote_image_max_width ||= nil # deprecated
@block_quote_image_width ||= @block_quote_image_max_width
@block_quote_image_width ||= canvas.width * 0.1
@block_quote_image_background_alpha ||= nil
@block_quote_image_frame ||= nil

load_quote = lambda do |file|
  return nil if file.nil?
  quote = ImageLoader.new(find_file(file))
  quote.keep_ratio = true
  quote.resize(@block_quote_image_width, nil)
  quote
end

compute_padding = lambda do |open_quote, close_quote|
  padding = {
    :left   => @block_quote_padding_left,
    :right  => @block_quote_padding_right,
    :top    => @block_quote_padding_top,
    :bottom => @block_quote_padding_bottom,
  }
  return padding if @block_quote_image_frame

  unless @block_quote_image_background_alpha
    padding[:left]  += @block_quote_image_width if open_quote
    padding[:right] += @block_quote_image_width if close_quote
  end
  padding
end

compute_margin = lambda do |open_quote, close_quote|
  margin = {
    :left   => @block_quote_margin_left,
    :right  => @block_quote_margin_right,
    :bottom => @space,
  }
  if @block_quote_image_frame
    margin[:top] = open_quote.height / 2 if open_quote
    margin[:bottom] = close_quote.height / 2 if close_quote
  end
  margin
end

render_open_quote = lambda do |open_quote, block, canvas, x, y, w, h|
  return unless open_quote
  quote_x = x
  quote_y = y
  if @block_quote_image_frame
    quote_x -= block.padding_left
    quote_x -= open_quote.width / 2
    quote_y -= block.padding_top
    quote_y -= open_quote.height / 2
  else
    quote_x -= block.padding_left / 2
    unless @block_quote_image_background_alpha
      quote_x -= open_quote.width / 2
    end
    quote_y -= block.padding_top / 2
  end
  open_quote.draw(canvas, quote_x, quote_y,
                  :alpha => @block_quote_image_background_alpha)
end

render_close_quote = lambda do |close_quote, block, canvas, x, y, w, h|
  return unless close_quote
  quote_x = x + w
  quote_y = y + block.height
  if @block_quote_image_frame
    quote_x += block.padding_right
    quote_x -= close_quote.width / 2
    quote_y -= block.padding_bottom
    quote_y -= close_quote.height / 2
  else
    quote_x -= (block.padding_right - close_quote.width) / 2
    if @block_quote_image_background_alpha
      quote_x -= close_quote.width
    else
      quote_x += close_quote.width / 2
    end
    quote_y -= close_quote.height
    quote_y -= block.padding_bottom
    quote_y -= block.padding_bottom / 2
  end
  close_quote.draw(canvas, quote_x, quote_y,
                   :alpha => @block_quote_image_background_alpha)
end

match("**", BlockQuote) do
  name = "block-quote"

  prop_set("style", "italic")

  params = {
    :proc_name => name,
    :fill_color => @block_quote_fill_color,
  }
  unless @block_quote_image_frame
    params[:frame_color] = @block_quote_frame_color
    params[:frame_width] = @block_quote_frame_width
  end
  draw_frame(params)

  each do |block|
    name = "block-quote-image"

    open_quote = load_quote.call(@block_quote_open_quote_image)
    close_quote = load_quote.call(@block_quote_close_quote_image)

    block.padding_with(compute_padding.call(open_quote, close_quote))
    block.margin_with(compute_margin.call(open_quote, close_quote))

    block.delete_pre_draw_proc_by_name(name)
    if open_quote or close_quote
      block.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
        unless simulation
          render_open_quote.call(open_quote, block, canvas, x, y, w, h)
          render_close_quote.call(close_quote, block, canvas, x, y, w, h)
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
          if @block_quote_image_frame
            title_w -= close_quote.width / 2 if close_quote
          end
          title.compile(canvas, x, y, title_w, h)
          layout = title.layout
          block.margin_bottom += title.height + @block_quote_frame_width
        end
        unless simulation
          base_x = (block.ox || x) - block.padding_left
          base_y = y + block.padding_bottom
          unless @block_quote_image_frame
            base_y += @block_quote_frame_width
          end
          canvas.draw_layout(layout, base_x, base_y, @block_quote_title_color)
        end
        [x, y, w, h]
      end
    end
  end
end

match("**", BlockQuote, TextContainerElement) do |texts|
  texts.justify = true
end
