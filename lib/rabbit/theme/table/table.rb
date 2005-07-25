all_table = ["**", Table]

match(*all_table) do |tables|
  params = {
    :frame_color => @table_frame_color,
    :fill_color => @table_fill_color,
  }
  
  tables.left_padding = @table_left_padding
  tables.right_padding = @table_right_padding
  tables.top_padding = @table_top_padding
  tables.bottom_padding = @table_bottom_padding

  tables.each do |table|
    layout = nil
    th = 0
    adjust_y = 0
    
    table.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
      if table.caption and layout.nil?
        caption = NormalText.new(table.caption)
        caption.align = Pango::Layout::ALIGN_CENTER
        caption.prop_set("size", @normal_font_size)
        set_font_family(caption)
        caption.compile(canvas, x, y, w, h)
        layout = caption.layout
        th = caption.height
      end
      if !simulation and layout
        canvas.draw_layout(layout, x, y)
      end
      adjust_y = th + @space
      [x, y + adjust_y, w, h - adjust_y]
    end
    
    draw_frame(table, params) do |_, canvas, x, y, w, h|
      new_x = x
      new_y = y
      new_w = w
      new_h = nil
      [new_x, new_y, new_w, new_h]
    end
  end
end

match(*(all_table + [TableHead, TableRow, TableHeader])) do |headers|
  params = {
    :frame_color => @table_head_frame_color,
    :fill_color => @table_head_fill_color,
  }

  headers.left_padding = @table_header_left_padding
  headers.right_padding = @table_header_right_padding
  headers.top_padding = @table_header_top_padding
  headers.bottom_padding = @table_header_bottom_padding

  headers.prop_set("size", @normal_font_size)
  set_font_family(headers)
  draw_frame(headers, params)

  headers.add_pre_draw_proc do |header, canvas, x, y, w, h, simulation|
    if simulation
      row = header.parent
      table = row.parent.parent
      header_w = table.available_w / row.elements.size
      header_w -= header.left_padding + header.right_padding
      header.dirty!
      header.text_compile(canvas, x, y, header_w, h)
    end
    [x, y, w, h]
  end
end

match(*(all_table + [TableBody, TableRow, TableCell])) do |cells|
  params = {
    :frame_color => @table_body_frame_color,
    :fill_color => @table_body_fill_color,
  }

  cells.prop_set("size", @normal_font_size)
  set_font_family(cells)

  cells.left_padding = @table_cell_left_padding
  cells.right_padding = @table_cell_right_padding
  cells.top_padding = @table_cell_top_padding
  cells.bottom_padding = @table_cell_bottom_padding

  cells.each do |cell|
    orig_x = nil
    cell_w = nil
    cell_h = nil

    cell.add_pre_draw_proc do |canvas, x, y, w, h, simulation|
      orig_x = x
      if simulation
        row = cell.parent
        table = row.parent.parent
        cell_w = table.available_w / row.elements.size
        cell.dirty!
        base_y = y + cell.top_padding
        text_width = cell_w - cell.left_padding - cell.right_padding
        cell.text_compile(canvas, x, base_y, text_width, h)
      end
      [x, y, w, h]
    end

    cell.add_post_draw_proc do |canvas, x, y, w, h, simulation|
      [orig_x + cell_w, y, w - cell_w, h]
    end

    draw_frame(cell, params) do |_, canvas, x, y, w, h|
      new_x = nil
      new_y = nil
      new_w = cell_w
      new_h = cell.parent.height
      [new_x, new_y, new_w, new_h]
    end
  end
end
