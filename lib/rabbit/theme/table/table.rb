all_table = ["**", Table]

match(*all_table) do |tables|
  space = screen_size(1)

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
        canvas.draw_layout(layout, x, y - table.top_padding)
      end
      adjust_y = th + space
      [x, y + adjust_y, w, h - adjust_y]
    end
    
    draw_frame(table, params) do |_, canvas, x, y, w, h|
      [nil, table.base_y + adjust_y, table.w, nil]
    end
  end
end

match(*(all_table + [TableHead, TableRow, TableHeader])) do |headers|
  params = {
    :frame_color => @table_head_frame_color,
    :fill_color => @table_head_fill_color,
  }

  headers.prop_set("size", @normal_font_size)
  set_font_family(headers)
  draw_frame(headers, params)

  headers.add_pre_draw_proc do |header, canvas, x, y, w, h, simulation|
    if simulation
      row = header.parent
      table = row.parent.parent
      header_w = table.available_w / row.elements.size
      header.layout.set_width(header_w * Pango::SCALE)
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
        cell.text_compile(canvas, x, y, cell_w, h)
      end
      [x, y, w, h]
    end

    cell.add_post_draw_proc do |canvas, x, y, w, h, simulation|
      [orig_x + cell_w, y, w - cell_w, h]
    end

    draw_frame(cell, params) do |_, canvas, x, y, w, h|
      new_x = cell.base_x
      new_y = cell.base_y
      new_w = cell_w
      new_h = cell.parent.height
      [new_x, new_y, new_w, new_h]
    end
  end
end
