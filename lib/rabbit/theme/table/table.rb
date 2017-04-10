all_table = ["**", Table]

match(*all_table) do |tables|
  name = "table"
  frame_name = "table-frame"
  
  params = {
    :proc_name => frame_name,
    :frame_color => @table_frame_color,
    :fill_color => @table_fill_color,
  }
  
  tables.padding_left = @table_padding_left
  tables.padding_right = @table_padding_right
  tables.padding_top = @table_padding_top
  tables.padding_bottom = @table_padding_bottom

  tables.delete_pre_draw_proc_by_name(frame_name)
  tables.delete_post_draw_proc_by_name(frame_name)
  tables.delete_pre_draw_proc_by_name(name)
  tables.delete_post_draw_proc_by_name(name)
  
  tables.each do |table|
    layout = nil
    th = 0
    adjust_y = 0
    
    table.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
      if table.caption and layout.nil?
        caption = Text.new(table.caption)
        caption.align = Pango::Alignment::CENTER
        caption.prop_set("size", @normal_font_size)
        set_font_family(caption)
        caption.compile(canvas, x, y, w, h)
        layout = caption.layout
        th = caption.height
        table.margin_top = th + @table_caption_space
        adjust_y = th + @table_caption_space / 2.0
      end
      if !simulation and layout
        layout.width = w * Pango::SCALE
        canvas.draw_layout(layout, x, y - adjust_y, @table_caption_color)
      end
      [x, y, w, h]
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
  name = "table-header"
  frame_name = "table-header-frame"
  
  params = {
    :proc_name => frame_name,
    :frame_color => @table_head_frame_color,
    :fill_color => @table_head_fill_color,
  }

  headers.padding_left = @table_header_padding_left
  headers.padding_right = @table_header_padding_right
  headers.padding_top = @table_header_padding_top
  headers.padding_bottom = @table_header_padding_bottom

  headers.delete_pre_draw_proc_by_name(frame_name)
  headers.delete_post_draw_proc_by_name(frame_name)
  headers.delete_pre_draw_proc_by_name(name)
  headers.delete_post_draw_proc_by_name(name)
  
  headers.prop_set("size", @normal_font_size)
  headers.font(@table_header_font_props)
  set_font_family(headers)
  
  draw_frame(headers, params) do |header, canvas, x, y, w, h|
    new_x = nil
    new_y = nil
    new_w = nil
    new_h = header.parent.height
    [new_x, new_y, new_w, new_h]
  end

  headers.add_pre_draw_proc(name) do |header, canvas, x, y, w, h, simulation|
    if simulation
      row = header.parent
      table = row.parent.parent
      header_w = table.available_w / row.elements.size
      header_w -= header.padding_left + header.padding_right
      header.dirty!
      header.text_compile(canvas, x, y, header_w, h)
    end
    [x, y, w, h]
  end

  headers.add_post_draw_proc(name) do |header, canvas, x, y, w, h, simulation|
    moved_x = header.width + header.padding_left + header.padding_right
    moved_y = header.height - header.padding_top - header.padding_bottom
    [x + moved_x, y - moved_y, w - moved_x, h + moved_y]
  end
end

match(*(all_table + [TableBody, TableRow, TableCell])) do |cells|
  name = "table-cell"
  frame_name = "table-cell-frame"
  
  params = {
    :proc_name => frame_name,
    :frame_color => @table_body_frame_color,
    :fill_color => @table_body_fill_color,
  }

  cells.prop_set("size", @normal_font_size)
  set_font_family(cells)
  cells.font(@table_cell_font_props)

  cells.padding_left = @table_cell_padding_left
  cells.padding_right = @table_cell_padding_right
  cells.padding_top = @table_cell_padding_top
  cells.padding_bottom = @table_cell_padding_bottom

  cells.delete_pre_draw_proc_by_name(frame_name)
  cells.delete_post_draw_proc_by_name(frame_name)
  cells.delete_pre_draw_proc_by_name(name)
  cells.delete_post_draw_proc_by_name(name)

  cells.each do |cell|
    orig_x = nil
    orig_y = nil
    orig_w = nil
    orig_h = nil
    cell_w = nil
    width_not_set = true

    row = cell.parent
    table = row.parent.parent
    
    cell.add_pre_draw_proc(name) do |canvas, x, y, w, h, simulation|
      orig_x = x
      orig_y = y
      orig_w = w
      orig_h = h
      if simulation
        cell_w = table.available_w / row.elements.size
        base_y = y + cell.padding_top
        text_width = cell_w - cell.padding_left - cell.padding_right

        if cell.layout.nil? or cell.width > text_width
          cell.dirty!
          cell.text_compile(canvas, x, base_y, text_width, h)
        end
      end
      [x, y, w, h]
    end

    cell.add_post_draw_proc(name) do |canvas, x, y, w, h, simulation|
      [orig_x + cell_w, orig_y, orig_w - cell_w, orig_h]
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
