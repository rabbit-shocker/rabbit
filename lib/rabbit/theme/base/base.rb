@huge_font_size = screen_size(11 * Pango::SCALE)
@large_font_size = screen_size(7 * Pango::SCALE)
@normal_font_size = screen_size(4.5 * Pango::SCALE)
@small_font_size = screen_size(4 * Pango::SCALE)
@x_small_font_size = screen_size(3.5 * Pango::SCALE)
@xx_small_font_size = screen_size(3 * Pango::SCALE)

@huge_script_font_size = @huge_font_size / 2
@large_script_font_size = @large_font_size / 2
@script_font_size = @normal_font_size / 2

@space = screen_y(2)

@left_margin = screen_x(3)
@right_margin = screen_x(3)
@top_margin = screen_y(3)
@bottom_margin = screen_y(3)

@preformatted_frame_color = "#55003dff0eff"
@preformatted_fill_color = "#fcfae2"

@preformatted_left_padding = screen_x(5)
@preformatted_right_padding = screen_x(5)
@preformatted_top_padding = screen_y(2)
@preformatted_bottom_padding = screen_y(2)

@table_frame_color = "#55003dff0eff"
@table_fill_color = "#fcfae2"

@table_left_padding = screen_x(5) * 0
@table_right_padding = screen_x(5) * 0
@table_top_padding = screen_y(2) * 0
@table_bottom_padding = screen_y(2) * 0

@table_head_frame_color = "#55003dff0eff"
@table_body_frame_color = "#55003dff0eff"
@table_head_fill_color = "#eeedcd"
@table_body_fill_color = nil

@table_cell_left_padding = screen_x(2)
@table_cell_right_padding = screen_x(2)
@table_cell_top_padding = screen_y(0.5)
@table_cell_bottom_padding = screen_y(0.5)

@table_header_left_padding = screen_x(2) * 0
@table_header_right_padding = screen_x(2) * 0
@table_header_top_padding = screen_y(0.5)
@table_header_bottom_padding = screen_y(0.5)

@image_with_frame = nil
@image_caption_space = screen_y(1)
@image_frame_color = "black"
@image_frame_shadow_color = "gray"
@image_frame_padding = screen_size(1)
@image_frame_shadow_width = 4
@image_frame_shadow_offset = 2


@font_family = nil
@monospace_font_family = nil

unless windows?
  sans_families = font_families.grep(/Sans/i)
  unless sans_families.empty?
    if sans_families.include?("Sans")
      @font_family = "Sans"
    else
      @font_family = sans_families.first
    end
  end

  monospace_families = font_families.grep(/Monospace/i)
  unless monospace_families.empty?
    if monospace_families.include?("Monospace")
      @monospace_font_family = "Monospace"
    else
      @monospace_font_family = monospace_families.first
    end
  end
end
