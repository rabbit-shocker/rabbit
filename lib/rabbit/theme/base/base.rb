@huge_font_size = screen_size(11 * Pango::SCALE)
@large_font_size = screen_size(7 * Pango::SCALE)
@normal_font_size = screen_size(4.5 * Pango::SCALE)
@small_font_size = screen_size(4 * Pango::SCALE)
@x_small_font_size = screen_size(3.5 * Pango::SCALE)
@xx_small_font_size = screen_size(3 * Pango::SCALE)

@huge_script_font_size = @huge_font_size / 2
@large_script_font_size = @large_font_size / 2
@script_font_size = @normal_font_size / 2

@left_margin = screen_size(3)
@right_margin = screen_size(3)
@top_margin = screen_size(3)
@bottom_margin = screen_size(3)

@preformatted_left_margin = screen_size(5)
@preformatted_right_margin = screen_size(5)
@preformatted_top_margin = screen_size(2)
@preformatted_bottom_margin = screen_size(2)

sans_families = font_families.grep(/Sans/i)
unless sans_families.empty?
  if sans_families.include?("Sans")
    @default_font_family = "Sans"
  else
    @default_font_family = sans_families.first
  end
end
