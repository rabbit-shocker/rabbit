proc_name = "show-frame"

@show_frame_color ||= "blue"

match("**") do |elems|
  elems.delete_pre_draw_proc_by_name(proc_name)
  elems.delete_post_draw_proc_by_name(proc_name)

  break if @show_frame_uninstall
  
  params = {
    :proc_name => proc_name,
    :frame_color => @show_frame_color,
  }
  
  draw_frame(elems, params)
end
