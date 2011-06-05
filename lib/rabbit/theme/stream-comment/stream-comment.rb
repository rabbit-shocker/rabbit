theme_exit unless display?

proc_name = "stream-comment"

@stream_comment_max_n_comments ||= 20
@stream_comment_n_moves_base ||= 150
@stream_comment_n_moves_fluctuation ||= 150

match(SlideElement) do |slides|
  slides.delete_post_draw_proc_by_name(proc_name)
  canvas.delete_on_comment_proc_by_name(proc_name)

  break if @stream_comment_uninstall

  stream_comments = []
  canvas.on_comment(proc_name) do |comment|
    text = Text.new(ERB::Util.h(comment.strip.gsub("\n", " ")))
    set_font_family(text)
    text.font(:size => (@normal_font_size * rand).ceil)
    text.compile(canvas, 0, 0, canvas.width, canvas.height)
    color = "\#%02x%02x%02x%02x" %
      [rand(256), rand(256), rand(256), 128 + rand(128)]
    width, height = text.layout.pixel_size
    stream_comment = {
      :text => text,
      :color => color,
      :x => canvas.width,
      :y => ((canvas.height - height) * rand).ceil,
      :terminate => false,
    }
    stream_comments << stream_comment
    if stream_comments.size > @stream_comment_max_n_comments
      oldest_stream_comments = stream_comments.shift
      oldest_stream_comments[:terminate] = true
    end

    n = @stream_comment_n_moves_base + rand(@stream_comment_n_moves_fluctuation)
    delta = (canvas.width + width) / n.to_f
    GLib::Timeout.add(1000 / 50) do
      stream_comment[:x] -= delta
      n -= 1
      stream_comment[:terminate] = true if n.zero?
      keep_callback = !stream_comment[:terminate]
      unless keep_callback
        stream_comments.reject! do |_stream_comment|
          _stream_comment == stream_comment
        end
      end
      canvas.activate("Redraw")
      keep_callback
    end
  end

  redraw_time = Time.now
  slides.add_post_draw_proc(proc_name) do |slide, canvas, x, y, w, h, simulation|
    unless simulation
      stream_comments.each do |stream_comment|
        canvas.draw_layout(stream_comment[:text].layout,
                           stream_comment[:x],
                           stream_comment[:y],
                           stream_comment[:color])
      end
    end
    [x, y, w, h]
  end
end
