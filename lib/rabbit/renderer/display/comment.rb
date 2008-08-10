require 'rabbit/comment-canvas'
require "rabbit/comment/log"

module Rabbit
  module Renderer
    module Display
      module Comment
        def initialize(*args, &block)
          super
          @comment_initialized = false
          init_comment_log
        end

        def showing_comment_frame?
          @comment_frame and @comment_frame.visible?
        end

        def showing_comment_view?
          @comment_log.showing?
        end

        def comment_frame_available?
          true
        end

        def comment_view_available?
          true
        end

        def toggle_comment_frame
          ensure_comment
          if showing_comment_frame?
            @comment_frame.hide
          else
            adjust_comment_frame
            @comment_frame.show
          end
          update_menu
        end

        def toggle_comment_view
          ensure_comment
          if showing_comment_view?
            @comment_log.hide
            @comment_view_frame.hide
          else
            adjust_comment_view
            @comment_log.show
            @comment_view_frame.show
            @comment_view_canvas.parse(@canvas.comment_source)
            @comment_view_canvas.move_to_last
          end
          adjust_comment_frame
        end

        def update_comment(source, &block)
          ensure_comment
          error_occurred = parse_comment(source, &block)
          unless error_occurred
            @comment_canvas.move_to_last
            @comment_log.reset(@comment_canvas)
            if @comment_view_frame.visible?
              @comment_view_frame.parse(source)
              @comment_view_canvas.move_to_last
            end
          end
        end

        def post_init_gui
          super
          @comment_log.hide
          @comment_view_frame.hide if @comment_view_frame
        end

        private
        def add_widget_to_window(window)
          super
          @vbox.pack_end(@comment_log.widget, false, false, 0)
          init_comment_view_canvas
          init_comment_view_frame
          @hbox.pack_start(@comment_view_frame.window, false, false, 0)
        end

        def parse_comment(source)
          error_occurred = false
          @comment_canvas.parse(source) do |error|
            error_occurred = true
            if block_given?
              yield(error)
            else
              @comment_canvas.logger.warn(error)
            end
          end
          error_occurred
        end

        def ensure_comment
          unless @comment_initialized
            init_comment
            @comment_initialized = true
          end
        end

        def init_comment
          init_comment_canvas
          init_comment_frame
          @comment_canvas.parse(@canvas.comment_source)
        end

        def init_comment_frame
          @comment_frame = Frame.new(@comment_canvas.logger, @comment_canvas)
          w, h = suggested_comment_frame_size
          @comment_frame.init_gui(w, h, false, Gtk::Window::POPUP)
          @comment_frame.hide
        end

        def init_comment_canvas
          @comment_canvas = Canvas.new(@canvas.logger, DrawingArea)
          @comment_canvas.use_gl = @canvas.use_gl?
        end

        def init_comment_view_frame
          args = [@comment_view_canvas.logger, @comment_view_canvas]
          @comment_view_frame = EmbedFrame.new(*args)
          @comment_view_frame.init_gui(-1, -1, false)
          @comment_view_frame.hide
        end

        def init_comment_view_canvas
          @comment_view_canvas = CommentCanvas.new(@canvas.logger,
                                                   CommentDrawingArea)
          @comment_view_canvas.saved_image_base_name =
            @canvas.saved_image_base_name
          @comment_view_canvas.filename = @canvas.filename
          @comment_view_canvas.slides_per_page = @canvas.slides_per_page
          @comment_view_canvas.use_gl = @canvas.use_gl?
        end

        def init_comment_log
          @comment_log = Rabbit::Comment::Log.new
        end

        def adjust_comment_frame(x=nil, y=nil, w=nil, h=nil)
          if @comment_initialized
            w, h = suggested_comment_frame_size(w, h)
            @comment_frame.window.set_size_request(w, h)
            Utils.move_to_bottom_left(@window, @comment_frame.window)
          end
        end

        def adjust_comment_view(x=nil, y=nil, w=nil, h=nil)
          ww, wh = suggested_comment_log_window_size(w, h)
          @comment_log.widget.set_size_request(ww, wh)
          begin
            header_height = @comment_log.header_height
          rescue TypeError
            header_height = nil
          end
          if header_height
            text_size = (wh - header_height) * 0.5
          else
            text_size = wh * 0.4
          end
          @comment_log.font_size = text_size * Pango::SCALE

          fw, fh = suggested_comment_view_frame_size(w, h)
          @comment_view_frame.set_size_request(fw, fh)
        end

        def suggested_comment_frame_size(w=nil, h=nil)
          w ||= @canvas.width
          h ||= @canvas.height
          [w / 10, h / 10]
        end

        def suggested_comment_log_window_size(w=nil, h=nil)
          h ||= @canvas.height
          [-1, h / 10]
        end

        def suggested_comment_view_frame_size(w=nil, h=nil)
          w ||= @canvas.width
          [w / 10, -1]
        end

        def configured(x, y, w, h)
          super
          adjust_comment_frame(x, y, w, h)
          adjust_comment_view(x, y, w, h)
        end
      end
    end
  end
end
