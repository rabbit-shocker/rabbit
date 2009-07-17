require 'gtk2'

module Rabbit
  module Comment
    class Log
      COMMENT_COLUMN = 0

      def initialize
        init_window
      end

      def widget
        @window
      end

      def show
        @window.show_all
      end

      def hide
        @window.hide
      end

      def showing?
        @window.visible?
      end

      def reset(canvas)
        @model.clear
        canvas.slides[1..-1].each do |slide|
          iter = @model.prepend
          iter.set_value(COMMENT_COLUMN, slide.headline.text)
        end
      end

      def font_size=(new_size)
        @renderer_comment.size = new_size
      end

      def header_height
        _, _, _, height = @column_comment.cell_size
        height
      end

      private
      def init_model
        @model = Gtk::ListStore.new(String)
      end

      def init_view
        init_model
        @view = Gtk::TreeView.new(@model)
        @view.can_focus = false
        @view.rules_hint = true
        @renderer_comment = Gtk::CellRendererText.new
        args = [
          _("comment"),
          @renderer_comment,
          {"text" => COMMENT_COLUMN}
        ]
        @column_comment = Gtk::TreeViewColumn.new(*args)
        @view.append_column(@column_comment)
      end

      def init_window
        init_view
        @window = Gtk::ScrolledWindow.new
        @window.set_policy(Gtk::POLICY_AUTOMATIC,
                           Gtk::POLICY_AUTOMATIC)
        @window.add(@view)
      end
    end
  end
end
