require 'rabbit/canvas'

module Rabbit
  class CommentCanvas < Canvas
    def quit
      toggle_comment_view
    end

    def saved_image_base_name
      super + "_comment"
    end

    def filename=(new_filename)
      if new_filename.nil?
        super(new_filename)
      else
        super(new_filename + "_comment")
      end
    end
  end
end
