require "forwardable"

module Rabbit

  class Front

    extend Forwardable
  
    def_delegators(:@canvas, :title, :slide_title)

    attr_reader :image_type
    
    def initialize(canvas)
      @canvas = canvas
      @image_type = "png"
      @mutex = Mutex.new
      reset
    end

    def current_page_image
      update_images_if_need
      @images[@canvas.current_index]
    end

    def total_page_number
      @canvas.slide_size
    end

    def current_page_number
      @canvas.current_index + 1
    end

    private
    def update_images_if_need
      @mutex.synchronize do
        reset if dirty?
        index = @canvas.current_index
        if @images[index].nil?
          pixbuf = @off_screen_canvas.to_pixbuf(index)
          @images[index] = pixbuf.save_to_buffer(@image_type)
          synchronize
        end
      end
    end

    def dirty?
      @last_modified < @canvas.last_modified
    end

    def synchronize
      @last_modified = @canvas.last_modified
    end

    def reset
      @off_screen_canvas = @canvas.off_screen_canvas
      @images = []
      @last_modified = @canvas.last_modified
    end
  end
end
