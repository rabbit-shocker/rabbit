require "forwardable"

module Rabbit

  class Front

    extend Forwardable
  
    def_delegators(:@canvas, :title, :slide_title)

    attr_reader :image_type
    
    def initialize(canvas)
      @original_canvas = canvas
      @canvas = canvas.offline_screen_canvas
      @last_modified = Time.at(0)
      @images = []
      @image_type = "png"
      @mutex = Mutex.new
    end

    def current_page_image
      update_images_if_need
      @images[@original_canvas.current_index]
    end

    def total_page_number
      @original_canvas.slide_size
    end

    def current_page_number
      @original_canvas.current_index + 1
    end

    private
    def update_images_if_need
      @mutex.synchronize do
        @images = [] if dirty?
        index = @original_canvas.current_index
        if @images[index].nil?
          pixbuf = @canvas.to_pixbuf(index)
          @images[index] = pixbuf.save_to_buffer(@image_type)
          synchronized
        end
      end
    end

    def dirty?
      @last_modified < @canvas.last_modified
    end

    def synchronized
      @last_modified = @canvas.last_modified
    end
  end
end
