require "forwardable"

module Rabbit

  class Front

    extend Forwardable
  
    def_delegators(:@canvas, :title, :slide_title)

    attr_reader :image_type
    
    def initialize(canvas)
      @canvas = canvas
      @last_modified = Time.at(0)
      @images = []
      @image_type = "png"
      @mutex = Mutex.new
    end

    def current_page_image
      update_images_if_need
      @images[@canvas.current_index]
    end

    def total_page_number
      update_images_if_need
      @images.size
    end

    def current_page_number
      update_images_if_need
      @canvas.current_index + 1
    end

    private
    def update_images_if_need
      @mutex.synchronize do
        if dirty?
          @images = []
          @canvas.each_slide_pixbuf do |pixbuf, slide_number|
            @images << pixbuf.save_to_buffer(@image_type)
          end
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
