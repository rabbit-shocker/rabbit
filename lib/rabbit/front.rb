require "monitor"

require "rabbit/rabbit"
require "rabbit/utils"

module Rabbit

  class Front

    module PublicLevel
      STRICT = 0
      MOVE = 1
      READ_SIZE = 2
      CHANGE_SIZE = 4
      SIZE = READ_SIZE | CHANGE_SIZE
      READ_SOURCE = 8
      CHANGE_SOURCE = 16
      SOURCE = READ_SOURCE | CHANGE_SOURCE
      ALL = MOVE | SIZE | SOURCE
    end

    AVAILABLE_INTERFACES = []

    %w(move_to_first move_to_last move_to_next_if_can
       move_to_previous_if_can move_to_if_can).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::MOVE, false]
    end

    %w(width height paper_width paper_height).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::READ_SIZE, false]
    end

    %w(width= height= paper_width= paper_height=).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::CHANGE_SIZE, true]
    end

    %w(source).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::READ_SOURCE, false]
    end

    %w(source=).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::CHANGE_SOURCE, true]
    end

    include MonitorMixin
    
    attr_reader :image_type, :public_level, :last_modified
    
    def initialize(canvas, public_level=nil)
      super()
      @canvas = canvas
      @image_type = "png"
      @public_level = public_level || PublicLevel::STRICT
      @previous_width = @canvas.width
      @previous_height = @canvas.height
      setup_public_interface
      reset
    end

    def current_slide_image
      update_images_if_need
      @images[@canvas.current_index]
    end

    def title
      check_dirty
      @canvas.title
    end

    def slide_title
      check_dirty
      @canvas.slide_title
    end
    
    def total_slide_number
      check_dirty
      @canvas.slide_size
    end

    def current_slide_number
      check_dirty
      @canvas.current_index + 1
    end

    def first_slide?
      current_slide_number == 1
    end

    def have_next_slide?
      total_slide_number > current_slide_number
    end

    def have_previous_slide?
      1 < current_slide_number
    end

    def last_slide?
      total_slide_number == current_slide_number
    end

    def available_interfaces
      AVAILABLE_INTERFACES.collect do |name, level, need_clear_cache|
        [name, level, @canvas.method(name).arity]
      end
    end

    def version
      VERSION
    end
    
    private
    def check_dirty
      mon_synchronize do
        _check_dirty
      end
    end

    def _check_dirty
      if dirty?
        reset 
        if off_screen_canvas.need_reload_source?
          off_screen_canvas.reload_source
          synchronize
        end
      end
    end
    
    def update_images_if_need
      mon_synchronize do
        _check_dirty
        index = @canvas.current_index
        if @images[index].nil?
          prev_size = [@previous_width, @previous_height]
          current_size = [@canvas.width, @canvas.height]
          if prev_size != current_size
            off_screen_canvas.reload_theme
          end
          pixbuf = off_screen_canvas.to_pixbuf(index)
          @images[index] = pixbuf.save_to_buffer(@image_type)
          synchronize
        end
      end
    end

    def dirty?
      @dirty or
        @last_modified < @canvas.last_modified or
        off_screen_canvas.need_reload_source?
    end

    def synchronize
      @last_modified = @canvas.last_modified
      @previous_width = @canvas.width
      @previous_height = @canvas.height
    end

    def setup_public_interface
      AVAILABLE_INTERFACES.each do |name, level, need_clear_cache|
        arg_list = []
        arg_list.concat(Utils.arg_list(@canvas.method(name).arity))
        arg_str = arg_list.join(", ")
        
        if (@public_level & level).zero?
          instance_eval(<<-EOS, __FILE__, __LINE__)
            def self.#{name}(#{arg_str})
              raise NotAvailableInterfaceError.new(#{name.to_s.dump})
            end
          EOS
        else
          instance_eval(<<-EOS, __FILE__, __LINE__)
            def self.#{name}(#{arg_str})
              result = @canvas.__send__(#{[name.to_s.dump, *arg_list].join(', ')})
              #{if need_clear_cache then "dirty!" end}
              result
            end
          EOS
        end
      end
    end
    
    def reset
      @off_screen_canvas = nil
      @dirty = false
      @images = []
      @last_modified = @canvas.last_modified
    end

    def dirty!
      @dirty = true
    end

    def off_screen_canvas
      @off_screen_canvas ||= @canvas.off_screen_canvas
    end
  end
end
