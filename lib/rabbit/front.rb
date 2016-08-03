require "monitor"

require "rabbit/rabbit"
require "rabbit/utils"

module Rabbit

  class Front

    module PublicLevel
      STRICT = 0
      MOVE = 1 << 0
      READ_SIZE = 1 << 1
      CHANGE_SIZE = 1 << 2
      SIZE = READ_SIZE | CHANGE_SIZE
      READ_SOURCE = 1 << 3
      CHANGE_SOURCE = 1 << 4
      SOURCE = READ_SOURCE | CHANGE_SOURCE
      CONTROL = 1 << 5
      ALL = MOVE | SIZE | SOURCE | CONTROL
    end

    AVAILABLE_INTERFACES = []

    %w(move_to_first move_to_last
       move_to_next_if_can move_to_next_slide_if_can
       move_to_previous_if_can move_to_previous_slide_if_can
       move_to_if_can).each do |name|
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

    %w(source= reset).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::CHANGE_SOURCE, true]
    end

    %w(toggle_fullscreen toggle_index_mode toggle_whiteout
       toggle_blackout quit).each do |name|
      AVAILABLE_INTERFACES << [name, PublicLevel::CONTROL, false]
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
      clean
    end

    def current_slide_image
      update_images_if_need
      @images[@canvas.current_index]
    end

    def current_slide_text
      slide = @canvas.current_slide
      slide ? slide.text : ""
    end

    def current_slide_rd
      slide = @canvas.current_slide
      slide ? slide.to_rd : ""
    end

    def title
      @canvas.title
    end

    def slide_title
      @canvas.slide_title
    end

    def total_slide_number
      @canvas.slide_size
    end

    def current_slide_number
      @canvas.current_index
    end

    def first_slide?
      @canvas.first_slide?
    end

    def have_next_slide?
      @canvas.have_next_slide?
    end

    def have_previous_slide?
      @canvas.have_previous_slide?
    end

    def have_next?
      @canvas.have_next?
    end

    def have_previous?
      @canvas.have_previous?
    end

    def last_slide?
      @canvas.last_slide?
    end

    def available_interfaces
      AVAILABLE_INTERFACES.collect do |name, level, need_clear_cache|
        [name, level, @canvas.method(name).arity]
      end
    end

    def version
      VERSION
    end

    def accept_move?
      not (@public_level & Front::PublicLevel::MOVE).zero?
    end

    def append_comment(comment)
      GLib::Idle.add do
        @canvas.append_comment(comment)
        false
      end
      true
    end

    def comments
      @canvas.comments
    end

    private
    def check_dirty
      mon_synchronize do
        _check_dirty
      end
    end

    def _check_dirty
      if dirty?
        clean
        if offscreen_canvas.need_reload_source?
          offscreen_canvas.reload_source
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
            offscreen_canvas.reload_theme
          end
          pixbuf = offscreen_canvas.to_pixbuf(index)
          @images[index] = pixbuf.save_to_buffer(@image_type)
          synchronize
        end
      end
    end

    def dirty?
      @dirty or
        @last_modified < @canvas.last_modified or
        offscreen_canvas.need_reload_source?
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
          instance_eval(<<-EOS, __FILE__, __LINE__ + 1)
            def self.#{name}(#{arg_str})
              raise NotAvailableInterfaceError.new(#{name.to_s.dump})
            end
          EOS
        else
          send_arguments = [name.to_s.dump, *arg_list].join(', ')
          dirty_source = nil
          dirty_source = "dirty!" if need_clear_cache
          if (level & PublicLevel::CONTROL).zero?
            instance_eval(<<-EOS, __FILE__, __LINE__ + 1)
              def self.#{name}(#{arg_str})
                result = @canvas.__send__(#{send_arguments})
                #{dirty_source}
                result
              end
            EOS
          else
            instance_eval(<<-EOS, __FILE__, __LINE__ + 1)
              def self.#{name}(#{arg_str})
                GLib::Idle.add do
                  @canvas.__send__(#{send_arguments})
                  #{dirty_source}
                  false
                end
                true
              end
            EOS
          end
        end
      end
    end

    def clean
      @offscreen_canvas = nil
      @dirty = false
      @images = []
      @last_modified = @canvas.last_modified
    end

    def dirty!
      @dirty = true
    end

    def offscreen_canvas
      @offscreen_canvas ||= @canvas.offscreen_canvas
    end
  end
end
