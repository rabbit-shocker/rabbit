require "forwardable"
require "gtk2"
require "rd/rdfmt"

require "rabbit/rabbit"
require 'rabbit/frame'
require 'rabbit/renderer'
require 'rabbit/element'
require "rabbit/rd2rabbit-lib"
require "rabbit/theme"
require "rabbit/index"
require "rabbit/front"

module Rabbit

  class Canvas
    
    include Enumerable
    extend Forwardable

    include GetText
    
    def_delegators(:@frame, :icon, :icon=, :set_icon)
    def_delegators(:@frame, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@frame, :quit, :update_title)
    def_delegators(:@frame, :toggle_fullscreen, :fullscreen?)
    def_delegators(:@frame, :iconify, :window)

    def_delegators(:@renderer, :width, :height)
    def_delegators(:@renderer, :width=, :height=)
    def_delegators(:@renderer, :paper_width, :paper_height)
    def_delegators(:@renderer, :paper_width=, :paper_height=)
    def_delegators(:@renderer, :left_margin, :left_margin=)
    def_delegators(:@renderer, :right_margin, :right_margin=)
    def_delegators(:@renderer, :top_margin, :top_margin=)
    def_delegators(:@renderer, :bottom_margin, :bottom_margin=)
    def_delegators(:@renderer, :left_page_margin, :left_page_margin=)
    def_delegators(:@renderer, :right_page_margin, :right_page_margin=)
    def_delegators(:@renderer, :top_page_margin, :top_page_margin=)
    def_delegators(:@renderer, :bottom_page_margin, :bottom_page_margin=)
    def_delegators(:@renderer, :slides_per_page, :slides_per_page=)
    def_delegators(:@renderer, :font_families)
    def_delegators(:@renderer, :destroy, :redraw)
    def_delegators(:@renderer, :cursor=, :filename, :filename=)
    def_delegators(:@renderer, :each_slide_pixbuf)
    def_delegators(:@renderer, :off_screen_canvas)
    def_delegators(:@renderer, :foreground, :background)
    def_delegators(:@renderer, :foreground=, :background=)
    def_delegators(:@renderer, :background_image, :background_image=)
    def_delegators(:@renderer, :progress_foreground)
    def_delegators(:@renderer, :progress_foreground=)
    def_delegators(:@renderer, :progress_background)
    def_delegators(:@renderer, :progress_background=)

    def_delegators(:@renderer, :make_color, :make_layout, :to_rgb)
    def_delegators(:@renderer, :draw_line, :draw_rectangle, :draw_arc)
    def_delegators(:@renderer, :draw_circle, :draw_layout, :draw_pixbuf)
    def_delegators(:@renderer, :draw_slide, :draw_polygon, :draw_flag)

    def_delegators(:@renderer, :draw_cube, :draw_sphere, :draw_cone)
    def_delegators(:@renderer, :draw_torus, :draw_tetrahedron)
    def_delegators(:@renderer, :draw_octahedron, :draw_dodecahedron)
    def_delegators(:@renderer, :draw_icosahedron, :draw_teapot)

    def_delegators(:@renderer, :to_attrs, :flag_size)
    
    def_delegators(:@renderer, :create_pango_context, :pango_context=)
    def_delegators(:@renderer, :confirm_quit)

    def_delegators(:@source, :source=)
    
    attr_reader :logger, :renderer, :theme_name, :source, :last_modified
    
    attr_writer :saved_image_basename

    attr_accessor :saved_image_type


    def initialize(logger, renderer)
      @logger = logger
      @frame = NullFrame.new
      @theme_name = nil
      @saved_image_basename = nil
      @processing = false
      clear
      @renderer = renderer.new(self)
    end

    def front(public_level=nil)
      Front.new(self, public_level)
    end

    def attach_to(frame, window)
      @frame = frame
      @renderer.attach_to(window)
    end
    
    def detach_from(frame, window)
      @frame = NullFrame.new
      @renderer.detach_from(window)
    end
    
    def title
      ts = title_slide
      if ts
        ts.title
      else
        "Rabbit"
      end
    end

    def slide_title
      return "" if slides.empty?
      slide = current_slide
      if slide.is_a?(Element::TitleSlide)
        slide.title
      else
        "#{title}: #{slide.title}"
      end
    end

    def slides
      if @index_mode
        @index_slides
      else
        @slides
      end
    end
    
    def slide_size
      slides.size
    end

    def current_slide
      slide = slides[current_index]
      if slide
        slide
      else
        move_to_first
        slides.first
      end
    end

    def current_index
      if @index_mode
        @index_current_index
      else
        @current_index
      end
    end

    def next_slide
      slides[current_index + 1]
    end

    def each(&block)
      slides.each(&block)
    end

    def <<(slide)
      slides << slide
    end

    def apply_theme(name=nil)
      _theme_name = name || theme_name
      if _theme_name and not @slides.empty?
        clear_theme
        clear_index_slides
        theme = Theme.new(self)
        theme.apply(_theme_name)
        @renderer.post_apply_theme
      end
    end

    def theme_name
      @theme_name || default_theme || "default"
    end

    def reload_theme
      apply_theme
    end

    def parse_rd(source=nil)
      @source = source || @source
      if @source.modified?
        begin
          keep_index do
            tree = RD::RDTree.new("=begin\n#{@source.read}\n=end\n")
            clear
            visitor = RD2RabbitVisitor.new(self)
            visitor.visit(tree)
            apply_theme
            @renderer.post_parse_rd
          end
        rescue Racc::ParseError
          logger.warn($!.message)
        end
      end
    end

    def reload_source
      if need_reload_source?
        parse_rd
      end
    end

    def need_reload_source?
      !@processing and @source and @source.modified?
    end

    def full_path(path)
      @source and @source.full_path(path)
    end

    def tmp_dir_name
      @source and @source.tmp_dir_name
    end

    def to_pixbuf(i)
      move_to_if_can(i)
      slide = current_slide
      slide.draw(self)
      @renderer.to_pixbuf(slide)
    end
    
    def save_as_image
      process do
        file_name_format =
          "#{saved_image_basename}%0#{number_of_places(slide_size)}d.#{@saved_image_type}"
        each_slide_pixbuf do |pixbuf, slide_number|
          file_name = file_name_format % slide_number
          pixbuf.save(file_name, normalized_saved_image_type)
        end
      end
    end

    def print
      process do
        @renderer.print
      end
    end
    
    def fullscreened
      @renderer.post_fullscreen
    end

    def unfullscreened
      @renderer.post_unfullscreen
    end

    def iconified
      @renderer.post_iconify
    end

    def saved_image_basename
      name = @saved_image_basename || GLib.filename_from_utf8(title)
      if @index_mode
        name + "_index"
      else
        name
      end
    end

    def move_to_if_can(index)
      if 0 <= index and index < slide_size
        move_to(index)
      end
    end

    def move_to_next_if_can
      move_to_if_can(current_index + 1)
    end

    def move_to_previous_if_can
      move_to_if_can(current_index - 1)
    end

    def move_to_first
      move_to_if_can(0)
    end

    def move_to_last
      move_to(slide_size - 1)
    end

    def index_mode?
      @index_mode
    end

    def toggle_index_mode
      process do
        if @index_mode
          @index_mode = false
          @renderer.index_mode_off
        else
          if @index_slides.empty?
            @index_slides = Index.make_index_slides(self)
          end
          @index_mode = true
          @renderer.index_mode_on
          move_to_first
        end
        modified
      end
      @renderer.post_toggle_index_mode
    end

    def index_mode?
      @index_mode
    end
    
    def source_force_modified(force_modified)
      prev = @source.force_modified
      @source.force_modified = force_modified
      yield @source
      @source.force_modified = prev
    end

    def source
      @source.read
    end

    def last_slide?
      slide_size.zero? or current_index == (slide_size - 1)
    end
    
    def cache_all_slides
      process do
        @renderer.cache_all_slides
      end
    end

    def processing?
      @processing
    end
    
    private
    def process
      if @processing
        @logger.info(_("processing..."))
        return
      end
      begin
        @processing = true
        yield
      ensure
        @processing = false
      end
    end
    
    def modified
      @last_modified = Time.now
    end
    
    def clear
      clear_slides
      clear_index_slides
      modified
    end
    
    def clear_slides
      @current_index = 0
      @slides = []
    end
    
    def clear_index_slides
      @index_mode = false
      @index_current_index = 0
      @index_slides = []
    end

    def clear_theme
      @slides.each do |slide|
        slide.clear_theme
      end
      modified
    end

    def keep_index
      index = @current_index
      index_index = @index_current_index
      yield
      @current_index = index
      @index_current_index = index_index
    end
    
    def title_slide
      @slides.find{|x| x.is_a?(Element::TitleSlide)}
    end

    def default_theme
      ts = title_slide
      ts and ts.theme
    end

    def set_current_index(new_index)
      if @index_mode
        @index_current_index = new_index
      else
        @current_index = new_index
      end
    end

    def with_index_mode(new_value)
      current_index_mode = @index_mode
      @index_mode = new_value
      yield
      @index_mode = current_index_mode
    end
    
    def move_to(index)
      set_current_index(index)
      @renderer.post_move(current_index)
    end

    def normalized_saved_image_type
      case @saved_image_type
      when /jpg/i
        "jpeg"
      else
        @saved_image_type.downcase
      end
    end

    def number_of_places(num)
      n = 1
      target = num
      while target >= 10
        target /= 10
        n += 1
      end
      n
    end

  end

end
