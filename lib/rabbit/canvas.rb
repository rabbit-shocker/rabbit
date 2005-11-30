require "forwardable"
require "gtk2"
require "rd/rdfmt"

require "rabbit/rabbit"
require 'rabbit/frame'
require 'rabbit/renderer'
require 'rabbit/element'
require "rabbit/rd2rabbit-lib"
require "rabbit/theme/manager"
require "rabbit/index"
require "rabbit/front"
require "rabbit/html/generator"

module Rabbit

  class Canvas
    
    include Enumerable
    extend Forwardable

    include GetText
    
    def_delegators(:@frame, :icon, :icon=, :set_icon)
    def_delegators(:@frame, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@frame, :update_title)
    def_delegators(:@frame, :toggle_fullscreen, :fullscreen?)
    def_delegators(:@frame, :iconify, :window)
    def_delegators(:@frame, :fullscreen_available?)
    def_delegators(:@frame, :iconify_available?)

    def_delegators(:@renderer, :width, :height)
    def_delegators(:@renderer, :width=, :height=)
    def_delegators(:@renderer, :paper_width, :paper_height)
    def_delegators(:@renderer, :paper_width=, :paper_height=)
    def_delegators(:@renderer, :margin_left, :margin_left=)
    def_delegators(:@renderer, :margin_right, :margin_right=)
    def_delegators(:@renderer, :margin_top, :margin_top=)
    def_delegators(:@renderer, :margin_bottom, :margin_bottom=)
    def_delegators(:@renderer, :margin_page_left, :margin_page_left=)
    def_delegators(:@renderer, :margin_page_right, :margin_page_right=)
    def_delegators(:@renderer, :margin_page_top, :margin_page_top=)
    def_delegators(:@renderer, :margin_page_bottom, :margin_page_bottom=)
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
    def_delegators(:@renderer, :draw_rounded_rectangle)

    def_delegators(:@renderer, :draw_cube, :draw_sphere, :draw_cone)
    def_delegators(:@renderer, :draw_torus, :draw_tetrahedron)
    def_delegators(:@renderer, :draw_octahedron, :draw_dodecahedron)
    def_delegators(:@renderer, :draw_icosahedron, :draw_teapot)

    def_delegators(:@renderer, :gl_compile, :gl_call_list)
    def_delegators(:@renderer, :new_list_id)
    
    def_delegators(:@renderer, :z_far, :z_view)
    
    def_delegators(:@renderer, :to_attrs, :flag_size)
    
    def_delegators(:@renderer, :create_pango_context, :pango_context=)
    def_delegators(:@renderer, :confirm_quit)

    def_delegators(:@renderer, :white_outing?, :black_outing?)
    def_delegators(:@renderer, :toggle_white_out, :toggle_black_out)

    def_delegators(:@renderer, :showing_comment_frame?, :showing_comment_view?)
    def_delegators(:@renderer, :toggle_comment_frame, :toggle_comment_view)
    def_delegators(:@renderer, :comment_frame_available?)
    def_delegators(:@renderer, :comment_view_available?)

    def_delegators(:@renderer, :adjustment_x, :adjustment_x=)
    def_delegators(:@renderer, :adjustment_y, :adjustment_y=)
    def_delegators(:@renderer, :reset_adjustment)

    def_delegators(:@renderer, :post_init_gui)
    
    def_delegators(:@renderer, :keys)
    
    def_delegators(:@source, :source=, :reset)
    
    attr_reader :logger, :renderer, :last_modified
    attr_reader :comment_source
    
    attr_writer :saved_image_basename

    attr_accessor :saved_image_type, :output_html


    def initialize(logger, renderer, comment_source=nil, comment_encoding=nil)
      @logger = logger
      @frame = NullFrame.new
      @theme_name = nil
      @saved_image_basename = nil
      @saved_image_type = "png"
      @processing = false
      @quitted = false
      @parse_request_queue = []
      @apply_theme_request_queue = []
      @auto_reload_thread = nil
      @output_html = false
      init_comment(comment_source, comment_encoding)
      clear
      @renderer = renderer.new(self)
    end

    def quitted?
      @quitted
    end

    def parsing?
      not @parse_request_queue.empty?
    end

    def applying?
      not @apply_theme_request_queue.empty?
    end

    def quit
      @quitted = true
      @frame.quit
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

    def change_current_index(new_index)
      mode = @index_mode
      if mode
        index = @index_current_index
        @index_current_index = new_index
      else
        index = @current_index
        @current_index = new_index
      end
      if 0 <= current_index and current_index < slide_size
        yield
      end
    ensure
      if mode
        @index_current_index = index
      else
        @current_index = index
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

    def apply_theme(name=nil, &block)
      _apply_theme(name, Object.new.__id__, &block)
    end
    
    def theme_name
      @theme_name || default_theme || "default"
    end

    def merge_theme(name)
      unless @slides.empty?
        manager = Theme::Manager.new(self)
        manager.apply(name)
        @renderer.post_apply_theme
      end
    end

    def reload_theme(&block)
      apply_theme(@theme_name, &block)
    end

    def parse_rd(source=nil, callback=nil, &block)
      _parse_rd(source, Object.new.__id__, callback, &block)
    end

    def reload_source(callback=nil, &block)
      if need_reload_source?
        parse_rd(nil, callback, &block)
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
        if @output_html
          generator = HTML::Generator.new(self)
        end
        file_name_format =
          "#{saved_image_basename}%0#{number_of_places(slide_size)}d.%s"
        each_slide_pixbuf do |pixbuf, slide_number|
          image_file_name = file_name_format % [slide_number, @saved_image_type]
          pixbuf.save(image_file_name, normalized_saved_image_type)
          if @output_html
            generator.save(file_name_format, slide_number, @saved_image_type)
          end
        end
      end
    end

    def print(&block)
      process do
        @renderer.print(&block)
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
      current_index
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

    def source_force_modified(force_modified)
      prev = @source.force_modified
      @source.force_modified = force_modified
      yield @source
      @source.force_modified = prev
    end

    def source
      @source.read
    end

    def first_slide?
      current_index.zero?
    end
    
    def last_slide?
      slide_size.zero? or current_index == (slide_size - 1)
    end

    def have_previous_slide?
      0 < current_index
    end

    def have_next_slide?
      slide_size - 1 > current_index
    end
    
    def cache_all_slides
      process do
        @renderer.cache_all_slides
      end
    end

    def processing?
      @processing
    end

    def start_auto_reload_thread(interval)
      stop_auto_reload_thread
      thread = Thread.new do
        loop do
          sleep(interval)
          break if quitted? or thread[:stop]
          redraw
        end
      end
      @auto_reload_thread = thread
    end

    def stop_auto_reload_thread
      @auto_reload_thread[:stop] = true if @auto_reload_thread
    end

    def append_comment(comment)
      comment = prepare_comment(comment)
      comment = "\n= #{comment}" if /\A\s*\z/ !~ comment
      prev_source = @comment_source.read
      @comment_source.source = "#{prev_source}#{comment}"
      @renderer.update_comment(@comment_source) do |error|
        @comment_source.source = prev_source
        if block_given?
          yield(error)
        else
          logger.warn(error)
        end
        return false
      end
      true
    end

    def comments
      comments = @comment_source.read.split(/^=\s*(?=[^=]+$)/)[2..-1]
      comments.collect do |comment|
        comment.strip
      end
    end
    
    def title_slide
      @slides.find{|x| x.is_a?(Element::TitleSlide)}
    end

    private
    def _apply_theme(name, id, &block)
      @theme_name = name if name
      _theme_name = name || theme_name
      if _theme_name and not @slides.empty?
        @apply_theme_request_queue.push(id)
        begin
          clear_theme
          clear_index_slides
          manager = Theme::Manager.new(self) do
            if @apply_theme_request_queue.last != id
              raise ApplyFinish
            end
            block.call if block
          end
          manager.apply(_theme_name)
          @renderer.post_apply_theme
        rescue ApplyFinish
        ensure
          @apply_theme_request_queue.delete_if {|x| x == id}
        end
      end
    end

    def _parse_rd(source, id, callback, &block)
      @parse_request_queue.push(id)
      @source = source || @source
      begin
        index = current_index
        keep_index do
          @renderer.pre_parse_rd
          tree = RD::RDTree.new("=begin\n#{@source.read}\n=end\n")
          clear
          visitor = RD2RabbitVisitor.new(self)
          visitor.visit(tree)
          set_current_index(index)
          reload_theme do
            if @parse_request_queue.last != id
              raise ParseFinish
            end
            callback.call if callback
          end
          @renderer.post_parse_rd
          index = current_index
        end
        set_current_index(index)
      rescue ParseFinish
      rescue Racc::ParseError
        if block_given?
          yield($!)
        else
          logger.warn($!.message)
        end
      ensure
        @parse_request_queue.delete_if {|x| x == id}
      end
    end

    def process
      if @processing
        @logger.info(_("processing..."))
        return
      end
      begin
        @processing = true
        yield
      rescue Exception
        puts $!.class
        puts $!
        puts $@
        raise
      ensure
        @processing = false
      end
    end
    
    def modified
      @last_modified = Time.now
    end
    
    def clear
      stop_auto_reload_thread
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
      @renderer.clear_theme
      modified
    end

    def keep_index
      index = @current_index
      index_index = @index_current_index
      yield
      @current_index = index
      @index_current_index = index_index
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

    def init_comment(comment_source, comment_encoding)
      args = [comment_encoding, logger, comment_source]
      @comment_source = Source::Memory.new(*args)
      if /^=\s*[^=]+$/ !~ @comment_source.read
        @comment_source.source = default_comment_source
      end
    end

    def default_comment_source
      source = "= " + _("Comment") + "\n"
      source << ": theme\n"
      source << "   comment\n"
      source
    end

    def prepare_comment(comment)
      comment.to_s.gsub(/\r?\n/, '')
    end
   
  end

  class CommentCanvas < Canvas
    def quit
      toggle_comment_view
    end

    def saved_image_basename
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
