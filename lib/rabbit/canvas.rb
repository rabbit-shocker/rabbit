require "forwardable"
require "gtk2"
require "rd/rdfmt"

require "rabbit/rabbit"
require 'rabbit/frame'
require 'rabbit/renderer'
require 'rabbit/element'
require "rabbit/parser"
require "rabbit/theme/manager"
require "rabbit/front"
require "rabbit/action"
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
    def_delegators(:@renderer, :page_margin_left, :page_margin_left=)
    def_delegators(:@renderer, :page_margin_right, :page_margin_right=)
    def_delegators(:@renderer, :page_margin_top, :page_margin_top=)
    def_delegators(:@renderer, :page_margin_bottom, :page_margin_bottom=)
    def_delegators(:@renderer, :slides_per_page, :slides_per_page=)
    def_delegators(:@renderer, :font_families)
    def_delegators(:@renderer, :redraw, :clear_slide)
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

    def_delegators(:@renderer, :make_color, :make_layout)
    def_delegators(:@renderer, :translate_context, :rotate_context)
    def_delegators(:@renderer, :scale_context, :reflect_context)
    def_delegators(:@renderer, :shear_context)
    def_delegators(:@renderer, :save_context, :restore_context)
    def_delegators(:@renderer, :draw_line, :draw_rectangle, :draw_arc)
    def_delegators(:@renderer, :draw_circle, :draw_layout, :draw_pixbuf)
    def_delegators(:@renderer, :draw_arc_by_radius, :draw_circle_by_radius)
    def_delegators(:@renderer, :draw_slide, :draw_polygon, :draw_flag)
    def_delegators(:@renderer, :draw_rounded_rectangle)
    def_delegators(:@renderer, :draw_rsvg_handle, :rsvg_available?)
    def_delegators(:@renderer, :draw_poppler_page, :poppler_available?)

    def_delegators(:@renderer, :draw_cube, :draw_sphere, :draw_cone)
    def_delegators(:@renderer, :draw_torus, :draw_tetrahedron)
    def_delegators(:@renderer, :draw_octahedron, :draw_dodecahedron)
    def_delegators(:@renderer, :draw_icosahedron, :draw_teapot)

    def_delegators(:@renderer, :gl_compile, :gl_call_list)
    def_delegators(:@renderer, :new_list_id)
    
    def_delegators(:@renderer, :z_far, :z_view)
    
    def_delegators(:@renderer, :to_attrs, :flag_size)
    
    def_delegators(:@renderer, :create_pango_context, :pango_context=)

    def_delegators(:@renderer, :confirm)

    def_delegators(:@renderer, :display?, :printable?)
    def_delegators(:@renderer, :x_dpi, :y_dpi)
    
    def_delegators(:@renderer, :white_outing?, :black_outing?)
    def_delegators(:@renderer, :toggle_white_out, :toggle_black_out)

    def_delegators(:@renderer, :showing_comment_frame?, :showing_comment_view?)
    def_delegators(:@renderer, :toggle_comment_frame, :toggle_comment_view)
    def_delegators(:@renderer, :comment_frame_available?)
    def_delegators(:@renderer, :comment_view_available?)

    def_delegators(:@renderer, :adjustment_x, :adjustment_x=)
    def_delegators(:@renderer, :adjustment_y, :adjustment_y=)
    def_delegators(:@renderer, :reset_adjustment)

    def_delegators(:@renderer, :graffiti_mode?, :have_graffiti?)
    def_delegators(:@renderer, :can_undo_graffiti?, :toggle_graffiti_mode)
    def_delegators(:@renderer, :clear_graffiti, :undo_graffiti)
    def_delegators(:@renderer, :change_graffiti_color)
    def_delegators(:@renderer, :graffiti_color, :graffiti_color=)
    def_delegators(:@renderer, :graffiti_line_width, :graffiti_line_width=)

    def_delegators(:@renderer, :toggle_info_window)
    def_delegators(:@renderer, :toggle_spotlight, :toggle_magnifier)

    def_delegators(:@renderer, :add_gesture_action)

    def_delegators(:@renderer, :post_init_gui)

    def_delegators(:@renderer, :connect_key, :disconnect_key)

    def_delegators(:@renderer, :expand_hole, :narrow_hole)

    def_delegators(:@renderer, :search_slide, :stop_slide_search, :searching?)

    def_delegators(:@source, :source=, :reset, :base)

    attr_reader :logger, :renderer, :last_modified
    attr_reader :comment_source, :actions
    
    attr_writer :saved_image_base_name
    attr_writer :use_gl

    attr_accessor :saved_image_type, :rss_base_uri
    attr_accessor :output_html, :output_index_html
    attr_accessor :migemo_dictionary_search_path, :migemo_dictionary_name


    def initialize(logger, renderer, comment_source=nil, comment_encoding=nil)
      @logger = logger
      @frame = NullFrame.new
      @theme_name = nil
      @saved_image_base_name = nil
      @saved_image_type = "png"
      @processing = false
      @quitted = false
      @parse_request_queue = []
      @apply_theme_request_queue = []
      @auto_redraw_timer = nil
      @output_html = false
      @rss_base_uri = true
      @migemo_dictionary_search_path = []
      @migemo_dictionary_name = nil
      @limit_time = nil
      @use_gl = false
      init_comment(comment_source, comment_encoding)
      clear
      @renderer = renderer.new(self)
      @actions = Action.action_group(self)
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

    def use_gl?
      @use_gl
    end

    def quit
      @quitted = true
      @frame.quit
    end
    
    def front(public_level=nil)
      Front.new(self, public_level)
    end

    def attach_to(frame, window)
      @frame = frame if frame
      @renderer.attach_to(window) if window
    end
    
    def detach
      @frame = NullFrame.new
      @renderer.detach
    end
    
    def title
      ts = title_slide
      if ts
        ts.title
      else
        "Rabbit"
      end
    end

    def slide_title(index=current_index)
      return "" if slides.empty?
      slide = slides[index]
      return title if slide.nil?
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

    def find_slide(pattern)
      slides.find {|slide| slide.match?(pattern)}
    end

    def slide_index(pattern)
      slides.each_with_index do |slide, i|
        return i if slide.match?(pattern)
      end
      nil
    end

    def slide_indexes(pattern)
      indexes = []
      slides.each_with_index do |slide, i|
        indexes << i if slide.match?(pattern)
      end
      indexes
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

    def parse(source=nil, callback=nil, &block)
      id = Object.new.__id__
      @parse_request_queue.push(id)
      @source = source || @source
      begin
        index = current_index
        keep_index do
          @renderer.pre_parse
          clear
          Parser.parse(self, @source)
          set_current_index(index)
          reload_theme do
            if @parse_request_queue.last != id
              raise ParseFinish
            end
            callback.call if callback
          end
          @renderer.post_parse
          index = current_index
        end
        set_current_index(index)
      rescue ParseFinish
      rescue ParseError, UnsupportedFormatError
        if block_given?
          yield($!)
        else
          logger.warning($!.message)
        end
      ensure
        @parse_request_queue.delete_if {|x| x == id}
      end
    end

    def reload_source(callback=nil, &block)
      if need_reload_source?
        parse(nil, callback, &block)
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
      current_slide.flush
      @renderer.to_pixbuf(current_slide)
    end
    
    def save_as_image
      process do
        generator = HTML::Generator.new(self,
                                        saved_image_base_name,
                                        @saved_image_type,
                                        @output_html,
                                        @output_index_html,
                                        @rss_base_uri)
        generator.save
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

    def saved_image_base_name
      if @saved_image_base_name
        base_name = GLib.filename_to_utf8(@saved_image_base_name)
      else
        base_name = title.dup
      end
      base_name << "-index" if index_mode?
      base_name
    end

    def move_to_if_can(index)
      if index and 0 <= index and index < slide_size
        move_to(index)
      end
      current_index
    end

    def move_to_next_if_can
      slide = current_slide
      if slide and !slide.last?
        slide.move_to_next
        activate("Redraw")
      else
        move_to_if_can(current_index + 1)
      end
    end

    def move_to_previous_if_can
      slide = current_slide
      if slide and !slide.first?
        slide.move_to_previous
        activate("Redraw")
      else
        move_to_if_can(current_index - 1)
      end
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
      @renderer.pre_toggle_index_mode
      process do
        if @index_mode
          @index_mode = false
          @renderer.index_mode_off
        else
          update_index_slides
          @index_mode = true
          @renderer.index_mode_on
        end
        modified
      end
      @renderer.post_toggle_index_mode
    end

    def update_index_slides
      if @index_slides.empty?
        @index_slides = Element::IndexSlide.make_index_slides(self)
      end
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

    def start_auto_redraw_timer(interval)
      stop_auto_redraw_timer
      timer = GLib::Timeout.add(interval * 1000) do
        if quitted?
          false
        else
          activate("Redraw")
          true
        end
      end
      @auto_redraw_timer = timer
    end

    def stop_auto_redraw_timer
      if @auto_redraw_timer
        if GLib::Source.respond_to?(:remove)
          GLib::Source.remove(@auto_redraw_timer)
        else
          Gtk.timeout_remove(@auto_redraw_timer)
        end
        @auto_redraw_timer = nil
      end
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
          logger.warning(error)
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

    def activate(name, &block)
      act = action(name)
      if act and act.sensitive?
        act.activate(&block)
        true
      else
        false
      end
    end

    def action(name)
      act = @actions.get_action(name)
      if act
        act
      else
        logger.warning(_("Unknown action: %s") % name)
        false
      end
    end

    def with_index_mode(new_value)
      current_index_mode = @index_mode
      @index_mode = new_value
      update_index_slides if @index_mode
      yield
    ensure
      @index_mode = current_index_mode
    end

    def allotted_time
      slide = title_slide
      slide ? slide.allotted_time : nil
    end

    def start_timer(limit)
      @limit_time = Time.now + limit
    end

    def rest_time
      @limit_time ? @limit_time - Time.now : nil
    end

    def reset_timer
      @limit_time = nil
    end

    private
    def _apply_theme(name, id, &block)
      @theme_name = name if name
      _theme_name = name || theme_name
      if _theme_name and not @slides.empty?
        @apply_theme_request_queue.push(id)
        success = false
        index_mode = @index_mode
        begin
          Action.update_status(self)
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
          success = true
        rescue ApplyFinish
        ensure
          @apply_theme_request_queue.delete_if {|x| x == id}
          Action.update_status(self)
        end
        activate("ToggleIndexMode") if success and index_mode
      end
    end

    def process
      if @processing
        @logger.info(_("Processing..."))
        return
      end
      begin
        @processing = true
        Action.update_status(self)
        yield
      rescue Exception
        puts $!.class
        puts $!
        puts $@
        raise
      ensure
        @processing = false
        Action.update_status(self)
      end
    end
    
    def modified
      @last_modified = Time.now
    end
    
    def clear
      reset_timer
      stop_auto_redraw_timer
      clear_slides
      clear_index_slides
      modified
    end
    
    def clear_slides
      @current_index = 0
      @slides = []
    end
    
    def clear_index_slides
      activate("ToggleIndexMode") if @index_mode
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
      index_mode = @index_mode
      index = @current_index
      index_index = @index_current_index
      yield
      @current_index = index
      @index_current_index = index_index
      @index_mode = index_mode
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

    def move_to(index)
      set_current_index(index)
      Action.update_status(self)
      @renderer.post_move(current_index)
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
end
