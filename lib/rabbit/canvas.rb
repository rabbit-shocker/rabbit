# Copyright (C) 2004-2024  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require "forwardable"
require "rabbit/gtk"

require "rabbit/rabbit"
require "rabbit/filename"
require "rabbit/frame"
require "rabbit/element"
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

    INTERNAL_DPI = 96.0

    def_delegators(:@frame, :icon, :icon=, :set_icon)
    def_delegators(:@frame, :icon_list, :icon_list=, :set_icon_list)
    def_delegators(:@frame, :update_title)
    def_delegators(:@frame, :toggle_fullscreen, :fullscreen?)
    def_delegators(:@frame, :iconify, :window)
    def_delegators(:@frame, :fullscreen_available?)
    def_delegators(:@frame, :iconify_available?)
    def_delegators(:@frame, :toggle_terminal)
    def_delegators(:@frame, :in_terminal?)

    def_delegators(:@renderer, :width, :height)
    # Deprecated. Use #base_width= instead.
    def_delegator(:@renderer, :base_width=, :width=)
    # Deprecated. Use #base_height= instead.
    def_delegator(:@renderer, :base_height=, :height=)
    def_delegators(:@renderer, :base_width, :base_height)
    def_delegators(:@renderer, :base_width=, :base_height=)
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
    def_delegators(:@renderer, :offscreen_canvas)
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
    def_delegators(:@renderer, :draw_lines)
    def_delegators(:@renderer, :draw_polygon)
    def_delegators(:@renderer, :draw_circle, :draw_layout, :draw_pixbuf)
    def_delegators(:@renderer, :draw_arc_by_radius, :draw_circle_by_radius)
    def_delegators(:@renderer, :draw_slide, :draw_polygon, :draw_flag)
    def_delegators(:@renderer, :draw_rounded_rectangle)
    def_delegators(:@renderer, :draw_rsvg_handle)
    def_delegators(:@renderer, :draw_poppler_page)
    def_delegators(:@renderer, :draw_link)

    def_delegators(:@renderer, :gl_compile, :gl_call_list)
    def_delegators(:@renderer, :new_list_id)

    def_delegators(:@renderer, :z_far, :z_view)

    def_delegators(:@renderer, :flag_size)

    def_delegators(:@renderer, :create_pango_context, :pango_context=)

    def_delegators(:@renderer, :confirm)

    def_delegators(:@renderer, :display?, :printable?)

    def_delegators(:@renderer, :whiteouting?, :blackouting?)
    def_delegators(:@renderer, :toggle_whiteout, :toggle_blackout)

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

    def_delegators(:@renderer, :draw_scaled_image, :draw_scaled_image=)

    def_delegators(:@source, :source=, :reset, :base)

    attr_reader :logger, :renderer, :last_modified
    attr_reader :comments, :actions

    attr_writer :saved_image_base_name
    attr_writer :use_gl, :allotted_time

    attr_accessor :saved_image_type, :rss_base_uri
    attr_accessor :output_html, :output_index_html
    attr_accessor :source_filename
    attr_accessor :migemo_dictionary_search_path, :migemo_dictionary_name
    attr_accessor :font_resolution_ratio
    attr_accessor :max_n_comments, :comment_theme

    def initialize(logger, renderer)
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
      @rss_base_uri = nil
      @source_filename = nil
      @migemo_dictionary_search_path = []
      @migemo_dictionary_name = nil
      @limit_time = nil
      @use_gl = false
      @font_resolution_ratio = 1
      @max_n_comments = 100
      @allotted_time = nil
      @comment_theme = nil
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

    def attach_to(frame, window, container=nil, &block)
      @frame = frame if frame
      @renderer.attach_to(window, container, &block) if window
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

    def slide_text(index=current_index)
      return "" if slides.empty?
      slide = slides[index]
      return "" if slide.nil?
      slide.text.strip
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
      slide.index = slides.size
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

    def parse(source=nil, progress=nil, &block)
      id = Object.new.__id__
      @parse_request_queue.push(id)
      @source = source || @source
      begin
        index = current_index
        current_allotted_time = allotted_time
        keep_index do
          @renderer.pre_parse
          clear
          Parser.parse(self, @source, progress: progress)
          new_allotted_time = allotted_time
          reset_timer if new_allotted_time != current_allotted_time
          apply_timer
          set_current_index(index)
          reload_theme do
            if @parse_request_queue.last != id
              raise ParseFinish
            end
            progress.call if progress
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
          logger.warn($!.message)
        end
      ensure
        @parse_request_queue.delete_if {|x| x == id}
      end
    end

    def reload_source(progress=nil, &block)
      if need_reload_source?
        parse(nil, progress, &block)
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
        generator.pdf_filename = filename if /\.pdf\z/i =~ filename.to_s
        generator.source_filename = @source_filename
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

    def pre_terminal
      @renderer.pre_terminal
    end

    def post_terminal
      @renderer.post_terminal
    end

    def saved_image_base_name
      if @saved_image_base_name
        base_name = @saved_image_base_name.encode("UTF-8")
      else
        base_name = Filename.sanitize(title)
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
        old_index = slide.drawing_index
        slide.move_to_next
        Action.update_status(self)
        @renderer.post_move_in_slide(old_index, slide.drawing_index)
      else
        move_to_next_slide_if_can
      end
    end

    def move_to_next_slide_if_can
      move_to_if_can(current_index + 1)
    end

    def move_to_previous_if_can
      slide = current_slide
      if slide and !slide.first?
        old_index = slide.drawing_index
        slide.move_to_previous
        Action.update_status(self)
        @renderer.post_move_in_slide(old_index, slide.drawing_index)
      else
        move_to_previous_slide_if_can
      end
    end

    def move_to_previous_slide_if_can
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

    def have_previous?
      return true if have_previous_slide?
      return !current_slide.first? if current_slide
      false
    end

    def have_next_slide?
      slide_size - 1 > current_index
    end

    def have_next?
      return true if have_next_slide?
      return !current_slide.last? if current_slide
      false
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
          @auto_redraw_timer = nil
          GLib::Source::REMOVE
        else
          activate("Redraw")
          GLib::Source::CONTINUE
        end
      end
      @auto_redraw_timer = timer
    end

    def stop_auto_redraw_timer
      if @auto_redraw_timer
        GLib::Source.remove(@auto_redraw_timer)
        @auto_redraw_timer = nil
      end
    end

    def on_comment(name, &callback)
      @on_comment_callbacks << [name, callback]
    end

    def delete_on_comment_proc_by_name(name)
      @on_comment_callbacks.reject! do |callback_name, callback|
        callback_name == name
      end
    end

    def append_comment(comment)
      @comments << comment
      @comments.shift if @comments.size > @max_n_comments
      @on_comment_callbacks.each do |name, callback|
        begin
          callback.call(comment)
        rescue
          logger.error($!)
        end
      end
      true
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
        logger.warn(_("Unknown action: %s") % name)
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
      time = @allotted_time
      if time.nil? and title_slide
        time = title_slide.allotted_time
        if time.nil?
          start_time = title_slide.start_time
          end_time = title_slide.end_time
          if start_time and end_time
            start_time = Time.parse(start_time)
            end_time = Time.parse(end_time)
            time = end_time - start_time
          end
        end
      end
      Utils.ensure_time(time)
    end

    def start_timer(limit)
      @limit_time = Time.now + limit
    end

    def rest_time
      @limit_time ? @limit_time - Time.now : nil
    end

    def reset_timer
      @limit_time = nil
      reload_theme
    end

    def apply_timer
      return unless title_slide

      start_time = title_slide.start_time
      end_time = title_slide.end_time
      return if start_time.nil?
      return if end_time.nil?

      start_time = Time.parse(start_time)
      end_time = Time.parse(end_time)
      return unless (start_time..end_time).cover?(Time.now)

      @allotted_time = end_time - start_time
      @limit_time = end_time
    end

    def font_resolution
      INTERNAL_DPI * @font_resolution_ratio
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
      ensure
        @processing = false
        Action.update_status(self)
      end
    end

    def modified
      @last_modified = Time.now
    end

    def clear
      clear_comments
      stop_auto_redraw_timer
      clear_slides
      clear_index_slides
      modified
    end

    def clear_comments
      @comments = []
      @on_comment_callbacks = []
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
      @font_resolution_ratio = 1
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
      old_index = current_index
      set_current_index(index)
      Action.update_status(self)
      @renderer.post_move(old_index, current_index)
    end
  end
end
