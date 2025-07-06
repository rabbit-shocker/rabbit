# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "icon"

module Rabbit
  Action = Struct.new(:name,
                      :parameter_type,
                      :initial_state,
                      :update_state,
                      :update_enabled,
                      :label,
                      :icon,
                      :icon_name,
                      :activate) do
    def update
      gaction.state = ensure_variant(update_state.call) if update_state
      gaction.enabled = update_enabled.call if update_enabled
    end

    def gaction
      @gaction ||= build_gaction
    end

    def icon
      self[:icon] || build_icon
    end

    private
    def build_gaction
      state = nil
      if initial_state.nil?
        state = update_state.call if update_state
      else
        state = initial_state
      end
      if state.nil?
        gaction = Gio::SimpleAction.new(name, parameter_type)
      else
        state = ensure_variant(state)
        gaction = Gio::SimpleAction.new(name, parameter_type, state)
      end
      gaction.signal_connect("activate") do |_, parameter|
        activate.call(self, parameter)
      end
      gaction
    end

    def ensure_variant(value)
      return nil if value.nil?
      return value if value.is_a?(GLib::Variant)
      GLib::Variant.new(value, parameter_type)
    end

    def build_icon
      return nil if icon_name.nil?
      Gio::ThemedIcon.new(icon_name)
    end
  end

  class Actions
    include GetText

    attr_reader :group
    def initialize(canvas)
      @canvas = canvas
      @actions = {}
      @group = Gio::SimpleActionGroup.new

      build_move_actions
      build_output_actions
      build_window_actions
      build_theme_actions
      build_view_actions
      build_graffiti_actions
      build_content_actions
      build_hole_actions
      build_search_actions
      build_log_level_actions
      build_quit_actions
    end

    def update_status
      @actions.each_value(&:update)
    end

    def find_action(name)
      @actions[name]
    end

    def [](name)
      @group.lookup_action(name)
    end

    private
    def guard
      begin
        yield
      rescue Exception
        Rabbit.logger.warn($!)
      end
    end

    def build_action(name,
                     parameter_type: nil,
                     initial_state: nil,
                     update_state: nil,
                     update_enabled: nil,
                     label: nil,
                     icon: nil,
                     icon_name: nil)
      parameter_type = GLib::VariantType.new(parameter_type) if parameter_type
      activate = lambda do |action, parameter|
        guard do
          yield(action, parameter)
        end
      end
      action = Action.new(name,
                          parameter_type,
                          initial_state,
                          update_state,
                          update_enabled,
                          label,
                          icon,
                          icon_name,
                          activate)
      @actions[name] = action
      @group.add_action(action.gaction)
    end

    def build_move_actions
      build_action("Next",
                   update_enabled: lambda {@canvas.have_next?},
                   label: _("Next"),
                   icon_name: "go-next") do
        @canvas.move_to_next_if_can
      end
      build_action("Previous",
                   update_enabled: lambda {@canvas.have_previous?},
                   label: _("Previous"),
                   icon_name: "go-previous") do
        @canvas.move_to_previous_if_can
      end
      build_action("NextSlide",
                   update_enabled: lambda {@canvas.have_next_slide?},
                   label: _("Next slide"),
                   icon_name: "go-next") do
        @canvas.move_to_next_if_can
      end
      build_action("PreviousSlide",
                   update_enabled: lambda {@canvas.have_previous_slide?},
                   label: _("Previous slide"),
                   icon_name: "go-previous") do
        @canvas.move_to_previous_if_can
      end
      build_action("FirstSlide",
                   update_enabled: lambda {not @canvas.first_slide?},
                   label: _("First slide"),
                   icon_name: "go-first") do
        @canvas.move_to_first
      end
      build_action("LastSlide",
                   update_enabled: lambda {not @canvas.last_slide?},
                   label: _("Last slide"),
                   icon_name: "go-last") do
        @canvas.move_to_last
      end
      build_action("JumpToSlide",
                   parameter_type: "i",
                   label: _("Jump to slide"),
                   icon_name: "go-jump") do |action, index|
        @canvas.move_to_if_can(index)
      end
      build_action("JumpToRelativeSlide",
                   parameter_type: "i",
                   label: _("Jump to relative slide"),
                   icon_name: "go-jump") do |action, index|
        @canvas.move_to_if_can(@canvas.current_index + index)
      end
    end

    def build_output_actions
      build_action("SaveAsImage",
                   update_enabled: lambda {not @canvas.processing?},
                   label: _("Save as image"),
                   icon_name: "document-save") do
        @canvas.save_as_image
      end
      build_action("Print",
                   update_enabled: lambda {not @canvas.processing?},
                   label: _("Print"),
                   icon_name: "document-print") do
        @canvas.print
      end
    end

    def build_window_actions
      build_action("Iconify",
                   update_state: lambda {false},
                   label: _("Iconify"),
                   icon: Icon.rabbit) do
        @canvas.iconify
      end
      build_action("ToggleFullscreen",
                   update_enabled: lambda {not @canvas.applying?},
                   update_state: lambda {@canvas.fullscreen?},
                   label: _("Fullscreen"),
                   icon_name: "view-fullscreen") do
        @canvas.toggle_fullscreen
      end
    end

    def build_theme_actions
      build_action("ChangeTheme",
                   parameter_type: "s",
                   update_enabled: lambda {not @canvas.applying?},
                   label: _("Change theme")) do |name|
        @canvas.apply_theme(name)
      end
      build_action("MergeTheme",
                   parameter_type: "s",
                   update_enabled: lambda {not @canvas.applying?},
                   label: _("Merge theme")) do |name|
        @canvas.merge_theme(name)
      end
      build_action("ReloadTheme",
                   update_enabled: lambda {not @canvas.applying?},
                   label: _("Reload theme"),
                   icon_name: "view-refresh") do
        @canvas.reload_theme
      end
    end

    def build_view_actions
      build_action("Redraw",
                   label: _("Redraw"),
                   icon_name: "view-refresh") do
        @canvas.redraw
      end
      build_action("CacheAllSlides",
                   update_enabled: lambda {not @canvas.processing?},
                   label: _("Cache all slides")) do
        @canvas.cache_all_slides
      end
      build_action("ResetAdjustment",
                   label: _("Reset adjustment"),
                   icon_name: "edit-clear") do
        @canvas.rest_adjustment
      end
      build_action("ResetTimer",
                   label: _("Reset timer"),
                   icon_name: "edit-clear") do
        @canvas.reset_timer
      end
      build_action("ToggleSpotlight",
                   update_state: lambda {@canvas.spotlighting?},
                   label: _("Spotlight")) do |action|
        @canvas.toggle_spotlight
        action.update
      end
      build_action("ToggleMagnifier",
                   update_state: lambda {@canvas.magnifying?},
                   label: _("Magnifier")) do |action|
        @canvas.toggle_magnifier
        action.update
      end
    end

    def graffiti_available?
      @canvas.graffiti_mode? or @canvas.have_graffiti?
    end

    def build_graffiti_actions
      build_action("ToggleGraffitiMode",
                   initial_state: false,
                   update_state: lambda {@canvas.graffiti_mode?},
                   label: _("Graffiti mode")) do |action|
        @canvas.toggle_graffiti_mode
        action.update
      end
      build_action("ClearGraffiti",
                   update_enabled: lambda {graffiti_available?},
                   label: _("Clear graffiti"),
                   icon_name: "edit-clear") do
        @canvas.clear_graffiti
      end
      build_action("UndoGraffiti",
                   update_enabled: lambda {graffiti_available?},
                   label: _("Undo graffiti"),
                   icon_name: "edit-undo") do
        @canvas.undo_graffiti
      end
      build_action("ChangeGraffitiColor",
                   update_enabled: lambda {graffiti_available?},
                   label: _("Change graffiti color")) do
        @canvas.change_graffiti_color
      end
    end

    def build_content_actions
      build_action("ClearSlide",
                   label: _("Clear slide"),
                   icon_name: "edit-clear") do
        @canvas.clear_slide
      end
      build_action("ToggleIndexMode",
                   update_enabled: lambda {not @canvas.processing?},
                   update_state: lambda {@canvas.index_mode?},
                   label: _("Index mode")) do |action|
        @canvas.toggle_index_mode
        action.update
      end
      build_action("ToggleInfoWindow",
                   update_state: lambda {@canvas.info_window_showing?},
                   label: _("Information window")) do |action|
        @canvas.toggle_info_window
        action.update
      end
      build_action("ToggleTerminal",
                   update_state: lambda {@canvas.in_terminal?},
                   label: _("Terminal")) do
        @canvas.toggle_terminal
      end
      build_action("ChangeBlankMode",
                   parameter_type: "s",
                   initial_state: "show",
                   label: _("Change blank mode")) do |action, parameter|
        case parameter
        when "show"
          if @canvas.whiteouting?
            @canvas.toggle_whiteout
          elsif @canvas.blackouting?
            @canvas.toggle_blackout
          end
        when "whiteout"
          @canvas.toggle_whiteout
        when "blackout"
          @canvas.toggle_blackout
        end
        action.gaction.change_state(GLib::Variant.new(parameter))
      end
      build_action("ToggleWhiteout",
                   update_state: lambda {@canvas.whiteouting?},
                   label: _("Whiteout")) do
        if @canvas.whiteouting?
          self["ChangeBlankMode"].activate("show")
        else
          self["ChangeBlankMode"].activate("whiteout")
        end
      end
      build_action("ToggleBlackout",
                   update_state: lambda {@canvas.blackouting?},
                   label: _("Blackout")) do
        if @canvas.blackouting?
          self["ChangeBlankMode"].activate("show")
        else
          self["ChangeBlankMode"].activate("blackout")
        end
      end
    end

    def build_hole_actions
      build_action("ExpandHole",
                   label: _("Expand hole")) do
        @canvas.expand_hole
      end
      build_action("NarrowHole",
                   label: _("Narrow hole")) do
        @canvas.narrow_hole
      end
    end

    def build_search_actions
      build_action("SearchSlideForward",
                   update_enabled: lambda {not @canvas.searching?},
                   label: _("Search slide forward"),
                   icon_name: "system-search") do
        @canvas.search_slide(true)
        update_status
      end
      build_action("SearchSlideBackward",
                   update_enabled: lambda {not @canvas.searching?},
                   label: _("Search slide backward"),
                   icon_name: "system-search") do
        @canvas.search_slide(false)
        update_status
      end
      build_action("SearchSlideForwardNext",
                   update_enabled: lambda {@canvas.searching?},
                   label: _("Search slide forward next"),
                   icon_name: "system-search") do
        @canvas.search_slide(true)
        update_status
      end
      build_action("SearchSlideBackwardNext",
                   update_enabled: lambda {@canvas.searching?},
                   label: _("Search slide backward next"),
                   icon_name: "system-search") do
        @canvas.search_slide(false)
        update_status
      end
      build_action("StopSlideSearch",
                   update_enabled: lambda {@canvas.searching?},
                   label: _("Stop search slide"),
                   icon_name: "system-search") do
        @canvas.stop_search_slide
        update_status
      end
    end

    def build_log_level_actions
      initial_state = Logger::Severity.name(Rabbit.logger.level)
      build_action("ChangeLogLevel",
                   parameter_type: "s",
                   initial_state: initial_state,
                   label: _("Change log level")) do |action, parameter|
        Rabbit.logger.level = Logger::Severity.level(parameter)
        action.gaction.change_state(GLib::Variant.new(parameter))
      end
    end

    def build_quit_actions
      build_action("Quit",
                   label: _("Quit"),
                   icon_name: "application-exit") do
        if !@canvas.processing? or
           @canvas.confirm(_("Now processing... Do you really quit?"))
          @canvas.quit
        end
      end
    end
  end
end
