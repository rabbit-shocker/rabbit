# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "erb"

require_relative "gtk"

require_relative "gettext"
require_relative "utils"
require_relative "theme/searcher"
require_relative "image"
require_relative "actions"

module Rabbit
  class Menu
    include ERB::Util
    include GetText

    def initialize(actions)
      @actions = actions
    end

    def attach(window)
      @window = window
    end

    def detach(window)
      @window = nil
    end

    def update_menu(canvas)
      @popover_menu = Gtk::PopoverMenu.new
      @popover_menu.modal = false
      @menu_model = Gio::Menu.new
      @popover_menu.insert_action_group("rabbit", @actions.group)
      add_jump_to_actions(canvas)
      add_theme_actions(canvas)
      add_items(canvas)
      @popover_menu.bind_model(@menu_model)
      @popover_menu.relative_to = @window.child if @window
      @popover_menu.pointing_to = Gdk::Rectangle.new(0, 0, 0, 0)

      @actions.update_status
    end

    def popup
      @popover_menu.popup
    end

    def popdown
      @popover_menu.popdown
    end

    private
    def build_action(name)
      action = Gio::SimpleAction.new(name)
      action.signal_connect("activate") do
        yield
      end
      action
    end

    def escape_label(label)
      label.gsub(/_/, '__')
    end

    def add_jump_to_actions(canvas)
      @jump_to_actions = Gio::SimpleActionGroup.new
      canvas.slides.each_with_index do |slide, i|
        action = build_action("JumpTo#{i}") do
          canvas.move_to_if_can(i)
        end
        @jump_to_actions.add_action(action)
      end
      @popover_menu.insert_action_group("jump", @jump_to_actions)
    end

    def jump_to_items(canvas)
      canvas.slides.collect.with_index do |slide, i|
        label = "#{i}: #{escape_label(Utils.unescape_title(slide.title))}"
        [:item, "jump.JumpTo#{i}", label]
      end
    end

    def add_theme_actions(canvas)
      @theme_actions = Gio::SimpleActionGroup.new
      themes = Theme::Searcher.collect_theme
      themes.each do |entry|
        action = build_action("ChangeThemeEntry#{entry.name}") do
          canvas.apply_theme(entry.name, &Utils.process_pending_events_proc)
        end
        action = build_action("MergeThemeEntry#{entry.name}") do
          canvas.merge_theme(entry.name, &Utils.process_pending_events_proc)
        end
      end
    end

    def theme_items(canvas, prefix)
      themes = Theme::Searcher.collect_theme
      themes_by_category = themes.group_by do |entry|
        entry.category
      end
      sorted_categories = themes_by_category.keys.sort_by do |category|
        _(category)
      end
      sorted_categories.collect do |category|
        items = themes_by_category[category].collect do |entry|
          [:item, "theme.#{prefix}ThemeEntry#{entry.name}", entry.name]
        end
        [:menu, "#{prefix[0]}: #{_(category)}", *items]
      end
    end

    def change_theme_items(canvas)
      theme_items(canvas, "Change")
    end

    def merge_theme_items(canvas)
      theme_items(canvas, "Merge")
    end

    def items(canvas)
      [
        [:section,
         [:item, "ToggleIndexMode"],
        ],
        [:section,
         [:item, "ToggleGraffitiMode"],
         [:menu, _("Graffiti"),
          [:item, "ClearGraffiti"],
          [:item, "UndoGraffiti"],
          [:item, "ChangeGraffitiColor"],
         ],
        ],
        [:section,
         [:item, "ToggleFullscreen"],
        ],
        [:section,
         [:item, "ToggleInfoWindow"],
        ],
        [:section,
         [:item, "ChangeBlank('whiteout')", _("Whiteout")],
         [:item, "ChangeBlank('blackout')", _("Blackout")],
         [:item, "ChangeBlank('show')", _("Show")],
        ],
        # [:section,
        #  [:item, "ToggleCommentFrame"],
        #  [:item, "ToggleCommentView"],
        # ],
        [:section,
         [:item, "ToggleSpotlight"],
         [:item, "ToggleMagnifier"],
        ],
        [:section,
         [:item, "ToggleTerminal"],
        ],
        [:section,
         [:menu, _("Jump to slide"), *jump_to_items(canvas)],
        ],
        [:section,
         [:item, "Previous"],
         [:item, "Next"],
         [:item, "PreviousSlide"],
         [:item, "NextSlide"],
         [:item, "FirstSlide"],
         [:item, "LastSlide"],
        ],
        [:section,
         [:item, "Iconify"],
        ],
        [:section,
         [:item, "Redraw"],
         [:item, "ClearSlide"],
         [:item, "ReloadTheme"],
         [:menu, _("Change theme"), *change_theme_items(canvas)],
         [:menu, _("Merge theme"), *merge_theme_items(canvas)],
        ],
        [:sction,
         [:item, "CacheAllSlides"],
        ],
        [:section,
         [:item, "SaveAsImage"],
         [:item, "Print"],
        ],
        [:section,
         [:item, "ResetTimer"],
         [:item, "ResetAdjustment"],
        ],
        [:section,
         [:menu, _("Log level"),
          [:item, "ChangeLogLevel('debug')", _("Debug")],
          [:item, "ChangeLogLevel('info')", _("Info")],
          [:item, "ChangeLogLevel('warning')", _("Warning")],
          [:item, "ChangeLogLevel('error')", _("Error")],
          [:item, "ChangeLogLevel('fatal')", _("Fatal")],
          [:item, "ChangeLogLevel('unknown')", _("Unknown")],
         ],
        ],
        [:section,
         [:item, "Quit"],
        ],
      ]
    end

    def add_item(container, item)
      type = item[0]
      case type
      when :section
        section = Gio::Menu.new
        item[1..-1].each do |i|
          add_item(section, i)
        end
        section_item = Gio::MenuItem.new
        section_item.section = section
        container.append_item(section_item)
      when :item
        action_name = item[1]
        label = item[2]
        icon = nil
        unless action_name.include?(".")
          action = @canvas.actions.find_action(action_name)
          label ||= action&.label
          icon = action&.icon
          action_name = "rabbit.#{action_name}"
        end
        label ||= action_name
        i = Gio::MenuItem.new(label, action_name)
        i.icon = icon if icon
        container.append_item(i)
      when :menu
        submenu = Gio::Menu.new
        label = item[1]
        item[2..-1].each do |i|
          add_item(submenu, i)
        end
        submenu_item = Gio::MenuItem.new(label)
        submenu_item.submenu = submenu
        container.append_item(submenu_item)
      end
    end

    def add_items(canvas)
      @canvas = canvas
      items(canvas).each do |item|
        add_item(@menu_model, item)
      end
      @canvas = nil
    end
  end
end
