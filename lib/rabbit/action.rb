# Copyright (C) 2005-2017  Kouhei Sutou <kou@cozmixng.org>
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

require 'rabbit/gtk'

module Gtk
  class Action
    alias _activate activate
    def activate(&block)
      @block = block
      _activate
    ensure
      @block = nil
    end

    def block_given?
      not @block.nil?
    end

    def call(*args, &block)
      @block.call(*args, &block)
    end
  end
end

require 'rabbit/rabbit'
require 'rabbit/stock'
require 'rabbit/utils'

module Rabbit
  module Action
    extend Utils
    include GetText

    @@actions = []
    @@toggle_actions = []
    @@radio_actions = []
    @@update_status_methods = []
    class << self
      def method_added(name)
        case name.to_s
        when /_config$/
          # ignore
        when /^act_(radio_.+)$/
          @@radio_actions << [Utils.to_class_name($1), name]
        when /^act_(toggle_.+)$/
          @@toggle_actions << [Utils.to_class_name($1), name]
        when /^act_(.+)$/
          @@actions << [Utils.to_class_name($1), name]
        when /^update_(.+)_status$/
          @@update_status_methods << name
        end
      end

      def action_group(canvas, name="Rabbit")
        Stock.init(canvas)
        group = Gtk::ActionGroup.new(name)
        group.set_translate_func(&method(:_))
        group.add_actions(to_gtk_actions(@@actions, canvas))
        actions = to_gtk_actions(@@toggle_actions, canvas, true)
        group.add_toggle_actions(actions)
        add_radio_actions(group, @@radio_actions, canvas)
        group
      end

      def update_status(canvas)
        @@update_status_methods.each do |method|
          __send__(method, canvas)
        end
      end

      private
      def to_gtk_actions(actions, canvas, toggle=false)
        actions.collect do |name, action|
          config = {:label => name}
          config_method = "#{action}_config"
          __send__(config_method, config, canvas) if respond_to?(config_method)
          callback = method(action)
          result = [
            name,
            config[:stock_id],
            config[:label],
            config[:accelerator],
            config[:tooltip],
            Proc.new do |group, act|
              guard(canvas) do
                callback.call(act, group, canvas)
              end
            end
          ]
          result << config[:is_active] if toggle
          result
        end
      end

      def add_radio_actions(group, actions, canvas)
        actions.each do |name, action|
          default_value = nil
          gtk_actions = methods(false).find_all do |method_name|
            /^#{action}_(?:.+)_config$/ =~ method_name
          end.collect do |config_method|
            /^act_(.*)_config$/ =~ config_method
            action_name = Utils.to_class_name($1)
            config = {:label => action_name}
            __send__(config_method, config, canvas)
            default_value = config[:value] if config[:default]
            [
              action_name,
              config[:stock_id],
              config[:label],
              config[:accelerator],
              config[:tooltip],
              config[:value],
            ]
          end

          callback = method(action)
          group.add_radio_actions(gtk_actions, default_value) do |act, current|
            guard(canvas) do
              callback.call(act, current, group, canvas)
            end
          end
        end
      end

      def guard(canvas)
        begin
          yield
        rescue Exception
          canvas.logger.warn($!)
        end
      end
    end

    module_function
    def update_processing_action_status(canvas)
      canvas.action("ToggleIndexMode").sensitive = !canvas.processing?
      canvas.action("CacheAllSlides").sensitive = !canvas.processing?
      canvas.action("SaveAsImage").sensitive = !canvas.processing?
      canvas.action("Print").sensitive = !canvas.processing?
    end
  end
end

require "rabbit/action/basic"
require "rabbit/action/radio"
require "rabbit/action/toggle"
