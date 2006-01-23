require 'gtk2'

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
    extend GetText

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
        group.set_translate_func(&GetText.method(:_))
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
              callback.call(act, group, canvas)
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
            callback.call(act, current, group, canvas)
          end
        end
      end
    end

    dir = ::File.join("rabbit", "action")
    require_files_under_directory_in_load_path(dir)

    module_function
    def update_processing_action_status(canvas)
      canvas.action("ToggleIndexMode").sensitive = !canvas.processing?
      canvas.action("CacheAllSlides").sensitive = !canvas.processing?
      canvas.action("SaveAsImage").sensitive = !canvas.processing?
      canvas.action("Print").sensitive = !canvas.processing?
    end
  end
end
