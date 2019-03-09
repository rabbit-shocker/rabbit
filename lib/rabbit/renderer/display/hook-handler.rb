module Rabbit
  module Renderer
    module Display
      module HookHandler
        def initialize(*args, &block)
          super
          init_hook_handler
        end

        def call_hook_procs(procs, *args)
          procs.any? {|proc| proc.call(*args)}
        end

        def add_motion_notify_hook(hook=nil, &block)
          hook ||= Proc.new(&block)
          @motion_notify_hook_procs << hook
        end

        def clear_motion_notify_hook
          @motion_notify_hook_procs.clear
        end

        def add_scroll_hook(hook=nil, &block)
          hook ||= Proc.new(&block)
          @scroll_hook_procs << hook
        end

        def clear_scroll_hook
          @scroll_hook_procs.clear
        end

        def add_button_press_hook(hook=nil, &block)
          hook ||= Proc.new(&block)
          @button_press_hook_procs << hook
        end

        def clear_button_press_hook
          @button_press_hook_procs.clear
        end

        def add_button_release_hook(hook=nil, &block)
          hook ||= Proc.new(&block)
          @button_release_hook_procs << hook
        end

        def clear_button_release_hook
          @button_release_hook_procs.clear
        end

        def clear_hooks
          init_hook_handler
        end

        private
        def init_hook_handler
          @motion_notify_hook_procs = []
          @scroll_hook_procs = []
          @button_press_hook_procs = []
          @button_release_hook_procs = []
        end
      end
    end
  end
end
