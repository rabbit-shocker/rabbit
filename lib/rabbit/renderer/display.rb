require "rabbit/utils"

module Rabbit
  module Renderer
    module Display
      extend Utils

      class << self
        @initialized = false
        @preferred_class_name = nil
        def init(options={})
          if options.has_key?(:preferred_class_name)
            @preferred_class_name = options[:preferred_class_name]
          end
          unless @initialized
            @initialized = true
            dir = ::File.join("rabbit", "renderer", "display")
            require_files_under_directory_in_load_path(dir)
          end
        end

        def new(*args, &block)
          init
          target_class = nil
          if @preferred_class_name
            if const_defined?(@preferred_class_name)
              target_class = const_get(@preferred_class_name)
              target_class = nil unless target_class.is_a?(Class)
            end
          end
          target_class ||= corresponding_class_under_module(self)
          target_class.new(*args, &block)
        end
      end
    end
  end
end
