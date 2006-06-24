require "rabbit/utils"

module Rabbit
  module Renderer
    module Pixmap
      extend Utils

      class << self
        @initialized = false
        def init
          unless @initialized
            @initialized = true
            dir = ::File.join("rabbit", "renderer", "pixmap")
            require_files_under_directory_in_load_path(dir)
          end
        end

        def new(*args, &block)
          init
          corresponding_class_under_module(self).new(*args, &block)
        end
      end
    end
  end
end
