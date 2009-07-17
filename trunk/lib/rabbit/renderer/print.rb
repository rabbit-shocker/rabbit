module Rabbit
  module Renderer
    module Print
      extend Utils

      A4_WIDTH = 596
      A4_HEIGHT = 842

      class << self
        @initialized = false
        def init
          unless @initialized
            @initialized = true
            dir = ::File.join("rabbit", "renderer", "print")
            require_files_under_directory_in_load_path(dir)
          end
        end

        def new(*args, &block)
          init
          corresponding_class_under_module(self).new(*args, &block)
        end

        def printable?
          init
          not corresponding_class_under_module(self).nil?
        end
      end
    end
  end
end
