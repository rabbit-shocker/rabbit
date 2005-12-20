module Rabbit
  module Renderer
    module Pixmap
      extend Utils
      
      dir = ::File.join("rabbit", "renderer", "pixmap")
      require_files_under_directory_in_load_path(dir)

      class << self
        def new(*args, &block)
          corresponding_class_under_module(self).new(*args, &block)
        end
      end
    end
  end
end
