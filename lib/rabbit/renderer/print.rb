module Rabbit
  module Renderer
    module Print
      extend Utils
      
      A4_WIDTH = 596
      A4_HEIGHT = 842
        
      dir = ::File.join("rabbit", "renderer", "print")
      require_files_under_directory_in_load_path(dir)

      class << self
        def new(*args, &block)
          corresponding_class_under_module(self).new(*args, &block)
        end

        def printable?
          not corresponding_class_under_module(self).nil?
        end
      end
    end
  end
end
