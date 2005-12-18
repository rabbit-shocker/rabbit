module Rabbit
  module Renderer
    module Pixmap
      extend Utils
      
      dir = ::File.join("rabbit", "renderer", "pixmap")
      require_files_under_directory_in_load_path(dir)

      class << self
        def new(*args, &block)
          corresponding_class.new(*args, &block)
        end

        def corresponding_class
          collect_classes_under_module(self).sort_by do |klass|
            klass.priority
          end.last
        end
      end
    end
  end
end
