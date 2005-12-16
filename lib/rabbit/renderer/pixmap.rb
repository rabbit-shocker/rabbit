module Rabbit
  module Renderer
    class Pixmap
      extend Utils
      
      dir = ::File.join("rabbit", "renderer", "pixmap")
      require_files_under_directory_in_load_path(dir)

      class << self
        def new(*args, &block)
          corresponding_class.new(*args, &block)
        end

        def collect_classes
          Renderer.constants.collect do |name|
            if /Pixmap[A-Z].+/ =~ name
              const = Renderer::const_get(name)
              if const.is_a?(Class) and const.respond_to?(:priority)
                const
              else
                nil
              end
            else
              nil
            end
          end.compact
        end
        
        def corresponding_class
          collect_classes.sort_by do |klass|
            klass.priority
          end.last
        end
      end
    end
  end
end
