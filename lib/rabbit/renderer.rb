require "rabbit/utils"

module Rabbit
  module Renderer
    extend Utils

    dir = ::File.join("rabbit", "renderer")
    require_files_under_directory_in_load_path(dir)

    class << self
      def printable?
        Print.printable?
      end

      def printable_renderer(slides_per_page)
        if slides_per_page > 1
          Print::Multiple
        else
          Print
        end
      end

      def renderer_module
        corresponding_module_under_module(self)
      end
    end
  end
end
