require "rabbit/utils"

module Rabbit

  module Renderer

    extend Utils

    dir = ::File.join("rabbit", "renderer")
    require_files_under_directory_in_load_path(dir)

    class << self
      def types
        collect_classes_under_module(self)
      end

      def printable?
        types.find {|t| /print/i =~ t.name}
      end
    end
  end
end
