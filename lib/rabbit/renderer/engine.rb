require 'rabbit/utils'

module Rabbit
  module Renderer
    module Engine
      extend Utils

      dir = ::File.join("rabbit", "renderer", "engine")
      require_files_under_directory_in_load_path(dir)

      class << self
        def renderer_module
          corresponding_module_under_module(self)
        end
      end
    end
  end
end
