require "rabbit/utils"

module Rabbit
  module Source
    extend Utils

    dir = ::File.join("rabbit", "source")
    require_files_under_directory_in_load_path(dir)

    def self.types
      collect_classes_under_module(self)
    end
  end
end
