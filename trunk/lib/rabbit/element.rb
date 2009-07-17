require 'rabbit/utils'

module Rabbit
  module Element
    dir = ::File.join("rabbit", "element")
    Utils.require_files_under_directory_in_load_path(dir)
  end
end
