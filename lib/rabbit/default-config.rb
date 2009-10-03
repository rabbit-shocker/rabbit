require 'rbconfig'

module Rabbit
  module Config
    IMAGE_PATH = [
      ::Config::CONFIG["datadir"],
      File.join(".", "data"),
    ]
    if Object.const_defined?(:Gem)
      rabbit_gem_spec = Gem.loaded_specs["rabbit"]
      if rabbit_gem_spec and rabbit_gem_spec.loaded?
        IMAGE_PATH.unshift(File.join(rabbit_gem_spec.full_gem_path, "data"))
      end
    end
  end
end
