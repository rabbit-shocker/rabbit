require 'rbconfig'

module Rabbit
  module Config
    IMAGE_PATH = [
      ::Config::CONFIG["datadir"],
      File.join(".", "data"),
    ]
    append_path_from_gem = lambda do
      return unless Object.const_defined?(:Gem)
      rabbit_gem_spec = Gem.loaded_specs["rabbit"]
      return if rabbit_gem_spec.nil?
      unless rabbit_gem_spec.respond_to?(:activated?)
        def rabbit_gem_spec.activated?
          loaded?
        end
      end
      if rabbit_gem_spec.activated?
        IMAGE_PATH.unshift(File.join(rabbit_gem_spec.full_gem_path, "data"))
      end
    end
    append_path_from_gem.call
  end
end
