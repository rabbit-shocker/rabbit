require 'rbconfig'

module Rabbit
  module Config
    IMAGE_PATH = [
      ::Config::CONFIG["datadir"],
      File.join(".", "data"),
    ]
  end
end
