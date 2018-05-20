require "rabbit/image/base"

module Rabbit
  module ImageManipulable

    class Default < Base

      push_loader(self)

      class << self
        def match?(filename)
          true
        end
      end

      private
      def update_size
        File.open(@filename, "rb") do |file|
          Dir.chdir(File.dirname(@filename)) do
            load_data(file.read)
          end
        end
      end
    end
  end
end
