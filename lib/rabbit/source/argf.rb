require "rabbit/source/base"

module Rabbit
  module Source
    class ARGF
      
      include Base
      
      def self.initial_args_description
        "none (get from STDIN) or FILE_NAMES"
      end
      
      def initialize(encoding, argf)
        super(encoding)
        @argf = argf
      end
      
      def full_path(path)
        ::File.join(@base, path)
      end
      
      private
      def _read
        @argf.read
      end
    end
  end
end
