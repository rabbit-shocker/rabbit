require 'rabbit/source/base'

module Rabbit
  module Source
    class Memory
      
      include Base

      def self.initial_args_description
        "FILENAME_OR_NOT"
      end

      def initialize(encoding, logger, name=nil)
        super(encoding, logger)
        if name
          file_source = File.new(encoding, logger, name)
          @original_source = file_source.read
          set_base(file_source.base)
        else
          @original_source = ""
        end
      end

      def source=(new_source)
        @original_source = new_source
      end
      
      def _read
        @original_source
      end
      
      def need_read?
        super or @original_source != @source
      end
    end
    
  end
end
