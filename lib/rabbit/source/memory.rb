require 'rabbit/source/base'

module Rabbit
  module Source
    class Memory
      include Base

      def self.initial_args_description
        N_("[FILENAME_OR_NOT]")
      end

      attr_accessor :extension

      def initialize(encoding, logger, name=nil)
        super(encoding, logger)
        if name
          file_source = File.new(encoding, logger, name)
          @original_source = file_source.read
          set_base(file_source.base)
          @extension = extract_extension(name)
        else
          @original_source = ""
          @extension = nil
        end
        reset
      end

      def source=(new_source)
        @current_source = new_source
      end

      def _read
        @current_source
      end

      def need_read?
        super or @current_source != @source
      end

      def reset
        @current_source = @original_source.dup
      end
    end
  end
end
