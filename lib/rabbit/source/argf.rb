require "rabbit/source/base"

module Rabbit
  module Source
    class ARGF

      include Base

      def self.initial_args_description
        N_("none (get from STDIN) or [FILE_NAMES]")
      end

      def initialize(encoding, logger, argf)
        super(encoding, logger)
        @argf = argf
      end

      private
      def _read
        begin
          @argf.read
        rescue
          @logger.error($!.message)
          ""
        end
      end
    end
  end
end
