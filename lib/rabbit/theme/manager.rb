require "forwardable"

require 'rabbit/theme/applier'

module Rabbit
  module Theme
    class Manager
      extend Forwardable

      def_delegators(:@canvas, :logger)
      
      attr_reader :canvas, :name
      def initialize(canvas, &callback)
        @canvas = canvas
        @applier = Applier.new(self, &callback)
        apply("base")
      end

      def apply(name)
        @name = name
        begin
          @applier.apply_theme(name)
        rescue ThemeExit
          logger.info($!.message) if $!.have_message?
        rescue StandardError, LoadError, SyntaxError
          logger.warn($!)
        end
      end
    
      def slides
        @canvas.slides
      end
    end
  end
end
