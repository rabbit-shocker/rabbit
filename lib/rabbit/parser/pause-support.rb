module Rabbit
  module Parser
    module PauseSupport
      def pause_targets
        @pause_targets ||= {}
      end

      def register_pause(target)
        pause_targets[@slides.last] ||= []
        pause_targets[@slides.last] << target
      end

      def unregister_pause(target)
        pause_targets[@slides.last] ||= []
        pause_targets[@slides.last].delete(target)
      end

      def burn_out_pause_targets
        @slides.each do |slide|
          (pause_targets[slide] || []).each do |target|
            slide.register_default_wait_target(target)
            slide.register_default_wait_proc(target.parent) do |*args|
              target.show do
                next_proc = args.pop
                next_proc.call(*args)
              end
            end
          end
        end
      end
    end
  end
end
