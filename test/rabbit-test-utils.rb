require "test/unit"

module Test
  module Unit

    class Failure
      def long_display
        first, *others = location
        others.unshift("") unless others.empty?
        others = others.join("\n  ")
        "Failure:\n#{@test_name}\n#{first}\n#{@message}#{others}"
      end
    end

    class Error
      def long_display
        backtrace = filter_backtrace(@exception.backtrace)
        first, *others = backtrace
        others.unshift("") unless others.empty?
        others = others.join("\n  ")
        "Error:\n#{@test_name}:\n#{first}\n#{message}#{others}"
      end
    end
    
  end
end
