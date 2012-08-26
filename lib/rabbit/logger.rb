require "rabbit/utils"

module Rabbit
  module Logger
    extend Utils

    dir = ::File.join("rabbit", "logger")
    require_files_under_directory_in_load_path(dir)

    class << self
      def types
        collect_classes_under_module(self)
      end

      def default
        if Utils.support_console_output? or !Logger.const_defined?(:GUI)
          Logger::STDERR.new
        else
          Logger::GUI.new
        end
      end
    end
  end
end
