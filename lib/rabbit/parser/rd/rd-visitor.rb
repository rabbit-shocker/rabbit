require "rd/rdvisitor"
require "rd/version"

require 'rabbit/rabbit'

module Rabbit
  class RDVisitor < ::RD::RDVisitor
    include RD::MethodParse
    
    class << self
      def version
        self.class::VERSION
      end
    end
    
    def initialize
      init_extensions
      super
    end
    
    private
    def init_extensions
      @installed_extensions = {}
      self.class::EXTENSIONS.each do |name, klass|
        @installed_extensions[name] =  klass.new
      end
    end
    
    def apply_to_extension(ext_type, label, source, content)
      result = nil
      unless @installed_extensions.has_key?(ext_type)
        fatal_on_extension_not_available(label, ext_type)
      end
      args = [label, source, content, self]
      extension = @installed_extensions[ext_type]
      result = extension.apply(*args)
      default_method_name = "default_ext_#{ext_type}"
      if result.nil? and
          extension.respond_to?(default_method_name, true)
        result = extension.__send__(default_method_name, *args)
      end
      fatal_on_extension_not_available(label, ext_type) if result.nil?
      result
    end
    
    def fatal_on_extension_not_available(label, type)
      message = _("[BUG] [%s] %s extension isn't available.")
      logger.fatal(message % [label, ext_type])
    end
  end
end
