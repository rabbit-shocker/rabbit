require 'rabbit/element'

require 'English'
require 'erb'

module Rabbit
  module Ext
    class Base
      include ERB::Util
      extend ERB::Util

      include Element
      
      class << self
        def inherited(klass)
          klass.const_set("EXTENSIONS", [])
        end
        
        def add_extension(name)
          extensions.push(name)
        end
        
        def extensions
          self::EXTENSIONS
        end
        
        def method_added(name)
          if /^ext_/ =~ name.to_s
            add_extension(name.to_s)
          end
        end
        
      end
      
      def apply(label, content, visitor)
        result = nil
        extensions.find do |entry|
          begin
            result = __send__(entry, label, content, visitor)
          rescue NameError
            $stderr.puts $!.inspect
            $stderr.puts $!.backtrace.join("\n")
            raise
          end
        end
        result
      end
      
      def extensions
        self.class.extensions
      end
      
    end
  end
end
