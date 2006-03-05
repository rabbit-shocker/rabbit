require 'English'
require 'erb'

require 'rabbit/element'

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
      
      def apply(label, source, content, visitor)
        result = nil
        extensions.find do |entry|
          begin
            result = __send__(entry, label, source, content, visitor)
          rescue NameError
            visitor.logger.error($!)
            raise
          end
        end
        result
      end
      
      def extensions
        self.class.extensions
      end
      
      private
      def parse_source(source)
        prop = {}
        in_src = false
        src = ""
        source.each do |line|
          if in_src
            src << line
          else
            case line
            when /^\s*$/
              in_src = true
            when /^(?:#\s*)?(\S+)\s*=\s*(.+)\s*$/
              prop[$1] = $2
            else
              in_src = true
              src << line
            end
          end
        end
        [src, prop]
      end
    end
  end
end
