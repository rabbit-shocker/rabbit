require 'rabbit/source/base'

module Rabbit
  module Source
    class File
      
      include Base
      
      def self.initial_args_description
        "FILENAME"
      end
      
      def initialize(encoding, name)
        @name = name
        super(encoding)
      end
      
      def _read
        ::File.open(@name) do |f|
          @mtime = f.mtime
          f.read
        end
      end
      
      def need_read?
        super or old?(@mtime, :mtime)
      end
      
      private
      def mtime
        ::File.mtime(@name)
      end
      
      def init_base
        set_base(::File.dirname(@name))
      end
      
    end
    
  end
end
