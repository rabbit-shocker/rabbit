require 'rabbit/source/base'

module Rabbit
  module Source
    class File
      
      include Base

      def self.initial_args_description
        N_("[FILENAME]")
      end
      
      def initialize(encoding, logger, name)
        @name = name
        super(encoding, logger)
        @mtime = nil
      end
      
      def _read
        begin
          check_file
          ::File.open(@name) do |f|
            @mtime = f.mtime
            f.read
          end
        rescue SourceUnreadableError
          @logger.error($!.message)
          @mtime = Time.now + LimitAccessInterval::MINIMUM_ACCESS_TIME
          ""
        end
      end
      
      def need_read?
        super or old?(@mtime, :mtime)
      end
      
      private
      def check_file
        unless ::File.exist?(@name)
          raise NotExistError.new(@name)
        end
        unless ::File.file?(@name)
          raise NotFileError.new(@name)
        end
        unless ::File.readable?(@name)
          raise NotReadableError.new(@name)
        end
      end
      
      def mtime
        begin
          check_file
          ::File.mtime(@name)
        rescue SourceUnreadableError
          Time.now
        end
      end
      
      def init_base
        set_base(::File.dirname(@name))
      end
      
    end
    
  end
end
