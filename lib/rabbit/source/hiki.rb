raise LoadError

require 'rabbit/source/base'

module Rabbit
  module Source
    
    class Hiki

      include Base

      def self.initial_args_description
        N_("[HIKI_CGI_URI] and [PAGE_NAME]")
      end

      def initialize(encoding, cgi_uri, name)
        @cgi_uri = cgi_uri
        @name = name
        @uri = URI.parse("#{@cgi_uri}?c=s;p=#{@name}")
        super(encoding)
      end

      private
      def _read
        @uri.open do |f|
          f.read
        end
      end

      def init_base
        set_base(::File.dirname(@uri.to_s))
      end
    end

  end
end
