require "xmlrpc/client"

require "rabbit/xmlrpc/base"

module Rabbit
  module XMLRPC
    class Client < ::XMLRPC::Client
      def initialize(host=nil, *args)
        super(host, PATH, *args)
      end

      def proxy(*args)
        super(PREFIX, *args)
      end
    end
  end
end
