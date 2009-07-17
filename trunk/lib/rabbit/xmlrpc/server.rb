require "webrick"
require "xmlrpc/server"

require "rabbit/xmlrpc/base"

module Rabbit
  module XMLRPC
    class Server < ::WEBrick::HTTPServer
      def initialize(front, *args)
        super(*args)
        servlet = Servlet.new(front)
        mount(PATH, servlet)
      end
    end
    
    class Servlet < ::XMLRPC::WEBrickServlet
      def initialize(front, *args)
        super(*args)
        add_handler(PREFIX, front)
      end
    end
  end
end
