require "soap/rpc/httpserver"

require "rabbit/soap/base"

module Rabbit
  module SOAP

    class Server < ::SOAP::RPC::HTTPServer

      def initialize(front, config)
        config[:SOAPDefaultNamespace] = NS
        super(config)
        setup_rpc_servant(front)
      end

      private
      def setup_rpc_servant(front)
        front.public_methods(false).each do |name|
          element_name = SOAP.element_name(name)
          if name != element_name
            front.instance_eval(<<-EOS, __FILE__, __LINE__)
              class << self
                alias_method(#{element_name.dump}, #{name.dump})
              end
EOS
          end
        end
        add_rpc_servant(front)
      end
    end
    
  end
end
