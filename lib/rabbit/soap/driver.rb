require 'soap/rpc/driver'

require 'rabbit/soap/base'
require 'rabbit/front'

module Rabbit
  module SOAP
    class Driver < ::SOAP::RPC::Driver

      APP_NAME = "RabbitSoapDriver"

      @@method_infos = []
      Front.instance_methods(false).each do |name|
        info = [name]
        Front.instance_method(name).arity.times do
          info << "arg"
        end
        @@method_infos << info
      end
      
      def initialize(end_point, soap_action=nil)
        super(end_point, Rabbit::SOAP::NS, soap_action)

        @@method_infos.each do |info|
          add_method(*info)
        end
      end

      def log_dir=(new_value)
        if new_value.nil?
          self.wiredump_file_base = nil
        else
          self.wiredump_file_base = File.join(new_value, APP_NAME)
        end
      end
    end
  end
end
