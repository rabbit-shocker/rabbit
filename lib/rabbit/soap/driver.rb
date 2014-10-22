require 'soap/rpc/driver'

require 'rabbit/soap/base'
require 'rabbit/front'

module Rabbit
  module SOAP
    class Driver < ::SOAP::RPC::Driver

      APP_NAME = "RabbitSoapDriver"

      @@method_infos = []
      Front.instance_methods(false).each do |name|
        info = [name, SOAP.element_name(name)]
        info.concat(Utils.arg_list(Front.instance_method(name).arity))
        @@method_infos << info
      end

      def initialize(end_point, soap_action=nil)
        super(end_point, Rabbit::SOAP::NS, soap_action)

        @@method_infos.each do |info|
          add_method_as(*info)
        end

        _public_level = public_level
        available_interfaces.each do |name, level, arity|
          unless (_public_level & level).zero?
            info = [name, SOAP.element_name(name)]
            info.concat(Utils.arg_list(arity))
            add_method_as(*info)
          end
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
