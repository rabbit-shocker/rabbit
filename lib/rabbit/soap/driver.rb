# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require "soap/rpc/driver"

require_relative "base"
require_relative "../front"

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
