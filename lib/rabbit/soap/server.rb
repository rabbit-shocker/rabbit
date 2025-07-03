# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "soap/rpc/httpserver"

require_relative "base"

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
