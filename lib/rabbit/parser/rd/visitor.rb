# Copyright (C) 2004-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "rd/rdvisitor"
require "rd/version"

require 'rabbit/rabbit'

module Rabbit
  module Parser
    class RD
      class Visitor < ::RD::RDVisitor
        include ::RD::MethodParse

        class << self
          def version
            self.class::VERSION
          end
        end

        def initialize
          init_extensions
          super
        end

        private
        def init_extensions
          @installed_extensions = {}
          self.class::EXTENSIONS.each do |name, klass|
            @installed_extensions[name] =  klass.new
          end
        end

        def apply_to_extension(ext_type, label, source, content)
          result = nil
          unless @installed_extensions.has_key?(ext_type)
            fatal_on_extension_not_available(label, ext_type)
          end
          args = [label, source, content, self]
          extension = @installed_extensions[ext_type]
          result = extension.apply(*args)
          default_method_name = "default_ext_#{ext_type}"
          if result.nil? and
              extension.respond_to?(default_method_name, true)
            result = extension.__send__(default_method_name, *args)
          end
          fatal_on_extension_not_available(label, ext_type) if result.nil?
          result
        end

        def fatal_on_extension_not_available(label, type)
          message = _("[BUG] [%s] %s extension isn't available.")
          Rabbit.logger.fatal(message % [label, ext_type])
        end
      end
    end
  end
end

