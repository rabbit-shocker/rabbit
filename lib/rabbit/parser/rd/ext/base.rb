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

require "English"
require "erb"

require_relative "../../../element"

module Rabbit
  module Parser
    class RD
      module Ext
        class Base
          include ERB::Util
          extend ERB::Util

          include Element

          class << self
            def inherited(klass)
              klass.const_set("EXTENSIONS", [])
            end

            def add_extension(name)
              extensions.push(name)
            end

            def extensions
              self::EXTENSIONS
            end

            def method_added(name)
              if /^ext_/ =~ name.to_s
                add_extension(name.to_s)
              end
            end
          end

          def apply(label, source, content, visitor)
            result = nil
            extensions.find do |entry|
              begin
                result = __send__(entry, label, source, content, visitor)
              rescue NameError
                Rabbit.logger.error($!)
                raise
              end
            end
            result
          end

          def extensions
            self.class.extensions
          end

          private
          def parse_source(source)
            prop = {}
            in_src = false
            src = ""
            source.each_line do |line|
              if in_src
                src << line
              else
                case line
                when /^\s*$/
                  in_src = true
                when /^(?:#\s*)?(\S+)\s*=\s*(.+)\s*$/
                  prop[$1] = $2
                else
                  in_src = true
                  src << line
                end
              end
            end
            [src, prop]
          end
        end
      end
    end
  end
end
