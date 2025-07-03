# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "hikidoc"

require_relative "base"

module Rabbit
  module Parser
    class Wiki < Base
    end
  end
end

require_relative "wiki/output"

module Rabbit
  module Parser
    class Wiki
      unshift_loader(self)
      class << self
        def format_name
          "Wiki"
        end

        def match?(source)
          extension = source.extension
          if extension.nil?
            head = source.read[0, 500]
            if head.respond_to?(:force_encoding)
              head.force_encoding("ASCII-8BIT")
            end
            /^!/.match(head)
          else
            /\A(?:hiki|wiki)\z/i =~ extension
          end
        end
      end

      include Element
      def parse
        parser = HikiDoc.new(RabbitOutput.new(@canvas),
                             :use_wiki_name => false)
        parser.compile(@source.read)
      end
    end
  end
end
