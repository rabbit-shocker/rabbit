# Copyright (C) 2007-2017  Kouhei Sutou <kou@cozmixng.org>
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

require "tempfile"

require "poppler"

require "rabbit/element"
require "rabbit/parser/base"

module Rabbit
  module Parser
    class PDF < Base
      unshift_loader(self)
      class << self
        def format_name
          "PDF"
        end

        def match?(source)
          extension = source.extension
          if extension.nil?
            source.read.start_with?("%PDF-1.")
          else
            /\Apdf\z/i =~ extension
          end
        end
      end

      include Element
      def parse
        # Workaround for Ruby/Poppler 3.1.9
        pdf_file = Tempfile.new(["rabbit", "pdf"])
        pdf_file.write(@source.read)
        pdf_file.flush
        doc = Poppler::Document.new(:path => pdf_file.path)
        pdf_file.close!

        doc.each_with_index do |page, i|
          if i.zero?
            @canvas << PopplerTitleSlide.new(page, doc)
          else
            @canvas << PopplerSlide.new(page)
          end
        end
      end
    end
  end
end
