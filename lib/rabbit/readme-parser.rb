# Copyright (C) 2012-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "rabbit/gettext"
require "rabbit/path-manipulatable"

module Rabbit
  class READMEParser
    include GetText
    include PathManipulatable

    attr_reader :title, :description
    def initialize
      @title = nil
      @description = nil
    end

    def parse(path=nil)
      path ||= remove_backup_paths(Dir.glob("README*"))[0]
      raise _("No README found") if path.nil?

      parse_content(File.read(path), File.extname(path))
    end

    private
    def parse_content(content, extension)
      type = guess_type(content, extension)
      _heading_mark_pattern = heading_mark_pattern(type)

      blocks = content.split(/(?:\r?\n){2,}/)
      if blocks[0]
        @title = blocks[0].gsub(_heading_mark_pattern, "")
      end
      first_paragraph_blocks = []
      blocks[1..-1].each do |block|
        break if _heading_mark_pattern =~ block
        first_paragraph_blocks << block
      end
      @description = first_paragraph_blocks.join("\n\n")
    end

    def guess_type(content, extension)
      guess_type_from_extension(extension) ||
        guess_type_from_content(content) ||
        :rd
    end

    def guess_type_from_extension(extension)
      case extension.downcase
      when ".rd", ".rab"
        :rd
      when ".hiki"
        :hiki
      when ".md"
        :markdown
      when ".textile"
        :textile
      else
        nil
      end
    end

    def guess_type_from_content(content)
      case content
      when /^==/
        :rd
      when /^!!/
        :hiki
      when /^\#\#/
        :markdown
      when /^h\d\./
        :textile
      else
        nil
      end
    end

    def heading_mark_pattern(type)
      case type
      when :rd
        /\A=+\s*/
      when :hiki
        /\A!+\s*/
      when :markdown
        /\A\#+\s*/
      when :textile
        /\Ah\d\.\s*/
      else
        heading_mark_pattern(:rd)
      end
    end

    def remove_backup_paths(paths)
      paths.reject do |path|
        path.end_with?("~")
      end
    end
  end
end
