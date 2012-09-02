# Copyright (C) 2012  Kouhei Sutou <kou@cozmixng.org>
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

    attr_accessor :logger
    attr_reader :title, :description
    def initialize(logger=nil)
      @logger = logger || Logger.default
      @title = nil
      @description = nil
    end

    def parse(path=nil)
      path ||= Dir.glob("README*")[0]
      raise _("No README found") if path.nil?

      parse_content(File.read(path))
    end

    private
    def parse_content(content)
      blocks = content.split(/(?:\r?\n){2,}/)
      if blocks[0]
        @title = blocks[0].gsub(/\A(?:[=*!]+|h\d\.)\s*/, "")
      end
      @description = blocks[1]
    end
  end
end
