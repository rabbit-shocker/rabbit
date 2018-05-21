# Copyright (C) 2018  Kouhei Sutou <kou@cozmixng.org>
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

require "cgi/util"

module Rabbit
  class PangoMarkup
    def initialize(name, attributes, content=nil)
      @name = name
      @attributes = attributes
      @content = content
    end

    def to_s
      tag = "<#{@name}"
      @attributes.each do |name, value|
        next if value.nil?
        tag << " #{CGI.escapeHTML(name.to_s)}='#{CGI.escapeHTML(value.to_s)}'"
      end
      tag << ">"
      tag << @content.to_s if @content
      tag << "</#{@name}>"
      tag
    end
  end
end
