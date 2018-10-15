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

require "glib2"

module Rabbit
  class Filename
    class << self
      def sanitize(string)
        string.gsub(/[\r\n]/, "").gsub(/[\/\\:]/, "-")
      end
    end

    def initialize(filename)
      @utf8_filename = filename.encode("UTF-8")
    end

    def encode
      if GLib.const_defined?(:Win32)
        GLib::Win32.locale_filename_from_utf8(@utf8_filename)
      else
        GLib.filename_from_utf8(@utf8_filename)
      end
    end
  end
end
