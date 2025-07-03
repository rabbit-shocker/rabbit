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

require_relative "base"

module Rabbit
  module Source
    class ARGF

      include Base

      def self.initial_args_description
        N_("none (get from STDIN) or [FILE_NAMES]")
      end

      def initialize(encoding, argf)
        super(encoding)
        @argf = argf
      end

      private
      def _read
        begin
          @argf.read
        rescue
          Rabbit.logger.error($!.message)
          +""
        end
      end
    end
  end
end
