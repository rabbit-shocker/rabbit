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

require 'rabbit/source/base'

module Rabbit
  module Source
    class File
      include Base

      def self.initial_args_description
        N_("[FILENAME]")
      end

      def initialize(encoding, name)
        @name = name
        super(encoding)
        @mtime = nil
      end

      def _read
        begin
          check_file
          ::File.open(@name, "rb") do |f|
            @mtime = f.mtime
            f.read
          end
        rescue SourceUnreadableError
          Rabbit.logger.error($!.message)
          @mtime = Time.now + LimitAccessInterval::MINIMUM_ACCESS_TIME
          +""
        end
      end

      def need_read?
        super or old?(@mtime, :mtime)
      end

      def extension
        extract_extension(@name)
      end

      private
      def check_file
        unless ::File.exist?(@name)
          raise NotExistError.new(@name)
        end
        unless ::File.file?(@name)
          raise NotFileError.new(@name)
        end
        unless ::File.readable?(@name)
          raise NotReadableError.new(@name)
        end
      end

      def mtime
        begin
          check_file
          ::File.mtime(@name)
        rescue SourceUnreadableError
          Time.now
        end
      end

      def init_base
        set_base(::File.dirname(@name))
      end

    end

  end
end
