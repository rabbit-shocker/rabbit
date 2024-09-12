# Copyright (C) 2004-2024  Sutou Kouhei <kou@cozmixng.org>
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

require "uri"
require "open-uri"

require 'rabbit/source/base'

module Rabbit
  module Source
    class URI

      include Base
      include LimitAccessInterval

      class << self
        def new(encoding, logger, uri)
          parsed_uri = ::URI.parse(uri)
          case parsed_uri.scheme
          when nil
            File.new(encoding, logger, parsed_uri.path)
          when /\Afile\z/i
            File.new(encoding, logger, uri.gsub(/\Afile:\/\//i, ""))
          else
            super
          end
        end

        def initial_args_description
          N_("URI")
        end
      end

      def initialize(encoding, logger, uri)
        @uri = ::URI.parse(uri)
        super(encoding, logger)
        @last_modified = nil
      end

      def need_read?
        super or old?(@last_modified, :last_modified)
      end

      def extension
        extract_extension(@uri.path)
      end

      private
      def _read
        begin
          @uri.open do |f|
            @last_modified = f.last_modified
            f.read
          end
        rescue
          @logger.error($!.message)
          @last_modified = Time.now + MINIMUM_ACCESS_TIME
          +""
        end
      end

      def init_base
        base = @uri.dup
        base.path = ::File.dirname(base.path)
        set_base(base.to_s)
      end

      def last_modified
        begin
          @uri.open do |f|
            f.last_modified
          end
        rescue
          @logger.error($!.message)
          Time.now
        end
      end

    end

  end
end
