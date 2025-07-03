# Copyright (C) 2006-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../utils"

module Rabbit
  module Renderer
    module Display
      extend Utils

      class << self
        @initialized = false
        @preferred_class_name = nil
        def init(options={})
          if options.has_key?(:preferred_class_name)
            @preferred_class_name = options[:preferred_class_name]
          end
          unless @initialized
            @initialized = true
            dir = ::File.join("rabbit", "renderer", "display")
            require_files_under_directory_in_load_path(dir)
          end
        end

        def new(*args, &block)
          init
          target_class = nil
          if @preferred_class_name
            if const_defined?(@preferred_class_name)
              target_class = const_get(@preferred_class_name)
              target_class = nil unless target_class.is_a?(Class)
            end
          end
          target_class ||= corresponding_class_under_module(self)
          target_class.new(*args, &block)
        end
      end
    end
  end
end
