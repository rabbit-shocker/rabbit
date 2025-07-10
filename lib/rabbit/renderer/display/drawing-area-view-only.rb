# Copyright (C) 2005-2025  Sutou Kouhei <kou@cozmixng.org>
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

require_relative "../engine"
require_relative "drawing-area-primitive"

module Rabbit
  module Renderer
    module Display
      class DrawingAreaViewOnly < Renderer::Base
        include Renderer::Engine::Cairo
        include DrawingAreaPrimitive

        def attach_to(window, container=nil, &block)
          super
          add_widgets_to_container(@container, &block)
          widget.show
        end

        def detach
          widget.hide
          unless @window.destroyed?
            remove_widgets_from_container(@container)
          end

          super
        end

        private
        def init_color
          super
          init_engine_color
        end

        def add_widgets_to_container(container, &block)
          if block_given?
            yield(container, @area)
          else
            container.add(@area)
          end
        end

        def remove_widgets_from_container(container)
          container.remove(@area)
        end
      end
    end
  end
end
