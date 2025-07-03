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

require_relative "container-element"

module Rabbit
  module Element
    module SlideElement
      include ContainerElement
      include BlockElement

      attr_accessor :index, :drawing_index, :transition
      def initialize(title_element)
        @index = -1
        @default_waited_draw_procs = []
        super(title_element)
      end

      def slide
        self
      end

      def title
        @elements.first.text
      end

      def size_ratio
        self["size-ratio"]
      end

      def draw(canvas, simulation=nil)
        if simulation.nil?
          begin
            draw(canvas, true)
            draw(canvas, false)
          rescue StandardError, LoadError
            Rabbit.logger.warn($!)
          end
        else
          canvas.draw_slide(self, simulation) do
            compile(canvas, 0, 0, canvas.width, canvas.height)
            super(simulation)
          end
          run_gc unless simulation
        end
      end

      def clear_waiting
        @drawing_index = 0
      end

      def clear_transition
        @transition = nil
      end

      def clear_theme
        super
        clear_waiting
        clear_transition
        @waited_draw_procs = @default_waited_draw_procs.dup
      end

      def first?(index=nil)
        (index || @drawing_index).zero?
      end

      def last?(index=nil)
        @waited_draw_procs.size == (index || @drawing_index)
      end

      def move_to_next
        @drawing_index += 1 unless last?
      end

      def move_to_previous
        @drawing_index -= 1 unless first?
      end

      def register_default_wait_proc(target, exact=false, &proc)
        @default_waited_draw_procs << [target, exact, proc]
      end

      def register_wait_proc(target, exact=false, &proc)
        @waited_draw_procs << [target, exact, proc]
      end

      def flush
        @drawing_index = @waited_draw_procs.size
      end

      def waited_draw_procs(target)
        procs = []
        candidates = @waited_draw_procs[0, @drawing_index]
        candidates.each_with_index do |(t, exact, proc), i|
          next unless target == t
          if exact
            procs << proc if i == @drawing_index - 1
          else
            procs << proc
          end
        end
        procs
      end

      private
      def run_gc
        lazy_gc_timout_msec = 1000
        @compressed_gc_task_id ||= GLib::Timeout.add(lazy_gc_timout_msec) do
          GC.start
          @compressed_gc_task_id = nil
          GLib::Source::REMOVE
        end
      end
    end
  end
end
