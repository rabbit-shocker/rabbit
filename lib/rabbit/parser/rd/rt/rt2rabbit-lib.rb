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

require "rt/rtparser"

require_relative "../../../element"

module Rabbit

  class RT2RabbitVisitor

    def initialize(rd_visitor)
      @rd_visitor = rd_visitor
    end

    def visit(parsed)
      @rt = parsed
      @header = @rt.header
      @body = @rt.body
      @caption = @rt.config['caption']

      process
    end

    private
    def process
      props = {
        "caption" => @caption
      }
      table = Element::Table.new(props)
      process_header(table)
      process_body(table)
      table
    end

    def process_block(table, targets, block_class, cell_class)
      return if targets.empty?
      block = block_class.new
      targets.each do |r|
        row = Element::TableRow.new
        each_cell(r) do |c|
          tree = ::RD::RDTree.new("=begin\n#{c.value}\n=end\n")
          if tree.root.children.empty?
            elements = []
          else
            elements = tree.root.children[0].accept(@rd_visitor).elements
          end
          cell = cell_class.new(elements)
          setup_text_align(cell, c.align)
          row << cell
        end
        block << row
      end
      table << block
    end

    def process_header(table)
      process_block(table, @header, Element::TableHead, Element::TableHeader)
    end

    def process_body(table)
      process_block(table, @body, Element::TableBody, Element::TableCell)
    end

    def each_cell(ary)
      ary.each do |x|
        if x.is_a?(RT::RTCell)
          yield x
        end
      end
    end

    def setup_text_align(target, align)
      case align
      when :center
        def target.default_align
          Pango::Alignment::CENTER
        end
      when :right
        def target.default_align
          Pango::Alignment::RIGHT
        end
      else
        def target.default_align
          Pango::Alignment::LEFT
        end
      end
      target.align = target.default_align
    end

  end
end
