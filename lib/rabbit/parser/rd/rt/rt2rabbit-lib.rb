require 'rt/rtparser'

require 'rabbit/element'

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
          Pango::Layout::ALIGN_CENTER
        end
      when :right
        def target.default_align
          Pango::Layout::ALIGN_RIGHT
        end
      else
        def target.default_align
          Pango::Layout::ALIGN_LEFT
        end
      end
      target.align = target.default_align
    end

  end
end
