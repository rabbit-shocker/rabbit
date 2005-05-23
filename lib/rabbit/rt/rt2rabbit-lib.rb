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

    def process_header(table)
      return if @header.empty?
      headers = Element::TableHeaders.new
      @header.each do |head|
        each_cell(head) do |cell|
          header = Element::TableHeader.new(cell.value)
          setup_text_align(header, cell.align)
          headers << header
        end
      end
      table << headers
    end

    def process_body(table)
      return if @body.empty?
      body = Element::TableBody.new
      @body.each do |r|
        row = Element::TableRow.new
        each_cell(r) do |c|
          cell = Element::TableCell.new(c.value)
          setup_text_align(cell, c.align)
          row << cell
        end
        body << row
      end
      table << body
    end

    def each_cell(ary)
      ary.each do |x|
        if x.is_a?(RT::RTCell)
          yield x
        end
      end
    end

    def setup_text_align(target, align)
      pango_align = nil
      case align
      when :center
        pango_align = Pango::Layout::ALIGN_CENTER
      when :RIGHT
        pango_align = Pango::Layout::ALIGN_RIGHT
      else
        pango_align = Pango::Layout::ALIGN_LEFT
      end
      target.align = pango_align
    end
    
  end
end
