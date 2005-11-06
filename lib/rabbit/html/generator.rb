require 'erb'

require 'rabbit/rabbit'
require 'rabbit/front'
require 'rabbit/utils'

module Rabbit
  module HTML
    class Generator
      extend ERB::DefMethod

      include ERB::Util

      path = ["rabbit", "html", "template.erb"]
      template_path = Utils.find_path_in_load_path(*path)
      raise CantFindHTMLTemplate.new(File.join(*path)) if template_path.nil?
      def_erb_method("to_html(file_name_format, slide_number, image_type)",
                     template_path)
      
      def initialize(canvas)
        @canvas = canvas
        @suffix = "html"
      end

      def save(file_name_format, slide_number, image_type)
        file_name = slide_file_name(file_name_format, slide_number)
        File.open(file_name, "w") do |f|
          f.print(to_html(file_name_format, slide_number, image_type))
        end
      end

      private
      def make_file_name(file_name_format, slide_number, suffix=@suffix)
        file_name_format % [slide_number, suffix]
      end

      def slide_file_name(file_name_format, slide_number)
        if slide_number.zero?
          File.join(File.dirname(file_name_format), "index.#{@suffix}")
        else
          make_file_name(file_name_format, slide_number, @suffix)
        end
      end
        
      def a_link(file_name_format, slide_number, label, label_only)
        name = slide_file_name(file_name_format, slide_number)
        href = File.basename(name)
        HTML.a_link("<a href=\"#{href}\">", label, label_only)
      end

      def first_slide?(slide_number)
        slide_number.zero?
      end

      def last_slide?(slide_number)
        @canvas.slide_size.zero? or slide_number == @canvas.slide_size - 1
      end
      
      def first_link(file_name_format, slide_number)
        a_link(file_name_format, 0, h("<<"), first_slide?(slide_number))
      end

      def previous_link(file_name_format, slide_number)
        a_link(file_name_format, slide_number - 1,
               h("<"), first_slide?(slide_number))
      end

      def next_link(file_name_format, slide_number)
        a_link(file_name_format, slide_number + 1,
               h(">"), last_slide?(slide_number))
      end

      def last_link(file_name_format, slide_number)
        a_link(file_name_format, @canvas.slide_size - 1,
               h(">>"), last_slide?(slide_number))
      end

      def navi(file_name_format, slide_number)
        result = ''
        result << '<div class="navi">'
        result << first_link(file_name_format, slide_number)
        result << previous_link(file_name_format, slide_number)
        result << next_link(file_name_format, slide_number)
        result << last_link(file_name_format, slide_number)
        result << '</div>'
        result
      end

      def image_title(slide_number)
        title = h(slide_title)
        title << "(#{slide_number}/#{@canvas.slide_size})"
        title
      end

      def image_src(file_name_format, slide_number, image_type)
        name = make_file_name(file_name_format, slide_number, image_type)
        File.basename(name)
      end

      def slide_title
        Utils.unescape_title(@canvas.slide_title)
      end
      
    end
  end
end
