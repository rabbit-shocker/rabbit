require 'erb'
require 'fileutils'

begin
  require 'rss/maker'
rescue LoadError
end

require 'rabbit/rabbit'
require 'rabbit/front'
require 'rabbit/utils'
require 'rabbit/keys'

module Rabbit
  module HTML
    class Generator
      include ERB::Util
      public :h

      path = ["rabbit", "html", "template.erb"]
      template_path = Utils.find_path_in_load_path(*path)
      raise CantFindHTMLTemplate.new(File.join(*path)) if template_path.nil?
      erb = File.open(template_path) {|f| ERB.new(f.read, nil, "-")}
      erb.def_method(self, "to_html", template_path)

      def initialize(canvas, base_name, image_type,
                     output_html, rss_base_uri)
        @canvas = canvas
        @suffix = "html"
        @rss_info = []
        @rss_file_name = "index.rdf"
        @rss_base_uri = rss_base_uri
        @base_name = base_name
        @base_dir = File.dirname(@base_name)
        @image_type = image_type
        @output_html = output_html
        @file_name_format =
          "#{@base_name}%0#{number_of_places(@canvas.slide_size)}d%s.%s"
        FileUtils.mkdir_p(to_filename_encoding(@base_dir))
      end

      def save(pixbuf, slide_number)
        @slide_number = slide_number
        save_pixbuf(pixbuf)
        file_name = slide_file_name
        File.open(file_name, "w") do |f|
          f.print(to_html)
        end
        if @rss_base_uri
          slide = @canvas.slides[slide_number]
          @rss_info << [file_name, slide_title(slide_number),
                        slide.text, slide.to_html(self)]
        end
        @slide_number = nil
      end

      def save_rss
        return true if @rss_base_uri.nil?
        if Object.const_defined?(:RSS)
          rss = make_rss
          name = File.join(@base_dir, @rss_file_name)
          File.open(to_filename_encoding(name), "w") do |f|
            f.print(rss.to_s)
          end
          true
        else
          false
        end
      end

      def save_pixbuf(pixbuf, optional=nil)
        pixbuf.save(pixbuf_file_name(@slide_number, optional),
                    normalized_image_type)
        h(image_src(@slide_number, optional))
      end

      def number_of_places(num)
        n = 1
        target = num
        while target >= 10
          target /= 10
          n += 1
        end
        n
      end

      def image_title(slide_number=@slide_number)
        title = h(slide_title(slide_number))
        title << "(#{slide_number}/#{@canvas.slide_size - 1})"
        title
      end

      private
      def to_filename_encoding(utf8_filename)
        if GLib.respond_to?(:win32_locale_filename_from_utf8)
          GLib.win32_locale_filename_from_utf8(utf8_filename)
        else
          GLib.filename_from_utf8(utf8_filename)
        end
      end

      def make_file_name(slide_number, suffix, optional=nil, convert=true)
        optional = "-#{optional}" if optional
        name = @file_name_format % [slide_number, optional || '', suffix]
        if convert
          to_filename_encoding(name)
        else
          name
        end
      end

      def slide_file_name(slide_number=@slide_number)
        if !@canvas.index_mode? and slide_number.zero?
          File.join(@base_dir, "index.#{@suffix}")
        else
          make_file_name(slide_number, @suffix)
        end
      end

      def image_file_name(slide_number=@slide_number, optional=nil)
        make_file_name(slide_number, @image_type, optional)
      end

      def pixbuf_file_name(slide_number=@slide_number, optional=nil)
        make_file_name(slide_number, @image_type, optional, false)
      end

      def normalized_image_type
        case @image_type
        when /jpg/i
          "jpeg"
        else
          @image_type.downcase
        end
      end

      def href(slide_number)
        name = slide_file_name(slide_number)
        h(File.basename(name))
      end

      def a_link(slide_number, label, label_only)
        _href = href(slide_number)
        HTML.a_link("<a href=\"#{_href}\">", label, label_only)
      end

      def slide_image(slide_number=@slide_number)
        src = image_src(slide_number)
        img = "<img title=\"#{image_title(slide_number)}\" src=\"#{src}\" />"
        if last_slide?(slide_number)
          img
        else
          href = next_href(slide_number)
          "<a href=\"#{href}\">\n#{img}\n</a>"
        end
      end

      def first_slide?(slide_number=@slide_number)
        slide_number.zero?
      end

      def last_slide?(slide_number=@slide_number)
        @canvas.slide_size.zero? or slide_number == @canvas.slide_size - 1
      end

      def first_index(slide_number=@slide_number)
        0
      end

      def previous_index(slide_number=@slide_number)
        slide_number - 1
      end

      def next_index(slide_number=@slide_number)
        slide_number + 1
      end

      def last_index(slide_number=@slide_number)
        @canvas.slide_size - 1
      end

      def first_link(slide_number=@slide_number)
        a_link(first_index(slide_number),
               h("<<"), first_slide?(slide_number))
      end

      def previous_link(slide_number=@slide_number)
        a_link(previous_index(slide_number),
               h("<"), first_slide?(slide_number))
      end

      def next_link(slide_number=@slide_number)
        a_link(next_index(slide_number),
               h(">"), last_slide?(slide_number))
      end

      def last_link(slide_number=@slide_number)
        a_link(last_index(slide_number),
               h(">>"), last_slide?(slide_number))
      end

      def first_href(slide_number=@slide_number)
        href(first_index(slide_number))
      end

      def previous_href(slide_number=@slide_number)
        href(previous_index(slide_number))
      end

      def next_href(slide_number=@slide_number)
        href(next_index(slide_number))
      end

      def last_href(slide_number=@slide_number)
        href(last_index(slide_number))
      end

      def navi(slide_number=@slide_number)
        result = ''
        result << '<div class="navi">'
        result << first_link(slide_number)
        result << previous_link(slide_number)
        result << next_link(slide_number)
        result << last_link(slide_number)
        result << '</div>'
        result
      end

      def image_src(slide_number=@slide_number, optional=nil)
        File.basename(image_file_name(slide_number, optional))
      end

      def slide_title(slide_number=@slide_number)
        title = Utils.unescape_title(@canvas.slide_title(slide_number))
        if Utils.windows?
          GLib.locale_from_utf8(title)
        else
          title
        end
      end

      def encoding
        if Utils.windows?
          "Shift_JIS"
        else
          "UTF-8"
        end
      end
      alias charset encoding

      def make_rss
        base_uri = @rss_base_uri.chomp('/') + '/'
        RSS::Maker.make('1.0') do |maker|
          now = Time.now
          title_slide_info = @rss_info.first
          filename, title, text, html = title_slide_info
          maker.channel.about = "#{base_uri}index.rdf"
          maker.channel.title = title
          maker.channel.description = text
          maker.channel.link = base_uri
          maker.channel.date = now

          @rss_info.each_with_index do |info, i|
            filename, title, text, html = info
            item = maker.items.new_item
            item.link = "#{base_uri}#{File.basename(filename)}"
            item.title = title
            item.description = text
            item.content_encoded = normalize_html_reference(html, base_uri)
            item.date = now - i
          end
        end
      end

      def normalize_html_reference(html, base_uri)
        html.gsub(/(href|src)=(["'])((?:(?!\2)[^:])+)\2/) do
          "#{$1}=#{$2}#{base_uri}#{$3}#{$2}"
        end
      end
    end
  end
end
