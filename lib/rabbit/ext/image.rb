require 'uri'
require 'cgi'
require 'open-uri'

require 'rabbit/element'

module Rabbit
  module Ext
    module Image
      ALLOWED_IMG_URL_SCHEME = ['http', 'file', '']
      def img(label, content, visitor)
        label = label.to_s
        return nil unless /^img:\s*(.+)$/ =~ label
        make_image(visitor, $1)
      end

      def make_image(visitor, uri_str, prop={})
        uri = URI.parse(uri_str)
        scheme = uri.scheme
        unless ALLOWED_IMG_URL_SCHEME.include?(scheme.to_s.downcase)
          return nil
        end
        begin
          Element::Image.new(image_filename(visitor.base, uri), prop)
        rescue ImageLoadError
          STDERR.puts $!.message
          nil
        end
      end

      private
      def image_filename(base, uri)
        filename = nil
        case uri.scheme
        when nil
          filename = GLib.filename_from_utf8(uri.to_s)
          new_uri = URI.parse([base, filename].join("/"))
          filename = new_uri.to_s
        when /file/i
          filename = GLib.filename_from_utf8(uri.path)
        else
          filename = generate_tmp_filename(base, uri.to_s)
          File.open(filename, "wb") do |out|
            uri.open("rb") do |in_file|
              out.print(in_file.read)
            end
          end
        end
        filename
      end

      def generate_tmp_filename(base, key)
        File.join(base, CGI.escape(key))
      end
      
    end
  end
end
