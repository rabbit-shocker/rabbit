require 'uri'
require 'cgi'
require 'open-uri'
require 'fileutils'

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
          Element::Image.new(image_filename(visitor, uri), prop)
        rescue ImageLoadError
          visitor.logger.warn($!.message)
          nil
        end
      end

      private
      def image_filename(visitor, uri)
        case uri.scheme
        when /file/i
          GLib.filename_from_utf8(uri.path)
        when /http|ftp/i
          other_uri_filename(visitor, uri)
        else
          related_path_filename(visitor, uri)
        end
      end

      def tmp_filename(visitor, key)
        dir = visitor.tmp_dir_name
        FileUtils.mkdir_p(dir)
        File.join(dir, CGI.escape(key))
      end
      
      def related_path_filename(visitor, uri)
        image_uri = visitor.full_path(GLib.filename_from_utf8(uri.to_s))
        filename = nil

        if URI.parse(image_uri).scheme.nil?
          filename = image_uri
        else
          filename = tmp_filename(visitor, image_uri)
          setup_image_file(visitor, image_uri, filename)
        end
        
        filename
      end
      
      def other_uri_filename(visitor, uri)
        filename = tmp_filename(visitor, uri.to_s)
        setup_image_file(visitor, uri, filename)
        filename
      end

      def setup_image_file(visitor, uri, filename)
        begin
          open(uri, "rb") do |in_file|
            File.open(filename, "wb") do |out|
              out.print(in_file.read)
            end
          end
        rescue SocketError, OpenURI::HTTPError
          visitor.logger.warn("#{$!.message}: #{uri}")
        end
      end
    end
  end
end
