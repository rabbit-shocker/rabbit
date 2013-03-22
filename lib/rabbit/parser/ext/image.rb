require 'uri'
require 'cgi'
require 'open-uri'
require 'fileutils'

require 'rabbit/element'

module Rabbit
  module Parser
    module Ext
      module Image
        module_function
        def make_image(canvas, uri_str, prop={})
          path = Private.uri_string_to_image_filename(canvas, uri_str)
          begin
            Element::Image.new(path, prop)
          rescue Error
            canvas.logger.warn($!.message)
            nil
          end
        end

        def make_image_from_file(canvas, source)
          src_file = Tempfile.new("rabbit-image-source")
          src_file.open
          src_file.print(source)
          src_file.close
          image_file = prop = nil
          begin
            image_file, prop = yield(src_file.path)
          rescue ImageLoadError
            canvas.logger.warn($!.message)
          end
          return nil if image_file.nil?
          image = make_image(canvas, %Q[file://#{image_file.path}], prop)
          return nil if image.nil?
          image["_src"] = image_file # for protecting from GC
          image
        end

        module Private
          ALLOWED_IMG_URL_SCHEME = ['http', 'https', 'file']

          module_function
          def uri_string_to_image_filename(canvas, uri_string)
            if start_with_scheme?(uri_string)
              uri = URI.parse(uri_string)
              scheme = uri.scheme
              unless ALLOWED_IMG_URL_SCHEME.include?(scheme.to_s.downcase)
                return nil
              end
              uri_to_image_filename(canvas, uri)
            else
              local_path_to_image_filename(canvas, uri_string)
            end
          end

          def uri_to_image_filename(canvas, uri)
            case uri.scheme.to_s.downcase
            when "file"
              GLib.filename_from_utf8(uri.path)
            when "http", "https", "ftp"
              other_uri_filename(canvas, uri)
            else
              nil
            end
          end

          def local_path_to_image_filename(canvas, path)
            path = Pathname.new(GLib.filename_from_utf8(path))
            return path.to_s if path.absolute?

            expanded_path = canvas.full_path(path.to_s)
            if start_with_scheme?(expanded_path)
            uri_string_to_image_filename(canvas, expanded_path)
            else
              expanded_path
            end
          end

          def tmp_filename(canvas, key)
            dir = canvas.tmp_dir_name
            FileUtils.mkdir_p(dir)
            File.join(dir, CGI.escape(key))
          end

          def other_uri_filename(canvas, uri)
            filename = tmp_filename(canvas, uri.to_s)
            setup_image_file(canvas, uri, filename)
            filename
          end

          def setup_image_file(canvas, uri, filename)
            begin
              open(uri, "rb") do |in_file|
                File.open(filename, "wb") do |out|
                  out.print(in_file.read)
                end
              end
            rescue SocketError, OpenURI::HTTPError
              canvas.logger.warn("#{$!.message}: #{uri}")
            end
          end

          def start_with_scheme?(uri_string)
            /\A[a-z][a-z\d+\-.]+:/i =~ uri_string
          end
        end
      end
    end
  end
end
