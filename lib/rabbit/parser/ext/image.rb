# Copyright (C) 2007-2025  Sutou Kouhei <kou@cozmixng.org>
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

require "uri"
require "cgi"
require "open-uri"
require "fileutils"

require "rabbit/element"
require "rabbit/filename"

module Rabbit
  module Parser
    module Ext
      module Image
        module_function
        def make_image(canvas, uri_str, prop={}, body: nil)
          path = Private.uri_string_to_image_filename(canvas, uri_str)
          begin
            image = Element::Image.new(path, prop, canvas: canvas)
          rescue Error
            Rabbit.logger.warn($!.message)
            nil
          else
            if prop["align"] == "right" and body
              if body["background-image"]
                raise ParseError,
                      _("multiple right aligns aren't supported.")
              end
              prop.each do |name, value|
                name = name.gsub(/_/, "-")
                next if name == "src"
                property_name = "background-image-#{name}"
                body[property_name] = value
              end
              body["background-image"] = uri_str
              :no_element
            else
              image
            end
          end
        end

        def make_image_from_file(canvas, source, extension: nil, **options)
          src_basename = "rabbit-image-source"
          src_basename = [src_basename, extension] if extension
          src_file = Tempfile.new(src_basename)
          src_file.open
          src_file.print(source)
          src_file.close
          image_file = prop = nil
          begin
            image_file, prop = yield(src_file)
          rescue ImageLoadError
            Rabbit.logger.warn($!.message)
          end
          return nil if image_file.nil?
          image = make_image(canvas,
                             "file://#{image_file.path}",
                             prop,
                             **options)
          # Protect image from GC
          case image
          when Element::Image
            image["_src"] = image_file
          when :no_element
            options[:body]["_background-image-src"] = image_file
          end
          image
        end

        module Private
          ALLOWED_IMG_URL_SCHEME = ["http", "https", "file"]

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
              Filename.new(uri.path).encode
            when "http", "https", "ftp"
              other_uri_filename(canvas, uri)
            else
              nil
            end
          end

          def local_path_to_image_filename(canvas, path)
            path = Pathname.new(Filename.new(path).encode)
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
            return if File.exist?(filename)
            begin
              URI.open(uri, "rb") do |in_file|
                File.open(filename, "wb") do |out|
                  out.print(in_file.read)
                end
              end
            rescue SocketError, OpenURI::HTTPError
              Rabbit.logger.warn("#{$!.message}: #{uri}")
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
