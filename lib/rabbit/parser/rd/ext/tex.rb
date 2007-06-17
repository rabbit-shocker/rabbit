require 'rabbit/parser/ext/tex'

module Rabbit
  module Parser
    class RD
      module Ext
        module TeX
          def make_image_by_LaTeX(source, visitor)
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::TeX.make_image_by_LaTeX(src_file_path, prop, visitor)
            end
          end

          def make_image_by_mimeTeX(source, visitor)
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::TeX.make_image_by_mimeTeX(src_file_path, prop,
                                                     visitor)
            end
          end

          def make_image_by_Tgif(source, visitor)
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::TeX.make_image_by_Tgif(src_file_path, prop, visitor)
            end
          end

          private
          def make_image_from_file(source, visitor)
            src, prop = parse_source(source)
            src_file = Tempfile.new("rabbit-image-source")
            src_file.open
            src_file.print(src)
            src_file.close
            image_file = nil
            begin
              image_file = yield(src_file.path, prop)
            rescue ImageLoadError
              visitor.logger.warn($!.message)
            end
            return nil if image_file.nil?
            image = make_image(visitor, %Q[file://#{image_file.path}], prop)
            return nil if image.nil?
            image["_src"] = image_file # for protecting from GC
            image
          end
        end
      end
    end
  end
end
