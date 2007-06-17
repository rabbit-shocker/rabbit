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
        end
      end
    end
  end
end
