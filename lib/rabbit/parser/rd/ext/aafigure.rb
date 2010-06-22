require 'rabbit/parser/ext/aafigure'

module Rabbit
  module Parser
    class RD
      module Ext
        module AAFigure
          def make_image_by_aafigure(source, visitor)
            make_image_from_file(source, visitor) do |src_file_path, prop|
              Parser::Ext::AAFigure.make_image_by_aafigure(src_file_path, prop,
                                                           visitor)
            end
          end
        end
      end
    end
  end
end
