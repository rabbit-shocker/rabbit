require 'rabbit/element'
require 'rabbit/parser/ext/image'

module Rabbit
  module Parser
    class RD
      module Ext
        module Image
          include Parser::Ext::Image

          def img(label, content, visitor)
            label = label.to_s
            return nil unless /^img:\s*(.+)$/ =~ label
            make_image(visitor.canvas, $1)
          end

          def make_image_from_file(source, visitor, **options)
            src, prop = parse_source(source)
            super(visitor.canvas, src, **options) do |src_file_path|
              [yield(src_file_path, prop), prop]
            end
          end
        end
      end
    end
  end
end
