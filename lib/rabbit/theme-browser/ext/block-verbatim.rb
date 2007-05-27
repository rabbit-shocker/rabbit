require 'rabbit/parser/rd'

module Rabbit
  class ThemeBrowser
    module Ext
      class BlockVerbatim < Parser::RD::Ext::Base
        include Parser::RD::Ext::Image

        def default_ext_block_verbatim(label, source, content, visitor)
          Proc.new do
            visitor.tag("verbatim-block") do
              visitor.insert(content)
            end
          end
        end

        def ext_block_verb_img(label, source, content, visitor)
          return nil unless /^(?:image|img)$/i =~ label
          src, prop = parse_source(source)
          return nil if prop['src'].nil?
          image = make_image(visitor, prop['src'], prop)
          return nil if image.nil?
          Proc.new do
            visitor.tag("image-description") do
              w = image.original_width
              h = image.original_height
              visitor.insert("(#{w}x#{h})")
              visitor.insert("\n")
            end
            visitor.tag("image") do
              visitor.insert(image.pixbuf)
              visitor.insert("\n")
            end
          end
        end
      end
    end
  end
end
