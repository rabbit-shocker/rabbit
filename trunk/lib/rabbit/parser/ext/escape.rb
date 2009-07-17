require 'English'

module Rabbit
  module Parser
    module Ext
      module Escape
        META_CHAR = {"<" => "&#60;", ">" => "&#62;", "&" => "&#38;"}

        module_function
        def escape_meta_character(str)
          str.gsub(/[<>&]/) do
            META_CHAR[$MATCH]
          end
        end
      end
    end
  end
end
