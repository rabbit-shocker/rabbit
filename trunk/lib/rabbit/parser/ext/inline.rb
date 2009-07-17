require 'rabbit/element'

module Rabbit
  module Parser
    module Ext
      module Inline
        include Element

        module_function
        def sub(text)
          Subscript.new(text)
        end

        def sup(text)
          Superscript.new(text)
        end

        def note(text)
          Note.new(text)
        end

        def lang(lang, text)
          text.add_default_prop("lang", lang)
          text
        end
      end
    end
  end
end
