require 'rabbit/utils'

module Rabbit
  class Searcher
    def initialize(canvas)
      @canvas = canvas
    end

    def regexp(text)
      unless text == @text
        @text = text
        @regexp = nil
      end
      @regexp ||= internal_regexp
    end

    private
    def internal_regexp
      begin
        /#{@text}/iu
      rescue RegexpError
        /#{Regexp.escape(@text)}/iu
      end
    end
  end
end
