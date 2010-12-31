require 'rabbit/canvas'

module Rabbit
  class DependencyCanvas < Canvas
    extend Forwardable

    def_delegators(:@parent,
                   :theme_name, :allotted_time, :rest_time, :activate)

    def initialize(parent, *rest, &block)
      @parent = parent
      super(*rest, &block)
    end
  end
end
