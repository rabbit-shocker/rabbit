module Rabbit
  module Utils
    def to_class_name(name)
      name.gsub(/(?:\A|_)([a-z])/) do |x|
        $1.upcase
      end
    end
  end
end
