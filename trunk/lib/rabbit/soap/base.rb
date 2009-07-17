module Rabbit
  module SOAP
    NS = "http://www.cozmixng.org/ns/rabbit/0.0.1/"

    module_function
    def element_name(name)
      case name
      when /[?!]$/
        name[0..-2]
      when /[=]$/
        "set_#{name[0..-2]}"
      else
        name
      end
    end
  end
end
