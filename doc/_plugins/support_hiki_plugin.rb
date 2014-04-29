module Liquid
  class Block
    # Accept the following plugin markup in Hiki:
    # {{...}}
    alias_method :create_variable_origin, :create_variable
    def create_variable(token)
      begin
        create_variable_origin(token)
      rescue SyntaxError
        token
      end
    end
  end
end
