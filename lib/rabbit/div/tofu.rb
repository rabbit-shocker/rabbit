require "div/div"
require "div/tofusession"

module Rabbit
  module Div
    class MainDiv < ::Div::Div
      set_erb(File.join("rabbit", "div", "main.erb"))

      private
      def rabbit
        @session.rabbit
      end

      def image_path(context)
        "#{context.req_script_name}#{@session.image_path}"
      end
    end
    
    class TofuSession < ::Div::TofuSession

      @@rabbit = nil
      
      def self.rabbit=(rabbit)
        @@rabbit = rabbit
      end

      attr_reader :rabbit
      
      def initialize(bartender, hint=nil)
        super
        @main = MainDiv.new(self)
        @rabbit = @@rabbit
      end

      def do_GET(context)
        update_div(context)
        if context.req_path_info == image_path
          context.res_header('Content-Type', 'image/#{@rabbit.image_type}')
          context.res_body(@rabbit.current_page_image)
        else
          context.res_header('Content-Type', 'text/html')
          context.res_body(@main.to_html(context))
        end
      end

      def image_path
        "/image/current/"
      end
    end
  end
end
