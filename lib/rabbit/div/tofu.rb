require "div/div"
require "div/tofusession"

require "rabbit/front"

module Rabbit
  module Div
    class MainDiv < ::Div::Div
      set_erb(File.join("rabbit", "div", "main.erb"))

      def initialize(session)
        super
        @navi = NaviDiv.new(session)
      end
      
      private
      def rabbit
        @session.rabbit
      end

      def image_path(context)
        "#{context.req_script_name}#{@session.image_path}"
      end
    end

    class NaviDiv < ::Div::Div
      set_erb(File.join("rabbit", "div", "navi.erb"))

      def do_first(context, params)
        rabbit.move_to_first
      end
      
      def do_previous(context, params)
        rabbit.move_to_previous_if_can
      end
      
      def do_next(context, params)
        rabbit.move_to_next_if_can
      end
      
      def do_last(context, params)
        rabbit.move_to_last
      end
      
      private
      def rabbit
        @session.rabbit
      end
      
      def a_link(context, key, label, label_only)
        result = "["
        result << a(key, {}, context) unless label_only
        result << label
        result << "</a>" unless label_only
        result << "]"
        result
      end
      
      def first_link(context)
        a_link(context, 'first', h("<<"), rabbit.first_slide?)
      end

      def previous_link(context)
        a_link(context, 'previous', h("<"), !rabbit.have_previous_slide?)
      end

      def next_link(context)
        a_link(context, 'next', h(">"), !rabbit.have_next_slide?)
      end

      def last_link(context)
        a_link(context, 'last', h(">>"), rabbit.last_slide?)
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
          context.res_header('Content-Type', "image/#{@rabbit.image_type}")
          context.res_body(@rabbit.current_page_image)
        else
          context.res_header('Content-Type', 'text/html; charset=UTF-8')
          context.res_body(@main.to_html(context))
        end
      end

      def image_path
        "/image/current/"
      end

      def accept_move?
        not (@rabbit.public_level & Front::PublicLevel::MOVE).zero?
      end
    end
  end
end
