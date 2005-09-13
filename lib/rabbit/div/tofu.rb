require "div/div"
require "div/tofusession"

require 'rabbit/utils'

module Rabbit
  module Div
    class MainDiv < ::Div::Div
      set_erb(File.join("rabbit", "div", "main.erb"))

      def initialize(session)
        super
        @navi = NaviDiv.new(session)
        @comment = CommentDiv.new(session)
      end
      
      private
      def rabbit
        @session.rabbit
      end

      def image_title
        title = h(rabbit.slide_title)
        title << "(#{rabbit.current_slide_number}/"
        title << "#{rabbit.total_slide_number - 1})"
        title
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
        HTML.a_link(a(key, {}, context), label, label_only)
      end
      
      def first_link(context)
        a_link(context, "first", h("<<"), rabbit.first_slide?)
      end

      def previous_link(context)
        a_link(context, "previous", h("<"), !rabbit.have_previous_slide?)
      end

      def next_link(context)
        a_link(context, "next", h(">"), !rabbit.have_next_slide?)
      end

      def last_link(context)
        a_link(context, "last", h(">>"), rabbit.last_slide?)
      end
    end
    
    class CommentDiv < ::Div::Div
      set_erb(File.join("rabbit", "div", "comment.erb"))

      def initialize(session)
        super
        @error_message = nil
      end
      
      def do_comment(context, params)
        comment = params[comment_param_name]
        rabbit.append_comment(comment) do |error|
          @error_message = error.message
        end
      end
      
      private
      def rabbit
        @session.rabbit
      end

      def comments
        rabbit.comments
      end

      def comment_param_name
        "comment"
      end
    end
    
    class TofuSession < ::Div::TofuSession

      @@rabbit = nil
      @@comment_rabbit = nil
      
      def self.rabbit=(rabbit)
        @@rabbit = rabbit
      end

      def self.comment_rabbit=(rabbit)
        @@comment_rabbit = rabbit
      end

      attr_reader :rabbit
      attr_reader :comment_rabbit
      
      def initialize(bartender, hint=nil)
        super
        @main = MainDiv.new(self)
        @rabbit = @@rabbit
        @comment_rabbit = @@comment_rabbit
      end

      def do_GET(context)
        update_div(context)
        if image_path?(context)
          context.res_header("Content-Type", "image/#{@rabbit.image_type}")
          context.res_body(@rabbit.current_slide_image)
        else
          context.res_header("Content-Type", "text/html; charset=UTF-8")
          context.res_body(@main.to_html(context))
        end
      end

      def image_path
        if @rabbit.accept_move?
          "/image/#{@rabbit.current_slide_number}/"
        else
          "/image/current/"
        end
      end
      
      def image_path?(context)
        %r!/image/(?:current|[0-9]+)/! =~ context.req_path_info
      end
    end
  end
end
