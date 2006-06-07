require "div/div"
require "div/tofusession"

require 'rabbit/utils'

module WEBrick
  class Tofulet
    def mobile?
      %r[(DoCoMo|J-PHONE|Vodafone|MOT-|UP\.Browser|DDIPOCKET|ASTEL|PDXGW|Palmscape|Xiino|sharp pda browser|Windows CE|L-mode|WILLCOM)]i =~ @req["user-agent"]
    end
  end
end

module Rabbit
  module Div
    module RabbitDiv
      def initialize(*args)
        super
        @cache = {}
      end

      def to_div_i(context)
        to_html_i(context)
      rescue
        "<p>error! #{h($!)}</p>"
      end

      def to_html_i(context)
        to_html(context)
      end

      private
      def rabbit
        @session.rabbit
      end

      def form2(attrs, method_name, context_or_param, context_or_empty=nil)
        result = form(method_name, context_or_param, context_or_empty)
        attrs = attrs.collect do |key, value|
          "#{h(key)}=\"#{h(value)}\""
        end.join(" ")
        result.gsub!(/(<form )/, "\\1#{attrs}")
        result
      end

      def param(method_name, add_param, separator=nil)
        param = make_param(method_name, add_param)
        ary = param.collect do |k, v|
          "#{u(k)}=#{u(v)}"
        end
        ary.join(separator || h("&"))
      end

      def ajax_param(method_name, add_js_param=[])
        result = ''
        result << param(method_name, {"ajax" => "1"}, "&").dump
        add_js_param.each do |k|
          result << " + "
          result << "&#{u(k)}=".dump
          result << " + $F(#{u(k).dump})"
        end
        result
      end

      def ajax?(params)
        params.has_key?("ajax")
      end

      def uri(method_name, add_param, context)
        "#{action(context)}?#{param(method_name, add_param)}"
      end

      def cache_read_file(key, *name)
        @cache[key] ||= read_file(*name)
      end

      def read_file(*name)
        path = Utils.find_path_in_load_path(*name)
        if path
          File.read(path)
        else
          nil
        end
      end

      def to_sjis(str)
        NKF.nkf("-sWdXm0", str.to_s)
      end
    end

    class MainDiv < ::Div::Div
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "main.erb"))
      add_erb("to_html_i(context)", File.join("rabbit", "div", "main-i.erb"))
      reload_erb

      attr_reader :navi
      def initialize(session)
        super
        @slide = SlideDiv.new(session)
        @navi = NaviDiv.new(session)
        @comment = CommentDiv.new(session)
        @js = JSDiv.new(session)
        @css = CSSDiv.new(session)
      end

      def on_display(context)
        if context.mobile?
          context.res_header("Content-Type", "text/html; charset=Shift_JIS")
          context.res_body(to_sjis(to_html_i(context)))
        else
          context.res_header("Content-Type", "text/html; charset=UTF-8")
          context.res_body(to_html(context))
        end
      end
    end

    class NaviDiv < ::Div::Div
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "navi.erb"))
      add_erb("to_html_i(context)", File.join("rabbit", "div", "navi-i.erb"))
      reload_erb

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
      def a_link(context, key, label, label_only)
        HTML.a_link("<a href=\"#{uri(key, {}, context)}\">", label, label_only)
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
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "comment.erb"))
      add_erb("to_html_i(context)", File.join("rabbit", "div", "comment-i.erb"))
      add_erb("to_log_html(context=nil)", File.join("rabbit", "div", "log.erb"))
      reload_erb

      def initialize(session)
        super
        @error_message = nil
      end

      def do_comment(context, params)
        comment = params[comment_param_name]
        rabbit.append_comment(comment) do |error|
          @error_message = error.message
        end
        if ajax?(params)
          context.res_header("Content-Type", "text/html; charset=UTF-8")
          context.res_body(to_log_html(context))
          throw(:tofu_done)
        end
      end

      private
      def comments
        rabbit.comments
      end

      def comment_param_name
        "comment"
      end

      def comment_form_name
        "comment-form"
      end
    end

    class SlideDiv < ::Div::Div
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "slide.erb"))
      add_erb("to_html_i(context)", File.join("rabbit", "div", "slide-i.erb"))
      reload_erb

      def do_current(context, params)
        context.res_header("Content-Type", "image/#{rabbit.image_type}")
        context.res_body(rabbit.current_slide_image)
        throw(:tofu_done)
      end

      private
      def image_title
        title = h(rabbit.slide_title)
        title << "(#{rabbit.current_slide_number}/"
        title << "#{rabbit.total_slide_number - 1})"
        title
      end

      def image_path(context, name="current", add_param={})
        uri(name, add_param, context)
      end
    end

    class JSDiv < ::Div::Div
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "js.erb"))

      def do_else(context, params)
        cmd, = params['div_cmd']
        content = cache_read_file(cmd, "rabbit", "div", "#{cmd}.js")
        setup_js_response(context, content)
      end

      private
      def setup_js_response(context, js)
        context.res_header("Content-Type", "text/javascript; charset=UTF-8")
        context.res_body(js.to_s)
        throw(:tofu_done)
      end

      def js_path(context, name, add_param={})
        uri(name, add_param, context)
      end

      def script(context, name)
        src = js_path(context, name)
        %Q!<script type="text/javascript" src="#{src}"></script>!
      end
    end

    class CSSDiv < ::Div::Div
      include RabbitDiv

      set_erb(File.join("rabbit", "div", "css.erb"))

      def do_else(context, params)
        cmd, = params['div_cmd']
        content = cache_read_file(cmd, "rabbit", "div", "#{cmd}.css")
        setup_css_response(context, content)
      end

      private
      def setup_css_response(context, css)
        context.res_header("Content-Type", "text/css; charset=UTF-8")
        context.res_body(css.to_s)
        throw(:tofu_done)
      end

      def css_path(context, name, add_param={})
        uri(name, add_param, context)
      end

      def css_link(context, name)
        path = css_path(context, name)
        %Q!<link rel="stylesheet" type="text/css" href="#{path}" />!
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
        @main.on_display(context)
      end

      def fetch(ref)
        super || (@rabbit.accept_move? ? @main.navi : nil)
      end
    end
  end
end
