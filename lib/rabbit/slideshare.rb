# Copyright (C) 2012 Kouhei Sutou <kou@cozmixng.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

require "uri"
require "digest/sha1"

require "nokogiri"
require "faraday"

require "rabbit/gettext"
require "rabbit/command/rabbit"

module Rabbit
  module Task
    class SlideShare
      class Error < StandardError
      end

      include GetText

      BASE_URL = "https://www.slideshare.net"
      API_PATH_PREFIX = "/api/2"
      API_KEY = "NB1B0IzS"
      SHARED_SECRET = "iuTFlPzU"

      attr_accessor :user, :pdf_path, :title, :description, :tags
      def initialize(logger)
        @logger = logger
        @user = nil
        @pdf_path = nil
        @title = nil
        @description = nil
        @tags = []
        @connection = Faraday.new(:url => BASE_URL) do |builder|
          builder.request  :multipart
          builder.request  :url_encoded
          builder.response :logger, @logger
          builder.adapter  :net_http
        end
      end

      def upload
        id = nil
        begin
          id = upload_slide
        rescue Error
          @logger.error(_("Feailed to upload: %s") % $!.message)
          return nil
        end

        slide_url = nil
        begin
          sldie_url = slide_url(id)
        rescue Error
          @logger.error(_("Feailed to get slide URL: %s") % $!.message)
          return nil
        end
        slide_url
      end

      private
      def upload_slide
        payload = {
          :username              => @user,
          :password              => password,
          :slideshow_title       => @title,
          :slideshow_srcfile     => Faraday::UploadIO.new(@pdf_path,
                                                          "application/pdf"),
          :slideshow_description => @description,
          :tags                  => @tags.join(","),
        }
        response = post("upload_slideshow", payload)
        parse_upload_slideshow_response(response)
      end

      def slide_url(id)
        payload = {
          :slideshow_id => id,
        }
        response = get("get_slideshow", payload)
        parse_get_slideshow_response(response)
      end

      def prepare_payload(payload)
        payload = common_payload.merge(payload)
        payload.keys.each do |key|
          payload.delete(key) if payload[key].nil?
        end
        payload
      end

      def get(command, payload)
        @connection.get(api_url(command), prepare_payload(payload))
      end

      def post(command, payload)
        @connection.post(api_url(command), prepare_payload(payload))
      end

      def api_url(command)
        "#{API_PATH_PREFIX}/#{command}"
      end

      def password
        @password ||= read_password(_("Enter password on SlideShare"))
      end

      def read_password(prompt)
        print("%s [%s]: " % [prompt, @user])
        system("/bin/stty -echo") if $stdin.tty?
        $stdin.gets.chomp
      ensure
        if $stdin.tty?
          system("/bin/stty echo")
          puts
        end
      end

      def common_payload
        timestamp = Time.now.to_i.to_s
        {
          :api_key => API_KEY,
          :ts => timestamp,
          :hash => Digest::SHA1.hexdigest("#{SHARED_SECRET}#{timestamp}"),
        }
      end

      def parse_response(http_response)
        @logger.debug(http_response.body)

        unless http_response.success?
          raise Error, "#{http_response.status}\n#{http_response.body}"
        end

        response = Nokogiri::XML(http_response.body)
        if response.root.name == "SlideShareServiceError"
          message = response.root.elements[0]
          raise Error, message
        end

        response
      end

      def parse_upload_slideshow_response(http_response)
        response = parse_response(http_response)
        response.xpath("/SlideShowUploaded/SlideShowID").text.to_i
      end


      def parse_get_slideshow_response(http_response)
        response = parse_response(http_response)
        response.xpath("/Slideshow/URL").text
      end
    end
  end
end
