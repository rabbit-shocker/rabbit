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
        payload = {
          :username              => @user,
          :password              => password,
          :slideshow_title       => @title,
          :slideshow_srcfile     => Faraday::UploadIO.new(@pdf_path,
                                                          "application/pdf"),
          :slideshow_description => @description,
          :tags                  => @tags.join(","),
        }
        payload = common_payload.merge(payload)
        payload.keys.each do |key|
          payload.delete(key) if payload[key].nil?
        end
        parse_response(@connection.post("#{API_PATH_PREFIX}/upload_slideshow",
                                        payload))
      end

      private
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
          detail = "#{http_response.status}\n#{http_response.body}"
          @logger.error(_("Failed to upload: %s") % detail)
          return nil
        end

        puts http_response.body
        response = Nokogiri::XML(http_response.body)
        if response.root.name == "SlideShareServiceError"
          message = response.root.elements[0]
          @logger.error("Failed to upload: %s" % message.text)
          nil
        else
          response.xpath("/SlideShowUploaded/SlideShowID").text.to_i
        end
      end
    end
  end
end
