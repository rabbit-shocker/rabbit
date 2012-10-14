# -*- mode: ruby; coding: utf-8 -*-
#
# Copyright (C) 2012  Kouhei Sutou <kou@cozmixng.org>
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

require "json"
require "yaml"
require "pathname"
require "racknga"

use Rack::CommonLogger
use Rack::Runtime
use Rack::ContentLength

base_dir = Pathname(__FILE__).dirname
config_file = base_dir + "config.yaml"
if config_file.exist?
  options = YAML.load_file(config_file.to_s)
  notifier_options = options["exception_notifier"]
  notifiers = [Racknga::ExceptionMailNotifier.new(notifier_options)]
  use Racknga::Middleware::ExceptionNotifier, :notifiers => notifiers
end

class GitHubPostReceiver
  def initialize(env)
    @request = Rack::Request.new(env)
    @response = Rack::Response.new
    @response.headers["Content-Type"] = "text/plain"
  end

  def run
    process
    @response.finish
  end

  private
  def process
    payload = parse_payload
    return if payload.nil?

    update if need_update?(payload)
  end

  def parse_payload
    payload = @request.params["payload"]
    if payload.nil?
      @response.status = Rack::Utils.status_code(:bad_request)
      @response.write("No payload.")
      return nil
    end

    log_payload(payload)

    begin
      JSON.parse(payload)
    rescue JSON::ParseError
      @response.status = Rack::Utils.status_code(:bad_request)
      @response.write("Invalid JSON: #{$!.message}\n#{payload}")
      nil
    end
  end

  MB = 1000 * 1000
  def log_payload(payload)
    return if payload.bytesize > (2 * MB)
    path = File.join(tmp_dir, "latest-payload.json")
    File.open(path, "w") do |latest_payload_file|
      latest_payload_file.print(payload)
    end
  end

  def need_update?(payload)
    return false unless rabbit_repository?(payload["repository"])
    payload["commits"].any? do |commit|
      doc_directory_changed?(commit)
    end
  end

  def rabbit_repository?(repository)
    repository["name"] == "rabbit" and
      repository["owner"]["name"] == "rabbit-shocker"
  end

  def doc_directory_changed?(commit)
    effected_files = []
    effected_files |= (commit["added"] || [])
    effected_files |= (commit["removed"] || [])
    effected_files |= (commit["modified"] || [])
    effected_files.any? do |effected_file|
      effected_file.start_with?("doc/")
    end
  end

  def update
    rake = Gem.bin_path("rake", "rake")
    Process.spawn([Gem.ruby, rake, "update", "html:publish:local"],
                  :chdir => top_dir,
                  [:out, :err] => File.open(File.join(tmp_dir, "update.log")))
  end

  def top_dir
    File.expand_path(File.join(File.dirname(__FILE__), "..", ".."))
  end

  def tmp_dir
    File.join(File.dirname(__FILE__), "tmp")
  end
end

receiver = lambda do |env|
  receiver = GitHubPostReceiver.new(env)
  receiver.run
end
run receiver
