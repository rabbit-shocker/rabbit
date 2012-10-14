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

require "fileutils"
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

    return unless rabbit_shocker_repository?(payload["repository"])

    update_site if need_update_site?(payload)
    update_github_post_receiver if need_update_github_post_receiver?(payload)
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

  def need_update_site?(payload)
    payload["commits"].any? do |commit|
      doc_directory_changed?(commit)
    end
  end

  def need_update_github_post_receiver?(payload)
    return false unless payload["repository"]["name"] == "rabbit"

    payload["commits"].any? do |commit|
      github_post_receiver_changed?(commit)
    end
  end

  def rabbit_shocker_repository?(repository)
    repository["owner"]["name"] == "rabbit-shocker"
  end

  def effected_files(commit)
    files = []
    files |= (commit["added"] || [])
    files |= (commit["removed"] || [])
    files |= (commit["modified"] || [])
    files
  end

  def doc_directory_changed?(commit)
    effected_files(commit).any? do |effected_file|
      effected_file.start_with?("doc/")
    end
  end

  def github_post_receiver_changed?(commit)
    effected_files(commit).any? do |effected_file|
      effected_file.start_with?("misc/github-post-receiver/")
    end
  end

  def update_site
    rake(["update", "html:publish:local"],
         :log_file_name => "update-site.log")
  end

  def update_github_post_receiver
    rake(["update", "github:post_receiver:restart"],
         :log_file_name => "update-github-post-receiver.log")
  end

  def rake(rake_options, options={})
    log_file_name = File.join(tmp_dir, options[:log_file_name] || "rake.log")
    env = {
      "LANG" => "ja_JP.UTF-8",
    }
    rake = Gem.bin_path("rake", "rake")
    File.open("/dev/null") do |null|
      File.open(log_file_name, "w") do |log|
        options = {
          :chdir => top_dir,
          :in => null,
          [:out, :err] => log,
        }
        Process.spawn(env,
                      "xvfb-run", Gem.ruby, rake, *rake_options,
                      options)
      end
    end
  end

  def top_dir
    File.expand_path(File.join(File.dirname(__FILE__), "..", ".."))
  end

  def tmp_dir
    File.expand_path(File.join(File.dirname(__FILE__), "tmp"))
  end
end

receiver = lambda do |env|
  receiver = GitHubPostReceiver.new(env)
  receiver.run
end
run receiver
