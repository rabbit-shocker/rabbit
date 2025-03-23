# -*- ruby -*-
#
# Copyright (C) 2008-2025  Kouhei Sutou <kou@cozmixng.org>
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

require "find"
require "open-uri"
require "tmpdir"

require "bundler/gem_helper"
require "gettext/tools/task"

task :default => :test

base_dir = File.expand_path(File.dirname(__FILE__))

rsync_local_path = ENV["LOCAL_DESTINATION_PATH"] || "~/public_html/"
rsync_base_path = "rabbit@rabbit-shocker.org:#{rsync_local_path}"

helper = Bundler::GemHelper.new(base_dir)
def helper.version_tag
  version
end

helper.install

release_task = Rake.application["release"]
# We use Trusted Publishing.
release_task.prerequisites.delete("build")
release_task.prerequisites.delete("release:rubygem_push")
release_task_comment = release_task.comment
if release_task_comment
  release_task.clear_comments
  release_task.comment = release_task_comment.gsub(/ and build.*$/, "")
end

spec = helper.gemspec
version = spec.version.to_s

GetText::Tools::Task.define do |task|
  task.spec = spec
  task.files -= Dir.glob("sample/**/*.*")
  task.files -= Dir.glob("test/**/*.*")
  task.mo_base_directory = "data/locale"
end

rule '.png' => ['.svg'] do |t|
  sh("inkscape", "--export-png", t.name, t.source)
end

["icon", "headline-background"].each do |base_name|
  full_base_name = "clear-blue-#{base_name}"
  png = "data/rabbit/image/clear-blue-images/#{full_base_name}.png"
  svg = "images/kou/#{full_base_name}.svg"
  task :images => png
  file png => svg do |t|
    sh("inkscape", "--export-png", t.name, *t.prerequisites)
  end
end

namespace :entity do
  desc "Update entities"
  task :update do
    ruby("entities/ent2rb.rb", *Dir.glob("entities/*.ent").sort)
  end
end

languages = ["ja", "en"]
related_products = [
  "rabwii",
  "rabbirack",
]
related_product_paths = []
related_products.each do |related_product|
  related_product_directory = "../#{related_product}"
  related_product_paths << related_product_directory
  directory related_product_directory do
    sh("git",
       "clone",
       "https://github.com/rabbit-shocker/#{related_product}.git",
       related_product_directory)
  end

  languages.each do |language|
    related_product_link = "doc/#{language}/#{related_product}"
    related_product_paths << related_product_link
    file related_product_link do
      sh("ln",
         "-s",
         "../../../#{related_product}/doc/#{language}",
         related_product_link)
    end
  end
end

namespace :doc do
  desc "Run documentation server"
  task :server => related_product_paths do
    Dir.chdir("doc") do
      sh("jekyll", "server", "--watch")
    end
  end
end

namespace :html do
  screenshots = []

  languages.each do |lang|
    screenshots_dir = "doc/images/screenshots/#{lang}"
    directory screenshots_dir

    screenshot_rab = "doc/screenshot.#{lang}.rab"

    screenshot_themes = ["blue-circle", "clear-blue", "cozmixng",
                         "dark-gradation", "day-white",
                         "debian", "green-circle", "night-black",
                         "rabbit", "ranguba", "red-frame", "ruby-gnome2",
                         "rubykaigi2011"]
    screenshot_themes.each do |theme|
      screenshot_base_name = "#{screenshots_dir}/#{theme}"
      screenshot_raw = "#{screenshot_base_name}-raw.png"
      screenshot = "#{screenshot_base_name}.png"
      screenshots << screenshot

      file screenshot_raw => [screenshots_dir, screenshot_rab, __FILE__] do
        ruby("-I", "lib", "bin/rabbit",
             "--save-as-image",
             "--theme", theme,
             "--size", "200,150",
             "--saved-image-base-name", screenshot_base_name,
             screenshot_rab)
        mv("#{screenshot_base_name}-0.png", screenshot_raw)
      end

      file screenshot => screenshot_raw do
        true_value = 1
        false_value = 0
        run_mode = false_value
        offset_x = 8
        offset_y = 8
        blur_radius = 15.0
        black = "'(122 122 122)"
        shadow_color = black
        opacity = 80.0
        allow_resizing = true_value
        drop_shadow = <<-EOC
(let* ((image (car (gimp-file-load RUN-NONINTERACTIVE
                                   "#{screenshot_raw}"
                                   "#{screenshot_raw}")))
       (picture-layer (car (gimp-image-get-active-drawable image))))
  (script-fu-drop-shadow image picture-layer
                         #{offset_x} #{offset_y}
                         #{blur_radius} #{shadow_color}
                         #{opacity} #{allow_resizing})
  (let ((layer (car (gimp-image-merge-visible-layers image CLIP-TO-IMAGE))))
    (file-png-save-defaults RUN-NONINTERACTIVE image layer
                            "#{screenshot}"
                            "#{screenshot}"))
  (gimp-image-delete image))
EOC
        sh("gimp",
           "-i",
           "-b", drop_shadow,
           "-b", "(gimp-quit TRUE)")
      end
    end
  end

  desc "generate HTML and needed files."
  task :generate => related_product_paths + screenshots do
    Dir.chdir("doc") do
      rm_rf("_site")
      ruby("-S", "jekyll", "build")
    end
  end

  namespace :publish do
    dependencies = ["html:generate"]
    exclude_paths = [
      "--exclude", "*-raw.png",
      "--exclude", "*.svg",
      "--exclude", "*.rab",
      "--exclude", "/download/",
      "--exclude", "/samples/",
    ]
    desc "publish HTML to remote."
    task :remote do
      Dir.mktmpdir do |dir|
        chdir(dir) do
          if (ENV["USE_RELEASE"] || "yes") == "yes"
            url = "https://github.com/rabbit-shocker/rabbit/releases/download/" +
                  "#{version}/docs.tar.gz"
            URI.open(url) do |response|
              File.open("docs.tar.gz", "wb") do |output|
                IO.copy_stream(response, output)
              end
            end
          else
            workflow_id = IO.pipe do |input, output|
              sh("gh",
                 "run",
                 "list",
                 "--jq", ".[].databaseId",
                 "--json", "databaseId",
                 "--limit", "1",
                 "--repo", "rabbit-shocker/rabbit",
                 "--status", "success",
                 "--workflow", "release.yaml",
                 out: output)
              output.close
              input.read.chomp
            end
            sh("gh",
               "run",
               "download",
               "--repo", "rabbit-shocker/rabbit",
               workflow_id)
            mv("docs/docs.tar.gz", "./")
          end
          sh("tar", "xf", "docs.tar.gz")
          sh("rsync",
             "-avz",
             # "--delete",
             # "--dry-run",
             *exclude_paths,
             "docs/",
             rsync_base_path)
        end
      end
    end

    desc "publish HTML to local."
    task :local => dependencies do
      sh("rsync",
         "-avz",
         "--delete",
         *exclude_paths,
         "doc/_site/",
         File.expand_path(rsync_local_path))
    end
  end
end

task :update do
  update_command = ["git", "pull", "--quiet", "--rebase"]
  sh(*update_command)
  related_projects = ["rabbirack", "rabwii"]
  related_projects.each do |project|
    project_dir = "../#{project}"
    next unless File.exist?(project_dir)
    Dir.chdir(project_dir) do
      sh(*update_command)
    end
  end
end

task :build => "gettext"
task :package => "gettext"

namespace :github do
  namespace :post_receiver do
    desc "Restart GitHub POST receiver"
    task :restart do
      touch("misc/github-post-receiver/tmp/restart.txt")
    end
  end
end

desc "Run test"
task :test do
  ruby("test/run-test.rb")
end
