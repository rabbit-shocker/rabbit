# -*- ruby -*-
#
# Copyright (C) 2008-2012  Kouhei Sutou <kou@cozmixng.org>
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
require "rubygems"
require "rubygems/package_task"
require "bundler/gem_helper"
require "gettext/task"

base_dir = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(base_dir, 'lib'))

rsync_base_path = "rabbit@rabbit-shocker.org:public_html/"

helper = Bundler::GemHelper.new(base_dir)
def helper.version_tag
  version
end

helper.install
spec = helper.gemspec
version = spec.version.to_s

def force_array(enumerable)
  array = []
  enumerable.each do |element|
    array << element
  end
  array
end

def spec.files
  @files = force_array(super)
end

def spec.extra_rdoc_files
  @extra_rdoc_files = force_array(super)
end

GetText::Task.new(spec) do |task|
  task.files -= Dir.glob("sample/**/*.*")
  task.mo_base_directory = "data"
end

Gem::PackageTask.new(spec) do |package|
  package.need_tar_gz = true
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
    sh("entities/ent2rb.rb entities/*.ent")
  end
end

namespace :html do
  screenshots = []

  languages = ["ja", "en"]
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
  task :generate => screenshots do
    Dir.chdir("doc") do
      sh("ruby", "-S", "jekyll")
    end
  end

  desc "publish HTML."
  task :publish => :generate do
    sh("rsync", "-avz", "--delete",
       "--exclude", "*.svn",
       "--exclude", "*-raw.png",
       "--exclude", "*.svg",
       "--exclude", "*.rab",
       "--exclude", "/download/",
       "--exclude", "/samples/",
       "doc/_site/",
       rsync_base_path)
  end
end

task :build => "gettext"
task :package => "gettext"

namespace :package do
  desc "Upload tar.gz."
  task :upload => :package do
    htaccess = ".htaccess"
    rabbit_tar_gz = "rabbit.tar.gz"
    current_rabbit_tar_gz = "rabbit-#{version}.tar.gz"
    File.open(htaccess, "w") do |file|
      file.puts("Options +Indexes +FollowSymlinks")
    end
    ln_s(current_rabbit_tar_gz, rabbit_tar_gz)
    sh("rsync", "-avz",
       htaccess,
       rabbit_tar_gz,
       "pkg/#{current_rabbit_tar_gz}",
       "#{rsync_base_path}download/")
    rm(rabbit_tar_gz)
    rm(htaccess)
  end
end
