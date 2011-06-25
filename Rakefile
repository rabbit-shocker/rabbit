# -*- ruby -*-

require 'find'
require 'rubygems'
require 'rubygems/package_task'
require 'jeweler'

base_dir = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(base_dir, 'lib'))
require 'rabbit/rabbit'

rsync_base_path = "rabbit@rabbit-shockers.org:public_html/"

ENV["VERSION"] ||= Rabbit::VERSION
version = ENV["VERSION"].dup
spec = nil
Jeweler::Tasks.new do |_spec|
  spec = _spec
  spec.name = "rabbit"
  spec.version = version.dup
  spec.rubyforge_project = "rabbit"
  spec.homepage = "http://rabbit-shockers.org/"
  spec.authors = ["Kouhei Sutou"]
  spec.email = ["kou@cozmixng.org"]
  spec.summary = 'Rabbit is an RD-document-based presentation application.'
  spec.description = spec.summary # FIXME
  spec.license = "GPLv2+"
  Bundler.load.dependencies_for(:default).each do |dependency|
    spec.add_runtime_dependency(dependency.name, dependency.requirement.to_s)
  end

  spec.files = FileList["{lib,data,entities,bin,sample,misc,doc,po}/**/*",
                        "*.rb",
                        "Rakefile",
                        "COPYING",
                        "GPL",
                        "README",
                        "Gemfile"]
  spec.test_files = FileList["test/**/*.rb"]
  spec.executables -= ["rabbit.bat"]
end

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

Gem::PackageTask.new(spec) do |package|
  package.need_tar_gz = true
end

rule '.png' => ['.svg'] do |t|
  sh("inkscape", "--export-png", t.name, t.source)
end

["icon", "headline-background"].each do |base_name|
  full_base_name = "clear-blue-#{base_name}"
  png = "data/rabbit/image/clear-blue-images/#{full_base_name}.png"
  svg = "sample/kou/#{full_base_name}.svg"
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
                         "rabbit", "ranguba", "red-frame", "ruby-gnome2"]
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
       "doc/_site/",
       rsync_base_path)
  end
end

desc "Tag the current revision."
task :tag do
  sh("git tag -a #{version} -m 'release #{version}!!!'")
end

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
