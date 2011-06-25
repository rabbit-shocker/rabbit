# -*- ruby -*-

require 'find'
require 'rubygems'
require 'rubygems/package_task'
require 'jeweler'

base_dir = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(base_dir, 'lib'))
require 'rabbit/rabbit'

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

  spec.files = FileList["{lib,data,entities,bin,sample,misc,doc,po}/**/*",
                        "*.rb",
                        "Rakefile",
                        "GPL",
                        "NEWS*",
                        "README*",
                        "Gemfile"]
  spec.test_files = FileList["test/**/*.rb"]
  spec.executables -= ["rabbit.bat"]
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
    sh("jekyll", "doc", "doc/_site")
  end

  desc "publish HTML."
  task :publish => :generate do
    sh("rsync", "-avz", "--delete",
       "--exclude", "*.svn",
       "--exclude", "*-raw.png",
       "--exclude", "*.svg",
       "--exclude", "*.rab",
       "doc/_site/",
       "rabbit@rabbit-shockers.org:public_html/")
  end
end

desc "Tag the current revision."
task do
  sh("git tag -a #{version} -m 'release #{version}!!!'")
end
