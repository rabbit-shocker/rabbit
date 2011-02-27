# -*- ruby -*-

require 'find'
require 'rubygems'
require 'hoe'

base_dir = File.expand_path(File.dirname(__FILE__))
$LOAD_PATH.unshift(File.join(base_dir, 'lib'))
require 'rabbit/rabbit'

truncate_base_dir = Proc.new do |x|
  x.gsub(/\A#{Regexp.escape(base_dir + File::SEPARATOR)}/, '')
end

manifest = File.join(base_dir, "Manifest.txt")
manifest_contents = []
base_dir_included_components = %w(COPYING COPYING.ja GPL Manifest.txt
                                  NEWS.en
                                  NEWS.ja
                                  README.en
                                  README.ja
                                  Rakefile TODO setup.rb
                                  update-mo.rb update-po.rb)
excluded_components = %w(.svn .test-result .tmp doc log tmp pkg config.rb)
excluded_suffixes = %w(.po~)
white_list_paths = []
Find.find(base_dir + File::SEPARATOR) do |target|
  target = truncate_base_dir[target]
  components = target.split(File::SEPARATOR)
  next if components.empty?
  if components.size == 1 and !File.directory?(target)
    next unless base_dir_included_components.include?(components[0])
  end
  unless white_list_paths.include?(target)
    Find.prune if (excluded_components - components) != excluded_components
    next if /~\z/ =~ target
    next if excluded_suffixes.include?(File.extname(target))
  end
  manifest_contents << target if File.file?(target)
end

File.open(manifest, "w") do |f|
  f.puts manifest_contents.sort.join("\n")
end
at_exit do
  FileUtils.rm_f(manifest)
end

ENV["VERSION"] = Rabbit::VERSION
project = Hoe.spec('rabbit') do
  self.version = Rabbit::VERSION
  self.rubyforge_name = 'rabbit'
  self.author = ['Kouhei Sutou']
  self.email = ['kou@cozmixng.org']
  self.summary = 'Rabbit is an RD-document-based presentation application.'
  self.url = 'http://www.cozmixng.org/~rwiki/?cmd=view;name=Rabbit'
  self.test_globs = ['test/test_*.rb']
  self.changes = File.read('NEWS.en').split(/^== /)[1].gsub(/^==/, '').strip
  self.extra_deps = [
                     ['gtk2'],
                     ['gdk_pixbuf2'],
                     ['rsvg2'],
                     ['poppler'],
                     ['hikidoc'],
                     # ['gettext'],
                    ]
  self.description = self.summary # FIXME
  self.need_tar = false
end

project.spec.executables -= ["rabbit.bat"]

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

namespace :html do
  screenshots = []

  languages = ["ja", "en"]
  languages.each do |lang|
    screenshots_dir = "html/images/screenshots/#{lang}"
    directory screenshots_dir

    screenshot_rab = "html/screenshot.#{lang}.rab"

    screenshot_themes = ["blue-circle", "clear-blue", "cozmixng", "day-white",
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
  end

  desc "publish HTML."
  task :publish => :generate do
    sh("rsync", "-avz", "--delete",
       "--exclude", "*.svn",
       "--exclude", "*-raw.png",
       "--exclude", "*.svg",
       "--exclude", "*.rab",
       "html/",
       "rabbit@rabbit-shockers.org:public_html/")
  end
end
