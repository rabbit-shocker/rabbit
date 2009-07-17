# -*- ruby -*-

rule '.png' => ['.svg'] do |t|
  sh("inkscape", "--export-png", t.name, t.source)
end

task :default => :images

clear_blue_images = 

["icon", "headline-background"].each do |base_name|
  full_base_name = "clear-blue-#{base_name}"
  png = "data/rabbit/image/clear-blue-images/#{full_base_name}.png"
  svg = "sample/kou/#{full_base_name}.svg"
  task :images => png
  file png => svg do |t|
    sh("inkscape", "--export-png", t.name, *t.prerequisites)
  end
end
