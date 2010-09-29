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
                                  INSTALL.macosx-macports.en
                                  INSTALL.macosx-macports.ja
                                  INSTALL.win32.en
                                  INSTALL.win32.ja
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
