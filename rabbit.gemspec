# Copyright (C) 2012-2019  Kouhei Sutou <kou@cozmixng.org>
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

require_relative "lib/rabbit/version"

Gem::Specification.new do |spec|
  spec.name = "rabbit"
  spec.version = Rabbit::VERSION.dup
  spec.homepage = "http://rabbit-shocker.org/"
  spec.authors = ["Kouhei Sutou"]
  spec.email = ["kou@cozmixng.org"]
  spec.summary = "Rabbit is a presentation tool for Rubyist"
  normalize_new_lines = lambda do |text|
    paragraphs = text.split("\n\n").collect do |paragraph|
      paragraph.gsub("\n", "")
    end
    paragraphs.join("\n\n")
  end
  spec.description = normalize_new_lines.call(<<-DESCRIPTION)
You can create your slide as a text file. It means that you can version control
your slide like your Ruby scripts. You can custom your slide style by Ruby.
So Rabbit is for Rubyist.

You can use RD, Markdown and Wiki format as slide source.

Rabbit provides programmer friendly keyboard interface. It uses Emacs and Vi
style keybindings by default.

You can use PDF and image as slide source. Rabbit can show PDF and image
directly. You can create your slide by other presentation tool and show your
slide by Rabbit. If you show your slide by Rabbit, you can use programmer
friendly keyboard interface provided by Rabbit to control your slide.

You can upload your slide as a gem. If you publish your slide as a gem, you
can see your slide at https://slide.rabbit-shocker.org/ .
  DESCRIPTION
  spec.license = "GPLv2+"

  spec.files = ["Rakefile", "COPYING", "GPL", "README", "Gemfile"]
  spec.files += ["#{spec.name}.gemspec"]
  spec.files += Dir.glob("lib/**/*.{rb,erb,ui}")
  spec.files += Dir.glob("{data,entities,sample,misc,doc}/**/*")
  spec.files += Dir.glob("po/*/*.po")
  spec.files -= Dir.glob("doc/_site/**/*")
  spec.files += Dir.glob("*.rb")
  spec.files.reject! do |file|
    not File.file?(file)
  end
  spec.test_files = Dir.glob("test/**/*.rb")
  Dir.chdir("bin") do
    spec.executables = Dir.glob("*")
  end

  spec.required_ruby_version = ">= 2.1.0"

  spec.add_runtime_dependency("gdk_pixbuf2", ">= 3.0.9")
  spec.add_runtime_dependency("gtk3")
  spec.add_runtime_dependency("rsvg2", ">= 3.1.4")
  spec.add_runtime_dependency("poppler", ">= 3.2.5")
  spec.add_runtime_dependency("hikidoc")
  spec.add_runtime_dependency("nokogiri")
  spec.add_runtime_dependency("rdtool")
  spec.add_runtime_dependency("rttool")
  spec.add_runtime_dependency("coderay", ">= 1.0.0")
  spec.add_runtime_dependency("kramdown-parser-gfm")
  spec.add_runtime_dependency("gettext", ">= 3.0.1")
  spec.add_runtime_dependency("faraday")
  # spec.add_runtime_dependency("gstreamer")
  spec.add_runtime_dependency("rouge")

  spec.add_development_dependency("test-unit")
  spec.add_development_dependency("test-unit-rr")
  spec.add_development_dependency("rake")
  spec.add_development_dependency("bundler")
  # spec.add_development_dependency("jekyll", ">= 1.0.2")
end
