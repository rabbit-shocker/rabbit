#!/usr/bin/env ruby

require "optparse"
require "ostruct"
require "fileutils"

options = OpenStruct.new
options.rabbit = "bin/rabbit"
options.output_base = "/tmp/rabbit"
options.locale_dir = "data/locale"

opts = OptionParser.new do |opts|
  opts.banner = "#{opts.banner} RD_FILES"

  opts.on("--rabbit=RABBIT",
          "rabbit path",
          "(#{options.rabbit})") do |rabbit|
    options.rabbit = rabbit
  end

  opts.on("--output-base=BASE",
          "output base directory",
          "(#{options.output_base})") do |base|
    options.output_base = base
  end

  opts.on("--locale-dir=DIR",
          "locale directory",
          "(#{options.locale_dir})") do |dir|
    options.locale_dir = dir
  end
end

opts.parse!(ARGV)

ARGV.each do |rd|
  original_base_name = File.basename(rd, ".*")
  themes = [nil]
  case original_base_name
  when "blue-circle"
    themes << ["green-circle", :replace]
  when "lightning-talk"
    themes << "lightning-talk-with-contact"
  when "theme-bench", "theme-bench-en"
    themes.concat([
                   "red-frame", "blue-bar", "clear-blue",
                   "blue-circle", "green-circle",
                   "ruby-gnome2", "lightning-rabbit",
                   "day-white", "night-black", "cozmixng",
                  ])
  end
  themes.each do |theme, option|
    base_name = original_base_name.dup
    if theme
      case option
      when :replace
        base_name = theme
      else
        base_name << "-#{theme.sub(/^#{base_name}-/, '')}"
      end
    end
    output_dir = File.join(options.output_base, base_name)
    FileUtils.mkdir_p(output_dir)
    message = "processing #{rd}..."
    message << " (#{theme})" if theme
    puts(message)
    args = [
      "-s",
      "-b", File.join(output_dir, base_name),
      "-i", "jpg",
      "-I", File.dirname(rd),
      "--locale-dir", options.locale_dir
    ]
    args.concat(["-t", theme]) if theme
    args << rd

    begin
      lang = ENV["LANG"]
      ENV["LANG"] = "C" if /-en\b/ =~ base_name
      system(*(options.rabbit.split + args))
    ensure
      ENV["LANG"] = lang
    end
    message = "finished #{rd}."
    message << " (#{theme})" if theme
    puts(message)

    system(*(options.rabbit.split + ["--index-mode"] + args))
    message = "finished #{rd}. (index mode)"
    message << " (#{theme})" if theme
    puts(message)
  end
end

