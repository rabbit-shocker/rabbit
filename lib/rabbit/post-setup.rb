File.open("config.rb", "w") do |f|
  f.print <<-EOC
require "rabbit/default-config"

Rabbit::Config::IMAGE_PATH.unshift('#{config("datadir")}')
EOC
end
