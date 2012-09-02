#!/usr/bin/env ruby

require "test-unit"
require "test/unit/notify"
require "test/unit/rr"

test_file = "test/test-*.rb"

$LOAD_PATH.unshift(File.join(File.expand_path("."), "lib"))
$LOAD_PATH.unshift(File.join(File.expand_path("."), "test"))

Dir.glob(test_file) do |file|
  require file.gsub(/\.rb$/, '')
end
