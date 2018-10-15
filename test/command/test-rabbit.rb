# Copyright (C) 2018  Kouhei Sutou <kou@cozmixng.org>
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

require "rabbit-test-utils"

class RabbitCommandRabbitTest < Test::Unit::TestCase
  def setup
    @base_dir = File.expand_path(File.join(__dir__, "..", ".."))
    @dir = Dir.mktmpdir
  end

  def teardown
    FileUtils.rm_rf(@dir)
  end

  def rabbit(*arguments)
    Dir.chdir(@dir) do
      pid = spawn({
                    "LANG" => "C",
                  },
                  RbConfig.ruby,
                  "-I", File.join(@base_dir, "lib"),
                  File.join(@base_dir, "bin", "rabbit"),
                  *arguments,
                  {
                    :out => "stdout",
                    :err => "stderr",
                  })
      _, status = Process.waitpid2(pid)
      return status.success?, File.read("stdout"), File.read("stderr")
    end
  end

  test("--save-as-image") do
    File.open(File.join(@dir, "slide.rab"), "w") do |slide|
      slide.puts(<<-'SLIDE')
= ス\nラ/イ\ド:タイトル

= ページタイトル
      SLIDE
    end
    success, stdout, stderr = rabbit("--save-as-image", "slide.rab")
    Dir.chdir(@dir) do
      assert_equal([
                     true,
                     "",
                     "[INFO]\nCreating a image for the 0th page\n" +
                     "[INFO]\nCreating a image for the 1th page\n",
                     [
                       "スラ-イ-ド-タイトル-0.png",
                       "スラ-イ-ド-タイトル-1.png",
                     ],
                   ],
                   [
                     success,
                     stdout,
                     stderr,
                     Dir.glob("*.png").sort,
                   ])
    end
  end

  test("--print") do
    File.open(File.join(@dir, "slide.rab"), "w") do |slide|
      slide.puts(<<-'SLIDE')
= ス\nラ/イ\ド:タイトル

= ページタイトル
      SLIDE
    end
    success, stdout, stderr = rabbit("--print", "slide.rab")
    Dir.chdir(@dir) do
      assert_equal([
                     true,
                     "",
                     "",
                     [
                       "スラ-イ-ド-タイトル.pdf",
                     ],
                   ],
                   [
                     success,
                     stdout,
                     stderr,
                     Dir.glob("*.pdf").sort,
                   ])
    end
  end
end
