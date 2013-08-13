---
layout: en
title: Install on Windows
---
== How to install on Windows

This document describes how to install and use Rabbit on
Windows.

There are required softwares and optional softwares.

=== Install required softwares

Here are required softwares.

  * Ruby
  * Rabbit

==== Install Ruby

Install Ruby 2.0.0 from ((<download page of RubyInstall for
Windows|URL:http://rubyinstaller.org/downloads/>)). For
example, Ruby 2.0.0-p247 installer can be downloaded via the
following URL.

: rubyinstaller-2.0.0-p247.exe
   ((<URL:http://dl.bintray.com/oneclick/rubyinstaller/rubyinstaller-2.0.0-p247.exe>))

==== Install Rabbit

RubyInstaller install "Start Command Prompt with Ruby"
program into start menu. This program shows command prompt
with PATH configuration for ruby.exe. You can install Rabbit
by the following command on the command prompt:

  > gem install rabbit

Related softwares (e.g. Ruby/GTK2 and so on) are also
install automatically.

Now you can run Rabbit by the following command:

  > rabbit rabbit-theme-benchmark-en.gem

=== Install optional softwares

Here are optional softwares:

  * RDtool
  * Ghostscript
  * Enscript
  * div

==== Install RDTool

Install ((<RAA:rdtool>)).

Extract the following archive and execute setup.rb to install.

: rdtool-0.6.20.tar.gz
   ((<URL:http://www.moonwolf.com/ruby/archive/rdtool-0.6.20.tar.gz>))

==== EPS format support

(1) Install AFPL Ghostscript for Win32.

    : gs853w32.exe
        ((<URL:ftp://mirror.cs.wisc.edu/pub/mirrors/ghost/AFPL/gs853/gs853w32.exe>))

(2) Add <Ghostscript Install Path>/gs/gs8.53/bin to the PATH environment variable.

==== Source code highlighting support

(1) Install Enscript.

    : enscript-1.6.3-9-bin.exe
        ((<URL:http://sourceforge.net/project/showfiles.php?group_id=23617&package_id=16960>))

(2) Add <Enscript Install Path>/bin to the PATH environment variable.

(3) You may need to obtain ruby.st for Ruby syntax highlighting.

      > cd <Enscript Install Path>/share/enscript/hl
      > ruby -ropen-uri -e "puts open('http://viewvc.rubyforge.mmmultiworks.com/cgi/viewvc.cgi/trunk/support/ruby.st?root=support&view=co').read" > ruby.st

(4) Install ((<RAA:htree>)).

    Extract the following archive and execute install.rb to install.

    : htree.tar.gz
        ((<URL:http://cvs.m17n.org/viewcvs/ruby/htree.tar.gz>))

==== Use rabrick

Install ((<RAA:div>)).

Extract the following archive and execute install.rb to install.

: div-1.3.2.tar.gz
   ((<URL:http://www2a.biglobe.ne.jp/~seki/ruby/div-1.3.2.tar.gz>))


Sorry for having so many steps.
