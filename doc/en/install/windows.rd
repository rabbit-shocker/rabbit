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

Install Ruby 2.0.0 or later from ((<download page of RubyInstall for
Windows|URL:http://rubyinstaller.org/downloads/>)). For
example, Ruby 2.0.0-p576 installer can be downloaded via the
following URL.

: rubyinstaller-2.0.0-p576.exe
   ((<URL:http://dl.bintray.com/oneclick/rubyinstaller/rubyinstaller-2.0.0-p576.exe>))

Note: You need to install 32bit version Ruby instead of 64bit, even if you use 64bit Windows.
Currently Rabbit doesn't work with 64bit Ruby on Windows.

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

  * Ghostscript

==== EPS format support

(1) Install AFPL Ghostscript for Win32.

    : gs853w32.exe
        ((<URL:ftp://mirror.cs.wisc.edu/pub/mirrors/ghost/AFPL/gs853/gs853w32.exe>))

(2) Add <Ghostscript Install Path>/gs/gs8.53/bin to the PATH environment variable.
