---
layout: en
title: Install with Homebrew
---
== How to install Rabbit with Homebrew on Mac OS X

This document describes how to install Rabbit with Homebrew and RubyGems on Mac OS X.

=== Requirements

* An Intel CPU
* OS X Leopard or higher
* Xcode with X11

=== Install Homebrew

 $ ruby <(curl -fsSkL raw.github.com/mxcl/homebrew/go)

Then add /usr/local/bin to your PATH.

=== Install softwares Rabbit needs

  $ brew install cairo
  $ brew link cairo
  $ brew install pango
  $ brew install gtk+
  $ brew install poppler --with-glib

=== Install Rabbit

Exec gem install.

  $ sudo gem install rabbit
  $ sudo gem install rabbiter # If you want to use Twitter related features

=== Set DYLD_LIBRARY_PATH environment variable

Add the following to your ~/.bach_login or ~/.zshenv:

  export DYLD_LIBRARY_PATH=/usr/local/opt/cairo/lib
