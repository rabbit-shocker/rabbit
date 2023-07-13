---
layout: en
title: Install with Homebrew
---
== How to install Rabbit with Homebrew on macOS

This document describes how to install Rabbit with Homebrew and RubyGems on macOS.

=== Requirements

* A macOS machine
* Xcode with X11 (XQuartz is required)

=== Install Homebrew

Follow the instruction on ((<URL:https://brew.sh>)).

=== Install softwares Rabbit needs

  $ brew install cairo
  $ brew install pango
  $ brew install gtk+3
  $ brew install gobject-introspection
  $ brew install poppler

=== Install Rabbit

Exec gem install.

  $ gem install rabbit
  $ gem install rabbiter # If you want to use Twitter related features

=== Set environment variable

Add the following to your ~/.bach_login or ~/.zshenv:
  export DYLD_LIBRARY_PATH=$HOMEBREW_PREFIX/opt/cairo/lib
