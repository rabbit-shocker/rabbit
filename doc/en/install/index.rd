---
layout: en
title: Install
---
== About how to install

Many package system supports Rabbit. So it's easy to
install Rabbit. You can use RubyGems or package system on
your platform.

== Hot to install by RubyGems

  % gem install rabbit
  % gem install twitter-stream # If you want to use Twitter related features
  % gem install twitter_oauth  # If you want to use Twitter related features

== How to install on Debian GNU/Linux

  % sudo aptitude install -y rabbit

== How to install on Ubuntu

  % sudo aptitude install -y rabbit

== How to install on Gentoo Linux

  % sudo env ACCEPT_KEYWORDS=~x86 FEATURES="digest" emerge rabbit

== How to install on NetBSD (or pkgsrc ready platform)

  % sudo pkg_add ruby18-rabbit

or

  % sudo pkg_add ruby19-rabbit

== How to install on FreeBSD

  % sudo portupgrade -NRr rabbit

== How to install with MacPorts on Mac OS X

See ((<macports.rd/Install with MacPorts>)).

== How to install with Homebrew on Mac OS X

See ((<homebrew.rd/Install with Homebrew>)).

== How to install on Windows.

See ((<windows.rd/Install on Windows>)).

== How to install from tar.gz

You just download the latest Rabbit from
((<URL:http://rabbit-shocker.org/download/rabbit.tar.gz>))
and run setup.rb. The URL always specifies the latest Rabbit
archive.

  % mkdir tmp
  % cd tmp
  % wget http://rabbit-shocker.org/download/rabbit.tar.gz
  % tar xvzf rabbit.tar.gz
  % cd rabbit-*
  % sudo ruby setup.rb
