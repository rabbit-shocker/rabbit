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

 $ ruby -e "$(curl -fsS http://gist.github.com/raw/323731/install_homebrew.rb)"

Then add /usr/local/bin to your PATH.

=== Install Softwares Rabbit Needs (brew install)

Change files in /usr/local/Library/Formula/*.rb as follows.
Then exec "brew install".

==== /usr/local/Library/Formula/pango.rb

  require 'formula'

  class Pango < Formula
    homepage 'http://www.pango.org/'
    url 'http://ftp.gnome.org/pub/GNOME/sources/pango/1.28/pango-1.28.4.tar.bz2'
    sha256 '7eb035bcc10dd01569a214d5e2bc3437de95d9ac1cfa9f50035a687c45f05a9f'

    depends_on 'pkg-config' => :build
    depends_on 'glib'

    fails_with_llvm "Undefined symbols when linking", :build => "2326"

    if MacOS.leopard?
      depends_on 'fontconfig' # Leopard's fontconfig is too old.
      depends_on 'cairo' # Leopard doesn't come with Cairo.
    end

    def install
      system "./configure", "--prefix=#{prefix}", "--without-x",
                            "--enable-static",
                            "--disable-introspection",
                            "--x-include=#{prefix}/include",
                            "--x-lib=#{prefix}/lib"
      system "make install"
    end
  end

==== /usr/local/Library/Formula/cairo.rb

  require 'formula'

  class Cairo < Formula
    homepage 'http://cairographics.org/'
    url 'http://www.cairographics.org/releases/cairo-1.10.2.tar.gz'
    sha1 'ccce5ae03f99c505db97c286a0c9a90a926d3c6e'

    depends_on 'pkg-config' => :build
    depends_on 'pixman'

    keg_only :provided_by_osx,
              "The Cairo provided by Leopard is too old for newer software to link against."

    fails_with_llvm "Gives an LLVM ERROR with Xcode 4 on some CPUs"

    def install
      system "./configure", "--disable-dependency-tracking",
                            "--prefix=#{prefix}",
                            "--without-x",
                            "--enable-quartz",
                            "--enable-quartz-font",
                            "--enable-quartz-image",
                            "--disable-xlib",
                            "--disable-xlib-xrender"

      system "make install"
    end
  end

==== /usr/local/Library/Formula/gtk+.rb

  require 'formula'

  class Gtkx < Formula
    homepage 'http://www.gtk.org/'
    url 'http://ftp.gnome.org/pub/gnome/sources/gtk+/2.24/gtk+-2.24.4.tar.bz2'
    sha256 '7d3033ad83647079977466d3e8f1a7533f47abd5cc693f01b8797ff43dd407a5'

    depends_on 'pkg-config' => :build
    depends_on 'glib'
    depends_on 'jpeg'
    depends_on 'libtiff'
    depends_on 'gdk-pixbuf'

    # Used by pango, but keg-only, so needs to be added to
    # the flags for gtk+ explicitly.
    depends_on 'cairo' if MacOS.leopard?

    depends_on 'pango'
    depends_on 'jasper' => :optional
    depends_on 'atk' => :optional

    fails_with_llvm "Undefined symbols when linking", :build => "2326"

    def install
      system "./configure", "--disable-debug", "--disable-dependency-tracking",
                            "--prefix=#{prefix}",
                            "--disable-glibtest",
                            "--with-gdktarget=quartz"
      system "make install"
    end

    def test
      system "gtk-demo"
    end
  end

=== /usr/local/Library/Formula/poppler.rb

  require 'formula'

  class PopplerData < Formula
    url 'http://poppler.freedesktop.org/poppler-data-0.4.4.tar.gz'
    md5 'f3a1afa9218386b50ffd262c00b35b31'
  end

  class Poppler < Formula
    url 'http://poppler.freedesktop.org/poppler-0.16.6.tar.gz'
    homepage 'http://poppler.freedesktop.org/'
    md5 '592a564fb7075a845f75321ed6425424'

    depends_on 'pkg-config' => :build
    depends_on "qt" if ARGV.include? "--with-qt4"

    def options
      [
        ["--with-qt4", "Include Qt4 support (which compiles all of Qt4!)"],
        ["--enable-xpdf-headers", "Also install XPDF headers."]
      ]
    end

    def install
      ENV.x11 # For Fontconfig headers

      if ARGV.include? "--with-qt4"
        ENV['POPPLER_QT4_CFLAGS'] = `pkg-config QtCore QtGui --libs`.chomp.strip
        ENV.append 'LDFLAGS', "-Wl,-F#{HOMEBREW_PREFIX}/lib"
      end

      args = ["--disable-dependency-tracking", "--prefix=#{prefix}"]
      args << "--disable-poppler-qt4" unless ARGV.include? "--with-qt4"
      args << "--enable-xpdf-headers" if ARGV.include? "--enable-xpdf-headers"
      args << "--enable-cairo-output"
      args << "--enable-poppler-glib"
      args << "--disable-gtk-test"

      system "./configure", *args
      system "make install"

      # Install poppler font data.
      PopplerData.new.brew do
        system "make install prefix=#{prefix}"
      end
    end
  end

==== Exec brew install and brew link

  $ brew install cairo
  $ brew link cairo
  $ brew install pango
  $ brew install gtk+
  $ brew install poppler

=== Install Rabbit

Exec gem install.

  $ sudo gem install rabbit
  $ sudo gem install twitter-stream # If you want to use Twitter related features
  $ sudo gem install twitter_oauth  # If you want to use Twitter related features
