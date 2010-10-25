# -*- rd -*-

= INSTALL.macosx-homebrew.en

This document describes how to install Rabbit with Homebrew and RubyGems on Mac OS X.

== Requirements

* An Intel CPU
* OS X Leopard or higher
* Xcode with X11

== Install Homebrew

 $ ruby -e "$(curl -fsS http://gist.github.com/raw/323731/install_homebrew.rb)"

Then add /usr/local/bin to your PATH.

== Install Softwares Rabbit Needs (brew install)

Change files in /usr/local/Library/Formula/*.rb as follows.
Then exec "brew install".

=== /usr/local/Library/Formula/pango.rb

  require 'formula'

  class Pango <Formula
    url 'http://ftp.gnome.org/pub/GNOME/sources/pango/1.28/pango-1.28.3.tar.bz2'
    homepage 'http://www.pango.org/'
    sha256 '5e278bc9430cc7bb00270f183360d262c5006b51248e8b537ea904573f200632'

    depends_on 'pkg-config'
    depends_on 'glib'

    if MACOS_VERSION < 10.6
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

=== /usr/local/Library/Formula/cairo.rb

If you use Snow Lepard which has cairo, you might not need this part.

  require 'formula'

  class Cairo <Formula
    url 'http://cairographics.org/releases/cairo-1.8.10.tar.gz'
    homepage 'http://cairographics.org/'
    md5 'b60a82f405f9400bbfdcf850b1728d25'

    depends_on 'pkg-config'
    depends_on 'libpng'
    depends_on 'pixman'

    # Comes with Snow Leopard, but not Leopard
    keg_only :provided_by_osx

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

=== /usr/local/Library/Formula/gtk+.rb

  require 'formula'

  class Gtkx <Formula
    url 'ftp://ftp.gnome.org/pub/gnome/sources/gtk+/2.20/gtk+-2.20.1.tar.bz2'
    homepage 'http://www.gtk.org/'
    sha256 '0e081731d21e34ff45c82199490c2889504fa8b3c7e117c043e82ababaec0f65'

    depends_on 'pkg-config'
    depends_on 'glib'
    depends_on 'jpeg'
    depends_on 'libtiff'
    depends_on 'pango'
    depends_on 'jasper' => :optional
    depends_on 'atk' => :optional

    def install
      system "./configure", "--disable-debug", "--disable-dependency-tracking",
                            "--prefix=#{prefix}",
                            "--disable-glibtest",
                            "--with-gdktarget=quartz"
      system "make install"
    end
  end

=== Excec brew install

  $ brew install pango
  $ brew install cairo
  $ brew install gtk+
  $ brew install poppler

== Install Softwares Rabbit Needs (gem install)

Excec gem install.

  $ sudo gem install pango
  $ sudo gem install cairo
  $ sudo gem install gtk2
  $ sudo gem install gdk_pixbuf2
  $ sudo gem install poppler
  $ sudo gem install hikidoc # If you use Wiki notation.

== Install Rabbit

Excec gem install.

  $ sudo gem install rabbit
