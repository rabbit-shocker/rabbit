---
layout: en
title: README
---
== What is Rabbit?

Rabbit is a presentation tool for programmer, especially
Rubyist.

You can create presentation slides as a text format. So you
can use your favorite editor and version control system for
creating presentation slides.

Rabbit has programmer-friendly keybord interface. If you
type a key then you will get what you want because Rabbit
uses Emacs/Vi like key bindings by default.

You can just use Rabbit as a slide viewer because Rabbit can
also show PDF format slide. You can create by Keynote or
what you like and use Rabbit as a slide viewer for
programmer-friendly keybord interface.

You can customize slide look and feed by Ruby. It's very
important for Rubyist that you can program when you make
presentation slides.

=== Format

Rabbit supports RD and Wiki notation for text format.

((<rd.html/RD>)) is an abbreviation of Ruby Document. It's
easy to read text format.

Rabbit supports a ((<hiki.html/Hiki notation>)) that is a
dialect of Wiki notations.

=== Interface

Rabbit has useful slide operation interface.

  * Keybord interface: If you type a key then you will get
    what you want.
  * Mouse gesture: Just for a fun. It'll not be used while
    presentation but it's fun while creating.
  * Here and tortoise: Visualize remaining time.

== Install

See ((<"install/"/Install>)) page.

== Usage

See ((<usage.html/Usage>)) page.

== How to make slides

See ((<"how-to-make/"/How to make a slide>)) page.

== Author

=== Program

Kouhei Sutou <kou@cozmixng.org>

=== Images (1)

* Lavie (mascot) ((-Lavie's sense of language is from Rabbit
  and spell is from `la vie' in French.-)) and Rabbit logos.

  (cute rabbit)

* Tailavi and Taiyaki

  gluttonous rabbit loves Taiyaki.

* Banner

  (cute banner)

MoMo

=== Images (2)

* USAGI (in Kanji)

  the cool rabbit.

* KAME

  the cute turtle.

sode

=== Images (3)

Flags.

gan

=== Images (4)

* Rabbit pink logo

* Ruby pink logo

* USA-TARO

* KAME-TARO

* TANU-KITARO (TANU-san)

* YUKIDARU-TARO (YUKIDARU-san)

usatti

=== Images (5)

* YUKI-USA

  pretty rabbit girl in Japanese dress.

* Alice

  pretty girl in blue dress.

asahina

((<URL:http://littleblue.chu.jp/>))

=== Mode for xyzzy

misc/xyzzy/

Mr. MIYAMUKO

=== Mode for Emacs

misc/emacs/

Atsushi Takeda

=== RabbIRC

bin/rabbirc

akira yamada

=== Rabbitter

bin/rabbitter

OBATA Akio

== Co-Authors

  * Mr. MIYAMUKO
  * Mr. noritada
  * Atsushi Takeda

== Copyright

The code author retains copyright of the source code. In
other words the committer retains copyright of his or her
committed code and patch authors retain the copyright of
their submitted patch code.

The images above are copyright their respective owners.

== License

Licensed under GPLv2 or later. For more information see
'GPL' file. Provided patches, codes and so on are also
licensed under GPLv2 or later. Kouhei Sutou can change their
license. Authores of them are cosidered agreeing with those
rules when they contribute their patches, codes and so on.

lib/rabbit/div/prototype.js released under an MIT-style
licence. For more information see ((<Prototype JavaScript
Framework|URL:http://prototype.conio.net/>)).

The author of
data/rabbit/image/rubykaigi2011-images/rubykaigi2011-background-white.jpg
and
data/rabbit/image/rubykaigi2011-images/rubykaigi2011-background-black.jpg
is norio. And it's licensed by CC-by-3.0.

If you want to get a copy of the mascot character PSD files
ask the program author. He'll forward the request to the
author of the image.

== Mailing list

See ((<users.html/Users>)) page.

== Join development

See ((<development.html/Development>)) page.

== FAQ

See ((<faq.html/FAQ>)) page.

== Thanks

Here is a contributor list. Thanks to them!!!

  * MoMo: Drew some pretty images.
  * MIYAMUKO: Wrote rabbit mode for xyzzy, and helps
    make Rabbit work on Windows.
  * zunda:
    * Fixed EPS handling.
    * Gave me advise for --margin.
    * Makes presentations with Rabbit.
    * Updated INSTALL.win32.ja.
    * Updated INSTALL.macosx-macports.{ja,en}.
  * Vincent: Helped make Rabbit work on Max OS X. He also 
    made french messages.
  * sode: Made the cool rabbit, cute rabbit and cute
    turtle.
  * Kazuhiko: Sent me many bug reports. He also makes 
    presentations with Rabbit and gives me ideas for theme
    design.
  * noritada: Sent me a bug report for document.
  * gan: Wrote some flags.
  * KAKUTANI:
    * Sent me a bug report for document.
    * Makes presentations with Rabbit.
    * Gave me ideas for new features and theme design.
    * Gave me a patch to fix a bug related HTML
      generation.
    * Made a Mac OS X (Intel) package.
  * atzm: Made Gentoo package (*.ebuild).
  * akira yamada:
    * Made Debian package (*.deb).
    * Reports some bugs and also fixes them.
    * Made rabbirc.
  * usatti: Drew some logos and taro series images.
  * dot: Helps with installation on Windows.
  * tanaka: Checked operations on Windows.
  * asahina: Drew the pretty rabbit girl and Alice.
  * nskj77: Gave a name to FAQ page. He sends action
    reporting and some bug reports.
  * Atsushi Takeda: Wrote rabbit mode for Emacs.
  * kimura wataru: Made a Mac OS X package.
  * kitaj:
    * Makes presentations with Rabbit.
    * Improved INSTALL.win32.en.
    * Improved a theme.
  * akapy: Wrote a document for emerge.
  * OBATA Akio:
    * Made a pkgsrc package.
    * Makes presentations with Rabbit.
    * Reports some bugs and also fixes them.
    * Helps debug many problems.
    * Made Rabbitter.
  * Masao Mutoh:
    * Makes presentations with Rabbit.
    * Updated INSTALL.win32.ja.
  * TADA Tadashi:
    * Makes presentations with Rabbit.
    * Contributed his themes.
    * Propposed the idea to have Rabbit provide a slide for
      benchmarking your theme.
  * Shugo Maeda:
    * Makes presentations with Rabbit.
    * Reported a bug.
  * JunichiNakai: Reports bugs.
  * Eduardo Gonzalez: Improves documents in English.
  * Kazuhiro NISHIYAMA: Reported a bug.
  * Masaki Suketa: Reported bugs related Ruby 1.9.
  * kdmsnr: Reported a bug.
  * Youhei SASAKI: The official Debian package maintainer.
  * Matz:
    * Makes presentations with Rabbit.
    * Suggested about information window.
  * tmtms:
    * Reported a bug related Ruby 1.9.
  * Kiwamu Okabe:
    * Contributed twitter-footer theme.
    * Contributed Rabbit control command by Wii Remote.
  * Nobuyoshi Nakada:
    * Suggested about console availability check way on Windows.
  * Key Muryoi:
    * Fixed a bug in document.
