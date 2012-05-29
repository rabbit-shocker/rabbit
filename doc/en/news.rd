---
layout: en
title: News
apply_data: false
---
== 1.0.7: 2012-05-29

twitter-stream 0.1.15 support release.

=== Improvements

==== rabbit

  * Supported markdown extension as Markdown file.
  * Supported Markdown's link markup. [Patch by KITAITI Makoto]

==== Theme

  * default-preformatted: Added @preformatted_centering variable that
    changes alignment of preformatted text.
    [Patch by kimura wataru]
    * (({true})): Centering (default)
    * (({false})): Left align

==== rabbiter

  * Supported twitter-stream 0.1.15.

=== Fixes

==== rabbit

  * Fixed computation of spacing.

==== Theme

  * Fixed a bug that horizontal centering isn't reset.

==== Documentation

  * Fixed typos [Reported by @tmtms]

=== Thanks

  * KITAITI Makoto
  * kimura wataru
  * @tmtms

== 1.0.6: 2012-03-03

CodeRay 1.0.x support release.

=== Improvements

==== rabbit

  * Supported CodeRay 1.0.x. [Reported by TAKATSU Tomonari]

=== Thanks

  * TAKATSU Tomonari

== 1.0.5: 2012-01-30

Markdown support release!

=== Improvements

==== rabbit

  * Added note mode. [Added by nari]

==== rabbirack

  * Supported wait.

==== Rendering

  * Improved rendering speed. (Especially CodeRay is used case)
  * SVG: Supported reflection.

==== Markup

  * Improved markup detection accuracy by using extension.
  * RD: Accepted "block-quote" keyword and "block_quote"
    keyword as block quote markup keyword.
  * Supported Markdown support by kramdown.

==== Theme

  * syntax-highlighting: Added the default foreground color.
  * nari: Supported headline-align.
  * tag: Supported tag handler customization.
  * slide-show: Supported wait.
  * slide-footer-info: Improved @slide_footer_info_line_color
    customizability.
    [Suggested by kimura wataru]

=== Fixes

==== rabbit

  * [GitHub#4]: Added missing warning method.
    [Patch by TAKATSU Tomonari]

==== rabbirack

  * [GitHub#5]: Supported Rack 1.2.1.
    [Reported by TAKATSU Tomonari]

==== rabbit-command

  * [GitHub#3]: Fixed a wrong variable name.
    [Patch by TAKATSU Tomonari]

==== rabbitter

  * Supported HTTPS. [Patch by OBATA Akio]

==== Theme

  * [GitHub#2]: default-slide: Fixed broken headline-align
    slide property. [Reported by nari]

==== Documents

  * Fixed wrong ML address in English.

=== Thanks

  * nari
  * TAKATSU Tomonari
  * OBATA Akio
  * kimura wataru

== 1.0.4: 2011-08-06

Theme improvements release!

=== Improvements

==== Theme

  * nari: Added. This is for nari. [Created by nari]
  * image-slide-number: Changed the default here to a cute here.
  * image-slide-number: Supported flag color customize.
  * image-timer: Changed the default tortoise to a cute tortoise.
  * rubykaigi2011: Supported the tortoise and the here.

=== Fixes

==== Documentation

  * Fixed Pango link. [Patch by Ken Muryoi]

=== Thanks

  * nari
  * Ken Muryoi

== 1.0.3: 2011-07-17

A bug fix release of 1.0.2.

=== Improvements

==== Core

  * Supported RubyGems 1.8.5.
    [Reported by okkez]
  * rabbit command doesn't use standard input as the default
    source type.

=== Fixes

==== Core

  * Fixed a problem that Wiki notation support doesn't show error.
    [Reported by kimura wataru]
  * Added missing Sinatra dependency.

==== Theme

  * footer-comment: Fixed a problem that a comment is hidden
    shortly.

=== Thanks

  * okkez
  * kimura wataru

== 1.0.2: 2011-07-15

RubyKaigi2011 release.

=== Improvements

==== Core

  * Re-supported GTK+ 2.20. (For Debian GNU/Linux squeeze)
    [Reported by akira yamada]

==== Theme

  * title-slide-background-image: Added. It shows background
    image with title slide slide.
  * table: Supported font color customize.
  * rubykaigi2011: Added. It's for RubyKaigi2011.

=== Fixes

==== Theme

  * default-item-mark-setup: Fixed duplicated font name
    specified error.
  * edge-info-toolkit: Fixed duplicated font name
    specified error.

=== Thanks

  * yamada akira

== 1.0.1: 2011-07-15

A bug fix release of 1.0.0.

=== Fixes

==== Core

  * Removed needless albino gem dependency.

== 1.0.0: 2011-07-15

The first major release! We spent seven years for it!

=== Improvements

==== Core

  * Made Twitter related gems optional not required.
  * Disabled Clutter by default. --use-gl is required to
    enable it.
    [Reported by OBATA Akio]
  * Supported ((<blockdiag|URL:http://blockdiag.com/blockdiag/build/html/>)).

    RD:
      # blockdiag
      # relative_width = 90
      # fonts = /usr/share/fonts/opentype/ipafont/ipag.ttf, /usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf
      {
        fontsize = 25;
        RD -> Rabbit;
        Hiki -> Rabbit;
        PDF -> Rabbit;
        group {
          Rabbit -> Display;
        }
      }

    Hiki:
      {{blockdiag("
      {
        fontsize = 25;
        RD -> Rabbit;
        Hiki -> Rabbit;
        PDF -> Rabbit;
        group {
          Rabbit -> Display;
        }
      }",
                  {
                    :relative_width => 90,
      #             :antialias => true,
                    :fonts => ["/usr/share/fonts/opentype/ipafont/ipag.ttf",
                               "/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf"],
                  })}}
  * Removed Anthy support.
  * Supported syntax highlighting by CodeRay.
  * Supported syntax highlighting by Emacs.
  * Improved "run from console" detection.
    [Reported by OBATA Akio]
  * Supported non-blocking mode for Twitter connection.
    [Reported by OBATA Akio]
  * Improved the default logger detection.
    [Reported by OBATA Akio]
  * Supported Nokogiri and dropped HTree.
  * Added --allotted-time option to specify presentation
    allotted time.
  * Added --comment-theme option to specify a theme for
    comment.

    For example, use --comment-theme stream-comment to
    stream comment on your slide.
  * Removed RWiki support.

==== Rabbiter

  * Added --log-status option to log streamed information
    for debug.

==== Rabbirack

  * Added. It is a new Web interface for Rabbit. It will
    deprecate Rabrick.

==== Theme

  * Added shadow-color, shadow-x and shadow-y properties to
    text element.
  * syntax-highlighting: Added. It specifies colors for
    syntax highlighting.
  * footer-comment:
    * Improved image-timer support.
    * Disabled the last comment remaining by default.
  * rabbit-powered-by: Disabled banner.
  * pdf-tortoise-and-hare: Removed because it's merged to
    pdf theme.
  * pdf:
    * Supported hare and tortoise.
    * Supported comment.
  * base: Added foreground color, background color and
    shadow color configurations.
  * default-comment: Added. It specifies a theme for comment.

=== Fixes

==== Core

  * Fixed a problem that .mo are missing in package.
    [Reported by OBATA Akio]
  * Fixed a problem that appending comment from rabbiter
    crashes Rabbit.
  * Fixed a problem that PDF file detection is failed in
    Ruby 1.9.

==== Documentation

  * Fixed link URL.
    [Reported znz]

=== Thanks

  * OBATA Akio
  * znz

== 0.9.3: 2011-06-25

A comment and Twitter support improved release.

=== Improvements

==== Core

  * Supported PDF viewer on Ruby 1.9.
    [Reported by tmtms]
  * [#199] Supported "~" expansion in path specified by "-I".
    [Reported by kdmsnr]
  * Supported Twitter's OAuth.
  * Supported Bundler.
  * Added Rabbit.add_cleanup_proc that is an API to register
    a hook on rabbit command finished.
  * Added rabwii command to control Rabbit via Wii Remote.
    [Based on Kiwamu Okabe's code]
  * Changed default dRuby server bind address to localhost.
  * Changed default --default-public-level to "all".
  * rabbiter: Used OAuth.
  * rabrick: Changed --druby-uri option to --rabbit-uri.
  * rabbit-command: Changed --druby-uri option to --rabbit-uri.
  * Removed comment view.
  * rabbiter: Added --user-language option.
  * Used GUI logger by default when Rabbit isn't invoked by
    rubyw.exe.
    [Helped by Nobuyoshi Nakada]
  * Added launcher mode that show file chose dialog when
    Rabbit invoked without source file on no console environment.
  * Supported tag in Hiki notation. ({{tag('tag-name', 'value')}}

==== Theme

  * title-background-image: Supported image file name in
    slide property.
    [Suggested by kdmsnr]
  * background-image-toolkit: Used slide property rather
    than options in theme.
  * image: Made image caption's font size customizable.
  * color-circle: Enabled newline-in-slides and tag theme by
    default.
  * slide-number: Made slide number position customizable.
    [Youhei SASAKI]
  * debian: Updated.
    [Youhei SASAKI]
  * dark-gradation: Added. It's Keynote like dark gradation
    theme.
    [Youhei SASAKI]
  * twitter-comment: Added. It imports tweets from Twitter
    as comments.
    [Based on Kiwamu Okabe's code]
  * comment: Removed. It's for removed comment view.
  * footer-comment: Added. It shows comments on the footer.
    [Based on Kiwamu Okabe's code]
  * clutter-comment: Added. It streams comments on slides
    with rolling by Clutter API.
  * stream-comment: Added. It streams comments on slides.

=== Fixes

==== Core

  * Fixed Clutter backend doesn't work.
    [Reported by OBATA Akio]
  * Fixed GUI logger doesn't work.

=== Thanks

  * tmtms
  * kdmsnr
  * OBATA Akio
  * Youhei SASAKI
  * Kiwamu Okabe
  * Nobuyoshi Nakada

== 0.9.2: 2010-12-31

A release for Matz.

=== Improvements

==== Theme

  * Added Debian theme. [Added by Youhei SASAKI]
  * cairo backend:
    * Supported line_cap.
    * Supported line_join.
    * Supported Pixbuf as pattern source.
    * Supported matrix transformation of pattern.
  * default-slide: Supported line width customization.
    * @default_headline_line_width
    * @default_headline_line_params
    * @default_headline_line_expand
  * tag:
    * Added margin-top tag.
    * Added margin-bottom tag.
  * slide-logo: Supported slide logo width/height/position customization.
    * @slide_logo_width
    * @slide_logo_height
    * @slide_logo_position
  * Added ranguba theme.

==== Information Window

  * Showed rest time before timer start. [Suggested by Yukihiro Matsumoto]
  * Showed the current slide. [Suggested by Yukihiro Matsumoto]
  * Supported keyboard/mouse operations from information window.
    [Suggested by Yukihiro Matsumoto]

=== Thanks

  * Youhei SASAKI
  * Yukihiro Matsumoto

== Changes 0.9.1 from 0.9.0: 2010-10-25

Changed license: Ruby's -> GPLv2 or later

=== Improvements

  * Removed rabbit.bat from gem. [U.Nakamura]
  * Added documentation for Homebrew. [kdmsnr]
  * Changed license to GPLv2 or later from Ruby's.
    (It assumed that Kouhei Sutou can change contributed
    codes and so on. If contributes can't accept the rule,
    those codes and so on aren't included in Rabbit.)

=== Thanks

  * U.Nakamura
  * kdmsnr

== Changes 0.9.0 from 0.6.5: 2010-09-26

Applied defalut custom tag styles.

=== Improvements

==== Theme

  * background-image-toolkit: Added vertical-align
    paramteter that specifies vertical position.

    Example:
      # image
      # src = lavie.png
      # relative-width = 30
      # align = right
      # vertical-align = top
      # relative-margin-right = -5

  * tag: New theme. It provides default style for some
    custome tags. The following custom tags are supports. It
    is enabled by default.

    * tag:x-large: Made specified text large.
    * tag:center: Placed specified text in center If no text
      is specified, block that has center tag is placed in
      center.
    * tag:right: Placed specified text in right If no text
      is specified, block that has right tag is placed in
      right.

    Example:
      (('tag:x-large:Large Text'))

      (('tag:center'))Centerized Text

      (('tag:right'))Right-Justified Text

==== RD

  * Supported recursive markup in (('('))('XXX:')((')')) markup.

  * Supported markup in cells in table.

=== Fix

  * Fixed missing require.
    [Reported by OBATA Akio]
  * Fixed aafigure's site URL.
    [Reported by kdmsnr]
  * Changed default align to center of
    background-image-toolkit theme for backward compatibility.
    [Suggested by nari]
  * Renamed Rabbitter to Rabbiter.

=== Thanks

  * OBATA Akio
  * kdmsnr
  * nari

== Changes 0.6.5 from 0.6.4: 2010-07-31

  * Added Rabbiter that collects comments from Twitter.
    [OBATA Akio]
  * Supported
    ((<aafigure|URL:https://launchpad.net/aafigure>)).
    [Suggested by kdmsnr]

=== Improvements

  * Removed needless newlines from default PDF filename.
  * Worked with \r\n newline. [Reported by zunda]
  * Rabbitter: Added. It collects comments from Twitter.
    [OBATA Akio]
  * Supported aafigure. [Suggested by kdmsnr]

==== Theme

  * clear-blue: Removed needless newlines from footer text.
  * per-slide-background-iamge:
    Supported "background-image-align: right" slide property.
    [Suggested by kdmsnr]

      = slide

      ...

      == properties

       : background-image
          lavie.png

       : background-image-relative-width
          30

       : background-image-align
          right

       : background-image-relative-margin-right
          3

  * Supported width parameter 'relative_width' as well as
    'relative-width'.

  * body-background-image: Added. It enables "align = right"
    image property:

      = title

        # image
        # src = lavie.png
        # relative-width = 30
        # align = right
        # relative-margin-right = -5

  * Enabled body-background-image and
    per-slide-background-image by default.

  * background-image-tookit: Added. It is a toolkit for
    shareing common processes in body-background-image and
    per-slide-background-image themes.

  * title-background-image:
    Supported customize by
    @title_background_image_properties. The same options for
    image element can be used.
    [Suggested by kdmsnr]

      @title_background_image_properties = {
        :align => :right,
        :as_large_as_possible => false,
        :relative_height => 75,
      }

    This shows image with the following layout:
        +-----------+
        |      +---+|
        |  title <- background image
        |      +---+|
        +-----------+

    A background image is showed in center and it is
    resized to as large as possible by default.

=== Fixes

  * Fixed comment view doesn't work.

=== Thanks

  * kdmsnr
  * zunda
  * OBATA Akio

== Changes 0.6.4 from 0.6.3: 2010-01-29

Rabbit had been included in Debian official packages! [Youhei
SASAKI]

=== Improvements

  * Added --keep-above option: Rabbit is always shown top of
    windows.
  * Added --source-filename option that adds source link to
    output HTML.
  * Added a directory in source file into theme load path.
  * Supported a single theme file in the same directory of
    source file. [zunda]
  * Improved PDF file detection.

==== Theme

  * Added:
    * per-slide-background-image:
      Uses background image per slide. A background
      image is specified by slide property:

        = target slide

        ...

        == properties

        : background-image
           my-picture.png
        : background-image-ralative-height
           95

      Supported options are same as image options in slide.
      Options name should be started with "background-image-".

      e.g.: "background-image-relative-height" for
      "relative-height" option.

    * per-slide-background-color:
      Uses background color per slide. A background color is
      specified by slide property:

        = target slide

        ...

        == properties

        : background-color
           black

      Color can be specified as color name, "black", or RGB
      values "#RRGGBBAA".

  * Changed:
    * default-slide:
      Supports title foreground color and shadow color
      change per slide. They are specified by slide
      property:

        = target slide

        ...

        == properties

        : headline-color
           red
        : headline-shadow-color
           gray

=== Fixes

  * Fixed a bug that empty title with
    @lightning_talk_as_large_as_possible = true crashes.
    [kdmsnr]
  * Added workaround for Ruby 1.8.7 p249.

=== Thanks

  * zunda
  * kdmsnr
  * Youhei SASAKI

== Changes 0.6.3 from 0.6.2: 2009-12-16

=== Improvements

  * [#180] Supported inline markup in DL with Wiki format.
    [kdmsnr]
  * Added butler rabbit to the standard theme.
  * Changed to use scaled images by default even when
    printing mode.
  * Supported pixbuf rendering with alpha channel.
    (when cairo is used as rendering engine)

==== Theme

  * Added:
    * lightning-monochrome:
      Takahashi method slide + monochrome normal slide
  * Changed:
    * default-block-quote: Added
      @block_quote_image_background_alpha parameter that
      specifies alpha channel of background images.
    * default-preformatted: Made auto text size adjustment
      optional. It is customized by
      @preformatted_keep_in_size parameter.
    * clear-blue: Displayed title in the left bottom by
      default. It can be disabled by the following theme:
        include_theme("clear-blue")
        @slide_footer_info_left_text = ""
  * Supported nest of "itemize > enum" and
    "itemize > itemize > enum".

==== Experimental

((*This maybe changed in the feature.*))

  * Supported tag:

    syntax:
      (('tag:name:content'))
    or
      (('tag:name'))content

    e.g.:
      slide.rab:
        (('tag:center'))Hi, look it!

      theme.rb:
        match("**", CustomTag) do |tags|
          tags.each do |tag|
            case tag.name
            when "center"
              tag.parent.horizontal_centering = true
            end
          end
        end

      output(before):
        +-------------------+
        | Hi, look it!      |
        +-------------------+

      output(after):
        +-------------------+
        |    Hi, look it!   |
        +-------------------+

=== Fixes

  * Fixed a problem that fullscreen doesn't work on Ruby 1.9
    [Masaki Suketa]
  * [#179] Fixed a problem that Rabbit sometimes crashes
    with mouse clicks. [kdmsnr]
  * newline-in-slides theme: Disabled newline substitution in
    preformatted text.
  * Fixed a problem that cursor isn't displayed after
    unfullscreen.

=== Thanks

  * Masaki Suketa
  * kdmsnr

== Changes 0.6.2 from 0.6.1: 2009-10-03

=== Improvements

  * Rabbit became an official MacPorts package! [kimura wataru]
  * Supported '''strong''' Wiki markup [kdmsnr]
  * Ignored level 2 or larger section in Wiki markup [kdmsnr]
  * Theme
    * pdf-tortoise-and-hare: Added margin between slide edge,
      tortoise and hare.
    * clear-blue: Supported changing images of tortoise and hare.
    * Added set_font_resolution_ratio that can change
      character size in a lump.
  * rabbit-mode: Supported rabbit-command.
  * Updated prototype.js: 1.4.0 -> 1.6.0
  * Supported justify.
  * Added a command that gets the current slide content as
    RD format.
  * Added --geometry option.
  * Supported RubyGems.

=== Fixes

  * Fixed a problem that allotted-time is ignored in
    lightning-simple theme. [Kazuhiro NISHIYAMA]
  * Removed system DPI dependency. [Kazuhiro NISHIYAMA]
  * Fixed a problem that displayed slide and PDF slide is
    different. [Kazuhiro NISHIYAMA]
  * Fixed a problem that images aren't resized with GDK
    backend.

== Changes 0.6.1 from 0.6.0: 2009-07-17

=== Improvements

  * Images are rendered after resized by Gdk::Pixbuf on
    display mode.
  * Theme
    * lightning-talk-toolkit: supported :hide-title
    * slide-show: timer is reset after each loop
    * slide-show: default slide show span is computed from
      allotted time and slide size.
  * "\n" is available by default.
  * Wiki format supports slide property.
  * RD format supports 'wait' in nested itemize. [Kazuhiro NISHIYAMA]

=== Fixes

  * Added missing GPL license file.
  * Fixed a problem that PDF isn't resized on display size change.
  * Fixed a problem that index mode doesn't work for title
    only slide. [rabbit-shockers:654] [OBATA Akio]
  * Fixed a problem that block wait doesn't work. [OBATA Akio]

== Changes 0.6.0 from 0.5.9: 2009-05-23

=== Improvements

  * Supported Bonjour: [kimura wataru]
  * Added new mascot character "Tailavi": [MoMo]
    * ((<URL:http://www.cozmixng.org/repos/rabbit/trunk/sample/momo/tailavi/>))
  * Added subtitle to windows title.
  * Supported slide property.
  * Added alternative images for "rabbit and tortoise".
  * Original image is used for PDF embedding instead of
    resized image.

=== Fixes

  * Fixed a typo in sample/kof2005/gesture.rb: [#143][IWAI, Masaharu]
  * Fixed install document: [zunda]
  * Fixed English: [Eduardo Gonzalez]

== Changes 0.5.9 from 0.5.8: 2009-02-09

  * Improvements
    * HTML output: supported link to PDF.
    * added rabbit-command command line tool to control Rabbit.
    * supported reflected image effect (requested by kdmsnr)

      Example:
        * ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-104.html>))
        * ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-085.html>))

      How to write:
        # image
        # src = XXX.jpg
        # relative_height = 80
        # reflect_ratio = 0.5

  * Theme
    * New themes
      * pdf-tortoise-and-hare:
        uses tortoise and hare timer when PDF viewer
        mode. Allotted time is specified by
        RABBIT_ALLOTTED_TIME environment variable.

        How to use:
          % RABBIT_ALLOTTED_TIME=4.5m rabbit --theme pdf-tortoise-and-hare XXX.pdf
      * lightning-clear-blue:
        uses large character slides in bluish clear-blue theme.
      * title-on-image-toolkit:
        superimposes title on image.

        Example:
        ((<URL:http://www.clear-code.com/archives/SendaiRubyKaigi01/love-and-continue-it-036.html>))

        How to use:
          In slide:
            = Users Group

              # image
              # src = shocker.jpg
              # relative_height = 90

          In theme:
            include_theme("title-on-image-toolkit")

            match(Slide) do |slides|
              slides.each do |slide|
                slide.title_on_image if slide.title_on_image?
              end
            end

  * Bug fixes
    * fixed wrong Wiki format detection.

== Changes 0.5.8 from 0.5.7: 2008-10-18

  * Improvements
    * supported Clutter
      * supported page transition
      * supported new comment view that flow comments on a slide.
    * supported IRC backend
      * RabbIRC (akira yamada)
  * Theme
    * added shadow to quote marks
    * try to use Hiragino font if available
    * add a new theme
      * newline-in-slides: supports '\n' newline notation
  * Bug fixes
    * fixed too much grabbed problem on magnifier mode
    * reworked Tofu backend
    * fixed a problem that was reported on Asakusa.rb

== Changes 0.5.7 from 0.5.6: 2008-07-21

  * Bug fixes
    * fixed broken PDF generation
  * Improvements
    * supported GTK+/Quartz (Mac OS X native UI)
    * supported context menu with Control + left click
      (suggested by OBATA Akio)
    * align = XXX accepts :right or "left" too
    * supported nested labels in (('note:XXX:YYY:...')) form
    * added "Next" and "Previous" actions that consider pause.
    * added "ResetTimer" action
    * do fullscreen as first as possible when --fullscreen
      command line option is specified
    * supported {{wait}} and {{br}} markup on Wiki format
    * fixed dirty sample slide
  * Theme
    * added body margin
    * added "dash" item mark
    * stopped to use vivid colors for list item mark
    * added properties
    * added new themes:
      * newline-in-title: supports '\n' newline notation
      * edge-info-toolkit: displays information at the edge
        of slide
      * slide-header-info: displays information at the header
        of slide
      * slide-footer-info: displays information at the footer
        of slide
      * footer-logo: displays logo image at the footer of slide
      * blue-bar: a theme that displays bars at the top and
        bottom of slide
      * clear-blue: clear blue theme

== Changes 0.5.6 from 0.5.5: 2008-04-21

  * removed RTtool
  * supported Ruby 1.9 a bit
  * supported pause

== Changes 0.5.5 from 0.5.4: 2008-03-01

  * Bug fixes
    * fixed tests (reported by OBATA Akio)
    * fixed --margin option
  * supported HikiDoc installed by RubyGems
  * removed Tgif related files (reported by OBATA Akio)
  * removed needless windows-adjust theme
  * stopped to show progress message when index page is generating
  * added --log-level option
  * added document for MacPorts
  * rabbit-mode.el:
    * removed a needless variable.

== Changes 0.5.4 from 0.5.3: 2007-12-15

  * Documentation update
    * fixed ruby.st's URL (kitaj)
    * updated document of MacPorts (kimura wataru)
  * Theme related
    * added some color configuration to night-black theme
    * added slide-logo theme
    * added slide-footer theme
  * Bug fixes
    * fixed cairo backend availability check
    * fixed Wiki parser loading bug (reported by OBATA Akio)
    * suppressed warnings on Windows
    * fixed "/" handling on Windows (JunichiNakai, OBATA Akio)
  * logged progress in background (suggested by OBATA Akio)
  * changed default print format to PDF
  * supported "check" for itemize mark
  * added --show-native-window-id option
  * supported <<< LANG\n...\n>>> with Wiki parser
  * added block quote sample
  * supported an image file as source (Rabbit can be an image viewer)
  * added as_large_as_possible image size option
  * removed config.rb on 'setup.rb clean' (Kobayashi Noritada)
  * removed Tgif support
  * added --man option (Kobayashi Noritada)
  * supported slideshare.net as source
  * supported DnD

== Changes 0.5.3 from 0.5.2: 2007-08-04

  * Theme related
    * added Day White/Night Black themes (TADA Tadashi)
    * added a slide for benchmarking theme (suggested by
      TADA Tadashi)
    * added Green Circle theme.
    * renamed Auto Slide to Slide Show.
    * clean-upped (codes of) theme.
  * Documentation update
    * INSTALL.win32 (Masao Mutoh, zunda）
  * Bug fixes
    * escaped '_' in page name for page name list in menu.
      (reported by Shugo Maeda)
    * fixed a GRClosure related problem. (akira yamada)
    * fixed a crash bug when reloading source.
    * fixed a bug that occurs with --output-html without
      --output-index-html.
    * fixed a bug that font configurations are ignored in
      printing.
  * improved GIMP support (akira yamada)
  * removed libgnomeprint support.
  * supported Wiki notation (HikiDoc).
    (included hikidoc.rb only in this release)
  * marked quality of PS/PDF output is enough.
  * added a toggle Magnifier action to menu.
  * added a toggle Spotlight action to menu.
  * supported partial rendering.
    (syntax in source is not supported yet）
  * {start,stop}_reload_timer -> {start,stop}_redraw_timer
    ({start,stop}_reload_timer are still available for
    backward compatibility）

== Changes 0.5.2 from 0.5.1: 2007-06-02

  * updated documents
    * rabbit-mode.el (Atsushi Takeda)
    * MacPorts (kimura wataru)
  * improved rabbit-mode.el (Atsushi Takeda)
    * added rabbit-default-image-size-unit variable
  * fixed bugs that causes start-up failure (reported by atzm)
  * other minor fixes (reported by OBATA Akio)

== Changes 0.5.1 from 0.5.0: 2007-03-29

  * supported PDF as input format.
  * improved rabbit-mode.el. (Atsushi Takeda)
    * rabbit-copy-slide: added
    * rabbit-duplicate-slide: added
  * updated messages in French. (Scritch)
  * fixed a bug related font families on printing.
  * removed libgnomeprint support.

== Changes 0.5.0 from 0.4.2: 2006-11-03

  * fixed a bug related GtkGLExt loading. (KAKUTANI Shintaro)
  * improved rabbit-mode.el. (Atsushi Takeda)
  * updated documentation of emerge. (akapy)
  * added documentation of MacPorts. (kimura wataru)
  * added workaround for RSVG path resolving.
  * (useless) Ruby/Anthy support.
  * supported information window for multi display environment.
  * use Rabbit/Rabbit Monospace font family preferentially.
  * added affine transformation related APIs.
    * canvas.rotate_context
    * canvas.scale_context
    * canvas.translate_context
    * canvas.reflect_context
    * canvas.shear_context
  * added APIs to save drawing context.
    * canvas.save_context
    * canvas.restore_context
  * supported "\n" to input new line in lightning-talk theme.
  * added 'around' hook.
  * added new themes.
    * rotate-zoom-effect
    * emphasize-keyword
    * scroll-effect
    * mirror-effect
  * added spotlight function.
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/spotlight/>))
  * added magnifier function.
    ((<URL:http://pub.cozmixng.org/~gallery/kou/screenshot/rabbit/magnifier/>))
  * supported gradation.
  * updated setup.rb to 3.4.1.

== Changes 0.4.2 from 0.4.1

  * fixed a search regular expression bug. (nskj77)
  * fixed documents. (nskj77, kitaj)
  * imported rabbit-mode.el. (Atsushi Takeda)
  * fixed a bug related HTML generation. (KAKUTANI Shintaro)
  * added Alice image. (asahina)
  * updated rabbit-mode.l. (MIYAMUKO Katsuyuki, id:wata_d)
  * supported takahashi method instead of takahashi alias.
  * supported PDF rendering with Ruby/Poppler.
  * improved OpenGL support.
  * supported AJAX in Rabrick.
  * supported mobile phone.
  * improved memory usage.

=== Other news

  * Mac OS X package (DarwinPorts) is available by kimura wataru.
