# To run with it without system install.
#  % ruby -I./lib bin/rabbit -f sample/rabbit_en.rd 
# $Id$

= Rabbit(en)

: subtitle
   Presentation with RD
: author
   Kouhei Sutou
: institution
   COZMIXNG
#: content_source
#   content source
: theme
   rabbit

= Rabbit

A presentation tool

  * Impl.: Ruby/GTK+ 2/cairo
  * Env.: PC-UNIX/Win/Mac
  * Format: RD/Wiki/PDF
  * View: Ruby(('note:(separated with format)'))

= Features: Display (1)

  * ((*Emphasis*))/(('del:Deletion'))
  * Sub(('sub:script'))/Super(('sup:script'))
  * Math characters: (('&sum;'))(('sub:i=0'))(('&sum;sub:i=0'))
  * Colorized source

= Features: Display (2)

  * Table
  * Interesting theme
  * Image
    * Many supported formats
    * PNG/JPEG/.../PDF/EPS/SVG

= Features: Display (3)

  * Folding long lines
  * Colorized source
  * Big text

= Features: UI (1)

  * Rich key bindings
  * Context menu
  * Mouse gesture
  * Spotlight
  * Magnifier

= Features: UI (2)

  * Index page
  * Graffiti
  * (('wait'))Pause
  * (('wait'))I18N
  * Search

= Features: UI (3)

  * Whiteout/Blackout
  * Rabbit hole
    * Make a hole in a slide
  * Visualize remaining time
    * The Tortoise and the Hare

= Features: Input

  * File
  * Standard input
  * HTTP
  * RWiki/Hiki
  * SlideShare

= Features: Format

  * RD
  * Wiki(Hiki)
  * PDF
    * (('&RightArrow;'))PDF viewer

= Features: Output

  * Images
  * Images + HTML
  * PS/PDF
  * PS/PDF for print
    * slides/page

= Features: Ext. API

  * HTTP
  * dRuby
  * XML-RPC
  * SOAP

= Features: Creating

  * Auto source reload
  * Theme reload
  * Change theme

= ToDo

  * Inline image
  * Jump to a link
  * Sound
  * Video
  * 3D

= Image

  # image
  # src = lavie.png
  # caption = Lavie
  # keep_ratio = true
  # width = 100
  # height = 100
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
#  # relative_height = 50

= Image size

Relative size is available

  # image
  # src = usagi.png
  # caption = USAGI
  # keep_ratio = true
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
  # relative_height = 80

= External image

Download an image at the URL

  # image
  # src = http://www.cozmixng.org/repos/images/cozmixchu.png
  # caption = COZMIX Chu

= Math. expressions

  * TeX (('note:(like)')) format
  * Backends
    * LaTeX
    * mimeTeX

= LaTeX

  # LaTeX
  # relative_width = 80

  $f(x)=\displaystyle\int_{-\infty}^x~e^{-t^2}dt$

  \LaTeX

= mimeTeX

  # mimeTeX
  # relative_width = 80

  \Large f(x)=\Bigint_{-\infty}^x~e^{-t^2}dt

= EPS

Create EPS on ahead((-need gs-))

  # image
  # src = equation.eps
  # keep_ratio = true
  # relative_width = 80

= SVG

  # image
  # src = spiral.svg
  # keep_ratio = true
  # relative_height = 100

= Dia

  # image
  # src = rabbit.dia
  # relative_width = 90

= GIMP

  # image
  # src = rabbit.xcf
  # relative_height = 100

= Folding

looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

= Source

The following is the source:

  # comment
  def method_name
    body
  end

The above is the source.

= Source: colorized

The following is the source:

  # enscript ruby
  # comment
  def method_name
    body
  end

The above is the source.

= Quotation

  # blockquote
  # title = The Matrix
  You take the ((*red pill*)), you stay in Wonderland and 
  I show you how deep the ((*rabbit-hole*)) goes.

= Enumeration

  (1) Level 1-1

      (1) Level 2-1

          (1) Level 3-1

          (1) Level 3-2

      (1) Level 2-2

  (1) Level 1-2

= Labeled list

: Rabbit
   USAGI

   : Turtle
      KAME

: USAGI
   Rabbit

= Table

  # RT
  caption = Sample of table

  Heading 1, Heading 2

  content 1, content 2
  very long content 3, veeeery looooooooooooooooooooooong content 4

= Op.: Move

: Next page
   Keys mean the next or left click

   n, f, j, l, Spc, Ret, +, (('&DownArrow;')),
   (('&RightArrow;')), ...

: Previous page
   Keys mean the prev. or center click

   p, b, k, h, BS, Del, -, (('&UpArrow;')),
   (('&LeftArrow;')), ...

= Op.: Advanced move

: Go to title page
   a, 0, <, Home
: Go to n page
   1-9. +Ctrl = +10, +Alt = +20
: Go to last page
   e, $, >, End

= Op.: On stage (1)

: Toggle full screen
   F5, F10, F11, gesture(('&DownArrow;'))(('&UpArrow;'))

: Toggle index mode
   i

: Go to the page
   double click

= Op.: On stage (2)

: Cache all slides
   c

: Toggle info window
   I

= Op.: Save

: Screenshot
   Save each page as image

   s

: Print
   Print each page as PS/PDF

   Ctrl+p

= Op.: Display

: Redraw
   Ctrl+l

: Reload theme
   t, r

: Reset slide adjustment
   Alt+a

= Op.: Hole

: Expand the hole
   E

: Narrow the hole
   N

= Op.: Search

: Search forward
   C-s, /

: Search backward
   C-r, ?

: Quit search
   C-g

= Op.: Quit

: Quit
   q, Esc

: Iconify
   z

= Conclusion

  * A presentation tool
  * Multi platform
  * Features/UI: High & Unique
  * Emphasize keybord
    * UI/text based source
