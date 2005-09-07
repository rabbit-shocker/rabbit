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

= What's this?

A presentation tool

  * written by Ruby
  * used GTK+ 2
    * because I want to do
      * OK?

= Features

  * source is RD
  * align with RWiki
    * edit source on the RWiki
    * when presentation, source is directory from RWiki
  * isolate RD and look and feel

= Available(('sup:up')) things(('sub:down'))

  * do (('del:not')) ((*emphasis*))
  * sub(('sub:script')) and super(('sup:script'))
  * mathematical characters (('&Sigma;'))(('&sum;sub:i=0'))
  * display image
  * auto reload modified source
  * reload theme

= More available things

  * auto generation of index page
  * right click menu
  * screenshot on the off screen
  * PS/PDF output (but quality is ...)
  * Internationalized message
  * Table

= Not available

  * graffiti on the slide
  * inline image
  * organize screenshots
  * jump to the link

= ToDo

  * handle sound
  * handle 3D (I wish there were a library of X3D)

= Call on

  * cool themes
  * great documents

= Save slides

  * can save slides as image
  * can save simple HTML for viewing images
  * but ...
    * organizing screenshots beautifully is the work of other tools
    * ((<RAA:gallery>)), zphoto and so on

= What about image?

  * OK, if it is not inline

    resizable

      # image
      # src = lavie.png
      # caption = Lavie
      # keep_ratio = true
      # width = 100
      # height = 100
#      # normalized_width = 50
#      # normalized_height = 50
#      # relative_width = 100
#      # relative_height = 50

= What about image size?

You can use relative size.

  # image
  # src = usagi.png
  # caption = USAGI
  # keep_ratio = true
#  # normalized_width = 50
#  # normalized_height = 50
#  # relative_width = 100
  # relative_height = 50

= Image not in local?

  * external URL is OK

      # image
      # src = http://www.cozmixng.org/repos/images/cozmixchu.png
      # caption = COZMIX Chu

= Mathematical expressions?

call Tgif or mimeTeX

  # TeX
  # keep_ratio = true
  # relative_width = 80
  
  \Large f(x)=\Bigint_{-\infty}^x~e^{-t^2}dt

= It's dirty

  * set up EPS before presentation
    * but need gs

  # image
  # src = equation.eps
  # keep_ratio = true
  # relative_width = 80

= SVG is OK, too

  # image
  # src = spiral.svg
  # keep_ratio = true
  # relative_height = 100

= Wrap word

looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

= Source code

What about?

  like
  this
  def It's
    OK?
  end

What about?

= Itemization

  (1) What

  (1) about

      (1) What What What

          (1) Once

          (1) More

      (1) What about?

  (1) ?


= List with label

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
  very long content 3, veeeery loooooooooooooooooooooooooooong content 4

= How to write theme

--- Rabbit::Theme#match(*path, &block)
     Specify elements applied theme by path

Ahh, I'll set up more better documents.

= Key bindings (basic)

: Quit
   q, Esc
: Next page
   n, f, j, l, Spc, Ret, Tab, +, ↓, →, left click and so on.
: Previous page
   p, b, k, h, BS, Del, -, ↑, ←, center click and so on.

= Key bindings (convenience)

: Go to title page
   a, 0, <, Home
: Go to n page
   1-9. +Ctrl is +10, +Alt is +20
: Go to last page
   e, >, End

= Key bindings (presentation)

: Toggle full screen
   F5, F10, F11

: Toggle index mode
   i

: Go to specified page in index mode
   double click

= Key bindings (feature)

: Screenshot
   save each page as image

   s

: Print
   print each page as PS/PDF

   Ctrl+p

= Key binding (draw)

: Redraw
   Ctrl+l

: Reload theme
   t, r

= Key binding (etc)

: Iconify
   z

: Cache all slides
   c

= Finish

What about this?
