# To run with it without system install.
#  % ruby -I./lib bin/rabbit -f --type file sample/rabbit_en.rd 
# $Id: rabbit.rd 386 2004-07-26 17:58:22Z kou $

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
  * align with RWiki (((*plan*)))
    * edit source on the RWiki
    * when presentation, source is directory from RWiki
  * isolate RD and look and feel

= Available(('sup:up')) thing(('sub:down'))

  * do (('del:not')) ((*emphasis*))
  * sub(('sub:script')) and super(('sup:script'))
  * mathematical characters (('Sigma:'))(('sum:'))(('sub:i=0'))
  * display image
  * auto reload modified source
  * reload theme

= Not available

  * graffiti on the slide
  * inline image
  * screenshot on the console
  * organize screenshots
  * jump to the link

= ToDo

  * auto generation of index page
  * align with RWiki

= Call on

  * cool themes
  * great docments

= Save slides

  * can save slides as image
    * organizing screenshots is work of other tools
    * ((<RAA:gallery>)), zphotok and so on

= What about image?

  * OK, if it is not inline

    resizable

      # image
      # src = rabbit.png
      # caption = mascot
      # keep_scale = true
      # width = 100
      # height = 100
#      # normalized_width = 50
#      # normalized_height = 50
#      # relative_width = 100
#      # relative_height = 50

= Image not in local?

  * external URL is OK

      # image
      # src = http://www.cozmixng.org/repos/images/cozmixchu.png
      # caption = COZMIX Chu

= Mathematical expressions?

call mimeTeX

  # TeX
  # keep_scale = true
  # relative_width = 80
  
  \Large f(x)=\Bigint_{-\infty}^x~e^{-t^2}dt

= It's dirty

  * set up EPS before presentation
    * but need gs

  * SVG is OK, too?

  # image
  # src = equation.eps
  # keep_scale = true
  # relative_width = 80

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

= How to write theme

--- Rabbit::Theme#match(*path, &block)
     Specify elements applied theme by path

Ahh, I'll set up more better documents.

= Key bindings (basic)

: Quit
   q, Esc
: Next page
   n, f, j, l, Spc, Ret, Tab, +, ↓, →, left click
: Previous page
   p, b, k, h, BS, Del, -, ↑, ←, center click

= Key bindings (convenience)

: Go to title page
   0, <
: Go to n page
   1-9
: Go to last page
   >

= Key bindings (feature)

: toggle full screen
   F10

: screenshot
   save each page as image

   s

: reload theme
   t, r

= Finish

What about this?
