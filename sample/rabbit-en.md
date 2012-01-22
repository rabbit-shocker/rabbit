# Rabbit

subtitle
:   Presentation with Markdown

author
:   Kouhei Sutou

institution
:   COZMIXNG

theme
:   rabbit

# Rabbit

A presentation tool

* Impl.: Ruby/GTK+ 2/cairo
* Env.: PC-UNIX/Win/Mac
* Format:
  RD/Wiki/Markdown/PDF
* View: Ruby

# Features: Display (1)

* *Emphasis*
* Syntax highlight

# Features: Display (2)

* Tables
* Interesting themes
* Images
  * Many supported formats
  * PNG/JPEG/.../PDF/EPS/SVG

# Features: Display (3)

* Folding long lines
* Syntax highlight
* Big text

# Features: UI (1)

* Rich key bindings
* Context menu
* Mouse gestures
* Spotlight
* Magnifier

# Features: UI (2)

* Index page
* Graffiti
* I18N
* Search

# Features: UI (3)

* Whiteout/Blackout
* Rabbit hole
  * Make a hole in a slide
* Visualization of remaining time
  * The Tortoise and the Hare

# Features: Input

* File
* Standard input
* HTTP
* Hiki
* SlideShare

# Features: Format

* Wiki（Hiki）
* RD
* Markdown (kramdown)
* PDF
  * →PDF viewer

# Features: Output

* Images
* Images + HTML
* PS/PDF
* PS/PDF for print
  * slides/page

# Features: Ext. API

* HTTP
* dRuby
* XML-RPC
* SOAP

# Features: Creating

* Auto source reload
* Theme reload
* Change theme

# ToDo

  * Inline images
  * Jump to a link
  * Sound
  * Video
  * 3D

# Image

![](lavie.png "Lavie"){:width='100' height='100'}

# Image: Reflect

![](shocker.jpg){:relative_height='80' reflect_ratio='0.5'}

# Image: Background (1)

* Background image
* Centering by default

## Properties

background-image
:   lavie.png

background-image-relative-width
:   50

{::comment}
background-image-align
:   right

background-image-relative-margin-right
:   3
{:/comment}

# Image: Background (2)

![](lavie.png){:relative_width="30" align="right" relative_margin_right="-5"}

* Right justified backgorund image
* Specify in slide
  * \{:align="right"\}

# Image size

Relative image sizes

![](usagi.png){:caption="USAGI" relative_height="50"}

# External image

Download an image from a URL

![](http://www.cozmixng.org/repos/images/cozmixchu.png "COZMIX Chu")

# Math. expressions

* TeX format
* Backends
  * LaTeX

# LaTeX

$$
$f(x)=\displaystyle\int_{-\infty}^x~e^{-t^2}dt$

\LaTeX
$$

# EPS

Create EPS ahead of time

![](equation.eps){:relative_width="80"}

# SVG

![](spiral.svg){:relative_height="100"}

# Dia

![](rabbit.dia){:relative_width="90"}

# GIMP

![](rabbit.xcf){:relative_height="100"}

# Word Wrapping

looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

# Source

The following is source code:

    # comment
    def method_name
      body
    end

End of source code.

# Source: Highlighted

The following is source code:

    # comment
    def method_name
      body
    end
{: lang="ruby"}

End of source code.

# Quotation

> You take the ''red pill'', you stay in Wonderland and 
> I show you how deep the ''rabbit-hole'' goes.

# Enumeration

* Level 1-1
  * Level 2-1
    * Level 3-1
    * Level 3-2
  * Level 2-2
* Level 1-2

# Labeled list

Rabbit
:   USAGI

    Tortoise
    :   KAME

USAGI
:   Rabbit

# Table

| Heading 1 | Heading 2 |
|---------|--------|
| content 1 | content 2 |
| very long content 3 | veeeery looooooooooooooooooooooong content 4 |

# Op.: Move

Next page
:   Bindings for next page/Left click

    n, f, j, l, Spc, Ret, +, ↓, →, ...

Previous page
:   Bindings for prev. page/Center click

    p, b, k, h, BS, Del, -, ↑, ←, ...

# Op.: Advanced move

Go to the title page
:   a, 0, <, Home

Go to page n
:   1-9, +Ctrl=+10, +Alt=+20

Go to the last page
:   e, $, >, End

# Op.: On stage (1)

Toggle full screen
:   F5, F10, F11, Gesture↓↑

Toggle index mode
:   i

Go to the page
:   Double click on the desired page

# Op.: On stage (2)

Cache all slides
:   c

Toggle info window
:   I

# Op.: On stage (3)

Magnifier
:   Ctrl + right click

    Change scale by wheel

Spotlight
:   Double right clicks

    Change radius by wheel

# Op.: On stage (4)

Graffiti
:   Popup menu (right click) →
    "Graffiti mode"

Mouse gesture
:   Right button drag

# Op.: On stage (5)

Whiteout
:   W

Blackout
:   B

# Op.: Save

Screenshot
:   Save each page as an image

    s

Print
:   Print each page as PS/PDF

    Ctrl+p

# Op.: Display

Redraw
:   Ctrl+l

Reload theme
:   t, r

Reset slide adjustment
:   Alt+a

# Op.: Hole

Expand the hole
:   E

Narrow the hole
:   N

# Op.: Search

Search forward
:   C-s, /

Search backward
:   C-r, ?

Quit search
:   C-g

# Op.: Quit

Quit
:   q, Escape

Iconify
:   z

# Conclusion

* A presentation tool
* Multi platform
* Feat./UI: High & Unique
* Emphasize keybord shortcuts
  * UI/text based source
