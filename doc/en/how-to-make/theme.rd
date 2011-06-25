---
layout: en
title: How to make a theme
---
== About theme

Slide look and feel is independent of slide source
format. It's theme.

There is a slide for testing theme at
sample/theme-bench-en.rab. It's useful to confirm theme.

== Where do I place the theme?

Rabbit searches for themes by using ruby's $LOAD_PATH. If you make
a theme, place it into
((|PATH_OF_INCLUDED_IN_$LOAD_PATH/rabbit/theme/THEME_NAME/THEME_NAME.rb|))
or ((|DIRECTORY_OF_SOURCE_FILE/THEME_NAME.rb|)).

== How do I write a theme?

A theme is just a Ruby script.

You may enumerate the following descriptions in your theme.

  (1) Select some elements (paragraphs, titles and so on) of
      the slide as a target.

  (2) Change properties of selected elements and add actions.

For example, the following changes color of the title of each
page except the title page.

  match(Page, HeadLine) do |heads|
    heads.prop_set("foreground", "red")
  end

== Property

You can set some properties by using
(({prop_set})). ((<Pango Text Attribute
Markup|URL:http://developer.gnome.org/doc/API/2.4/pango/PangoMarkupFormat.html>))
has more information.

: font_desc
   Specifies font description.

: font_family
   Specifies font family.
   
   You can get a list of font families by using (({font_families})).

: face
   Same as font_family.

: size
   Specifies size of font.
   
   If you specify size of font by numeric value, you should
   convert the value by (({screen_size})) and multiply it by
   (({Pango::SCALE})). The following example shows how to
   specify size of font to 2 (in the Rabbit's theme world).

     screen_size(2) * Pango::SCALE

: style
   Specifies style of font.

: weight
   Specifies weight of font.

: variant
   Specifies variant of font.

: stretch
   Specifies stretch of font.

: foreground
   Specifies foreground.

: background
   Specifies background.

: underline
   Specifies kind of underline.

: rise
   Specifies vertical position of character.

: strike through
   Specifies whether use strike line or not.

: fallback
   Specifies whether use alternative font or not when
   specified font is not found.

: lang
   Specifies the language.

: b
   Changed to bold.

: big
   Changed size of font to big.

: i
   Changed to italic.

: s
   Draws a strike line.

: sub
   Changed to subscript.

: sup
   Changed to superscript.

: small
   Changed size of font to small.

: tt
   Changed font family to fixed width font.

: u
   Draws a underline.

== Hooks

You can add procedures which are executed before/after
element is drawn by
(({add_pre_draw_proc}))/(({add_post_draw_proc})).  And you
can remove them by
(({clear_pre_draw_procs}))/(({clear_post_draw_procs})).

The `default' theme has more code. Please see it.
