---
layout: en
title: How to make a RD format slide source
---
== About RD format for Rabbit slide

Rabbit uses RD format but it uses different usage with
normal document. See ((<"../rd.rd"/RD>)) about basic RD
syntax, ((<"../sample/"/sample slide>)) about sample slide.

== Page

The largest headline (=) is title of page. The page follows
next title.

  = Title

  SOMETHING

  ...

  = Next Page

  ...

This example has two pages.

== Title page

The first page is the title page. You can specify meta
information of the slide by using a labeled list (:) in the
title page.

  = Title of slide

  : author
     Kouhei Sutou
  : institution
     COZMIXNG

This example shows the author is Kouhei Sutou and his
institution is COZMIXNG.

At the moment, you can specify subtitle, content_source and
even the theme. "theme" is for the default theme of this slide.

== Image

Written as a verbatim block. See sample/rabbit-en.rd.

== Entity reference

Written as an inline verbatim. See sample/rabbit-en.rd.

== Sub/Superscript

Written as an inline verbatim. See sample/rabbit-en.rd.

== Headline

You can only use headlines for the biggest headline. (The
only headline you really need is the title, right?)

== Footnote

You can footnotes but you shouldn't use it in a presentation.
