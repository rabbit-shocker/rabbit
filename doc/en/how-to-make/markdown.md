---
layout: en
title: How to make a Makrdown format slide source
---
## About Markdown format for Rabbit slide

Rabbit uses Markdown format but it uses different usage with
normal document. See [sample slide](../sample/) about sample slide.

### Page

The largest headline (#) is title of page. The page follows
next title.

    # Title

    SOMETHING

    ...

    # Next Page

    ...

This example has two pages.

### Title page

The first page is the title page. You can specify meta
information of the slide by using a labeled list (:) in the
title page.

    # Title of slide

    author
    :    Kouhei Sutou
    institution
    :    COZMIXNG

This example shows the author is Kouhei Sutou and his
institution is COZMIXNG.

At the moment, you can specify subtitle, content_source and
even the theme. "theme" is for the default theme of this slide.

### Emphasis

You can emphasize strings. Enclose it with an asterisk.

    *Emphasis*

### Image

You can describe as normal markdown format.

    ![image](lavie.png)

### Table

Also you can make table.

    |Head1       |Head2        |Head3         |
    |:-----------|------------:|:------------:|
    |Content1    |Content2     |Content3      |

### Items

Markdown has several list notation such as \*, \+, \-
You can use any of the notation.

    * Level1-1
      * Level2-1
    * Level1-2
    * Level1-3

### Quotation

If you put ">" on the head of the line, it will be quoted text.

    > You take the *red pill*, you stay in Wonderland and
    > I show you how deep the *rabbit-hole* goes.
