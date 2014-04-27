---
layout: en
title: FAQ
---
== FAQ

This page lists frequently asked questions. Here is a format used in this page.

== Question...

Answer...

== How can I embed tweets on Rabbit slide?

Use "rabbiter" command. For exapmle, you hit rabbitter command as follows
 after starting Rabbit and you will see tweets including word "ruby" in the bottom of the slides in form of comment.

  % rabbiter --filter ruby

== How can I flow tweets on slides like embeded video?

Run ((%rabbit%)) command with ((%--comment-theme stream-comment%)) options.

  % rabbit --comment-theme stream-comment slide.rab

Taking into tweets with ((%rabbiter%)) command,
you can flow them on your slides.

== How to make a PDF which has some slides per page?

For example, we make XXX_multi.pdf which has 8 slides per
page from XXX.rd.

  % rabbit -p --slides-per-page 8 -o XXX_multi.pdf XXX.rd

== What should I do to use hare and tortoise on PDF slide?

Use --allotted-time option.

  % rabbit --allotted-time 5m slide.pdf

The same value format of 

  # _
  : allotted-time
      5m

on slides are available for that of --allotted-time. 

For this example you set "5m" and the tortoise runs just 5 minutes.

== How can I reset tortoise to start?

Hit Alt+t

== How can I include default style slides with one used "Takahashi method"?

Takahashi method is called lightning-talk as far as Rabbit.

We generally use the theme when we want to make pure Takahashi method slides.

  = Title
  : author
      Anonymous
  : theme
     lightning-talk # <= like this

  = I am

  = a Rubyist!

If you'd like to include plain slide which has e.g. unordered list as well as Takahashi method, you cannot do it in that way. Because the theme lightning-talk does not have any style definition for its display like list.

You can resolve the issue with including theme. Some themes which are mixed with lightning-talk theme are prepared. For example, a theme named "lightning-rabbit" is a mixture of "rabbit" and "lightning-talk". With this, you will get what you want as follows:

  = Title
  : author
     anonymous
  : theme
     lightning-rabbit

  = I am

  = ．．．

    * ．．．
    * ．．．
    * ．．．

  = a Rubyist

 The theme "lightning-rabbit" is very neat like below.

  # enscript ruby
  include_theme("rabbit")       # (1)

  @lightning_talk_proc_name = "lightning-rabbit"
  @lightning_talk_as_large_as_possible = true
  include_theme("lightning-talk-toolkit")

  match(Slide) do |slides|
    slides.each do |slide|
      if slide.lightning_talk?  # (2)
        slide.lightning_talk    # (3)
      end
    end
  end

So in other words,

  (1) Apply a theme for its base.（(1)）
  (2) For only slides for lightning-talk（(2)）
      overwrite its definition for look in one for lightning-talk. （(3)）

In this way, you can include Takahashi method to your slide. Note that ((*slide.lightning_talkは最後に！*)) should be loaded in the last owing to overwrite default settings.

Finally, some themes have another name, for example, (({slide.lightning_talk})) also can be called (({slide.takahashi})) and (({slide.lightning_talk?}))can be called (({slide.takahashi?})).

Happy Rabbitting!

== How can I change property of specific slides?

In following example, you can change font to Italic on slides containing phrase "Redhanded".

  match Slide do |slides|
    slides.each do |slide|
      if slide.match?(/Redhanded/)
        slide.prop_set("style", "italic")
      end
    end
  end

== How can I set new line?

Use following charachters.

  \n

This is available in both content and title.

TODO: Formery we should use &NewLine; but now can use \n
, which means following sentence is oblsolate. Oh my god...

Rabbit has got into the magic as well as Ruby, which "makes what you shouldn't looks ugly". ((-Using (({$})) for global vars is a kind of "Magic Powder" for Ruby. -))．

The reason it is difficult to put new lines in Rabbit and they makes source dirty is that Rabbit wants you not to use so many new lines.

The most important reason of it is that you shouldn't take too long sentences to set new line on slides. You'd better think of shorter one rather than use long terms with many new lines.

If you show so long sentence on your stage, they will achieve more attention than that towards you. Note that some says you should be appearant when want to tell them the most important thing((-sure ?-)). Longer sentence makes it much difficult for you to do so.
