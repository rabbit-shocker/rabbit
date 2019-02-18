---
layout: en
title: rabbit-slide command usage
---
== Abstract

((%rabbit-slide%)) is a command that generates a slide template.

Template is helpful for creating a slide even if you create just a
simple slide. You just need a source file for a simple slide. But you
need some files for publishing your slide. ((%rabbit-slide%))
generates all needed files for creating and publishng your slide.

You create your slide after you generate a slide template. The
template includes helper features for creating and publishing your
slide. These helper features are provided as Rake tasks. You can run
an useful task by a command line. Here is a list for useful features:

  * Showing your slide.
  * Generating a PDF from your slide source.
  * Publishing your slide to RubyGems.org as a gem.
  * Publishing your slide to SlideShare.

Let's create a good slide, do a good presentation and share your good
slide!

You can view published slide gems at
((<URL:https://slide.rabbit-shocker.org/>)).

== Templates generation

First, you generate templates by ((%rabbit-slide%)) command. You pass
slide information to the command. It is inconvenient because you need
to pass many information in command line. ((%rabbit-slide%)) will
provide GUI to pass slide information.

Here is a command line that generates templates for a teheme benchmark
slide:

  % rabbit-slide new \
     --id theme-benchmark-en \
     --base-name theme-benchmark \
     --markup-language rd \
     --name "Kouhei Sutou" \
     --email kou@cozmixng.org \
     --rubygems-user kou \
     --tags rabbit

((%--id%)) and ((%--base-name%)) is the required parameters.

((%--id%)) is the slide ID. You need to use only ASCII characters.

((%--base-name%)) is the base name. The base name is a name that is
computed by removing extension from slide source file name. You need
to use oly ASCII characters.

If you publish your slide as a gem on RubyGems.org, you need to
specify user information by ((%--name%)), ((%--email%)),
((%--rubygems-user%)) and so on.

TODO: Describe all parameters.

You can confirm all patermeters by ((%--help%)):

  Usage: rabbit-slide COMMAND [OPTIONS]
   e.g.: rabbit-slide new \
            --id rubykaigi2012 \
            --base-name rabbit-introduction \
            --markup-language rd \
            --name "Kouhei Sutou" \
            --email kou@cozmixng.org \
            --rubygems-user kou \
            --slideshare-user kou \
            --speaker-deck-user kou

  COMMAND
    new:    create a new slide
    change: change an existing slide

  Slide information
          --id=ID                      Slide ID
                                       (e.g.: --id=rubykaigi2012)
                                       (must)
          --base-name=NAME             Base name for the slide source file and generated PDF file
                                       (e.g.: --base-name=rabbit-introduction)
                                       (must)
          --markup-language=LANGUAGE   Markup language for the new slide
                                       (e.g.: --markup-language=rd)
                                       (available markup languages: [rd, hiki, markdown])
                                       (default: rd)
                                       (optional)
          --title=TITLE                Title of the new slide
                                       (e.g.: --title="Rabbit Introduction")
                                       (optional)
          --tags=TAG,TAG,...           Tags of the new slide
                                       (e.g.: --tags=rabbit,presentation,ruby)
                                       (optional)
          --allotted-time=TIME         Allotted time in presentaion
                                       (e.g.: --allotted-time=5m)
                                       (optional)
          --presentation-date=DATE     Presentation date with the new slide
                                       (e.g.: --presentation-date=2012/06/29)
                                       (optional)
  Your information
          --name=NAME                  Author name of the new slide
                                       (e.g.: --name="Kouhei Sutou")
                                       (default: Kouhei Sutou)
                                       (optional)
          --email=EMAIL                Author e-mail of the new slide
                                       (e.g.: --email=kou@cozmixng.org)
                                       (default: kou@clear-code.com)
                                       (optional)
          --rubygems-user=USER         Account for RubyGems.org
                                       It is used to publish your slide to RubyGems.org
                                       (e.g.: --rubygems-user=kou)
                                       (default: kou)
                                       (optional)
          --slideshare-user=USER       Account for SlideShare
                                       It is used to publish your slide to SlideShare
                                       (e.g.: --slideshare-user=kou)
                                       (default: kou)
                                       (optional)
          --speaker-deck-user=USER     Account for Speaker Deck
                                       It is used to publish your slide to Speaker Deck
                                       (e.g.: --speaker-deck-user=kou)
                                       (default: kou)
                                       (optional)

  Common options
          --options-file=FILE          Load options from FILE.
                                       (none)

          --locale-dir=DIR             Specify locale dir as [DIR].
                                       (auto)

          --logger-type=TYPE           Specify logger type as [TYPE].
                                       Select from [gui, stderr].
                                       (STDERR)
          --log-level=LEVEL            Specify log level as [LEVEL].
                                       Select from [debug, info, warning, error, fatal, unknown].
                                       (info)

          --help                       Show this message.
          --version                    Show version.

== Display

You can find the directory that name is same as the slide ID after you
generate slide template. You move to the directory. This documentation
assumes that you specified ((%--id theme-benchmark-en%)):

  % cd theme-benchmark-en

You can display your slide by ((%rake%)):

  % rake

You edit your source file with checking displayed slide. You edit your
source file, displayed slide is updated automatically.

== PDF generation

Let's publish your slide after you did presentation. You can confirm
your slide as PDF on your computer before you publish your slide. You
can generate PDF for your slide by ((%rake pdf%)):

  % rake pdf

It generates ((%pdf/theme-benchmark-en.pdf%)). You open the PDF by
your favorite PDF viewer. You can use ((%rabbit%)) command because
Rabbit is a PDF viewer:

  % cd pdf
  % rabbit theme-benchmark-en.pdf

== Publication

There are publication features for RubyGems.org and SlideShare. There
is no publication feature for Speaker Deck because Speaker Deck
doesn't provide API to upload a presentation.

You need to specify the following parameters for publication on
generating templates:

  * ((%--name%))
  * ((%--email%))
  * ((%--rubygems-user%)): for publishing to RubyGems.org
  * ((%--slideshare-user%)): for publishing to SlideShare

You need to edit the following part in ((%README.rd%)):

  = TODO: SLIDE TITLE

  TODO: SLIDE DESCRIPTION

For example, here is a theme-benchmark-en case:

  = Theme benchmark

  It's a slide for checking a Rabbit's theme. It contains many
  elements. So it's useful for confirming your theme.

You can publish your slide to both RubyGems.org and SlideShare by
((%rabbit publish%)). The following documentation describes to publish
your slide separately.

=== Publication for RubyGems.org

You can publish your slide to RubyGems.org by ((%rabbit
publish:rubygems%)):

  % rake publish:rubygems

You can display a slide published on RubyGems.org by
((%rabbit #{the user name on RubyGems.org}-#{the slide ID}.gem%)).
For theme-benchmark-en case, "the user name on RubyGems.org" is
((%rabbit%)) and "the slide ID" is ((%theme-benchmark-en%)). So you
can display by the following command:

  % rabbit rabbit-theme-benchmark-en.gem

You can view a slide published on RubyGems.org on ((<Rabbit Slide
Show|URL:https://slide.rabbit-shocker.org/>)). URL is
((%https://slide.rabbit-shocker.org/authors/#{the user name on
RubyGems.org}/#{the slide ID}/%)).

Rabbit Slide Show collects and displays all slides published on
RubyGems.org automatically. So you don't need to publish your slide to
Rabbit Slide Show explicitly. You only need to publish your slide to
RubyGems.org.

=== Publication for SlideShare

You can publish your slide to SlideShare by ((%rabbit publish:slideshare%)).

  % rake publish:slideshare

If the task is completed successfully, the slide page on SlideShare is
opened by your favorite browser automatically.
