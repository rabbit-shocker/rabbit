---
layout: en
title: Usage
---
== Run methods

Rabbit can be ran via command line or command launcher,
double click on Windows.

== Command line

You need to specify a file that has slide source when you
run Rabbit via command line.

  % rabbit XXX.rd

or

  % rabbit --type file XXX.rd

or ...

Type the following if you want to run sample/rabbit_en.rd.

  % rabbit --type file sample/rabbit_en.rd

However, some samples use themes in sample/rabbit/theme and
are not displayed correctly when run from the top directory.
It's better to run the samples from the sample directory.

If you are a Windows user, you can drag and drop an RD file
onto bin/rabbit.bat.

Otherwise you can invoke Rabbit by double clicking an RD file if
you associate *.rd (or *.rab (an abbreviation of Rabbit))
with the following.

  <ruby install folder>\bin\rubyw -S rabbit

=== Options

: -t, --theme=THEME
   Specifies the theme.

: -I, --include=PATH 
   Adds PATH to the load path. Load paths are used to find themes.

: -B, --base=BASE
   Specifies the URI or path to resolve relative paths in the
   input. (For example, the path of image). If you don't
   specify it, it'll automatically decide based on the input
   source type.

: -T, --type=TYPE
   Specifies the input source type.

   Available types are rwiki, file (default), argf and uri .

   : argf

     Uses ARGF as input source. This means, you can
     input via standard input.
     
     Note: argf doesn't support the auto source reload feature.
     
     Example:
       % rabbit --type argf file1.rd file2.rd ...
     or
       % cat file1.rd file2.rd ... | rabbit --type argf
     or
       % rabbit --type argf
       = title
       ...
       ^D
     or...

   : file

     Default. This means, `--type argf' is optional.
     
     Gets source from specified file.

     Supports the auto source reload feature.
     
     Example:
       % rabbit --type file file.rd

   : uri

     Gets source from the specified URI.

     You can use the auto source reload feature. But
     Rabbit will only reload once every 60 seconds for
     performance reasons.

     Example:
       % rabbit --type uri http://www.cozmixng.org/repos/rabbit/trunk/sample/rabbit-implementation.rd
     or
       % rabbit --type uri ftp://.../XXX.rd
     or ...

     Incidentally,
       % rabbit --type uri file:///.../XXX.rd
     and
       % rabbit --type uri /.../XXX.rd
     are same as
       % rabbit --type file /.../XXX.rd
     

   : rwiki

     Uses the content of the specified RWiki's page as
     the source. RWiki must have the SOAP interface enabled.

     You can use the auto source reload feature. But
     Rabbit will only reload once every 60 seconds for
     performance reasons.

     Example:
       % rabbit --type rwiki #{URI of SOAP interface of RWiki} #{page name}

     The encoding of #{page name} is the same as the RWiki
     page's encoding.

   : memory

     Manage the source in memory. In this type, the source
     can be modified by the dRuby/SOAP/etc. interfaces.
     
     You can set initial source by specifying a file name.

     Example (without initial source):
       % rabbit --type memory

     Example (with initial source):
       % rabbit --type memory file.rd

: -e, --encoding=ENCODING
   Specifies encoding of the input source.
   
   Default is auto-detect.

: -f, --full-screen, --no-f, --no-full-screen
   Specifies whether Rabbit is invoked in full screen mode.

   Default is no.

: --index-mode, --no-index-mode
   Specifies whether Rabbit is invoked in index mode.

   Default is no.

: -w, --width=WIDTH
   Specifies the width of the window.
   
   Default is 800.
   
: -h, --height=HEIGHT
   Specifies the height of the window.

   Default is 600.
   
: -S, --size=WIDTH,HEIGHT
   Specifies the width and height of the window.

: -s, --save-as-image
   Saves each slide as image and exit.

: -i, --saved-image-type=TYPE
   Specifies image type to save as.
   
   For example, png (default), jpeg etc.

: -b, --saved-image-base-name=BASE_NAME 
  Specifies base name of the saved image. Saved image's
  file name is "#{base name}#{page number}.#{extension}".
   
   Default is the title of the slide.
   
   If the encoding of your file system isn't UTF-8 and the
   saved filename is UTF-8, you may need to setup an
   environment variable such as LANG and
   G_FILENAME_ENCODING.

: --output-html, --no-output-html
   Specifies whether Rabbit generates HTML or not for
   viewing saved slides.
   
   Default is off.

: --output-index-html, --no-output-index-html
   Specifies whether Rabbit generates an index HTML file
   with thumbnailed slides or not.
   
   Default is off.

: -p, --print
   Prints slide and exit. You can print to file or directory
   send printer by using --output-filename.
   
   But the quality is not good.

: -o, --output-filename=FILENAME
   Specifies printed file name. Printed format is decided by
   the extension. The printed format is PostScript when the 
   extension is .ps, PDF when extension is .pdf and
   PostScript otherwise.
   
   You can send the output formatted by PostScript to a
   program by specifying "|program-name".
   
   Default is "#{title of slide}.ps".

: --paper-width=WIDTH
   Specifies the width (in inches) of the paper when printing.
   
   Default is the width of landscape A4.
   
: --paper-height=HEIGHT
   Specifies the height (in inches) of the paper when printing. 

   Default is the height of landscape A4.
   
: --paper-size=WIDTH,HEIGHT
   Specifies the width and height of the paper when printing.
   The width and height is in inches.
   
   Default is the size of landscape A4.

: --slides-per-page=SLIDES
   Specifies slides per page.
   
   Default is 1.

: --margin=={ALL|TOP_BOTTOM,LEFT_RIGHT|TOP,LEFT_RIGHT,BOTTOM|TOP,RIGHT,BOTTOM,LEFT}, --margin-*=MARGIN
   Specifies margin of slide when slides per margin is
   greater than 1.
    
   Default is automatically computed by using slides per
   page. But the algorithm doesn't work very well when 
   the slides per page is anything other than 2 or 8.

: --page-margin=={ALL|TOP_BOTTOM,LEFT_RIGHT|TOP,LEFT_RIGHT,BOTTOM|TOP,RIGHT,BOTTOM,LEFT}, --page-margin-*=MARGIN
   Specifies page margin when printing.
   
   Default is 0.

: --locale-dir=DIR
   Specifies the directory which has the locale data (*.mo).
   If you want to use Rabbit without installing to system,
   you can type the following in the top directory.

     % ruby -I./lib bin/rabbit --locale-dir data/locale sample/rabbit-en.rd
   
   Default is /usr/local/share/locale/, 
   /usr/share/locale/ and so on.

: --logger-type=TYPE
   Specifies how display the error log. If you specify `gui',
   the error log is displayed. You should specify --logger-type
   option because errors may occur when parsing rabbit's options.

     % rabbit --logger-type gui ...
   
   Default is stderr which outputs the log to standard error
   output.

: --use-druby, --no-use-druby
   Specifies whether to use dRuby interface.

   Default is use.

: --druby-uri=URI
   Specifies dRuby interface URI.

   Default is druby://:10101.

: --output-druby-uri, --no-output-druby-uri
   Specifies whether to output dRuby interface URI.

   Default is not output.

: --use-soap, --no-use-soap
   Specifies whether to use SOAP interface.

   Default is not use.

: --soap-host=HOST
   Specifies SOAP interface host.

   Default is 0.0.0.0.

: --soap-port=PORT
   Specifies SOAP interface port.

   Default is 10103.

: --use-xmlrpc, --no-use-xmlrpc
   Specifies whether to use XML-RPC interface.

   Default is not use.

: --xmlrpc-host=HOST
   Specifies XML-RPC interface host.

   Default is 0.0.0.0.

: --xmlrpc-port=PORT
   Specifies XML-RPC interface port.

   Default is 10104.

: --server, --no-server
   Specifies whether to run as server.
   
   Default is not server.

: --public-level=LEVEL
   Specifies what Rabbit functions are public to external
   interfaces (dRuby/XML-RPC/SOAP). Select public level from
   strict, move, read-size, change-size, size, read-source,
   change-source, source and all. The later the public level
   indicates that Rabbit publishes more functions.

   Default is strict.

: --comment-source=FILE
   Specifies initial comment source file name.
   
   Default is simple comment source provided by system.

: --comment-encoding=ENCODING
   Specifies comment source encoding.

   Default is auto-detect.

: --migemo-dictionary-search-path=PATH1,PATH2,...
   Specifies search paths for Migemo's static
   dictionary. Search paths are specified as a directory
   that has a static dictionary whose name is specified by
   --migemo-dictionary-name or as a path of a static
   dictionary. Two or more search paths can be specified by
   separating them with commas.

   Default is /usr/local/share and /usr/share.

: --migemo-dictionary-name=NAME
   Specifies Migemo's static dictionary name.

   Default is migemo-dict.

: --use-gl, --no-use-gl
   Specifies whether to use OpenGL if available.

   Default is not use.

: --show-native-window-id, --no-show-native-window-id
   Specifies whether show window ID if available.

   Default is not show.

== Launcher mode

Rabbit shows file chose dialog on no console
environment. Rabbit shows a slide what you chose.
