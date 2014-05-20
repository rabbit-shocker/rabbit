---
layout: en
title: RD
---
{% raw %}
== What is RD?

RD is Ruby's POD, embeddable documentation format in script file. 

RD is influenced mainly from plain2, a program to translate from plain text to
some mark-up language. So, RD looks like plain text, and its simpleness and
neatness make it easy to read and write.

== How does the interpreter work for RD?

Ruby's interpreter, (({ruby})), simply ignores text between a line beginning
with "(({=begin}))" and one beginning with "(({=end}))". So, RD is not only 
embeddable. You can write anything between (({=begin})) and (({=end})). RD is
one of them, but RD will be a standard one.((- If you are interested in others,
see rubyapi2
((<URL:http://www.ueda.info.waseda.ac.jp/~igarashi/ruby/xml.html#rubyapi2>))
for example. -))

== Concepts and Syntax
=== Element, Block and Inline

We will use the term "Element" for features of RD which add information
of document structure to text. In addition, we use a term
"((<Block>))" for Elements for large and global structures, and a term
"((<Inline>))" for ones for small and local decorations.

Paragraph, headline or list is a kind of Block. RD uses indentation and
special characters to describe Blocks. You can also write complicated
structure to nested Blocks. And this style of Block looks naturally like
plain text format.  See ((<Block>)) for details.

Emphasis, code or reference is a kind of Inline. Parentheses and special
characters, like (('((? ... ?))')), are used for Inline. Almost all
Inline may be nested inside each other. See ((<Inline>)) for more details.

=== Block
==== Basic Syntax

Block has line oriented syntax. i.e. each character on the same line belongs to the
same Block. And the indentation of the line represents nesting level and type of Block.
The first character of the line represents the type of Block.

+ Concepts and Terms

:Baseline
  Baseline is standard depth of indent. Relative depth between indent of one
  line and Baseline affects its Block-type.

:Head Char
  Head Char is the first character of line, except whitespace.

:STRINGLINE
  STRINGLINE is line which consists of strings.
  STRINGLINE doesn't have "(({*}))", "(({(((|num|)))}))", "(({:}))", "(({=}))"
  nor "(({+}))" as Head Char. But if the line is indented, "(({=}))" and "(({+}))"
  can be Head Char.

:WHITELINE
  WHITELINE is a line which consists of whitespace, "(({\s}))", "(({\t}))" and
  "(({\n}))".

:Comment
  A line which is matched with (({/^#/})) is regarded as a comment.

+ How Baseline is defined and how it works
At the top level, Baseline is at left end, i.e., indent is 0. In List,
Baseline is defined by first Block of ListItem. For example,

  Example: "|" means Baseline
    |Regard this line as one of TextBlock's in top level.
    |<- So this Baseline is at the left-most position.
      *|it is in List. (1)
       |<- this Baseline is defined by the marked with a (1).
      *    |Different Item of List can have different Baseline inside.
           |<- So this Baseline differs from one of this list's first item.

|Regard this line as one of TextBlock's in top level.
|<- So this Baseline is at the left-most position.
  *|it is in List. (1)
   |<- this Baseline is defined by the marked with a (1).
  *    |Different Items in Lists can have different Baselines inside.
       |<- So this Baseline differs from the one of this list's first item.


If one Block is nested in another Block, the Baseline of the inside Blocks is deeper
than the Baseline of outside Blocks.

The relative position between Baseline and indent affects the type of Block.
If a ((<STRINGLINE>)) has same indent with Baseline, it belongs to
((<TextBlock>)), otherwise, i.e. it has deeper indent than Baseline,
it belongs to ((<Verbatim>)).

==== Types of Block
+ Headline

Headline consists of a line which ((<Head Char>)) is "(({=}))" or
"(({+}))". And Headline can't include more than one line. Headline can
be on only top level.
  Example:
  |<- Regard this as top level Baseline.
  = Headline 1.
  === Headline 1.1.1.
  + Headline 1.1.1.1.1.

= Headline 1.
=== Headline 1.1.1.
+ Headline 1.1.1.1.1.

Headline Mark represents level of Headline. See following figure. first
Mark is biggest one, and last Mark has two parts.
  Fig: Headline Marks
  =
  ==
  ===
  ====
  +
  ++

Text which follows Mark is title of Headline. It is also used as Label of
((<Reference>)).

Title of Headline can contain ((<Inline>))s except for ((<Reference>)) and
Footnote.

+ Include

Include is a line that line head  "(({<<<}))" and included file name. 
You can input from other file text, both RD and target format, with Include.

When you include RD file, included file name have to have ".rd" or ".rb" as
suffix, like "foo.rd", and write full name (not full path) of file after
"(({<<<}))". For example,
  <<< foo.rd

When you include target format file, include file name have to have suffix
of target format standard one, for example ".html" in the case of outputting
HTML, ".texi" in the case of outputting Texinfo, and write base name of file
after "(({<<<}))". For example,
   <<< foo
RD formatter include "foo.html" to output HTML, and include "foo.texi" to
output Texinfo. If you want to use Include for target format, you should
prepare plural type of included file. 

+ TextBlock

TextBlock is composed of ((<STRINGLINE>))s, but the ((<STRINGLINE>))s
must have same indent with ((<Baseline>)). If its indent is deeper
than ((<Baseline>)), the ((<STRINGLINE>)) belongs to ((<Verbatim>)).

TextBlock can include ((<Inline>)) inside.

  Example:
  |
  This is TextBlock. 
  Second line of same TextBlock.
    This line is not TextBlock, but Verbatim.
  * And this line is List. (But this line is exactly TextBlock in ListItem.)

And this example is formatted such like:

This is TextBlock. 
Second line of same TextBlock.
  This line is not TextBlock, but Verbatim.
* And this line is List. (But this line is exactly TextBlock in ListItem.)

+ Verbatim

You can use Verbatim to cite Ruby script. Verbatim is composed of
((<STRINGLINE>))s, and they must be indented deeper than
((<Baseline>)). Verbatim can also include a line whose ((<Head Char>)) is
"(({*}))", "(({(1)}))" and "(({:}))", But it can't be first line of
Verbatim, it is regarded as ((<List>)).  Verbatim can't include a line
which is indented shallower than first line.  Verbatim can include
((<WHITELINE>)).

You can't use ((<Inline>)) in Verbatim.

 Example:
  This is Verbatim.
    Even if a line is indented deeper than first line, it is also in same
    Verbatim.
  * A line seems like list is also included in Verbatim.
 But if the line is indented shallower, it is not in same Verbatim.
 It is in other Verbatim.

And this example is formatted such like:

  This is Verbatim.
    Even if a line is indented deeper than first line, it is also in same
    Verbatim.
  * A line seems like list is also included in Verbatim.
 But if the line is indented shallower, it is not in same Verbatim.
 It is in other Verbatim.

+ List

List is special ((<Block>)). List is composed of ListItems, and
ListItem is composed of Blocks. So List can include Blocks inside,
even also List itself.((- But List can't include ((<Headline>))
nor ((<Include>)). -))

ListItem can include ((<WHITELINE>)), and ((<TextBlock>)) can't
include WHITELINE, so when you want to write 2 TextBlock inside
ListItem, put a WHITELINE between TextBlocks.

There is 3 type of List, "((<ItemList>))", "((<EnumList>))",
"((<DescList>))" and "((<MethodList>)).

++ ItemList

ItemList is simple and not numbered List. ItemListItem begins by a line
whose ((<Head Char>)) is "(({*}))", and first Block of ItemListItem must be
((<TextBlock>)).

  Example:
  * first item of parent list
      * first item of sub list
      * second item of sub list
    text block ( line of first item of parent list)

And this example is formatted such like:

  * first item of parent list
      * first item of sub list
      * second item of sub list
    text block ( line of first item of parent list)

++ EnumList

EnumList is numbered List. EnumListItem starts with a line whose
((<Head Char>)) is "(({(((|num|)))}))"(((|num|)) is integer). EnumList
is same as ((<ItemList>)) on other points.

  Example:
  (1) first line of parent list
        * first line of sub list(ItemList)
  (2) second list of parent list
  (10) number is ignored...

And this example is formatted such like:

  (1) first line of parent list
       * first line of sub list(ItemList)
  (2) second list of parent list
  (10) number is ignored...

++ DescList

DescList is List for descriptions. DescListItem has 2 part. One is Term part,
the other is Description part. Term of DescListItem is also used as Label 
for ((<Reference>)).

Term part is composed of a line whose ((<Head Char>)) is "(({:}))",
and Term part is same as ((<Headline>)), except that a line of Term
part can be indented.

Description part is starts with next line of Term part. ((<Baseline>)) of
Description part must be same or deeper than term of Term part of its pair.
For example, following style is illegal.
  Example:
  :   |Term
    |Description.

Description part can include ((<Block>))s. ((<List>)) can be first
Block of Description part. Term part can contain ((<Inline>))s except
for ((<Reference>)) and ((<Footnote>)).

  Example:
  :Term
     first line of Description
     second line of Description
  :Term2
     * also include List
     * ...

And this example is formatted such like:

  :Term
     first line of definition.
     second line of definition
  :Term2
     * also include list
     * ...

++ MethodList

MethodList is the special type of ((<DescList>)) for explanation
methods of your classes. MethodList is similar with ((<DescList>)) in
almost all part, but it is labeled differently. RD formatters know it
is some kind of program code, e.g. method or constants of class in
Ruby or function prototype in C..., which is on Term part of
MethodList.  So, MethodList is labeled without the part which seems to
be parameters of methods. See ((<Label and Reference>)) for more
detail.

Each item of MethodList has Term part and Description part like
((<DescList>)). And its Term part start with its ((<Head Char>)),
"(({---}))". Its Description part can contain ((<TextBlock>)),
((<Verbatim>)) and ((<List>)). But you shouldn't write MethodList
in any kind of ((<List>)). RD will come to deny it possibly in
future.

  Example:
  --- Array#each {|i| ... } # => labeled as "Array#each"
        yield block for each item.
  --- Array#index(val) # => labeled as "Array#index"
        return index of first item which equals with ((|val|)). if it hasn't
        same item, return (({nil})).

And this example is formatted such like:

  --- Array#each {|i| ... }
        yield block for each item.
  --- Array#index(val)
        return index of first item which equals with ((|val|)). if it hasn't
        same item, return (({nil})).

Some formatter assumes it is Ruby's methods, constants or etc. which
is in Term part of MethodList. it can format term part of MethodList
intelligently, but you have to write according as specific formula
to make the best of its feature. 

Standard Ruby class reference formula is suggested such like: 
  : instance method
      instance method ((|method|)) of class ((|Class|))
        Class#method(its params  ...) { parameter block }
  : class method (class singleton method)
      class method ((|method|)) of class ((|Class|))
        Class.method(its params ...) { parameter block }
  : class constants
      constant ((|Const|)) of class ((|Class|))
        Class::Const
  : functions (private methods on top level)
      function ((|func|))
        function#func(its params ...) { parameter block }

Ruby use some symbol characters (e.g. [], []=, +, - or <<) for the names
of methods. ruby, Ruby interpreter, parses them with different manner from
normal methods, but write them as same as others in this formula.

  Example:
    --- Array#[](key)
          return value which in at index ((|key|)).
    --- Array#[]=(key, value)
          put ((|value|)) into cell of index ((|key|)).
    --- Array#+(other)
          return concatenated (({Array})).

=== Inline

You can use Inline in ((<TextBlock>)), ((<Headline>)) and Term part of
((<DescList>)). Common style of parentheses is used for Inline. Inline
can nest each other.

in following list of Inlines, preformatted text is on left hand side
and postformatted text is on right hand side.

:(('((*Em*))')) => ((*Em*))
    Emphasis.

:(('(({while gets...}))')) => (({while gets...}))
    Code.

:(('((|var|))')) => ((|var|))
    Var.((- You can read very good explanation about Var in texinfo.info. -))

:(('((%ruby -v%))')) => ((%ruby -v%))
    Keyboard.

:(('((:Term:))'))
    => ((:Term:))
    
    Term of Index.

:(('((<Identity or URL>))'))
    => ((<Identity or URL>))
    
    Link, Reference. See ((<RD/Label and Reference>)) for more detail.

:(('((-Footnote-))'))
    => ((-Footnote-))
    
    Footnote.

:(('(('verb\'))')) => (('verb'))
    Inline Verbatim.

==== Label and Reference

Reference needs Label. In RD, only title of ((<Headline>)) and Term of
((<DescList>)) and ((<MethodList>)) is regarded as Label. So, you must
choose different titles for different ((<Headline>))s. This problem
has not resolved yet.

+ How to RD generates Label from Headline, DescList or MethodList

Title of ((<Headline>)) and Term part of ((<DescList>)) and ((<MethodList>))
are regarded as Label. But they can contain ((<Inline>))s, so the situation
isn't so simple.

First, ((<MethodList>)) is special in the part of Labeling. Term part of
((<MethodList>)) can't contain ((<Inline>))s, and RD assumes it is method
reference or such thing which is in Term part of ((<MethodList>)). So 
it is Labeled under following rules.

  (1) It is regarded as Label which is before a character of "(({(}))" or
      "(({{}))".((- text inside "(({(...)}))" is regarded as parameters
      of method, and text inside "(({{...}}))" is regarded as parameter
      block of methods.-))

Following example will help you to understand how this rule works. the Label
which is generated from the term part of ((<MethodList>)) is after mark of
"(({# =>}))".

  Example:
    --- Array.new([size[, val]]) # => Array.new
    --- Array#[]=(key, val) # => Array#[]=
    --- Array#each { ... } # => Array#each
    --- void rb_define_method(VALUE class, ...) # => void rb_define_method

Second, in the case of title of ((<Headline>)) or term part of ((<DescList>)),
there is not such a special rule. But you can use ((<Inline>))s for text on
them, so there are rules to strip ((<Inline>)) mark-ups from text.

  (1) Any ((<Inline>)) mark-ups makes any difference to Label. So, both
        = ((*Headline*))
      and
        = Headline
      are Labeled as "Headline".
  (2) But white spaces which is after open parenthesis and before close
      parenthesis of ((<Inline>)) are striped when RD generates Label from
      it. So, both
        = ((* Headline  *))
      and
        = ((*Headline*))
      are Labeled as "Headline".

+ Reference

You can refer Labeled element with Reference which is a kind of ((<Inline>))
marked up with (('((<...>))')).

Most simple use of Reference is to write Label inside parenthesis.
  ((<Label>))
This works as reference to Label of "Label" and text "Label" is used
also for display such like: ((<Label>)).((- There is no element Labeled
"Label", so it doesn't seems to be Reference probably. -))

When you want to refer a resource pointed with URL, write such like:
  ((<URL:http://www.ruby-lang.org/en/raa.html>))
RD formatter will format it as hyper-link to its URL if possible
such like:((<URL:http://www.ruby-lang.org/en/raa.html>)).

When you want to use other text than Label for display, write such like:
  ((<Text for display|Label>))
"Text for display" is used for display and "Label" is used as Label to
refer such like: ((<Text for display|Label>))

Text for display can contain some ((<Inline>)), except for Footnotes and
Reference.

In Reference, "|" and "/" are syntax keywords. So if you will write them
inside Reference, you have to double-quote the part it's inside, such like:
   ((<"Bar | inside display text"|Label>))
((<"Bar | inside display text"|Label>))

The situations are same whether you will write them inside Substitution
part, Filename part and Label part.

Of course, you can use the text for the display for Reference to URL resource.
   ((<Ruby Application Archive|URL:http://www.ruby-lang.org/en/raa.html>))
((<Ruby Application Archive|URL:http://www.ruby-lang.org/en/raa.html>))

When the text for the display is omitted, you can use ((<Inline>)) for Label.
   ((<((*Label*))>))
((<((*Label*))>))
{% endraw %}
