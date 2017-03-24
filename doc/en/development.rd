---
layout: en
title: Development
---
== Repository

Rabbit's repository is
((<GitHub|URL:https://github.com/rabbit-shocker/rabbit/>)).

== Commit mail

You can stay up to date on the latest development by
subscribing to the git commit ML. If you want to join,
send an e-mail like the following.

  To: rabbit@ml.commit-email.info
  Cc: null@commit-email.info
  Subject: Subscribe

  Subscribe

== Bug report

Use mailing list (see ((<users.rd/Users>)) page about
mailing list) or ((<Issues on
GitHub|URL:https://github.com/rabbit-shocker/rabbit/issues>)) for
reporting a bug or a request.

== Contribution

We welcome your contribution. :)

=== Theme

We always welcome new theme. Please tell us your own theme
when you create one.

=== Documentation

Rabbit lacks of documentation in English. Please consider 
to write your tips to ((<faq.rd/FAQ>)) page.

=== Translation

Please translate messages in Rabbit into your native
language. You can make po/#{LANG}/rabbit.po by running
the following:

  % mkdir po/#{LANG}
  % ./update-po.rb

=== Build development environment

==== Clone repository

Source code of ((<"http://rabbit-shocker.org/"/this site>)) are on the Github. It's same of Rabbit's ((<repository|URL:https://github.com/rabbit-shocker/rabbit/>)).

  % git clone https://github.com/rabbit-shocker/rabbit.git

It's under doc dir.


==== Install gems

Install gems by Bundler.

  % cd rabbit
  % bundle install --path vendor/bundle

((<"http://bundler.io/"/Bundler>)) is package manager tool. It needs installing before.

  % gem install bundler


==== Start development server

Start development server by rake task.

  % bundle exec rake doc:server

Web server is booted after run doc:server task. Access ((<"http://127.0.0.1:4000/index.html.ja"/"http://127.0.0.1:4000/index.html.ja">)) locallly.
