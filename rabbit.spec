%define rbindir %(ruby -r rbconfig -e 'print Config::CONFIG["bindir"]')
%define rlibdir %(ruby -r rbconfig -e 'print Config::CONFIG["rubylibdir"]')

Summary: an application to do presentation with RD document
Summary(ja): RDで書かれた文書をもとにプレゼンテーションを行うためのソフトウェア
Name: rabbit
Version: 0.0.7
Release: 1
URL: http://www.cozmixng.org/~rwiki/?cmd=view;name=Rabbit
Source0: http://www.cozmixng.org/~kou/download/%{name}-%{version}.tar.gz
License: Ruby's
Group: Applications/Text
BuildRoot: %{_tmppath}/%{name}-%{version}-root
BuildArch: noarch

%description
This is an application to do presentation with RD document.

%description -l ja
RDで書かれた文書をもとにプレゼンテーションを行うためのソフト
ウェアです．

%prep
%setup -q

%build

%install
rm -rf %{buildroot}
%{ruby} setup.rb all --bindir=%{buildroot}%{rbindir} \
                     --rbdir=%{buildroot}%{rlibdir} \
                     --datadir=%{buildroot}%{_datadir}
%{find_lang} %{name}

%clean
rm -rf %{buildroot}

%files -f %{name}.lang
%defattr(-,root,root)
%doc README.* misc sample
%{rbindir}/rabbit
%{rlibdir}/rabbit
%{rlibdir}/rwiki

%changelog
* Fri Feb 25 2005 IWAI, Masaharu <iwai@alib.jp> 0.0.7-1
- Initial build.

