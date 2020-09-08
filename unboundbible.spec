
Name: unboundbible
Version: 5.1
Release: 0
Group: Applications/Education
Summary: Open Source Bible Application 
License: GPLv3+
URL: http://vladimirrybant.org

#Requires: libc6 >= 2.4, libsqlite3-0 >= 3.5.9, libgtk2.0-0 >= 2.6.0

%define _rpmdir ../SPECS
%define _rpmfilename %%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm

%description
Unbound Bible is an open source and free, multilingual Bible software.

%prep

%build

%install

%files
%{_bindir}/%{name}
%{_datadir}/applications/*.desktop
%{_datadir}/pixmaps/*
%{_datadir}/%{name}/bibles/*
%{_datadir}/%{name}/localization/*
%{_datadir}/%{name}/titles/*

%changelog
* Tue Sep 01 2020 Vladimir Rybant <vrybant@gmail.com> - 5.1-0
- Fixed some bugs.

