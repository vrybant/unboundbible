{ LazDebian packager for debian packages.

  Copyright (C) 2012 Bernd Kreuss prof7bit@gmail.com

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  A copy of the GNU General Public License is available on the World Wide
  Web at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite
  330, Boston, MA 02111-1307, USA.
}

unit LazPackagerDebian;

{$mode objfpc}{$H+}

interface
uses
  LazPackagerBase;

const
  CONFNAME_DEB = CONFNAME_BASE + '/deb';

  DEFAULT_CONTROL
    = 'Source: ?PACKAGE_NAME?'+ LF
    + 'Maintainer: ?MAINTAINER? <?MAINTAINER_EMAIL?>'+ LF
    + 'Section: misc'+ LF
    + 'Priority: optional'+ LF
    + 'Standards-Version: 3.9.3'+ LF
    + 'Build-Depends: fpc, lazarus, lcl, lcl-utils, debhelper (>= 8)'+ LF
    + LF
    + 'Package: ?PACKAGE_NAME?'+ LF
    + 'Architecture: any'+ LF
    + 'Depends: ${shlibs:Depends}, ${misc:Depends},'+ LF
    + 'Description: ?DESCRIPTION?'+ LF
    + ' ?DESCRIPTION_LONG?'+ LF
    ;

  DEFAULT_RULES
    = '#!/usr/bin/make -f' + LF
    + LF
    + 'ROOT = $(CURDIR)/debian/?PACKAGE_NAME?' + LF
    + LF
    + 'override_dh_auto_clean:' + LF
    + TAB + '$(RM) -r lib' + LF
    + TAB + '$(RM) lib *.res ?EXECUTABLE?' + LF
    + LF
    + 'override_dh_auto_build:' + LF
    + TAB + 'lazbuild ?PROJECT?' + LF
    + LF
    + 'override_dh_auto_install:' + LF
    + TAB + 'install -d -m 755 $(ROOT)/usr/bin' + LF
    + TAB + 'install -s -m 755 ?EXECUTABLE? $(ROOT)/usr/bin' + LF
    + LF
    + '%:' + LF
    + TAB + 'dh $@' + LF
    ;

  DEFAULT_CHANGELOG
    = '?PACKAGE_NAME? (?FULLVERSION?) ?SERIES?; urgency=low' + LF
    + LF
    + '  * Original version ?VERSION? packaged with lazdebian' + LF
    + LF
    + ' -- ?MAINTAINER? <?MAINTAINER_EMAIL?>  ?DATER?' + LF
    ;

  DEFAULT_COPYRIGHT
    = 'Format: http://www.debian.org/doc/packaging-manuals/copyright-format/1.0/' + LF
    + LF
    + 'Files: *' + LF
    + 'Copyright: ?COPYRIGHT?' + LF
    + 'License: GPL-2+' + LF
    + ' This program is free software; you can redistribute it and/or modify' + LF
    + ' it under the terms of the GNU General Public License as published by' + LF
    + ' the Free Software Foundation; either version 2 of the License, or' + LF
    + ' at your option) any later version.' + LF
    + ' .' + LF
    + ' This program is distributed in the hope that it will be useful,' + LF
    + ' but WITHOUT ANY WARRANTY; without even the implied warranty of' + LF
    + ' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the' + LF
    + ' GNU General Public License for more details.' + LF
    + ' .' + LF
    + ' You should have received a copy of the GNU General Public License along' + LF
    + ' with this program; if not, write to the Free Software Foundation, Inc.,' + LF
    + ' 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.' + LF
    + ' .' + LF
    + ' On Debian systems, the full text of the GNU General Public' + LF
    + ' License version 2 can be found in the file' + LF
    + ' /usr/share/common-licenses/GPL-2' + LF
    ;

type
  { TPackagerDebian adds some debian specific methods to the
    packager base class and also a few more debian related
    templates and settings, creates the debian files and creates
    and runs DEBUILD.sh build script to build the package.}
  TPackagerDebian = class(TPackagerBase)
  public
    Control: String;
    Rules: String;
    Changelog: String;
    Copyright: String;
    Series: String;  // "precise", "quantal", etc.
    PPA: String;
    procedure Save; override;
    procedure Load; override;
    function FillTemplate(Template: String): String; override;
    procedure DoMakePackage(Binary, Sign, Upload: Boolean); override;
  protected
    procedure CreateDebianBuildScript(Binary, Sign, Upload: Boolean);
    procedure CreateDebianFiles;
    function GetBuildScriptName: String; override;
    function GetDateRFC2822: String;
    function GetOrigTarNameOnly: String;
    function GetDebuildPathAbsolute: String;
    function GetDebuildSrcPathAbsolute: String;
    function GetDebuildSrcDebianPathAbsolute: String;
  end;


implementation
uses
  sysutils,
  process,
  FileUtil;

{ TPackagerDebian }

procedure TPackagerDebian.Save;
begin
  inherited Save;
  SaveValue(CONFNAME_DEB, 'series', Series);
  SaveValue(CONFNAME_DEB, 'ppa', PPA);

  SaveValue(CONFNAME_DEB, 'tpl_control', Control);
  SaveValue(CONFNAME_DEB, 'tpl_rules', Rules);
  SaveValue(CONFNAME_DEB, 'tpl_changelog', Changelog);
  SaveValue(CONFNAME_DEB, 'tpl_copyright', Copyright);
end;

procedure TPackagerDebian.Load;
begin
  inherited Load;
  Series := LoadValue(CONFNAME_DEB, 'series', 'precise');
  PPA := LoadValue(CONFNAME_DEB, 'ppa', 'ppa:johndoe/use-your-own');

  Control := LoadValue(CONFNAME_DEB, 'tpl_control', DEFAULT_CONTROL);
  Rules := LoadValue(CONFNAME_DEB, 'tpl_rules', DEFAULT_RULES);
  Changelog := LoadValue(CONFNAME_DEB, 'tpl_changelog', DEFAULT_CHANGELOG);
  Copyright := LoadValue(CONFNAME_DEB, 'tpl_copyright', DEFAULT_COPYRIGHT);
end;

function TPackagerDebian.FillTemplate(Template: String): String;
begin
  Result := inherited FillTemplate(Template);
  ReplaceMany(Result, ['?SERIES?',            Series
                      ,'?FULLVERSION?',       GetOriginalProjectVersion + '-1'
                      ,'?DATER?',             GetDateRFC2822
                      ]);
end;

procedure TPackagerDebian.DoMakePackage(Binary, Sign, Upload: Boolean);
begin
  CreateDebianFiles;
  CreateDebianBuildScript(Binary, Sign, Upload);
  RunBuildScriptAsync;
end;

procedure TPackagerDebian.CreateDebianBuildScript(Binary, Sign, Upload: Boolean);
var
  S: String;
  SName: String;
  DEBUILD: STRING;
begin
  s := '#!/bin/sh' + LF
    + LF
    + 'set -v' + LF
    + 'set -e' + LF
    + Format('cd "%s"', [GetProjectPathAbsolute]) + LF
    + Format('mkdir -p %s', [GetTempPathAbsolute]) + LF
    + FillTemplate(ExportCommands) + LF
    + LF
    + Format('cd %s', [GetTempPathAbsolute]) + LF
    + 'rm -rf DEBUILD' + LF
    + 'rm -f DEBUILD.sh' + LF
    + LF
    + 'cd ..' + LF
    + Format('tar czf %s %s', [GetOrigTarNameOnly, GetOrigFolderNameOnly]) + LF
    + Format('mv %s "%s"', [GetOrigFolderNameOnly, GetDebuildPathAbsolute]) + LF
    + Format('mv %s "%s"', [GetOrigTarNameOnly, GetDebuildPathAbsolute]) + LF
    + LF
    + Format('cd "%s"', [GetDebuildSrcPathAbsolute]) + LF
    + 'mkdir -p debian/source' + LF
    + 'echo "1.0" > debian/source/format' + LF
    + 'echo "8" > debian/compat' + LF
    + 'mv ../control debian/' + LF
    + 'mv ../rules debian/' + LF
    + 'chmod +x debian/rules' + LF
    + 'mv ../changelog debian/' + LF
    + 'mv ../copyright debian/' + LF
    ;

  if Binary then
    S += 'debuild -us -uc -d -b' + LF
  else
    S += 'debuild -us -uc -S' + LF;

  if Sign then begin
    S += 'cd ..' + LF;
    S += 'xterm -e "debsign *.changes"' + LF;
  end;

  if Upload then begin
    S += Format('dput %s *.changes', [PPA]) + LF;
  end;

  SName := ConcatPaths([GetProjectPathAbsolute, 'DEBUILD.sh']);
  CreateFile(SName, S);
end;

procedure TPackagerDebian.CreateDebianFiles;
var
  DirDebuild: String;
begin
  DirDebuild := GetDebuildPathAbsolute;
  if DirectoryExists(DirDebuild) then
    DeleteDirectory(DirDebuild, False);
  MkDir(DirDebuild);
  CreateFile(ConcatPaths([DirDebuild, 'control']), FillTemplate(Control));
  CreateFile(ConcatPaths([DirDebuild, 'rules']), FillTemplate(Rules));
  CreateFile(ConcatPaths([DirDebuild, 'changelog']), FillTemplate(Changelog));
  CreateFile(ConcatPaths([DirDebuild, 'copyright']), FillTemplate(Copyright));
end;

function TPackagerDebian.GetBuildScriptName: String;
begin
  Result := 'DEBUILD.sh';
end;

function TPackagerDebian.GetDateRFC2822: String;
var
  P: TProcess;
  N: Integer;
begin
  P := TProcess.Create(nil);
  P.Executable := 'date';
  P.Parameters.Add('-R');
  P.Options := [poUsePipes, poWaitOnExit];
  try
    P.Execute;
    SetLength(Result, 31);
    // needs to look like this; "Thu, 27 Sep 2012 19:19:14 +0200"
    // exactly 31 characters long, no more, no less.
    N := P.Output.Read(Result[1], 31);
    if N < 31 then
      Result := '### date -R gave wrong data ###';
  except
    Result := '#### error calling date -R ####';
  end;
  P.Free;
end;

function TPackagerDebian.GetOrigTarNameOnly: String;
begin
  Result := Format('%s_%s.orig.tar.gz', [PackageName, GetOriginalProjectVersion]);
end;

function TPackagerDebian.GetDebuildPathAbsolute: String;
begin
  Result := ConcatPaths([GetProjectPathAbsolute, 'DEBUILD']);
end;

function TPackagerDebian.GetDebuildSrcPathAbsolute: String;
begin
  Result := ConcatPaths([GetDebuildPathAbsolute, GetOrigFolderNameOnly]);
end;

function TPackagerDebian.GetDebuildSrcDebianPathAbsolute: String;
begin
  Result := ConcatPaths([GetDebuildSrcPathAbsolute, 'debian']);
end;

end.

