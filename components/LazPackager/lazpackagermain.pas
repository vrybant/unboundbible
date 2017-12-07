{ LazDebian main unit registers and handles IDE menu items

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

unit LazPackagerMain;

{$mode objfpc}{$H+}

interface

procedure Register;

implementation
uses
  Forms,
  MenuIntf,
  frmLazPackagerOptionsDeb,
  frmLazPackagerMakeDeb;

procedure OpenConfigDialog(Sender: TObject);
begin
  FDebianOptions := TFDebianOptions.Create(Application.MainForm);
  FDebianOptions.ShowModal;
end;

procedure MakeBinaryPackage(Sender: TObject);
begin
  FMakePackage := TFMakePackage.Create(Application.MainForm);
  FMakePackage.SetType(debBinary);
  FMakePackage.ShowModal;
end;

procedure MakeSourcePackage(Sender: TObject);
begin
  FMakePackage := TFMakePackage.Create(Application.MainForm);
  FMakePackage.SetType(debSource);
  FMakePackage.ShowModal;
end;


procedure Register;
begin
  RegisterIDEMenuCommand(itmProjectWindowSection, 'debianopts','Debian Options ...', nil, @OpenConfigDialog);
  {$IFDEF LINUX}
  RegisterIDEMenuCommand(itmProjectSaveSection, 'debianmakesource','Publish Debian Source Package ...', nil, @MakeSourcePackage);
  RegisterIDEMenuCommand(itmRunBuilding, 'debianmakebin','Build Debian Binary Package ...', nil, @MakeBinaryPackage);
  {$ENDIF}
end;


end.

