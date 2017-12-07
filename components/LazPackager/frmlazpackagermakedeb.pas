{ LazDebian dialog to start the packaging

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

unit frmLazPackagerMakeDeb;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls;

type
  TPackageType = (debBinary, debSource);

  { TFMakePackage }

  TFMakePackage = class(TForm)
    btnCancel: TButton;
    btnCreate: TButton;
    chkSign: TCheckBox;
    chkUpload: TCheckBox;
    procedure btnCreateClick(Sender: TObject);
    procedure chkSignChange(Sender: TObject);
    procedure chkUploadChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FTyp: TPackageType;
  public
    procedure SetType(Typ: TPackageType);
  end;

var
  FMakePackage: TFMakePackage;

implementation
uses
  LazPackagerDebian;

{$R *.lfm}

{ TFMakePackage }

procedure TFMakePackage.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFMakePackage.chkUploadChange(Sender: TObject);
begin
  if chkUpload.Checked then
    chkSign.Checked := True;
end;

procedure TFMakePackage.chkSignChange(Sender: TObject);
begin
  if not chkSign.Checked then
    chkUpload.Checked := False;
end;

procedure TFMakePackage.btnCreateClick(Sender: TObject);
var
  Packager: TPackagerDebian;
begin
  Packager := TPackagerDebian.Create;
  Packager.DoMakePackage(FTyp=debBinary, chkSign.Checked, chkUpload.Checked);
  // Packager will free itself after it is done
end;

procedure TFMakePackage.SetType(Typ: TPackageType);
begin
  FTyp := Typ;
  if Typ = debBinary then begin
    Caption := 'Create .deb';
    chkUpload.Enabled := False;
  end
  else begin
    Caption := 'Create source package';
  end;
end;

end.

