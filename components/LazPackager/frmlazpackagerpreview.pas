{ LazPackager preview window for generated files

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

unit frmLazPackagerPreview;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SynEdit, Forms;

type

  { TFFilePreview }

  TFFilePreview = class(TForm)
    EdPreview: TSynEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure SetText(Title, Txt: String);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FFilePreview: TFFilePreview;

implementation

{$R *.lfm}

{ TFFilePreview }

procedure TFFilePreview.SetText(Title, Txt: String);
begin
  Caption := Format('Preview of %s', [Title]);
  EdPreview.Text := Txt;
end;

procedure TFFilePreview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

