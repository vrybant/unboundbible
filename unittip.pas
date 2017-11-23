unit UnitTip;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type
  TFormTip = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormTip: TFormTip;

implementation

{$R *.lfm}

{
  Add('  WELCOME TO UNBOUND BIBLE APPLICATION !');
  Add('');
  Add('  Please, perform the following steps :');
  Add('     1) Download bibles which you want from  http://unboundbible.org');
  Add('     2) Unzip it to  ' + AppDataPath + '  folder');
}

end.

