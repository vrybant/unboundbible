unit UnitInfo;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, UnboundMemo, UnitLib, UnitType;

type

  { TFormInfo }

  TFormInfo = class(TForm)
    Memo: TUnboundMemo;
    procedure FormActivate(Sender: TObject);
  public
    ParagraphStart : integer;
    ParagraphEnd : integer;
    procedure Translate;
  end;

var
  FormInfo: TFormInfo;

implementation

uses UnitTool, UnitLang;

{$R *.lfm}

procedure TFormInfo.Translate;
begin
  Caption := ' ' + T('Info');
end;

procedure TFormInfo.FormActivate(Sender: TObject);
begin
  //
end;

end.

