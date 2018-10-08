unit UnitInfo;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, UnboundMemo, UnitLib, UnitType, UnitStream;

type

  { TFormInfo }

  TFormInfo = class(TForm)
    ButtonCopy: TButton;
    Memo: TUnboundMemo;
    procedure FormActivate(Sender: TObject);
  private
    procedure LoadText;
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
  ButtonCopy  .Caption := T('OK');
end;

procedure TFormInfo.FormActivate(Sender: TObject);
begin
  LoadText;
end;

procedure TFormInfo.LoadText;
var
  Stream: TRichStream;
begin
  Stream := TRichStream.Create;
  Load_Verses(Stream);
  Memo.LoadRichText(Stream);
  Stream.Free;
end;


end.

