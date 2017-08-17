unit UnitInfo;

interface

uses Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, SysUtils;

type

  { TInfoBox }

  TInfoBox = class(TForm)
    OKButton: TButton;
    Memo: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Translate;
  end;

var
  InfoBox: TInfoBox;

implementation

{$R *.lfm}

uses UnitShelf, UnitLang;

//-----------------------------------------------------------------------------------------

procedure TInfoBox.Translate;
begin
  Caption := ' ' + T('Menu.Info');
end;

procedure TInfoBox.FormCreate(Sender: TObject);
begin
  {$ifdef darwin}
  OKButton.Visible := False;
  Height := 250;
  {$endif}
end;

procedure TInfoBox.FormActivate(Sender: TObject);
var
  List : TStringList;
  s : string;
  i : integer;
begin
  List := TStringList.Create;

  for i:=0 to Bible.InfoList.Count-1 do
    begin
      s := Bible.Info[i];
      System.Delete(s,1,1);
      List.Add(s);
    end;

  Memo.Lines.Clear;
  Memo.Lines.AddStrings(List);

  List.free;
end;

//-----------------------------------------------------------------------------------------

end.

