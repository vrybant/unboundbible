unit FormCompare;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst, SysUtils;

type

  { TCompareForm }

  TCompareForm = class(TForm)
    ButtonOK: TButton;
    CheckListBox: TCheckListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure MakeListBox;
    procedure ShelfToListBox;
    procedure ListBoxToShelf;
  public
    procedure Localize;
  end;

var
  CompareForm: TCompareForm;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

procedure TCompareForm.Localize;
begin
  Caption := ' ' + T('Compare');
  ButtonOK.Caption := T('OK');
end;

procedure TCompareForm.FormCreate(Sender: TObject);
begin
  MakeListBox;

  {$ifdef darwin}
  ButtonOK.Visible := False;
  Height := 182;
  {$endif}
end;

procedure TCompareForm.FormShow(Sender: TObject);
begin
  ShelfToListBox;
end;

procedure TCompareForm.MakeListBox;
var i: integer;
begin
  for i:=0 to Shelf.Count-1 do
    CheckListBox.Items.Add(Shelf[i].Name);
end;

procedure TCompareForm.ShelfToListBox;
var i: integer;
begin
  for i:= 0 to Shelf.Count-1 do
    CheckListBox.Checked[i] := Shelf[i].Compare;
end;

procedure TCompareForm.ListBoxToShelf;
var i: integer;
begin
  for i:= 0 to Shelf.Count-1 do
    Shelf[i].Compare := CheckListBox.Checked[i];
end;

procedure TCompareForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ListBoxToShelf;
end;

end.

