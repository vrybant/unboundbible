unit FormFavorite;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst, SysUtils;

type

  { TFavoriteForm }

  TFavoriteForm = class(TForm)
    ButtonOK: TButton;
    CheckListBox: TCheckListBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure MakeListBox;
    procedure ShelfToListBox;
    procedure ListBoxToShelf;
  public
    procedure Localize;
    function CurrItem: string;
  end;

var
  FavoriteForm: TFavoriteForm;

implementation

uses UnitShelf, UnitLocal;

{$R *.lfm}

procedure TFavoriteForm.Localize;
begin
  Caption := ' ' + T('Bible');
  ButtonOK.Caption := T('OK');
end;

procedure TFavoriteForm.FormCreate(Sender: TObject);
begin
  MakeListBox;

  {$ifdef darwin}
  ButtonOK.Visible := False;
  Height := 182;
  {$endif}
end;

procedure TFavoriteForm.FormShow(Sender: TObject);
begin
  ShelfToListBox;
end;

procedure TFavoriteForm.MakeListBox;
var i: integer;
begin
  for i:=0 to Shelf.Count-1 do
    CheckListBox.Items.Add(Shelf[i].Name);
end;

procedure TFavoriteForm.ShelfToListBox;
var i: integer;
begin
  for i:= 0 to Shelf.Count-1 do
    CheckListBox.Checked[i] := Shelf[i].Favorite;
end;

procedure TFavoriteForm.ListBoxToShelf;
var i: integer;
begin
  for i:= 0 to Shelf.Count-1 do
    if CheckListBox.Items[i] <> currBible.name then
      Shelf[i].Favorite := CheckListBox.Checked[i];
end;

procedure TFavoriteForm.ButtonOKClick(Sender: TObject);
begin
  if CheckListBox.ItemIndex >= 0 then
    CheckListBox.Checked[CheckListBox.ItemIndex] := True;
  ListBoxToShelf;
end;

function TFavoriteForm.CurrItem: string;
begin
  Result := '';
  if CheckListBox.ItemIndex >= 0 then
    Result := CheckListBox.Items[CheckListBox.ItemIndex];
end;

end.

