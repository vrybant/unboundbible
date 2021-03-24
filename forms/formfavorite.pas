unit FormFavorite;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst,
  SysUtils, UnitLib;

type

  { TFavoriteForm }

  TFavoriteForm = class(TForm)
    ButtonOK: TButton;
    CheckListBox: TCheckListBox;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckListBoxSelectionChange(Sender: TObject; User: boolean);
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

uses UnitBible, UnitShelf, UnitLocal;

{$R *.lfm}

procedure TFavoriteForm.Localize;
begin
  Caption := ' ' + T('Bible');
  ButtonOK.Caption := T('Close');
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
var
  Bible : TBible;
begin
  for Bible in Shelf do CheckListBox.Items.Add(Bible.Name);
end;

function TFavoriteForm.CurrItem: string;
begin
  Result := '';
  if CheckListBox.ItemIndex >= 0 then
    Result := CheckListBox.Items[CheckListBox.ItemIndex];
end;

procedure TFavoriteForm.ShelfToListBox;
var
  i : integer;
begin
  for i:= 0 to Shelf.Count-1 do
    CheckListBox.Checked[i] := Shelf[i].Favorite;
end;

procedure TFavoriteForm.ListBoxToShelf;
var
  i: integer;
begin
  for i:= 0 to Shelf.Count-1 do
    if CheckListBox.Items[i] <> currBible.name then
      Shelf[i].Favorite := CheckListBox.Checked[i];
end;

procedure TFavoriteForm.ButtonOKClick(Sender: TObject);
begin
//  if CheckListBox.ItemIndex >= 0 then
//    CheckListBox.Checked[CheckListBox.ItemIndex] := True;
  ListBoxToShelf;
end;

procedure TFavoriteForm.CheckListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  {$ifdef windows} if User then CheckListBox.ItemIndex := -1; {$endif}
end;

end.

