unit FormCompare;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst, SysUtils;

type

  { TCompareForm }

  TCompareForm = class(TForm)
    ButtonOK: TButton;
    CheckListBox: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    procedure MakeListBox;
    procedure ListBoxToShelf;
  public
    procedure Localize;
  end;

var
  CompareForm: TCompareForm;

implementation

uses UnitBible, UnitLocal;

{$R *.lfm}

procedure TCompareForm.Localize;
begin
  Caption := ' ' + T('Compare');
  ButtonOK.Caption := T('OK');
end;

procedure TCompareForm.FormCreate(Sender: TObject);
begin
  {$ifdef darwin}
  ButtonOK.Visible := False;
  Height := 182;
  {$endif}
end;

procedure TCompareForm.FormShow(Sender: TObject);
begin
  MakeListBox;
end;

procedure TCompareForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ListBoxToShelf;
end;

procedure TCompareForm.MakeListBox;
var
  Bible : TBible;
begin
  CheckListBox.Clear;
  for Bible in Shelf do
    begin
      CheckListBox.Items.Add(Bible.Name);
      CheckListBox.Checked[CheckListBox.Count-1] := Bible.Compare;
    end;
end;

procedure TCompareForm.ListBoxToShelf;
var
  Bible : TBible;
begin
  for Bible in Shelf do
    Bible.Compare := CheckListBox.Checked[Shelf.IndexOf(Bible)];
end;

end.

