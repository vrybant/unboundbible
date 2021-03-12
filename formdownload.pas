unit FormDownload;

interface

uses
  Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, Grids, SysUtils,
  LCLIntf, UnitLib;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    ButtonDownloads: TButton;
    ButtonFolder: TButton;
    ButtonOK: TButton;
    LabelInfo: TLabel;
    LabelTest: TLabel;
    Panel1: TPanel;
    StringGrid: TStringGrid;
    procedure ButtonDownloadsClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure StringGridGetCheckboxState(Sender: TObject; aCol, aRow: Integer;
      var Value: TCheckboxState);
    procedure StringGridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
  private
    procedure ShelfToListBox;
    procedure ListBoxToShelf;
  public
    procedure LoadGrid(Strings: TStringsArray);
    procedure Localize;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitData, UnitShelf, UnitLocal;

var
  CheckList: array of TCheckBoxState;

{$R *.lfm}

procedure TDownloadForm.Localize;
begin
  Caption := ' ' + T('Modules');
  ButtonOK.Caption := T('OK');
  ButtonDownloads.Caption := T('Download');
  ButtonFolder.Caption := T('Folder');
  StringGrid.Columns[1].Title.Caption := T('Title');
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
var i: integer;
begin
  StringGrid.RowCount := 1;
  Application.HintPause := 1;

  SetLength(CheckList, 500);
  for i:= 0 to Shelf.Count-1 do CheckList[i] := cbUnchecked;

  for i:= 0 to Shelf.Count-1 do
    CheckList[i] := iif(Shelf[i].Favorite, cbChecked, cbUnchecked);
end;

procedure TDownloadForm.LoadGrid(Strings: TStringsArray);
var
  List : TStringArray;
  index : integer = 1;
begin
   for List in Strings do
     begin
       StringGrid.InsertRowWithValues(index, List);
       index += 1;
     end;
//   StringGrid.SortColRow(True, 1);
end;

procedure TDownloadForm.ButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TDownloadForm.ButtonOKClick(Sender: TObject);
begin
  ListBoxToShelf;
end;

procedure TDownloadForm.ButtonDownloadsClick(Sender: TObject);
begin
  OpenURL(DownloadsURL);
end;

procedure TDownloadForm.StringGridCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then CheckList[aRow-1] := aState;
end;

procedure TDownloadForm.StringGridGetCheckboxState(Sender: TObject; aCol,
  aRow: Integer; var Value: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then Value := CheckList[aRow-1];
end;

procedure TDownloadForm.ShelfToListBox;
var i: integer;
begin
end;

procedure TDownloadForm.ListBoxToShelf;
var i: integer;
begin
  output(StringGrid.Row);
  output(StringGrid.Cells[1,StringGrid.Row]);

  for i:= 0 to Shelf.Count-1 do
      if StringGrid.Cells[1,StringGrid.Row] <> currBible.name then
        Shelf[i].Favorite := CheckList[i] = cbChecked;
end;

procedure TDownloadForm.StringGridGetCellHint(Sender: TObject; aCol,
  aRow: Integer; var HintText: String);
const
  delta = {$ifdef windows} 4 {$else} 6 {$endif};
begin
  HintText := '';
  LabelTest.Visible := True;
  LabelTest.Caption := StringGrid.Cells[aCol, aRow];
  if LabelTest.Width > StringGrid.Columns[aCol].Width - delta then
    HintText := StringGrid.Cells[aCol, aRow];
  LabelTest.Visible := False;
end;

procedure TDownloadForm.StringGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  LabelInfo.Caption := Trim(StringGrid.Cells[1, aRow]);

end;

end.

