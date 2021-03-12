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
    ButtonClose: TButton;
    LabelFile: TLabel;
    LabelFilename: TLabel;
    LabelTest: TLabel;
    Memo: TMemo;
    Panel: TPanel;
    StringGrid: TStringGrid;
    procedure ButtonDownloadsClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure StringGridGetCheckboxState(Sender: TObject; aCol, aRow: Integer;
      var Value: TCheckboxState);
    procedure StringGridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
  private
    procedure ListBoxToShelf;
  public
    procedure LoadGrid;
    procedure Localize;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitData, UnitModule, UnitShelf, UnitLocal, UnitCommentary, UnitDictionary;

var
  CheckList: array of TCheckBoxState;

const
  roFavorite = 0;
  roName = 1;
  roLang = 2;
  roInfo = 3;

{$R *.lfm}

procedure TDownloadForm.Localize;
begin
  Caption := ' ' + T('Modules');
  ButtonClose.Caption := T('Close');
  ButtonDownloads.Caption := T('Download');
  ButtonFolder.Caption := T('Folder');
  StringGrid.Columns[1].Title.Caption := T('Title');
  LabelFile.Caption := T('File Name') + ' : ';
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  StringGrid.RowCount := 1;
  Application.HintPause := 1;
end;

procedure TDownloadForm.FormPaint(Sender: TObject);
begin
  LabelFilename.Left := LabelFile.Left + LabelFile.Width;
end;

procedure TDownloadForm.FormShow(Sender: TObject);
var i: integer;
begin
  LoadGrid;

  SetLength(CheckList, 500);
  for i:= 0 to Shelf.Count-1 do CheckList[i] := cbUnchecked;

  for i:= 0 to Shelf.Count-1 do
    CheckList[i] := iif(Shelf[i].Favorite, cbChecked, cbUnchecked);
end;

procedure TDownloadForm.LoadGrid;
var
  i : integer;

  function GetInfo(const Module: TModule): TStringArray;
  begin
    SetLength(Result, 5);
    Result[0] := '';
    Result[1] := ' ' + Module.Name;
    Result[2] := Module.language;
    Result[3] := Module.fileName;
    Result[4] := Module.info;
    if Module.info = '' then Result[4] := Module.Name;
  end;

begin
  for i:=0 to Shelf.Count-1 do
    StringGrid.InsertRowWithValues(i+1, GetInfo(Shelf[i]));

  for i:=0 to Commentaries.Count-1 do
    StringGrid.InsertRowWithValues(i+1, GetInfo(Commentaries[i]));

  for i:=0 to Dictionaries.Count-1 do
    StringGrid.InsertRowWithValues(i+1, GetInfo(Dictionaries[i]));
end;

procedure TDownloadForm.ButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TDownloadForm.ButtonCloseClick(Sender: TObject);
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

procedure TDownloadForm.ListBoxToShelf;
var i: integer;
begin
  output(StringGrid.Row);
  output(StringGrid.Cells[1,StringGrid.Row]);

  //for i:= 0 to Shelf.Count-1 do
  //    if StringGrid.Cells[1,StringGrid.Row] <> currBible.name then
  //      Shelf[i].Favorite := CheckList[i] = cbChecked;
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
  LabelFilename.Caption := StringGrid.Cells[3, aRow];

  Memo.Clear;
  Memo.ScrollBars := iif(Length(StringGrid.Cells[4, aRow]) < 400, ssNone, ssAutoVertical);
  Memo.Lines.Add(StringGrid.Cells[4, aRow]);
  Memo.SelStart := 1;


  output(Length(StringGrid.Cells[4, aRow]));
end;

end.

