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
    procedure LoadGrid;
  public
    procedure Localize;
  end;

var
  DownloadForm: TDownloadForm;

implementation

uses UnitData, UnitModule, UnitShelf, UnitLocal, UnitCommentary, UnitDictionary;

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
  Application.HintPause := 1;
  StringGrid.Columns[0].Visible := False;
  LabelFilename.Caption := '';

  {$ifdef linux}
    StringGrid.Top := StringGrid.Top + 1;
    StringGrid.Height := StringGrid.Height - 1;
  {$endif}
end;

procedure TDownloadForm.FormPaint(Sender: TObject);
begin
  LabelFilename.Left := LabelFile.Left + LabelFile.Width;
end;

procedure TDownloadForm.FormShow(Sender: TObject);
begin
  LoadGrid;
end;

procedure TDownloadForm.LoadGrid;
var
  i : integer;

  function GetInfo(const Module: TModule): TStringArray;
  begin
    SetLength(Result, 5);
    Result[0] := iif(Module.Favorite, '*', '');
    Result[1] := ' ' + Module.Name;
    Result[2] := Module.language;
    Result[3] := Module.fileName;
    Result[4] := Module.info;
    if Module.info = '' then Result[4] := Module.Name;
  end;

begin
  StringGrid.RowCount := 1;

  for i:=0 to Shelf.Count-1 do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Shelf[i]));

  for i:=0 to Commentaries.Count-1 do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Commentaries[i]));

  for i:=0 to Dictionaries.Count-1 do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Dictionaries[i]));
end;

procedure TDownloadForm.ButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TDownloadForm.ButtonDownloadsClick(Sender: TObject);
begin
  OpenURL(DownloadsURL);
end;

procedure TDownloadForm.StringGridCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    StringGrid.Cells[aCol, aRow] := iif(aState = cbChecked, '*', '');
end;

procedure TDownloadForm.StringGridGetCheckboxState(Sender: TObject; aCol,
  aRow: Integer; var Value: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    Value := iif(StringGrid.Cells[aCol, aRow] = '*', cbChecked, cbUnchecked);
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
  LabelFile.Visible := LabelFilename.Caption <> '';
  Memo.Clear;
  Memo.ScrollBars := ssAutoVertical;
  if Length(StringGrid.Cells[4, aRow]) < 400 then Memo.ScrollBars := ssNone;
  Memo.Lines.Add(StringGrid.Cells[4, aRow]);
  Memo.SelStart := 1;
end;

end.

