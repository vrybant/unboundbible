unit FormDownload;

interface

uses
  Classes, Graphics, Dialogs, Forms, Controls, ComCtrls, StdCtrls, Buttons, ExtCtrls, Grids,
  SysUtils, LCLIntf, LCLType, Menus, UnitLib;

type

  { TDownloadForm }

  TDownloadForm = class(TForm)
    Images: TImageList;
    ButtonDownloads: TButton;
    ButtonFolder: TButton;
    ButtonClose: TButton;
    LabelFile: TLabel;
    LabelFilename: TLabel;
    LabelTest: TLabel;
    Memo: TMemo;
    Panel: TPanel;
    StandardToolBar: TToolBar;
    PopupMenu: TPopupMenu;
    MenuItemDelete: TMenuItem;
    StringGrid: TStringGrid;
    ToolButtonDownload: TToolButton;
    ToolButtonFolder: TToolButton;
    ToolButtonDelete: TToolButton;
    procedure ButtonDownloadsClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure StringGridCheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure StringGridGetCheckboxState(Sender: TObject; aCol, aRow: Integer;
      var Value: TCheckboxState);
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StringGridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure ToolButtonDeleteClick(Sender: TObject);
  private
    {$ifdef windows} MemoWidth : integer; {$endif}
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
  MenuItemDelete.Caption := T('Delete');
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  Application.HintPause := 1;
  StringGrid.Columns[0].Visible := False;
  LabelFilename.Caption := '';
  {$ifdef windows} MemoWidth := Memo.Width; {$endif}

  {$ifdef linux}
    StringGrid.Top := StringGrid.Top + 1;
    StringGrid.Height := StringGrid.Height - 1;
  {$endif}

  PopupMenu.AutoPopup := False;
end;

procedure TDownloadForm.FormPaint(Sender: TObject);
begin
  LabelFilename.Left := LabelFile.Left + LabelFile.Width;
  {$ifdef windows}
    Memo.Width := MemoWidth - iif(Memo.ScrollBars = ssNone, 10, 0);
  {$endif}
end;

procedure TDownloadForm.FormShow(Sender: TObject);
begin
  LoadGrid;
  StringGridSelection(Self, StringGrid.Col, StringGrid.Row);
end;

procedure TDownloadForm.MenuItemDeleteClick(Sender: TObject);
begin
end;

procedure TDownloadForm.LoadGrid;
var
  Module : TModule;

  function GetInfo(const Module: TModule): TStringArray;
  begin
    Result := [];
    Result.Add('*');
    Result.Add(' ' + Module.Name);
    Result.Add(Module.language);
    Result.Add(Module.fileName);
    Result.Add(iif(Module.info.IsEmpty, Module.Name, Module.info));
  end;

begin
  StringGrid.RowCount := 1;

  for Module in Shelf do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Module));

  for Module in Commentaries do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Module));

  for Module in Dictionaries do
    StringGrid.InsertRowWithValues(StringGrid.RowCount, GetInfo(Module));
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

procedure TDownloadForm.StringGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  ToolButtonDelete.Enabled := CurrBible.name <> StringGrid.Cells[1, aRow].TrimLeft;
  LabelFilename.Caption := StringGrid.Cells[3, aRow];
  LabelFile.Visible := LabelFilename.Caption <> '';
  Memo.Clear;
  Memo.ScrollBars := ssAutoVertical;
  if Length(StringGrid.Cells[4, aRow]) < 400 then Memo.ScrollBars := ssNone;
  Memo.Lines.Add(StringGrid.Cells[4, aRow]);
  Memo.SelStart := 1;
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

procedure TDownloadForm.ToolButtonDeleteClick(Sender: TObject);
begin
  if QuestionDlg(' ' + T('Confirmation'),
    T('Do you wish to delete this module?') + LineBreaker + LineBreaker +
      StringGrid.Cells[1, StringGrid.Row].Trim + LineBreaker,
        mtWarning, [mrYes, T('Delete'), mrCancel, T('Cancel'), 'IsDefault'], 0) = idYes then
          begin
            StringGrid.DeleteRow(StringGrid.Row);
            StringGridSelection(Sender, StringGrid.Col, StringGrid.Row);
            // delete module
          end;
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

end.

