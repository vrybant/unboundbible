unit FormShelf;

interface

uses
  Classes, Graphics, Dialogs, Forms, Controls, ComCtrls, StdCtrls, Buttons, ExtCtrls, Grids,
  SysUtils, LCLIntf, LCLType, Menus, UnitLib, UnitModule;

type

  { TShelfForm }

  TShelfForm = class(TForm)
    Images: TImageList;
    LabelFile: TLabel;
    LabelFilename: TLabel;
    LabelTest: TLabel;
    Memo: TMemo;
    PageControl: TPageControl;
    Panel: TPanel;
    StandardToolBar: TToolBar;
    PopupMenu: TPopupMenu;
    MenuItemDelete: TMenuItem;
    StringGrid: TStringGrid;
    TabSheetBibles: TTabSheet;
    TabSheetCommentaries: TTabSheet;
    TabSheetDictionaries: TTabSheet;
    ToolButtonDownload: TToolButton;
    ToolButtonFolder: TToolButton;
    ToolButtonDelete: TToolButton;
    ButtonClose: TButton;
    procedure ButtonDownloadsClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure StringGridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StringGridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure StringGridSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonDownloadClick(Sender: TObject);
    procedure ToolButtonFolderClick(Sender: TObject);
  private
    Modules : TModules;
    {$ifdef windows} MemoWidth : integer; {$endif}
    procedure LoadGrid;
  public
    procedure Localize;
  end;

var
  ShelfForm: TShelfForm;

implementation

uses
  UnitTools, UnitUtils, UnitLocal, UnitBible, UnitCommentary, UnitDictionary, UnitReference;

{$R *.lfm}

const
  clFavr = 0;
  clName = 1;
  clLang = 2;
  clsMax = 3;

procedure TShelfForm.Localize;
begin
  Caption := ' ' + T('Modules');
  MenuItemDelete.Caption := T('Delete');
  StringGrid.Columns[clName].Title.Caption := T('Title');
  LabelFile.Caption := T('File Name') + ' : ';
  ButtonClose.Caption := T('Close');
  ToolButtonDownload.Hint := T('Download');
  ToolButtonFolder.Hint := T('Folder');
  ToolButtonDelete.Hint := T('Delete');
end;

procedure TShelfForm.FormCreate(Sender: TObject);
begin
  Modules := TModules.Create;
  Application.HintPause := 1;
  LabelFilename.Caption := '';
  {$ifdef windows} MemoWidth := Memo.Width; {$endif}

  {$ifdef linux}
    StringGrid.Top := StringGrid.Top + 1;
    StringGrid.Height := StringGrid.Height - 1;
  {$endif}

  PopupMenu.AutoPopup := False;
end;

procedure TShelfForm.FormDestroy(Sender: TObject);
begin
  Modules.Free;
end;

procedure TShelfForm.FormPaint(Sender: TObject);
begin
  LabelFilename.Left := LabelFile.Left + LabelFile.Width;
  {$ifdef windows}
    Memo.Width := MemoWidth - iif(Memo.ScrollBars = ssNone, 10, 0);
  {$endif}
end;

procedure TShelfForm.FormShow(Sender: TObject);
begin
  LoadGrid;
  StringGridSelection(Self, StringGrid.Col, StringGrid.Row);
end;

procedure TShelfForm.StringGridCheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    StringGrid.Cells[aCol, aRow] := iif(aState = cbChecked, '*', '');
end;

procedure TShelfForm.StringGridGetCheckboxState(Sender: TObject; aCol,
  aRow: Integer; var Value: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    Value := iif(StringGrid.Cells[aCol, aRow] = '*', cbChecked, cbUnchecked);
end;

procedure TShelfForm.StringGridSetCheckboxState(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckboxState);
begin
  //
end;

procedure TShelfForm.LoadGrid;
var
  Module: TModule;

  procedure Insert(const Module: TModule);
  var
    List : TStringArray = [];
  begin
    if Module = nil then Exit;
    SetLength(List,clsMax);

    List[clFavr] := iif(Module.favorite,'*','');
    List[clName] := ' ' + Module.Name;
    List[clLang] := Module.language;

    StringGrid.InsertRowWithValues(StringGrid.RowCount, List);
  end;

begin
  Modules.Clear;
  Modules.Add(nil); // titles
  Tools.Get_Modules(Modules);
  StringGrid.RowCount := 1;
  for Module in Modules do Insert(Module);
end;

procedure TShelfForm.StringGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if Modules.Count <= 1 then Exit;
  ToolButtonDelete.Enabled := CurrBible.name <> StringGrid.Cells[clName, aRow].TrimLeft;
  LabelFilename.Caption := Modules[aRow].fileName;
  LabelFile.Visible := LabelFilename.Caption <> '';
  Memo.Clear;
  Memo.ScrollBars := ssAutoVertical;
  if Length(Modules[aRow].info) < 400 then Memo.ScrollBars := ssNone;
  Memo.Lines.Add(Modules[aRow].info);
  Memo.SelStart := 1;
end;

procedure TShelfForm.StringGridGetCellHint(Sender: TObject; aCol,
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

procedure TShelfForm.ToolButtonDeleteClick(Sender: TObject);
begin
  if QuestionDlg(' ' + T('Confirmation'),
    T('Do you wish to delete this module?') + LineBreaker + LineBreaker +
      Modules[StringGrid.Row].name + LineBreaker, mtWarning,
        [mrYes, T('Delete'), mrCancel, T('Cancel'), 'IsDefault'], 0) = idYes then
          begin
            Tools.DeleteModule(Modules[StringGrid.Row]);
            Modules.Delete(StringGrid.Row);
            StringGrid.DeleteRow(StringGrid.Row);
            StringGridSelection(Sender, StringGrid.Col, StringGrid.Row);
          end;
end;

procedure TShelfForm.ToolButtonDownloadClick(Sender: TObject);
begin
  OpenURL(DownloadsURL);
end;

procedure TShelfForm.ToolButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TShelfForm.ButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TShelfForm.ButtonDownloadsClick(Sender: TObject);
begin
  OpenURL(DownloadsURL);
end;

end.

