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
    BiblesGrid: TStringGrid;
    CommentariesGrid: TStringGrid;
    DictionariesGrid: TStringGrid;
    BiblesSheet: TTabSheet;
    CommentariesSheet: TTabSheet;
    DictionariesSheet: TTabSheet;
    ToolButtonDownload: TToolButton;
    ToolButtonFolder: TToolButton;
    ToolButtonDelete: TToolButton;
    ButtonClose: TButton;
    procedure ButtonDownloadsClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure GridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonDownloadClick(Sender: TObject);
    procedure ToolButtonFolderClick(Sender: TObject);
  private
    {$ifdef windows} MemoWidth : integer; {$endif}
    procedure LoadGrids;
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
  BiblesGrid.Columns[clName].Title.Caption := T('Title');
  LabelFile.Caption := T('File Name') + ' : ';
  ButtonClose.Caption := T('Close');
  ToolButtonDownload.Hint := T('Download');
  ToolButtonFolder.Hint := T('Folder');
  ToolButtonDelete.Hint := T('Delete');
end;

procedure TShelfForm.FormCreate(Sender: TObject);
begin
  Application.HintPause := 1;
  LabelFilename.Caption := '';
  {$ifdef windows} MemoWidth := Memo.Width; {$endif}

  {$ifdef linux}
    BiblesGrid.Top := BiblesGrid.Top + 1;
    BiblesGrid.Height := BiblesGrid.Height - 1;
  {$endif}

  PopupMenu.AutoPopup := False;
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
  LoadGrids;
  GridSelection(BiblesGrid, 1, 1);
end;

//-------------------------------------------------------------------------------------------------
//                                     CheckboxStates
//-------------------------------------------------------------------------------------------------

procedure TShelfForm.GridGetCheckboxState(Sender: TObject; aCol, aRow: Integer; var Value: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    Value := iif((Sender as TStringGrid).Cells[aCol, aRow] = '*', cbChecked, cbUnchecked);
end;

//-------------------------------------------------------------------------------------------------
//                                       Actions
//-------------------------------------------------------------------------------------------------

procedure TShelfForm.GridCheckboxToggled(Sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
begin
  if (aRow > 0) and (aCol = 0) then
    begin
      (Sender as TStringGrid).Cells[aCol, aRow] := iif(aState = cbChecked, '*', '');
      if Sender = BiblesGrid then Tools.Bibles[aRow-1].favorite := aState = cbChecked;
      if Sender = CommentariesGrid then Tools.Commentaries[aRow-1].favorite := aState = cbChecked;
    end;
end;

//-------------------------------------------------------------------------------------------------

procedure TShelfForm.LoadGrids;
var
  Module: TModule;

  procedure Insert(Grid: TStringGrid; const Module: TModule);
  var
    List : TStringArray = [];
  begin
    if Module = nil then Exit;
    SetLength(List,clsMax);

    List[clFavr] := iif(Module.favorite,'*','');
    List[clName] := ' ' + Module.Name;
    List[clLang] := Module.language;

    Grid.InsertRowWithValues(Grid.RowCount, List);
  end;

begin
  BiblesGrid.RowCount := 1;
  CommentariesGrid.RowCount := 1;
  DictionariesGrid.RowCount := 1;

  for Module in Tools.Bibles       do Insert(BiblesGrid, Module);
  for Module in Tools.Commentaries do Insert(CommentariesGrid, Module);
  for Module in Tools.Dictionaries do Insert(DictionariesGrid, Module);
end;

procedure TShelfForm.GridSelection(Sender: TObject; aCol, aRow: Integer);
var
  Module: TModule;
begin
  if aRow < 1 then Exit;

  if Sender = BiblesGrid       then Module := Tools.Bibles[aRow-1];
  if Sender = CommentariesGrid then Module := Tools.Commentaries[aRow-1];
  if Sender = DictionariesGrid then Module := Tools.Dictionaries[aRow-1];

  ToolButtonDelete.Enabled := CurrBible.name <> (Sender as TStringGrid).Cells[clName, aRow].TrimLeft;
  LabelFilename.Caption := Module.fileName;
  LabelFile.Visible := LabelFilename.Caption <> '';
  Memo.Clear;
  Memo.ScrollBars := ssAutoVertical;
  if Length(Module.info) < 400 then Memo.ScrollBars := ssNone;
  Memo.Lines.Add(Module.info);
  Memo.SelStart := 1;
end;

procedure TShelfForm.GridGetCellHint(Sender: TObject; aCol, aRow: Integer; var HintText: String);
const
  delta = {$ifdef windows} 4 {$else} 6 {$endif};
begin
  HintText := '';
  LabelTest.Visible := True;
  LabelTest.Caption := (Sender as TStringGrid).Cells[aCol, aRow];
  if LabelTest.Width > (Sender as TStringGrid).Columns[aCol].Width - delta then
    HintText := (Sender as TStringGrid).Cells[aCol, aRow];
  LabelTest.Visible := False;
end;

procedure TShelfForm.ToolButtonDeleteClick(Sender: TObject);
begin
  //if QuestionDlg(' ' + T('Confirmation'),
  //  T('Do you wish to delete this module?') + LineBreaker + LineBreaker +
  //    Modules[BiblesGrid.Row].name + LineBreaker, mtWarning,
  //      [mrYes, T('Delete'), mrCancel, T('Cancel'), 'IsDefault'], 0) = idYes then
  //        begin
  //          Tools.DeleteModule(Modules[BiblesGrid.Row]);
  //          Modules.Delete(BiblesGrid.Row);
  //          BiblesGrid.DeleteRow(BiblesGrid.Row);
  //          GridSelection(Sender, BiblesGrid.Col, BiblesGrid.Row);
  //        end;
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

procedure TShelfForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  output('Close')
end;

procedure TShelfForm.ButtonDownloadsClick(Sender: TObject);
begin
  OpenURL(DownloadsURL);
end;

end.

