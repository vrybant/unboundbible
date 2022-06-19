unit FormShelf;

interface

uses
  Classes, Graphics, Dialogs, Forms, Controls, ComCtrls, StdCtrls, Buttons, ExtCtrls, Grids,
  SysUtils, LCLIntf, LCLType, LazUtf8, Menus, UnitLib, UnitModule;

type

  { TShelfForm }

  TShelfForm = class(TForm)
    ButtonDelete: TButton;
    ButtonOpen: TButton;
    Images: TImageList;
    LabelFile: TLabel;
    LabelFilename: TLabel;
    LabelTest: TLabel;
    Memo: TMemo;
    PageControl: TPageControl;
    Panel: TPanel;
    BiblesGrid: TStringGrid;
    CommentariesGrid: TStringGrid;
    DictionariesGrid: TStringGrid;
    BiblesSheet: TTabSheet;
    CommentariesSheet: TTabSheet;
    DictionariesSheet: TTabSheet;
    ToggleBox1: TToggleBox;
    ButtonClose: TButton;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridCheckboxToggled(sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
    procedure GridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure PageControlChange(Sender: TObject);
  private
    CurrModule : TModule;
    ExpertMode : boolean;
    {$ifdef windows} MemoWidth : integer; {$endif}
    procedure LoadGrids;
    function ActiveGrid: TStringGrid;
    procedure ShowDetails;
  public
    procedure Localize;
  end;

var
  ShelfForm: TShelfForm;

implementation

uses
  UnitExport, UnitTools, UnitUtils, UnitLocal, UnitBible, UnitCommentary,
    UnitDictionary, UnitReference;

{$R *.lfm}

const
  apBible        = 0; // active page
  apCommentaries = 1;
  apDictionaries = 2;

const
  clFavr = 0; // column
  clName = 1;
  clLang = 2;
  clsMax = 3;

procedure TShelfForm.Localize;
begin
  Caption := ' ' + T('Modules');
  BiblesGrid.Columns[clName].Title.Caption := T('Title');
  LabelFile.Caption := T('File Name') + ' : ';
  ButtonOpen.Caption := T('Download');
  ButtonDelete.Caption := T('Delete');
  ButtonClose.Caption := T('Close');
end;

procedure TShelfForm.FormCreate(Sender: TObject);
begin
  CurrModule := nil;
  ExpertMode := false;
  Application.HintPause := 1;
  LabelFilename.Caption := '';
  {$ifdef windows} MemoWidth := Memo.Width; {$endif}

  {$ifdef linux}
    BiblesGrid.Top := BiblesGrid.Top + 1;
    BiblesGrid.Height := BiblesGrid.Height - 1;
  {$endif}
end;

procedure TShelfForm.FormPaint(Sender: TObject);
begin
  Caption := ' ' + T('Modules');
  if ExpertMode then Caption := Caption + ' - ' + UTF8UpperCase(T('Expert Mode'));

  ButtonOpen.Caption := iif(ExpertMode, T('Convert'), T('Open'));
  ButtonOpen.Enabled := ExpertMode or (PageControl.ActivePageIndex = apBible);

  LabelFilename.Left := LabelFile.Left + LabelFile.Width;
  {$ifdef windows} Memo.Width := MemoWidth - iif(Memo.ScrollBars = ssNone, 10, 0); {$endif}
end;

procedure TShelfForm.FormShow(Sender: TObject);
begin
  LoadGrids;
  GridSelection(BiblesGrid, 1, 1);
end;

procedure TShelfForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F6) and (Shift = []) then
    begin
      ExpertMode := not ExpertMode;
      ShowDetails;
      Repaint;
    end;
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
  if (aCol <> 0) or (aRow <= 0) then Exit;

  if Sender = BiblesGrid then
    begin
      if CurrBible = Tools.Bibles[aRow-1] then aState := cbChecked;
      Tools.Bibles[aRow-1].favorite := aState = cbChecked
    end;

  if Sender = CommentariesGrid then
    Tools.Commentaries[aRow-1].favorite := aState = cbChecked;

  (Sender as TStringGrid).Cells[aCol, aRow] := iif(aState = cbChecked, '*', '');
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

  for Module in Tools.Bibles do Insert(BiblesGrid, Module);

  for Module in Tools.Commentaries do
    if not (Module as TCommentary).footnotes then Insert(CommentariesGrid, Module);

  for Module in Tools.Dictionaries do
    if not (Module as TDictionary).embedded then Insert(DictionariesGrid, Module);
end;

function TShelfForm.ActiveGrid: TStringGrid;
begin
  case PageControl.ActivePageIndex of
    apBible        : Result := BiblesGrid;
    apCommentaries : Result := CommentariesGrid;
    apDictionaries : Result := DictionariesGrid;
  end;
end;

procedure TShelfForm.ShowDetails;
begin
  if ActiveGrid.Row < 1 then
    begin
      CurrModule := nil;
      LabelFile.Visible := false;
      LabelFilename.Caption := '';
      Memo.Clear;
      Exit;
    end;

  case PageControl.ActivePageIndex of
    apBible        : CurrModule := Tools.Bibles[BiblesGrid.Row-1];
    apCommentaries : CurrModule := Tools.Commentaries[CommentariesGrid.Row-1];
    apDictionaries : CurrModule := Tools.Dictionaries[DictionariesGrid.Row-1];
  end;

  LabelFilename.Caption := CurrModule.fileName;
  LabelFile.Visible := LabelFilename.Caption <> '';
  Memo.Clear;
  Memo.ScrollBars := ssAutoVertical;
  if Length(CurrModule.info) < 400 then Memo.ScrollBars := ssNone;
  Memo.Lines.Add(CurrModule.info);
  Memo.SelStart := 1;

  ButtonOpen.Enabled := not ExpertMode or (ExtractFileExt(CurrModule.fileName) <> '.unbound');
  ButtonDelete.Enabled := CurrBible <> CurrModule;
end;

procedure TShelfForm.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  ShowDetails;
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

procedure TShelfForm.PageControlChange(Sender: TObject);
begin
  ShowDetails;
end;

procedure TShelfForm.ButtonFolderClick(Sender: TObject);
begin
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TShelfForm.ButtonDeleteClick(Sender: TObject);
begin
  if QuestionDlg(' ' + T('Confirmation'), T('Do you wish to delete this module?') +
    LineBreak + LineBreak + CurrModule.name + LineBreak, mtWarning,
      [mrYes, T('Delete'), mrCancel, T('Cancel'), 'IsDefault'], 0) <> idYes then Exit;

  if not Tools.DeleteModule(CurrModule) then
    if QuestionDlg(' ' + T('Error'), T('Cannot delete the module.') + LineBreak,
      mtError, [mrCancel, T('Close')], 0) = idCancel then Exit;

  ActiveGrid.DeleteRow(ActiveGrid.Row);
  GridSelection(Sender, BiblesGrid.Col, BiblesGrid.Row);
end;

procedure TShelfForm.ButtonOpenClick(Sender: TObject);
begin
  if not ExpertMode then
    if PageControl.ActivePageIndex = apBible then
      if Tools.SetCurrBible(CurrModule as TBible) then Exit;

  case PageControl.ActivePageIndex of
    apBible        : Tools.ExportBible(CurrModule as TBible);
    apCommentaries : Tools.ExportCommentary(CurrModule as TCommentary);
    apDictionaries : Tools.ExportDictionary(CurrModule as TDictionary);
  end;

  QuestionDlg(' ', 'Module ' + CurrModule.fileName + ' has been extracted.',
    mtInformation, [mrOK, T('OK'), 'IsDefault'], 0);
end;

end.

