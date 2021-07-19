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
    Panel: TPanel;
    StandardToolBar: TToolBar;
    PopupMenu: TPopupMenu;
    MenuItemDelete: TMenuItem;
    StringGrid: TStringGrid;
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
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StringGridGetCellHint(Sender: TObject; aCol, ARow: Integer; var HintText: String);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonDownloadClick(Sender: TObject);
    procedure ToolButtonFolderClick(Sender: TObject);
  private
    Modules : TModules;
    {$ifdef windows} MemoWidth : integer; {$endif}
    procedure LoadGrid;
    procedure DeleteModule(Module: TModule);
  public
    procedure Localize;
  end;

var
  ShelfForm: TShelfForm;

implementation

uses
  UnitTools, UnitConst, UnitLocal, UnitBible, UnitCommentary, UnitDictionary, UnitReference;

{$R *.lfm}

const
  clName = 0;
  clLang = 1;
  clsMax = 2;

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

procedure TShelfForm.LoadGrid;
var
  Module: TModule;

  procedure InsertRow(Module: TModule);
  var
    List : TStringArray = [];
  begin
    SetLength(List,clsMax);

    List[clName] := ' ' + Module.Name;
    List[clLang] := Module.language;

    StringGrid.InsertRowWithValues(StringGrid.RowCount, List);
    Modules.Add(Module);
  end;

begin
  Modules.Clear;
  Modules.Add(nil); // titles
  StringGrid.RowCount := 1;

  for Module in Bibles       do InsertRow(Module);
  for Module in Commentaries do InsertRow(Module);
  for Module in Dictionaries do InsertRow(Module);
  for Module in References   do InsertRow(Module);
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

procedure TShelfForm.DeleteModule(Module: TModule);
begin
 if Module.ClassType = TBible      then Bibles      .DeleteItem(Module as TBible      );
 if Module.ClassType = TCommentary then Commentaries.DeleteItem(Module as TCommentary );
 if Module.ClassType = TDictionary then Dictionaries.DeleteItem(Module as TDictionary );
 if Module.ClassType = TReference  then References  .DeleteItem(Module as TReference  );
end;

procedure TShelfForm.ToolButtonDeleteClick(Sender: TObject);
begin
  if QuestionDlg(' ' + T('Confirmation'),
    T('Do you wish to delete this module?') + LineBreaker + LineBreaker +
      Modules[StringGrid.Row].name + LineBreaker, mtWarning,
        [mrYes, T('Delete'), mrCancel, T('Cancel'), 'IsDefault'], 0) = idYes then
          begin
            DeleteModule(Modules[StringGrid.Row]);
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

