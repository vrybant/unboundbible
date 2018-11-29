unit UnitMain;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, ComCtrls, IniFiles, LCLIntf, LCLType, LCLProc, ActnList,
  ClipBrd, StdActns, PrintersDlgs, Types, RichMemo, UnboundMemo, UnitType;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionInterline: TAction;
    IdleTimer: TIdleTimer;
    PrintDialog: TPrintDialog;
    FontDialog: TFontDialog;
    FontDialogNotes: TFontDialog;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    TabSheetCommentary: TTabSheet;
    ToolSeparator1: TToolButton;
    ToolButtonVerses: TToolButton;
    ActionList: TActionList;
    ComboBox: TComboBox;
    FileOpen1: TFileOpen;
    EditCut1: TEditCut;

    MemoBible: TUnboundMemo;
    MemoSearch: TUnboundMemo;
    MemoCompare: TUnboundMemo;
    MemoNotes: TUnboundMemo;

    ActionAbout: THelpAction;
    ActionBold: TAction;
    ActionBullets: TAction;
    ActionCenter: TAction;
    ActionCompare: TAction;
    ActionCopyAs: TAction;
    ActionCopyVerses: TAction;
    ActionEditCopy: TAction;
    ActionEditCut: TAction;
    ActionEditDel: TAction;
    ActionEditFont: TAction;
    ActionEditPaste: TAction;
    ActionEditSelAll: TEditSelectAll;
    ActionEditUndo: TAction;
    ActionExit: TAction;
    ActionFileNew: TAction;
    ActionFileOpen: TAction;
    ActionFilePrint: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFont: TAction;
    ActionItalic: TAction;
    ActionLeft: TAction;
    ActionLink: TAction;
    ActionOptions: TAction;
    ActionRight: TAction;
    ActionSearch: TAction;
    ActionTrans: TAction;
    ActionUnderline: TAction;

    ChapterBox: TListBox;
    BookBox: TListBox;
    MenuItem1: TMenuItem;
    Ruler: TPanel;
    PanelLeft: TPanel;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    Images: TImageList;

    PageControl: TPageControl;
    TabSheetBible: TTabSheet;
    TabSheetSearch: TTabSheet;
    TabSheetCompare: TTabSheet;
    TabSheetNotes: TTabSheet;

    MainMenu: TMainMenu;
    miBibleFolder: TMenuItem;
    miClear: TMenuItem;
    miCompare: TMenuItem;
    miCopy: TMenuItem;
    miCopyAs: TMenuItem;
    miCut: TMenuItem;
    miDownload: TMenuItem;
    miEdit: TMenuItem;
    miExit: TMenuItem;
    miHelp: TMenuItem;
    miHelpAbout: TMenuItem;
    miHome: TMenuItem;
    miLocalization: TMenuItem;
    miNoteNew: TMenuItem;
    miNoteOpen: TMenuItem;
    miNotes: TMenuItem;
    miNoteSave: TMenuItem;
    miNoteSaveAs: TMenuItem;
    miOptions: TMenuItem;
    miPaste: TMenuItem;
    miPrint: TMenuItem;
    miRecent: TMenuItem;
    miSearch: TMenuItem;
    miSelectAll: TMenuItem;
    miTools: TMenuItem;
    miUndo: TMenuItem;
    miVerses: TMenuItem;
    miInterlinear: TMenuItem;
    miTranslate: TMenuItem;

    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N9: TMenuItem;

    PopupMenu: TPopupMenu;
    pmCut: TMenuItem;
    pmCopy: TMenuItem;
    pmPaste: TMenuItem;
    pmCopyAs: TMenuItem;
    pmVerses: TMenuItem;
    pmInterlinear: TMenuItem;

    StandardToolBar: TToolBar;
    ToolButtonBold: TToolButton;
    ToolButtonBullets: TToolButton;
    ToolButtonCenter: TToolButton;
    ToolButtonCompare: TToolButton;
    ToolButtonCopy: TToolButton;
    ToolButtonCut: TToolButton;
    ToolButtonFB: TToolButton;
    ToolButtonFont: TToolButton;
    ToolButtonItalic: TToolButton;
    ToolButtonLeft: TToolButton;
    ToolButtonLink: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonPrint: TToolButton;
    ToolButtonRight: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonUnderline: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolSeparator2: TToolButton;
    ToolSeparator3: TToolButton;
    ToolSeparator4: TToolButton;
    ToolSeparator5: TToolButton;
    MemoCommentary: TUnboundMemo;

    procedure CmdAbout(Sender: TObject);
    procedure CmdCompare(Sender: TObject);
    procedure CmdCopyAs(Sender: TObject);
    procedure CmdCopyVerses(Sender: TObject);
    procedure CmdEdit(Sender: TObject);
    procedure CmdExit(Sender: TObject);
    procedure CmdFileNew(Sender: TObject);
    procedure CmdFileOpen(Sender: TObject);
    procedure CmdFilePrint(Sender: TObject);
    procedure CmdFileSave(Sender: TObject);
    procedure CmdFileSaveAs(Sender: TObject);
    procedure CmdInterline(Sender: TObject);
    procedure CmdOptions(Sender: TObject);
    procedure CmdSearch(Sender: TObject);
    procedure CmdStyle(Sender: TObject);
    procedure CmdStyle2(Sender: TObject);
    procedure CmdTrans(Sender: TObject);

    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure BookBoxClick(Sender: TObject);
    procedure ChapterBoxClick(Sender: TObject);
    procedure MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure MemoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure MemoAttrChange(Sender: TObject);
    procedure miBibleFolderClick(Sender: TObject);
    procedure miDownloadClick(Sender: TObject);
    procedure miHomeClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure ToolButtonFBClick(Sender: TObject);
  private
    NoteFileName: string;
    RecentList: TStringList;
    ShortLink: boolean;
    FBPageVisited: boolean;
    {$ifdef linux} IdleMessage : string; {$endif}
    function UnboundMemo: TUnboundMemo;
    function CheckFileSave: boolean;
    procedure ComboBoxInit;
    procedure EnableButtons;
    procedure UpDownButtons;
    procedure SelectBook(title: string; scroll: boolean);
    procedure GoToVerse(Verse: TVerse; select: boolean);
    procedure LangMenuInit;
    procedure LoadChapter;
    procedure LoadCompare;
    procedure LoadTranslate(Verse: TVerse);
    procedure LoadCommentary;
    procedure LoadStrong(s: string);
    procedure LoadFootnote(s: string);
    procedure MakeBookList;
    procedure MakeChapterList(n: integer);
    procedure OnLangClick(Sender: TObject);
    procedure OnRecentClick(Sender: TObject);
    procedure PerformFileOpen(const FileName: string);
    procedure ReadIniFile;
    procedure RebuildRecentList;
    procedure RecentMenuInit;
    procedure SaveIniFile;
    procedure SearchText(s: string);
    procedure SelectPage(page: integer);
    procedure Translate;
    procedure UpdateCaption(s: string);
    procedure UpdateStatus(s, Hint: string);
    procedure UpdateActionImage;
    procedure VersesToClipboard;
    procedure ShowPopup;
  public
    procedure TranslateAll;
  end;

var
  MainForm: TMainForm;

implementation

uses
  UnitAbout, UnitNotify, UnitSearch, UnitCompare, UnitTool, UnitLang,
  UnitShelf, UnitCopy, UnitTrans, UnitLib;

const
  apBible      = 0; // active page
  apSearch     = 1;
  apCompare    = 2;
  apCommentary = 3;
  apNotes      = 4;

const
  sUntitled = 'Untitled';
  RecentMax = 10;

const
  ms_Confirm   : string = '';
  ms_Strong    : string = 'Strong Dictionary';
  ms_Footnote  : string = '';
  ms_Found     : string = '';
  ms_Message   : string = '';
  ms_Overwrite : string = '';
  ms_Save      : string = '';

{$R *.lfm}

//=================================================================================================
//                                     Create Main Form
//=================================================================================================

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := AppName + ' ' + VersionInfo + ' - Open Source Application';

  CreateDirectories;

  RecentList := TStringList.Create;
  SaveDialog.InitialDir := DocumentsPath;

  NoteFileName := sUntitled;

  ReadIniFile;
  ComboBoxInit;
  LangMenuInit;
  RecentMenuInit;

  if Shelf.Count > 0 then
  begin
    MakeBookList;
    if not Bible.GoodLink(ActiveVerse) then ActiveVerse := Bible.FirstVerse;
    UpdateStatus(Bible.Info, Bible.fileName);

    // LoadChapter; // RichMemo doesn't load from Stream,
                    // so we call it from FormActivate
  end;

  if Shelf.Count = 0 then
  begin
    ActionSearch .Enabled := False;
    ActionOptions.Enabled := False;
    ActionCompare.Enabled := False;
    ActionCopyAs .Enabled := False; // ??
  end;

  NoteFileName := sUntitled;
  MemoNotes.Lines.Clear;
  MemoNotes.Font.Size := DefaultFont.Size;
  ToolButtonFB.Visible := not FBPageVisited;

  {$ifdef linux}
  StandardToolBar.ParentColor := True;
  ActionFilePrint.Visible := False;
  IdleMessage := '';
  IdleTimer.Enabled := true;
  {$endif}

  {$ifdef unix}
  ActionEditUndo.Visible := False;
  ActionLeft    .Visible := False;
  ActionCenter  .Visible := False;
  ActionRight   .Visible := False;
  ActionBullets .Visible := False;
  ToolSeparator1.Visible := False;
  ToolSeparator2.Visible := False;
  ToolSeparator3.Visible := False;
  ToolSeparator4.Visible := False;
  ToolSeparator5.Visible := False;
  {$endif}

  UpdateActionImage;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if ChapterBox.Items.Count = 0 then GoToVerse(ActiveVerse,(ActiveVerse.number > 1)); // first time
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveIniFile;
  RecentList.Free;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  try
    CanClose := CheckFileSave;
  except
    CanClose := False;
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
const
  Streak = 3;
begin
  ComboBox.Width := PanelLeft.Width - ComboBox.Left - Streak;

  BookBox.Top := ComboBox.Top + ComboBox.Height + Streak;
  ChapterBox.Top := BookBox.Top;

  BookBox.Height := PanelLeft.Height - BookBox.Top - BookBox.Left;
  ChapterBox.Height := BookBox.Height;

  ChapterBox.Width := WidthInPixels('150') + 30;
  BookBox.Width := PanelLeft.Width - BookBox.Left - BookBox.Left - ChapterBox.Width - Streak;
  ChapterBox.Left := PanelLeft.Width - ChapterBox.Width - Streak;
end;

procedure TMainForm.Translate;
begin
  miTools.Caption := T('Tools');
  miEdit.Caption := T('Edit');
  miNotes.Caption := T('Notes');
  miHelp.Caption := T('Help');
  miSearch.Caption := T('Search');
  miCompare.Caption := T('Modules');
  miTranslate.Caption := T('Translation');
  miInterlinear.Caption := T('Interlinear');
  miPrint.Caption := T('Print');
  miOptions.Caption := T('Font…');
  miLocalization.Caption := T('Localization');
  miExit.Caption := T('Exit');
  miUndo.Caption := T('Undo');
  miCut.Caption := T('Cut');
  miCopy.Caption := T('Copy');
  miPaste.Caption := T('Paste');
  miClear.Caption := T('Delete');
  miSelectAll.Caption := T('Select All');
  miCopyAs.Caption := T('Copy As…');
  miVerses.Caption := T('Copy Verses');
  miNoteNew.Caption := T('New');
  miNoteOpen.Caption := T('Open…');
  miNoteSave.Caption := T('Save');
  miNoteSaveAs.Caption := T('Save As…');
  miRecent.Caption := T('Open Recent');

  miHome.Caption := T('Home Page');
  miDownload.Caption := T('Bible Downloads');
  miBibleFolder.Caption := T('Bible Folder');
  miHelpAbout.Caption := T('About');

  pmCut.Caption := T('Cut');
  pmCopy.Caption := T('Copy');
  pmPaste.Caption := T('Paste');
  pmCopyAs.Caption := T('Copy As…');
  pmVerses.Caption := T('Copy Verses');
  pmInterlinear.Caption := T('Interlinear') + ' (biblehub.com)';

  TabSheetBible.Caption := T('Bible');
  TabSheetSearch.Caption := T('Search');
  TabSheetCompare.Caption := T('Compare');
  TabSheetCommentary.Caption := T('Commentary');
  TabSheetNotes.Caption := T('Notes');

  ToolButtonNew.Hint := T('New');
  ToolButtonOpen.Hint := T('Open');
  ToolButtonSave.Hint := T('Save');
  ToolButtonPrint.Hint := T('Print');
  ToolButtonSearch.Hint := T('Search');
  ToolButtonCut.Hint := T('Cut');
  ToolButtonCopy.Hint := T('Copy');
  ToolButtonVerses.Hint := T('Copy Verses');
  ToolButtonPaste.Hint := T('Paste');
  ToolButtonUndo.Hint := T('Undo');
  ToolButtonCompare.Hint := T('Modules');

  ToolButtonFont.Hint := T('Font');
  ToolButtonBold.Hint := T('Bold');
  ToolButtonItalic.Hint := T('Italic');
  ToolButtonUnderline.Hint := T('Underline');
  ToolButtonLink.Hint := T('Hyperlink');
  ToolButtonLeft.Hint := T('Align Left');
  ToolButtonCenter.Hint := T('Center');
  ToolButtonRight.Hint := T('Align Right');
  ToolButtonBullets.Hint := T('Bullets');

  ms_Confirm := T('Confirmation');
  ms_Footnote := T('Footnote');
  ms_Found := T('verses found');
  ms_Overwrite := T('OK to overwrite %s?');
  ms_Save := T('Save changes?');
  ms_Message := T('This search returned too many results.') + ' ' +
                T('Please narrow your search.');
end;

procedure TMainForm.TranslateAll;
begin
  Language := TLanguage.Create;

                Translate;
  SearchForm   .Translate;
  CompareForm  .Translate;
  AboutBox     .Translate;
  FormCopy     .Translate;
  FormTranslate.Translate;

  Language.Free;
end;

//-------------------------------------------------------------------------------------------------
//                                       Actions
//-------------------------------------------------------------------------------------------------

procedure TMainForm.CmdAbout(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.CmdStyle(Sender: TObject);
var
  fp: TFontParams;
  tempStart, tempLength: integer;
begin
  fp := MemoNotes.SelAttributes;

  tempStart  := MemoNotes.SelStart;
  tempLength := MemoNotes.SelLength;

  if MemoNotes.SelLength = 0 then MemoNotes.SelectWord;

  if Sender = ActionBold then
    if fsBold in fp.Style then fp.Style := fp.Style - [fsBold]
                          else fp.Style := fp.Style + [fsBold];

  if Sender = ActionItalic then
    if fsItalic in fp.Style then fp.Style := fp.Style - [fsItalic]
                            else fp.Style := fp.Style + [fsItalic];

  if Sender = ActionUnderline then
    if fsUnderline in fp.Style then fp.Style := fp.Style - [fsUnderline]
                               else fp.Style := fp.Style + [fsUnderline];

  if Sender = ActionLink then
    if fp.Color = clNavy then fp.Color := clBlack
                         else fp.Color := clNavy;

//if Sender = ActionSuper then
//  if fp.vScriptPos = vpSuperscript then fp.vScriptPos := vpNormal
//                                   else fp.vScriptPos := vpSuperscript;

  if Sender = ActionFont then
    begin
      FontDialogNotes.Font.Name  := fp.Name;
      FontDialogNotes.Font.Size  := fp.Size;
      FontDialogNotes.Font.Style := fp.Style;
      FontDialogNotes.Font.Color := fp.Color;

      if FontDialogNotes.Execute then
        begin
          fp.Name  := FontDialogNotes.Font.Name;
          fp.Size  := FontDialogNotes.Font.Size;
          fp.Style := FontDialogNotes.Font.Style;
          fp.Color := FontDialogNotes.Font.Color;
        end;
    end;

  MemoNotes.SelAttributes := fp;

  MemoNotes.SelStart := tempStart; // unselect word
  MemoNotes.SelLength := tempLength;

  MemoNotes.Repaint;
end;

procedure TMainForm.CmdStyle2(Sender: TObject);
{$ifdef windows} var ParaNumbering : TParaNumbering; {$endif}
begin
  {$ifdef windows}
  with MemoNotes do
    begin
      if Sender = ActionLeft    then SetParaAlignment(SelStart, SelLength, paLeft   );
      if Sender = ActionCenter  then SetParaAlignment(SelStart, SelLength, paCenter );
      if Sender = ActionRight   then SetParaAlignment(SelStart, SelLength, paRight  );

      if Sender = ActionBullets then
        begin
          GetParaNumbering(SelStart, ParaNumbering );
          if ToolButtonBullets.Down
            then ParaNumbering.Style := pnBullet
            else ParaNumbering.Style := pnNone;
          SetParaNumbering(SelStart, SelLength, ParaNumbering );
        end;
    end;

  MemoNotes.Repaint;
  {$endif}
end;

procedure TMainForm.ComboBoxChange(Sender: TObject);
var
  select : boolean;
begin
  Shelf.SetCurrent(ComboBox.ItemIndex);
  UpdateStatus(Bible.Info, Bible.fileName);
  MakeBookList;
  select := ActiveVerse.number > 1;
  {$ifdef linux}
    if select then IdleMessage := 'GotoVerse(ActiveVerse,true)'
              else IdleMessage := 'GotoVerse(ActiveVerse,false)';
  {$else}
    GotoVerse(ActiveVerse, select);
  {$endif}
end;

procedure TMainForm.ComboBoxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  ComboBox.canvas.fillrect(ARect);
  Canvas.TextOut(ARect.Left + 5, ARect.Top, ComboBox.Items[Index]);
  Canvas.TextOut(ARect.Left + 220, ARect.Top, '[ru]');
end;

procedure TMainForm.CmdCompare(Sender: TObject);
begin
  if CompareForm.ShowModal = mrOk then LoadCompare;
end;

procedure TMainForm.CmdInterline(Sender: TObject);
var s : string;
begin
  if not (ActiveVerse.book in [1..66]) then Exit;
  s := BibleHubArray[ActiveVerse.book];
  s := s + '/' +  ToStr(ActiveVerse.chapter) + '-' + ToStr(ActiveVerse.number) + '.htm';
  OpenURL('http://biblehub.com/interlinear/' + s);
end;

procedure TMainForm.CmdEdit(Sender: TObject);
begin
  if Sender = ActionEditCopy   then UnboundMemo.CopyToClipboard;
  if Sender = ActionEditPaste  then UnboundMemo.PasteFromClipboard;
  if Sender = ActionEditDel    then UnboundMemo.ClearSelection;
  if Sender = ActionEditUndo   then UnboundMemo.Undo;

  if Sender = ActionEditSelAll then
    begin
      UnboundMemo.SelectAll;
      ActiveVerse.Number := MemoBible.ParagraphStart;
      ActiveVerse.Count  := MemoBible.ParagraphCount;
   end;

  if Sender = ActionEditCut then
    begin
      UnboundMemo.CopyToClipboard;
      UnboundMemo.ClearSelection;
    end;

  EnableButtons;
end;

procedure TMainForm.CmdCopyAs(Sender: TObject);
begin
  {$ifdef linux} MemoBible.SaveSelection; {$endif}
  FormCopy.ShowModal;
  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
end;

procedure TMainForm.CmdCopyVerses(Sender: TObject);
begin
  VersesToClipboard;
end;

procedure TMainForm.CmdSearch(Sender: TObject);
begin
  SearchForm.Edit.Font.Name := DefaultFont.Name;
  if SearchForm.ShowModal = mrOk then SearchText(SearchForm.Edit.Text);
end;

procedure TMainForm.CmdTrans(Sender: TObject);
begin
  FormTranslate.Show;
  LoadTranslate(ActiveVerse);
end;

procedure TMainForm.CmdFileNew(Sender: TObject);
begin
  SelectPage(apNotes);
  if not CheckFileSave then Exit;
  NoteFileName := sUntitled;
  MemoNotes.Lines.Clear;
  MemoNotes.Modified := False;
  UpdateCaption(NoteFileName);
end;

procedure TMainForm.CmdFileOpen(Sender: TObject);
begin
  if not CheckFileSave then Exit;
  if OpenDialog.Execute then
  begin
    PerformFileOpen(OpenDialog.FileName);
    MemoNotes.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TMainForm.CmdFileSave(Sender: TObject);
begin
  SelectPage(apNotes);
  if not MemoNotes.Modified then Exit;
  if NoteFileName = sUntitled then
    CmdFileSaveAs(Sender)
  else
  begin
    MemoNotes.SaveToFile(NoteFileName);
    MemoNotes.Modified := False;
  end;
end;

procedure TMainForm.CmdFileSaveAs(Sender: TObject);
begin
  SelectPage(apNotes);

  if NoteFileName = sUntitled then SaveDialog.InitialDir := DocumentsPath
                              else SaveDialog.InitialDir := ExtractFilePath(NoteFileName);

  if SaveDialog.Execute then
  begin
    if Pos('.rtf', SaveDialog.FileName) = 0 then
      SaveDialog.FileName := SaveDialog.FileName + '.rtf';

    if FileExists(SaveDialog.FileName) then
      if MessageDlg(Format(ms_Overwrite, [SaveDialog.FileName]),
        mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;

    MemoNotes.SaveToFile(SaveDialog.FileName);
    NoteFileName := SaveDialog.FileName;

    RebuildRecentList;

    MemoNotes.Modified := False;
    UpdateCaption(ExtractOnlyName(NoteFileName));
  end;
end;

procedure TMainForm.CmdFilePrint(Sender: TObject);
var
  Params : TPrintParams;
begin
  InitPrintParams(Params{%H-});
  if PrintDialog.Execute then UnboundMemo.Print(Params);
end;

procedure TMainForm.CmdExit(Sender: TObject);
begin
  Close
end;

//-------------------------------------------------------------------------------------------------
//                                        memo's events
//-------------------------------------------------------------------------------------------------

procedure TMainForm.MemoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Memo : TUnboundMemo;
  Verse : TVerse;
begin
  if Shelf.Count = 0  then Exit;
  Memo := Sender as TUnboundMemo;

  if Button = mbRight then ShowPopup;
  if Button <> mbLeft then Exit;

//if Memo.hyperlink = '' then
//  begin
      ActiveVerse.Number := MemoBible.ParagraphStart;
      ActiveVerse.Count  := MemoBible.ParagraphCount;
      if FormTranslate.Visible then LoadTranslate(ActiveVerse);
//    Exit;
//  end;

  if Memo.Foreground = fgLink then
    begin
      Verse := Bible.SrtToVerse(Memo.hyperlink);

      if Verse.Book > 0 then
        begin
          if FormTranslate.Visible then LoadTranslate(Verse);
          if (Memo = MemoSearch) or (not FormTranslate.Visible) or (ssCtrl in Shift) then
            GoToVerse(Verse, True);
        end;
    end;

  if Sender <> MemoBible then Exit;

  if Memo.Foreground = fgStrong   then LoadStrong(Memo.Hyperlink);
  if Memo.Foreground = fgFootnote then LoadFootnote(Memo.Hyperlink);
end;

procedure TMainForm.MemoAttrChange(Sender: TObject);
begin
  EnableButtons;
  if Sender = MemoNotes then UpDownButtons;
  // Calling UpDownButtons on SelectionChange event works with bugs.
end;

procedure TMainForm.MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True; // disable system popup menu
end;

//-------------------------------------------------------------------------------------------------
//                                       Interface
//-------------------------------------------------------------------------------------------------

procedure TMainForm.BookBoxClick(Sender: TObject);
var
  Book : TBook;
  s : string;
begin
  if BookBox.Count = 0 then Exit;
  s := BookBox.Items[BookBox.ItemIndex];

  Book := Bible.BookByName(s);
  if not Assigned(Book) then Exit;

  ActiveVerse := minVerse;
  ActiveVerse.Book := Book.Number;

  ChapterBox.ItemIndex := 0;
  LoadChapter;
end;

procedure TMainForm.ChapterBoxClick(Sender: TObject);
begin
  ActiveVerse.Chapter := ChapterBox.ItemIndex + 1;
  ActiveVerse.Number := 1;
  ActiveVerse.Count := 1;
  LoadChapter;
end;

procedure TMainForm.ComboBoxInit;
var i : integer;
begin
  ComboBox.Items.Clear;

  for i := 0 to Shelf.Count - 1 do
    begin
      ComboBox.Items.Add(Shelf[i].Name);
      if i = Shelf.Current then ComboBox.ItemIndex := i;
    end;
end;

procedure TMainForm.UpdateCaption(s: string);
begin
  Caption := AppName + ' ' + VersionInfo + ' - ' + s;
end;

procedure TMainForm.UpdateStatus(s, Hint: string);
begin
  StatusBar.SimpleText := ' ' + s;
  StatusBar.Hint := Hint;
end;

procedure TMainForm.ShowPopup;
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  PopupMenu.Popup(CursorPos.X, CursorPos.Y);
end;

procedure TMainForm.SelectBook(title: string; scroll: boolean);
var
  i, index : integer;
begin
  index := -1;
  for i:=0 to BookBox.Items.Count-1 do
    if BookBox.Items[i] = title then index := i;

  if index < 0 then Exit;

  BookBox.ItemIndex := index;
  if scroll then BookBox.TopIndex := index;
end;

procedure TMainForm.GoToVerse(Verse: TVerse; select: boolean);
var
  Book : TBook;
begin
  if Shelf.Count = 0 then Exit;
  if not Bible.GoodLink(Verse) then Exit;

  Book := Bible.BookByNum(Verse.Book);
  if not Assigned(Book) then Exit;

  ActiveVerse := Verse;
  LoadChapter;

  SelectBook(Book.title, IsNewTestament(Verse.book));
  ChapterBox.ItemIndex := Verse.Chapter - 1;

  if select then MemoBible.SelectParagraph(Verse.Number);
  Repaint;
end;

function TMainForm.CheckFileSave: boolean;
var
  Response : integer;
begin
  Result := True;

  if not MemoNotes.Modified then Exit;
  SelectPage(apNotes);

  {$ifdef windows}
    Response := MessageBox(Handle, PChar(ms_Save), PChar(ms_Confirm), MB_YESNOCANCEL or MB_ICONQUESTION);
  {$else}
    Response := MessageDlg(ms_Save, mtConfirmation, mbYesNoCancel, 0);
    // этот вариант рисует кнопки с картинками
  {$endif}

  // remake!!
  case Response of
    idYes:
    begin
      CmdFileSave(self);
      Result := not MemoNotes.Modified;
    end;
    idNo: {Nothing};
    idCancel: Result := False; // Abort;
  end;
end;

procedure TMainForm.OnLangClick(Sender: TObject);
var
  i: integer;
begin
  facelang := LowerCase((Sender as TMenuItem).Hint);

  for i := 0 to miLocalization.Count - 1 do
    miLocalization.Items[i].Checked := False;
  (Sender as TMenuItem).Checked := True;

  TranslateAll;
end;

procedure TMainForm.OnRecentClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to miRecent.Count - 1 do
    if (Sender as TMenuItem).tag = i then
      if CheckFileSave then
        PerformFileOpen(RecentList[i]);
end;

procedure TMainForm.LangMenuInit;
var
  MenuItem : TMenuItem;
  i : integer;
begin
  for i := 0 to Localization.Count-1 do
  begin
    MenuItem := TMenuItem.Create(MainMenu);

    MenuItem.Caption := Localization.Native(i);
    MenuItem.Hint    := Localization[i];
    MenuItem.Checked := (Localization[i] = FaceLang);
    MenuItem.OnClick := OnLangClick;

    miLocalization.Insert(i, MenuItem);
  end;
end;

procedure TMainForm.RecentMenuInit;
var
  Item : TMenuItem;
  s : string;
  i : integer;
begin
  miRecent.Enabled := RecentList.Count > 0;
  miRecent.Clear;

  for i := RecentList.Count - 1 downto 0 do
    begin
      s := ExtractOnlyName(RecentList[i]);
      Item := NewItem(s, 0, False, True, OnRecentClick, 0, '');
      Item.Tag := i;
      miRecent.Add(Item);
    end;
end;

function TMainForm.UnboundMemo: TUnboundMemo;
begin
  case PageControl.ActivePageIndex of
    apBible      : Result := MemoBible;
    apSearch     : Result := MemoSearch;
    apCompare    : Result := MemoCompare;
    apCommentary : Result := MemoCommentary;
    apNotes      : Result := MemoNotes;
    else
      Result := nil;
  end;
end;

procedure TMainForm.EnableButtons;
var
  B, L : boolean;
  x : integer;
begin
  B := PageControl.ActivePageIndex = apBible;
  L := PageControl.ActivePageIndex = apNotes;

  if B then x := 1 else x:= 0;

  ActionEditCopy.Enabled   := UnboundMemo.SelLength > x;
  ActionEditCut.Enabled    := L and (UnboundMemo.SelLength > 0);
  ActionEditDel.Enabled    := L and (UnboundMemo.SelLength > 0);;
  ActionEditPaste.Enabled  := L and UnboundMemo.CanPaste;
  ActionEditUndo.Enabled   := L and UnboundMemo.CanUndo;

  ActionCopyAs.Enabled     := B;
  ActionCopyVerses.Enabled := B;
  ActionInterline.Enabled  := B;

  ActionFont.Enabled       := L;
  ActionBold.Enabled       := L;
  ActionItalic.Enabled     := L;
  ActionUnderline.Enabled  := L;
  ActionLink.Enabled       := L;
  ActionLeft.Enabled       := L;
  ActionCenter.Enabled     := L;
  ActionRight.Enabled      := L;
  ActionBullets.Enabled    := L;

  UpdateActionImage;
end;

procedure TMainForm.UpDownButtons;
{$ifdef windows} var ParaNumbering : TParaNumbering; {$endif}
begin
  if PageControl.ActivePageIndex <> apNotes then Exit;

  with MemoNotes do
    try
      ToolButtonBold.Down := fsBold in SelAttributes.Style;
      ToolButtonItalic.Down := fsItalic in SelAttributes.Style;
      ToolButtonUnderline.Down := fsUnderline in SelAttributes.Style;
      ToolButtonLink.Down := clNavy = SelAttributes.Color;

      {$ifdef windows}

      case GetParaAlignment(SelStart) of
        paLeft: ToolButtonLeft.Down := True;
        paRight: ToolButtonRight.Down := True;
        paCenter: ToolButtonCenter.Down := True;
      end;

      GetParaNumbering(SelStart, ParaNumbering );
      ToolButtonBullets.Down := ParaNumbering.Style = pnBullet;

      {$endif}
    except
      //
    end;
end;

procedure TMainForm.UpdateActionImage;
var i: integer;
begin
  with ActionList do
    for i := 0 to ActionCount - 1 do
      if TAction(Actions[i]).Tag > 0 then
        if TAction(Actions[i]).Enabled
          then TAction(Actions[i]).ImageIndex := TAction(Actions[i]).Tag
          else TAction(Actions[i]).ImageIndex := TAction(Actions[i]).Tag + 1;
end;

{$ifdef darwin}
procedure TMainForm.UpdateShortCut;
begin
  ActionSearch    .ShortCut := ShortCut(VK_F, [ssMeta]);
  ActionEditUndo  .ShortCut := ShortCut(VK_Z, [ssMeta]);
  ActionEditCut   .ShortCut := ShortCut(VK_X, [ssModifier]);
  ActionEditCopy  .ShortCut := ShortCut(VK_C, [ssMeta]);
  ActionCopyVerses.ShortCut := ShortCut(VK_R, [ssMeta]);
  ActionEditPaste .ShortCut := ShortCut(VK_V, [ssMeta]);
  ActionEditSelAll.ShortCut := ShortCut(VK_A, [ssMeta]);
  ActionFileNew   .ShortCut := ShortCut(VK_N, [ssMeta]);
  ActionFileOpen  .ShortCut := ShortCut(VK_O, [ssMeta]);
  ActionFileSave  .ShortCut := ShortCut(VK_S, [ssMeta]);
  ActionFilePrint .ShortCut := ShortCut(VK_P, [ssMeta]);
  ActionBold      .ShortCut := ShortCut(VK_B, [ssMeta]);
  ActionItalic    .ShortCut := ShortCut(VK_I, [ssMeta]);
  ActionUnderline .ShortCut := ShortCut(VK_U, [ssMeta]);
  ActionLink      .ShortCut := ShortCut(VK_K, [ssMeta]);
end;
{$endif}

procedure TMainForm.RebuildRecentList;
var
  i: integer;
begin
  for i := 0 to RecentList.Count - 1 do
    if RecentList[i] = NoteFileName then
    begin
      RecentList.Delete(i);
      Break;
    end;

  RecentList.Add(NoteFileName);
  if RecentList.Count > RecentMax then RecentList.Delete(0);
  RecentMenuInit;
end;

procedure TMainForm.PerformFileOpen(const FileName: string);
begin
  if not FileExists(FileName) then Exit;
  MemoNotes.LoadFromFile(FileName);
  NoteFileName := FileName;
  RebuildRecentList;
  SelectPage(apNotes);
  MemoNotes.SetFocus;
  MemoNotes.Modified := False;
  UpdateCaption(ExtractOnlyName(NoteFileName));
end;

procedure TMainForm.SelectPage(page: integer);
begin
  PageControl.ActivePageIndex := page;
  EnableButtons;
  Refresh;
end;

procedure TMainForm.miBibleFolderClick(Sender: TObject);
begin
  OpenFolder(GetUserDir + AppName);
end;

procedure TMainForm.miHomeClick(Sender: TObject);
begin
  if (facelang = 'russian') or (facelang = 'ukrainian')
    then OpenURL('http://vladimirrybant.org/ru')
    else OpenURL('http://vladimirrybant.org');
end;

procedure TMainForm.miDownloadClick(Sender: TObject);
begin
  if (facelang = 'russian') or (facelang = 'ukrainian')
    then OpenURL('http://vladimirrybant.org/goto/ubdownloadru.php')
    else OpenURL('http://vladimirrybant.org/goto/ubdownload.php');
end;

procedure TMainForm.CmdOptions(Sender: TObject);
begin
  FontDialog.Font.Assign(DefaultFont);
  if FontDialog.Execute then
  begin
    DefaultFont.Assign(FontDialog.Font);
    MakeBookList;
    LoadChapter;
    FormPaint(self);
    Invalidate;
  end;
end;

procedure TMainForm.IdleTimerTimer(Sender: TObject);
begin
  {$ifdef linux}
  if IdleMessage = 'GotoVerse(ActiveVerse,true)' then
                    GotoVerse(ActiveVerse,true);

  if IdleMessage = 'GotoVerse(ActiveVerse,false)' then
                    GotoVerse(ActiveVerse,false);

  if IdleMessage <> '' then IdleMessage := '';
  {$endif}
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  EnableButtons;
  UpDownButtons;
  UpdateStatus('','');
  UnboundMemo.SetFocus;
  UnboundMemo.Repaint;
  if PageControl.ActivePageIndex = apCompare then LoadCompare;
  if PageControl.ActivePageIndex = apCommentary then LoadCommentary;
end;

procedure TMainForm.RadioButtonClick(Sender: TObject);
begin
  MakeBookList;
  LoadChapter;
end;

procedure TMainForm.ToolButtonFBClick(Sender: TObject);
begin
  OpenURL('http://facebook.com/unbound.bible');
  FBPageVisited := True;
end;

procedure TMainForm.MakeBookList;
var
  List: TStringList;
  l : boolean;
begin
  l := BookBox.ItemIndex < 0;

  BookBox.Items.BeginUpdate;
  BookBox.Items.Clear;
  BookBox.Font.Assign(DefaultFont);

  List := TStringList.Create;
  Bible.GetTitles(List);
  BookBox.BiDiMode := bdLeftToRight;
  if Bible.RightToLeft then BookBox.BiDiMode := bdRightToLeft;
  BookBox.Items.Assign(List);
  List.Free;

  if l and (BookBox.Count > 0) then BookBox.ItemIndex := 0;
  BookBox.Items.EndUpdate;
end;

//-----------------------------------------------------------------------------------------

procedure TMainForm.MakeChapterList(n: integer);
var
  i: integer;
begin
  ChapterBox.Font.Assign(DefaultFont);
  if ChapterBox.Items.Count = n then Exit;

  ChapterBox.Items.BeginUpdate;
  ChapterBox.Items.Clear;

  for i := 1 to n do ChapterBox.Items.Add(ToStr(i));

  ChapterBox.ItemIndex := 0;
  ChapterBox.Items.EndUpdate;
end;

//----------------------------------------------------------------------------------------
//                                       tools
//----------------------------------------------------------------------------------------

procedure TMainForm.LoadChapter;
begin
  if Shelf.Count = 0 then Exit;
  MemoBible.LoadRichText(Load_Chapter());
  MakeChapterList(Bible.ChaptersCount(ActiveVerse));
  if FormTranslate.Visible then LoadTranslate(ActiveVerse);
  SelectPage(apBible);
end;

procedure TMainForm.SearchText(s: string);
var count : integer = 0;
{$ifdef linux} const max = 200; {$endif}
begin
  if Shelf.Count = 0 then Exit;
  {$ifdef linux} if count > max then Show_Message('\i\par  ' + ms_Message); {$endif}
  Cursor := crHourGlass;
  MemoSearch.LoadRichText(Search_Text(s, count));
  Cursor := crArrow;
  SelectPage(apSearch);
  UpdateStatus(ToStr(count) + ' ' + ms_found,'');
end;

procedure TMainForm.LoadCompare;
begin
  if Shelf.Count = 0 then Exit;
  SelectPage(apCompare);
  MemoCompare.LoadRichText(Load_Compare());
end;

procedure TMainForm.LoadTranslate(Verse: TVerse);
begin
  if Shelf.Count = 0 then Exit;
  FormTranslate.Memo.LoadRichText(Load_Translate(Verse));
  FormTranslate.Repaint;
end;

procedure TMainForm.LoadCommentary;
begin
  SelectPage(apCommentary);
  MemoCommentary.LoadRichText(Load_Commentary());
end;

procedure TMainForm.LoadStrong(s: string);
begin
  NotifyForm.Memo.LoadRichText(Load_Strong(s));
  NotifyForm.Title.Caption := ' Словарь Стронга'; // ms_Strong;
  NotifyForm.ShowAtPos(Mouse.CursorPos);
end;

procedure TMainForm.LoadFootnote(s: string);
begin
  NotifyForm.Memo.LoadRichText(Load_Footnote(s));
  NotifyForm.Title.Caption := ms_Footnote;
  NotifyForm.ShowAtPos(Mouse.CursorPos);
end;

{$ifdef windows}
procedure TMainForm.VersesToClipboard;
begin
  StringToClipboard(Load_Verses());
end;
{$endif}

{$ifdef unix}
procedure TMainForm.VersesToClipboard;
var
  MemoPreview : TUnboundMemo;
begin
  MemoPreview := TUnboundMemo.Create(self);

  with MemoPreview do
    begin
      Parent := MainForm;
      Left   := 0;
      Top    := 0;
      Height := 100;
      Width  := 100;
    end;

  {$ifdef linux} MemoBible.SaveSelection; {$endif}
  // saving selection because of strange bug in the gtk2's richmemo

  MemoPreview.LoadRichText(Load_Verses());
  MemoPreview.SelectAll;
  MemoPreview.CopyToClipboard;

  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
  MemoPreview.Free;
end;
{$endif}

//-------------------------------------------------------------------------------------------------

procedure TMainForm.SaveIniFile;
var
  IniFile: TIniFile;
  i: integer;
begin
  if Shelf.Count = 0 then Exit;

  IniFile := TIniFile.Create(ConfigFile);

  if WindowState = wsNormal then
  begin
    IniFile.WriteInteger('Application', 'Left',   Left);
    IniFile.WriteInteger('Application', 'Top',    Top);
    IniFile.WriteInteger('Application', 'Width',  Width);
    IniFile.WriteInteger('Application', 'Height', Height);
  end;

  if WindowState = wsMaximized then IniFile.WriteString('Application', 'State', 'Maximized')
                               else IniFile.WriteString('Application', 'State', 'Normal');

  IniFile.WriteString('Application', 'FileName', Bible.FileName);
  IniFile.WriteString('Application', 'FontName', DefaultFont.Name);
  IniFile.WriteInteger('Application', 'FontSize', DefaultFont.Size);
  IniFile.WriteInteger('Application', 'Splitter', PanelLeft.Width);
  IniFile.WriteString('Application', 'Interface', FaceLang);
  IniFile.WriteBool('Application', 'ShortLink', ShortLink);
  IniFile.WriteBool('Application', 'FBPage', FBPageVisited);
  IniFile.WriteBool('Options', 'Abbreviate', Options.cvAbbreviate);
  IniFile.WriteBool('Options', 'Enumerated', Options.cvEnumerated);
  IniFile.WriteBool('Options', 'Guillemets', Options.cvGuillemets);
  IniFile.WriteBool('Options', 'Parentheses', Options.cvParentheses);
  IniFile.WriteBool('Options', 'End', Options.cvEnd);
  IniFile.WriteInteger('Verse', 'Book', ActiveVerse.book);
  IniFile.WriteInteger('Verse', 'Chapter', ActiveVerse.chapter);
  IniFile.WriteInteger('Verse', 'Number', ActiveVerse.number);
  IniFile.WriteInteger('Verse', 'Count', ActiveVerse.count);
  IniFile.WriteInteger('Recent', 'Count', RecentList.Count);

  for i := 0 to RecentList.Count - 1 do
    IniFile.WriteString('Recent', 'File_' + ToStr(i), RecentList[i]);

  IniFile.Free;
end;

function GetDefaultBible: string;
begin
  Result := 'kjv.unbound';
  if GetDefaultLanguage = 'russian'   then Result := 'rstw.unbound';
  if GetDefaultLanguage = 'ukrainian' then Result := 'ubio.unbound';
end;

procedure TMainForm.ReadIniFile;
var
  IniFile: TIniFile;
  BibleFile: string;
  i, max: integer;
begin
  IniFile := TIniFile.Create(ConfigFile);
  {
  if IniFile.ReadString('Application', 'State', 'Normal') = 'Maximized'  // not working
    then WindowState := wsMaximized else WindowState := wsNormal;
  }
  BibleFile := IniFile.ReadString('Application', 'FileName', GetDefaultBible);

  Height := IniFile.ReadInteger('Application', 'Height', Screen.Height - 200);
  Width := IniFile.ReadInteger('Application', 'Width', Screen.Width - 400);
  Left := IniFile.ReadInteger('Application', 'Left', 200);
  Top := IniFile.ReadInteger('Application', 'Top', 70);

  DefaultFont.Name := IniFile.ReadString('Application', 'FontName', DefaultFont.Name);
  DefaultFont.Size := IniFile.ReadInteger('Application', 'FontSize', DefaultFont.Size);
  PanelLeft.Width := IniFile.ReadInteger('Application', 'Splitter', 270);
  FaceLang := IniFile.ReadString('Application', 'Interface', GetDefaultLanguage);
  ShortLink := IniFile.ReadBool('Application', 'ShortLink', True);
  FBPageVisited := IniFile.ReadBool('Application', 'FBPage', False);
  Options.cvAbbreviate := IniFile.ReadBool('Options', 'Abbreviate', False);
  Options.cvEnumerated := IniFile.ReadBool('Options', 'Enumerated', False);
  Options.cvGuillemets := IniFile.ReadBool('Options', 'Guillemets', False);
  Options.cvParentheses := IniFile.ReadBool('Options', 'Parentheses', False);
  Options.cvEnd := IniFile.ReadBool('Options', 'End', False);
  ActiveVerse.book := IniFile.ReadInteger('Verse', 'Book', 0);
  ActiveVerse.chapter := IniFile.ReadInteger('Verse', 'Chapter', 0);
  ActiveVerse.number := IniFile.ReadInteger('Verse', 'Number', 0);
  ActiveVerse.count := IniFile.ReadInteger('Verse', 'Count', 0);
  Max := IniFile.ReadInteger('Recent', 'Count', RecentList.Count);

  for i := 0 to Max - 1 do
    RecentList.Add(IniFile.ReadString('Recent', 'File_' + ToStr(i), ''));

  Shelf.SetCurrent(BibleFile);

  IniFile.Free;
end;

end.

