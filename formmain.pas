unit FormMain;

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, ComCtrls, IniFiles, LCLIntf, LCLType, LCLProc, ActnList, ClipBrd,
  StdActns, PrintersDlgs, Types, RichMemo, UnboundMemo, UnitData, UmLib, UnitLib;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionDecrease: TAction;
    ActionIncrease: TAction;
    ActionModules: TAction;
    ActionCommentary: TAction;
    ActionInterline: TAction;
    IdleTimer: TIdleTimer;
    miModules: TMenuItem;
    MenuItem4: TMenuItem;
    PrintDialog: TPrintDialog;
    FontDialog: TFontDialog;
    FontDialogNotes: TFontDialog;
    pmSearch: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolButtonCommentary: TToolButton;
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
    ActionTranslate: TAction;
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
    miCommentary: TMenuItem;
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
    pmCommentary: TMenuItem;
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

    procedure CmdCommentary(Sender: TObject);
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
    procedure CmdModules(Sender: TObject);

    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure BookBoxClick(Sender: TObject);
    procedure ChapterBoxClick(Sender: TObject);
    procedure MemoMouseLeave(Sender: TObject);
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
    ms_Commentary, ms_Confirm, ms_Strong : string;
    ms_Footnote, ms_Found, ms_Message, ms_Overwrite, ms_Save : string;
    DefaultCurrent: string;
    NoteFileName: string;
    RecentList: TStringList;
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
    procedure LoadStrong(s: string);
    procedure LoadFootnote(s: string);
    procedure MakeBookList;
    procedure MakeChapterList(n: integer);
    procedure OnRecentClick(Sender: TObject);
    procedure OnLangClick(Sender: TObject);
    procedure PerformFileOpen(const FileName: string);
    procedure ReadConfig;
    procedure RebuildRecentList;
    procedure RecentMenuInit;
    procedure SaveConfig;
    procedure SearchText(s: string);
    procedure SelectPage(page: integer);
    procedure UpdateCaption(s: string);
    procedure UpdateStatus(s, Hint: string);
    procedure UpdateActionImage;
    procedure VersesToClipboard;
    procedure ShowPopup;
  public
    procedure Translate;
  end;

var
  MainForm: TMainForm;

implementation

uses
  {$ifdef windows} UmParseWin, {$endif}
  FormAbout, FormNotify, FormSearch, FormCompare, UnitTool, UnitLang,
  UnitShelf, FormCopy, FormTranslate, FormCommentary, FormDownload;

const
  apBible   = 0; // active page
  apSearch  = 1;
  apCompare = 2;
  apNotes   = 3;

const
  Untitled = 'Untitled';
  RecentMax = 10;

{$R *.lfm}

//=================================================================================================
//                                     Create Main Form
//=================================================================================================

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := ApplicationName + ' ' + ApplicationVersion
    {$ifdef windows} + ' - Open Source Application' {$endif};

  RecentList := TStringList.Create;
  SaveDialog.InitialDir := DocumentsPath;
  NoteFileName := Untitled;
  ReadConfig;

  if Shelf.Count > 0 then
  begin
    Shelf.SetCurrent(DefaultCurrent);
    ComboBoxInit;
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
    ActionCopyAs .Enabled := False;
  end;

  LangMenuInit;
  RecentMenuInit;

  NoteFileName := Untitled;
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

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  RecentList.Free;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if ChapterBox.Items.Count = 0 then GoToVerse(ActiveVerse,(ActiveVerse.number > 1)); // first time
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$ifdef linux}
    MemoBible  .Clear;
    MemoSearch .Clear;
    MemoCompare.Clear;
    MemoNotes  .Clear;
  {$endif}
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

  ChapterBox.Width := WidthInPixels(DefaultFont,'150') + 30;
  BookBox.Width := PanelLeft.Width - BookBox.Left - BookBox.Left - ChapterBox.Width - Streak;
  ChapterBox.Left := PanelLeft.Width - ChapterBox.Width - Streak;
end;

procedure TMainForm.MemoMouseLeave(Sender: TObject);
var
  Point : TPoint;
begin
  if NotifyForm.Visible then
    begin
      Point := UnboundMemo.ScreenToClient(Mouse.CursorPos);
      if (Point.x <= 0) or (Point.x >= UnboundMemo.Width  - 30) or
         (Point.y <= 0) or (Point.y >= UnboundMemo.Height - 30) then NotifyForm.Close;
    end;
end;

procedure TMainForm.Translate;
begin
  miTools.Caption := T('Tools');
  miEdit.Caption := T('Edit');
  miNotes.Caption := T('Notes');
  miHelp.Caption := T('Help');
  miSearch.Caption := T('Search');
  miCompare.Caption := T('Compare');
  miCommentary.Caption := T('Commentaries');
  miTranslate.Caption := T('Translation');
  miInterlinear.Caption := T('Interlinear');
  miPrint.Caption := T('Print');
  miModules.Caption := T('Modules');
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
  miDownload.Caption := T('Module Downloads');
  miBibleFolder.Caption := T('Bible Folder');
  miHelpAbout.Caption := T('About');

  pmCommentary.Caption := T('Commentaries');
  pmCut.Caption := T('Cut');
  pmCopy.Caption := T('Copy');
  pmPaste.Caption := T('Paste');
  pmCopyAs.Caption := T('Copy As…');
  pmVerses.Caption := T('Copy Verses');
  pmInterlinear.Caption := T('Interlinear') + ' (biblehub.com)';

  TabSheetBible.Caption := T('Bible');
  TabSheetSearch.Caption := T('Search');
  TabSheetCompare.Caption := T('Compare');
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
  ToolButtonCompare.Hint := T('Compare');
  ToolButtonCommentary.Hint := T('Commentaries');

  ToolButtonFont.Hint := T('Font');
  ToolButtonBold.Hint := T('Bold');
  ToolButtonItalic.Hint := T('Italic');
  ToolButtonUnderline.Hint := T('Underline');
  ToolButtonLink.Hint := T('Hyperlink');
  ToolButtonLeft.Hint := T('Align Left');
  ToolButtonCenter.Hint := T('Center');
  ToolButtonRight.Hint := T('Align Right');
  ToolButtonBullets.Hint := T('Bullets');

  ms_Commentary := T('Commentaries');
  ms_Confirm := T('Confirmation');
  ms_Strong := T('Strong''s Dictionary');
  ms_Footnote := T('Footnote');
  ms_Found := T('verses found');
  ms_Overwrite := T('OK to overwrite %s?');
  ms_Save := T('Save changes?');
  ms_Message := T('This search returned too many results.') + ' ' +
                T('Please narrow your search.');
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

  if Sender = ActionIncrease then fp.Size += 1;
  if Sender = ActionDecrease then fp.Size -= 1;

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
  if Shelf.Count = 0 then Exit;
  if Sender <> PageControl then
    if CompareForm.ShowModal <> mrOk then Exit;
  MemoCompare.Font.Assign(DefaultFont);
  MemoCompare.LoadText(Load_Compare);
  if Sender <> PageControl then SelectPage(apCompare);
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
  CopyForm.ShowModal;
  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
end;

procedure TMainForm.CmdCopyVerses(Sender: TObject);
begin
  // saving selection because of strange bug in the gtk2's richmemo
  {$ifdef linux} MemoBible.SaveSelection; {$endif}
  VersesToClipboard;
  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
end;

procedure TMainForm.CmdSearch(Sender: TObject);
begin
  SearchForm.Edit.Font.Name := DefaultFont.Name;
  if SearchForm.ShowModal = mrOk then SearchText(SearchForm.Edit.Text);
end;

procedure TMainForm.CmdTrans(Sender: TObject);
begin
  if Shelf.Count = 0 then Exit;
  TranslateForm.Memo.Font.Assign(DefaultFont);
  TranslateForm.Memo.LoadText(Load_Translate);
  TranslateForm.Repaint;
  TranslateForm.Show;
end;

procedure TMainForm.CmdCommentary(Sender: TObject);
begin
  if Shelf.Count = 0 then Exit;
  CommentaryForm.Caption := ms_Commentary + ' - ' + Bible.VerseToStr(ActiveVerse, true);
  CommentaryForm.Memo.LoadHTML(Load_Commentary);
  if Sender = ActionCommentary then CommentaryForm.Show;
  CommentaryForm.Repaint;
end;

procedure TMainForm.CmdFileNew(Sender: TObject);
begin
  SelectPage(apNotes);
  if not CheckFileSave then Exit;
  NoteFileName := Untitled;
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
  if NoteFileName = Untitled then
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

  if NoteFileName = Untitled then SaveDialog.InitialDir := DocumentsPath
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

procedure TMainForm.CmdModules(Sender: TObject);
begin
  if Shelf.Count = 0 then Exit;
  DownloadForm.Memo.LoadText(Load_ModulesInfo);
  DownloadForm.ShowModal;
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

  if Memo = MemoBible then
    begin
      ActiveVerse.Number := MemoBible.ParagraphStart;
      ActiveVerse.Count  := MemoBible.ParagraphCount;
      if TranslateForm.Visible then CmdTrans(Sender);
      if CommentaryForm.Visible then CmdCommentary(Sender);
    end;

  if Memo.hyperlink = '' then Exit;

  if Memo.Foreground = fgLink then
    begin
      Verse := Bible.SrtToVerse(Memo.hyperlink);

      if Verse.Book > 0 then
        begin
          if TranslateForm.Visible then CmdTrans(Sender);
          if CommentaryForm.Visible then CmdCommentary(Sender);
          if (Memo = MemoSearch) or (not TranslateForm.Visible) or (ssCtrl in Shift) then
            GoToVerse(Verse, True);
        end;
    end;

  if Memo = MemoBible then
    if Memo.Foreground = fgFootnote then LoadFootnote(Memo.hyperlink);

  if Memo <> MemoNotes then
    if Memo.Foreground = fgStrong then LoadStrong(Memo.hyperlink);
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
  Caption := ApplicationName + ' ' + ApplicationVersion + ' - ' + s;
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

procedure TMainForm.OnRecentClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to miRecent.Count - 1 do
    if (Sender as TMenuItem).tag = i then
      if CheckFileSave then
        PerformFileOpen(RecentList[i]);
end;

procedure TMainForm.OnLangClick(Sender: TObject);
var
  i: integer;
begin
  InterfaceLang := (Sender as TMenuItem).Hint;

  for i := 0 to miLocalization.Count - 1 do
    miLocalization.Items[i].Checked := False;
  (Sender as TMenuItem).Checked := True;

  TranslateAll;
end;

procedure TMainForm.LangMenuInit;
var
  Local : TLocal;
  MenuItem : TMenuItem;
begin
  for Local in Localization do
  begin
    MenuItem := TMenuItem.Create(MainMenu);

    MenuItem.Caption := Local.language;
    MenuItem.Hint := Local.id;
    MenuItem.Checked := Local.id = InterfaceLang;
    MenuItem.OnClick := OnLangClick;

    miLocalization.Add(MenuItem);
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
  CreateDataDirectory;
  OpenFolder(DataPath);
end;

procedure TMainForm.miHomeClick(Sender: TObject);
begin
  if (InterfaceLang = 'ru') or (InterfaceLang = 'uk')
    then OpenURL('http://vladimirrybant.org/ru')
    else OpenURL('http://vladimirrybant.org');
end;

procedure TMainForm.miDownloadClick(Sender: TObject);
begin
  if (InterfaceLang = 'ru') or (InterfaceLang = 'uk')
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
  if PageControl.ActivePageIndex = apCompare then CmdCompare(PageControl);
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
  MemoBible.Font.Assign(DefaultFont);
  MemoBible.LoadText(Load_Chapter, true);
  MakeChapterList(Bible.ChaptersCount(ActiveVerse));
  if TranslateForm.Visible then CmdTrans(nil);
  if CommentaryForm.Visible then CmdCommentary(nil);
  SelectPage(apBible);
end;

procedure TMainForm.SearchText(s: string);
var
  richtext : string;
  count : integer = 0;
{$ifdef linux} const max = 2000; {$endif}
begin
  if Shelf.Count = 0 then Exit;
  Cursor := crHourGlass;
  richtext := Search_Text(s, count);
  {$ifdef linux} if count > max then richtext := Show_Message(ms_Message); {$endif}
  MemoSearch.Font.Assign(DefaultFont);
  MemoSearch.LoadText(richtext);
  Cursor := crArrow;
  SelectPage(apSearch);
  UpdateStatus(ToStr(count) + ' ' + ms_found,'');
end;

procedure TMainForm.LoadStrong(s: string);
var text : string;
begin
  text := Load_Strong(s);
  if text = '' then Exit;
  NotifyForm.Title.Caption := ms_Strong;
  NotifyForm.Compact := True;
  NotifyForm.Memo.LoadText(text);
  NotifyForm.ShowAtPos(Mouse.CursorPos);
  Self.SetFocus;
end;

procedure TMainForm.LoadFootnote(s: string);
var text : string;
begin
  text := Load_Footnote(s);
  if text = '' then Exit;
  NotifyForm.Title.Caption := ms_Footnote;
  NotifyForm.Compact := False;
  NotifyForm.Memo.LoadText(text);
  NotifyForm.ShowAtPos(Mouse.CursorPos);
  Self.SetFocus;
end;

{$ifdef windows}
procedure TMainForm.VersesToClipboard;
begin
  StringToClipboard(ParseWin(Load_Verses(), DefaultFont));
end;
{$endif}

{$ifdef linux}
procedure TMainForm.VersesToClipboard;
var
  MemoPreview : TUnboundMemo;
begin
  MemoPreview := TUnboundMemo.Create(self);
  MemoPreview.Parent := MainForm;
  MemoPreview.Font.Assign(DefaultFont);
  MemoPreview.LoadText(Load_Verses);
  MemoPreview.SelectAll;
  MemoPreview.CopyToClipboard;
  MemoPreview.Visible := false;
  MemoPreview.FreeOnRelease;
end;
{$endif}

{$ifdef darwin}
procedure TMainForm.VersesToClipboard;
begin
  //
end;
{$endif}

//-------------------------------------------------------------------------------------------------

procedure TMainForm.SaveConfig;
var
  IniFile: TIniFile;
  i: integer;
begin
  if Shelf.Count = 0 then Exit;
  IniFile := TIniFile.Create(ConfigFile);

  if WindowState = wsNormal then
  begin
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
  end;

  if WindowState = wsMaximized then IniFile.WriteString('Window', 'State', 'Maximized')
                               else IniFile.WriteString('Window', 'State', 'Normal');

  IniFile.WriteInteger('Window', 'Splitter', PanelLeft.Width);
  IniFile.WriteString('Application', 'FileName', Bible.FileName);
  IniFile.WriteString('Application', 'Interface', InterfaceLang);
  IniFile.WriteBool('Application', 'FBPage', FBPageVisited);
  IniFile.WriteBool('Options', 'Abbreviate', Options.cvAbbreviate);
  IniFile.WriteBool('Options', 'Enumerated', Options.cvEnumerated);
  IniFile.WriteBool('Options', 'Guillemets', Options.cvGuillemets);
  IniFile.WriteBool('Options', 'Parentheses', Options.cvParentheses);
  IniFile.WriteBool('Options', 'End', Options.cvEnd);
  IniFile.WriteInteger('Recent', 'Count', RecentList.Count);

  for i := 0 to RecentList.Count - 1 do
    IniFile.WriteString('Recent', 'File_' + ToStr(i), RecentList[i]);

  IniFile.Free;
end;

function GetDefaultBible: string;
begin
  Result := 'kjv.unbound';
  if GetLanguageID = 'ru' then Result := 'rstw.unbound';
  if GetLanguageID = 'uk' then Result := 'ubio.unbound';
end;

procedure TMainForm.ReadConfig;
var
  IniFile: TIniFile;
  i, max: integer;
begin
  IniFile := TIniFile.Create(ConfigFile);

  DefaultCurrent := IniFile.ReadString('Application', 'FileName', GetDefaultBible);

  Height := IniFile.ReadInteger('Window', 'Height', Screen.Height - 220);
  Width := IniFile.ReadInteger('Window', 'Width', Screen.Width - 450);
  Left := IniFile.ReadInteger('Window', 'Left', 200);
  Top := IniFile.ReadInteger('Window', 'Top', 80);

  PanelLeft.Width := IniFile.ReadInteger('Window', 'Splitter', 270);
  InterfaceLang := IniFile.ReadString('Application', 'Interface', GetDefaultLanguage);
  FBPageVisited := IniFile.ReadBool('Application', 'FBPage', False);
  Options.cvAbbreviate := IniFile.ReadBool('Options', 'Abbreviate', False);
  Options.cvEnumerated := IniFile.ReadBool('Options', 'Enumerated', False);
  Options.cvGuillemets := IniFile.ReadBool('Options', 'Guillemets', False);
  Options.cvParentheses := IniFile.ReadBool('Options', 'Parentheses', False);
  Options.cvEnd := IniFile.ReadBool('Options', 'End', False);
  Max := IniFile.ReadInteger('Recent', 'Count', RecentList.Count);

  for i := 0 to Max - 1 do
    RecentList.Add(IniFile.ReadString('Recent', 'File_' + ToStr(i), ''));

  IniFile.Free;
end;

end.

