unit FormMain;

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ComCtrls, IniFiles, LCLIntf, LCLType, LCLProc, ActnList,
  ClipBrd, StdActns, Buttons, PrintersDlgs, Types, RichMemo, UnboundMemo,
  UnitData, UmLib, UnitLib;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionXref: TAction;
    Edit: TEdit;
    IdleTimer: TIdleTimer;
    MenuItem1: TMenuItem;
    miRref: TMenuItem;
    miDictionary: TMenuItem;
    N7: TMenuItem;
    pmXref: TMenuItem;
    pmCommentary: TMenuItem;
    PrintDialog: TPrintDialog;
    FontDialog: TFontDialog;
    FontDialogNotes: TFontDialog;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ComboBox: TComboBox;
    FileOpen1: TFileOpen;
    EditCut1: TEditCut;

    MemoBible: TUnboundMemo;
    MemoSearch: TUnboundMemo;
    MemoCompare: TUnboundMemo;
    MemoXref: TUnboundMemo;
    MemoCommentary: TUnboundMemo;
    MemoDictionary: TUnboundMemo;
    MemoNotes: TUnboundMemo;

    ActionList: TActionList;
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
    ActionUnderline: TAction;
    ActionDictionary: TAction;
    ActionDecrease: TAction;
    ActionIncrease: TAction;
    ActionModules: TAction;
    ActionCommentary: TAction;
    ActionInterline: TAction;

    ChapterBox: TListBox;
    BookBox: TListBox;
    Ruler: TPanel;
    PanelLeft: TPanel;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    Images: TImageList;

    PageControl: TPageControl;
    TabSheetXref: TTabSheet;
    TabSheetBible: TTabSheet;
    TabSheetSearch: TTabSheet;
    TabSheetCompare: TTabSheet;
    TabSheetCommentary: TTabSheet;
    TabSheetDictionary: TTabSheet;
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
    miModules: TMenuItem;
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
    pmSearch: TMenuItem;
    pmCut: TMenuItem;
    pmCopy: TMenuItem;
    pmPaste: TMenuItem;
    pmCopyAs: TMenuItem;
    pmVerses: TMenuItem;
    pmInterlinear: TMenuItem;
    pmDictionary: TMenuItem;
    pmSeparator1: TMenuItem;
    pmSeparator2: TMenuItem;

    StandardToolBar: TToolBar;
    ToolPanel: TPanel;
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
    ToolButtonCopyright: TToolButton;
    ToolButtonDictionary: TToolButton;
    ToolButtonCommentary: TToolButton;
    ToolSeparatorCompare: TToolButton;
    ToolSeparatorEdit: TToolButton;
    ToolSeparatorFont: TToolButton;
    ToolSeparatorAlign: TToolButton;
    ToolSeparatorBullets: TToolButton;
    ToolButtonVerses: TToolButton;

    procedure CmdXref(Sender: TObject);
    procedure CmdCommentary(Sender: TObject);
    procedure CmdDictionary(Sender: TObject);
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
    procedure CmdModules(Sender: TObject);

    procedure ComboBoxChange(Sender: TObject);
    procedure ComboBoxDrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure MemoSelectionChange(Sender: TObject);
    procedure miBibleFolderClick(Sender: TObject);
    procedure miDownloadClick(Sender: TObject);
    procedure miHomeClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure RadioButtonClick(Sender: TObject);
    procedure ToolButtonFBClick(Sender: TObject);
    procedure ToolButtonSearchClick(Sender: TObject);
  private
    DefaultCurrent: string;
    NoteFileName: string;
    RecentList: TStringList;
    FBPageVisited: boolean;
    {$ifdef linux} IdleMessage : string; {$endif}
    function UnboundMemo: TUnboundMemo;
    function CheckFileSave: boolean;
    procedure ComboBoxInit;
    procedure EnableActions;
    procedure UpDownButtons;
    procedure SelectBook(title: string; scroll: boolean);
    procedure GoToVerse(Verse: TVerse; select: boolean);
    procedure LangMenuInit;
    procedure LoadChapter;
    procedure LoadSearch(s: string);
    procedure LoadXref;
    procedure LoadCommentary;
    procedure LoadDictionary(s: string);
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
    procedure SelectPage(page: integer);
    procedure UpdateCaption(s: string);
    procedure UpdateStatus(s, Hint: string);
    procedure UpdateActionImage;
    procedure VersesToClipboard;
    procedure ShowPopup;
  public
    procedure Localize;
  end;

var
  MainForm: TMainForm;

implementation

uses
  {$ifdef windows} UmParseWin, {$endif}
  FormAbout, FormNotify, FormSearch, FormCompare, UnitTool, UnitLang, UnitShelf, FormCopy,
  FormDownload;

const
  apBible      = 0; // active page
  apSearch     = 1;
  apCompare    = 2;
  apXref       = 3;
  apCommentary = 4;
  apDictionary = 5;
  apNotes      = 6;

{$R *.lfm}

//=================================================================================================
//                                     Create Main Form
//=================================================================================================

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := ApplicationName + ' ' + ApplicationVersion;

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
  ActionEditUndo.Visible := False;
  ActionBullets.Visible := False;
  ToolSeparatorBullets.Visible := False;
  IdleMessage := '';
  IdleTimer.Enabled := true;
  {$endif}

  TabSheetSearch    .TabVisible := False;
  TabSheetXref      .TabVisible := False;
  TabSheetCommentary.TabVisible := False;
  TabSheetDictionary.TabVisible := False;

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
    MemoBible     .Clear;
    MemoSearch    .Clear;
    MemoCompare   .Clear;
    MemoXref      .Clear;
    MemoCommentary.Clear;
    MemoDictionary.Clear;
    MemoNotes     .Clear;
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


  ToolPanel.Width := StandardToolBar.Width - ToolButtonRight.Width - ToolButtonSearch.Width
                   - {$ifdef linux} ToolButtonRight.Left {$else} ToolButtonBullets.Left {$endif};

  Edit.Left := StandardToolBar.Width - ToolButtonSearch.Width - Edit.Width - 4;
  Edit.Visible := ToolPanel.Width > Edit.Width;
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

procedure TMainForm.Localize;
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

  pmSearch.Caption := T('Search');
  pmDictionary.Caption := T('Lookup');
  pmCut.Caption := T('Cut');
  pmCopy.Caption := T('Copy');
  pmPaste.Caption := T('Paste');
  pmCopyAs.Caption := T('Copy As…');
  pmVerses.Caption := T('Copy Verses');
  pmInterlinear.Caption := T('Interlinear') + ' (biblehub.com)';

  TabSheetBible.Caption := T('Bible');
  TabSheetSearch.Caption := T('Search');
  TabSheetCompare.Caption := T('Compare');
  TabSheetXref.Caption := 'Cсылки'; // 'Параллельные места'; //
  TabSheetCommentary.Caption := T('Commentary');
  TabSheetDictionary.Caption := T('Dictionary');
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
begin
  fp := MemoNotes.SelAttributes;
  MemoNotes.SaveSelection;
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

  MemoNotes.SetTextAttributes(MemoNotes.SelStart, MemoNotes.SelLength, fp);
  MemoNotes.RestoreSelection; // unselect word
  MemoNotes.Repaint;
end;

procedure TMainForm.CmdStyle2(Sender: TObject);
{$ifdef windows} var pn : TParaNumbering; {$endif}
begin
  with MemoNotes do
    begin
      if Sender = ActionLeft    then SetParaAlignment(SelStart, SelLength, paLeft   );
      if Sender = ActionCenter  then SetParaAlignment(SelStart, SelLength, paCenter );
      if Sender = ActionRight   then SetParaAlignment(SelStart, SelLength, paRight  );

      {$ifdef windows}
      if Sender = ActionBullets then
        begin
          pn := SelParaNumbering;
          if ToolButtonBullets.Down then pn.Style := pnBullet else pn.Style := pnNone;
          SetParaNumbering(SelStart, SelLength, pn);
        end;
      {$endif}
    end;

  MemoNotes.Repaint;
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

procedure TMainForm.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if PageControl.ActivePageIndex = apDictionary then LoadDictionary(Edit.Text)
      else LoadSearch(Edit.Text);
end;

procedure TMainForm.CmdCompare(Sender: TObject);
begin
  if Shelf.Count = 0 then Exit;
  if Sender <> PageControl then
    if CompareForm.ShowModal <> mrOk then Exit;
  MemoCompare.Font.Assign(DefaultFont);
  MemoCompare.LoadText(Load_Compare);
  SelectPage(apCompare);
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

  EnableActions;
end;

procedure TMainForm.CmdCopyAs(Sender: TObject);
begin
  // saving selection because of strange bug in the gtk2's richmemo
  {$ifdef linux} MemoBible.SaveSelection; {$endif}
  CopyForm.ShowModal;
  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
end;

procedure TMainForm.CmdCopyVerses(Sender: TObject);
begin
  {$ifdef linux} MemoBible.SaveSelection; {$endif}
  VersesToClipboard;
  {$ifdef linux} MemoBible.RestoreSelection; {$endif}
end;

procedure TMainForm.CmdSearch(Sender: TObject);
begin
  Edit.Text := Trim(UnboundMemo.SelText);
  if Edit.Text = '' then Edit.SetFocus else LoadSearch(Edit.Text);
end;

procedure TMainForm.CmdXref(Sender: TObject);
begin
  LoadXref;
end;

procedure TMainForm.CmdCommentary(Sender: TObject);
begin
  LoadCommentary;
end;

procedure TMainForm.CmdDictionary(Sender: TObject);
begin
  Edit.Text := Trim(UnboundMemo.SelText);
  LoadDictionary(Edit.Text);
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
      if MessageDlg(Format(ls.Overwrite, [SaveDialog.FileName]),
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
//                                         Events
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
    end;

  if Memo.hyperlink = '' then Exit;

  if Memo.Foreground = fgLink then
    begin
      Verse := Bible.SrtToVerse(Memo.hyperlink);
      if Verse.Book > 0 then GoToVerse(Verse, True);
    end;

  if Memo = MemoBible then
    if Memo.Foreground = fgFootnote then LoadFootnote(Memo.hyperlink);

  if Memo <> MemoNotes then
    if Memo.Foreground = fgStrong then LoadStrong(Memo.hyperlink);
end;

procedure TMainForm.MemoSelectionChange(Sender: TObject);
begin
  EnableActions;
  if Sender = MemoNotes then UpDownButtons;
end;

procedure TMainForm.MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True; // disable system popup menu
end;

//-------------------------------------------------------------------------------------------------

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
    Response := MessageBox(Handle, PChar(ls.Save), PChar(ls.Confirm), MB_YESNOCANCEL or MB_ICONQUESTION);
  {$else}
    Response := MessageDlg(ls.Save, mtConfirmation, mbYesNoCancel, 0);
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

  LocalizeApplication;
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
    apXref       : Result := MemoXref;
    apCommentary : Result := MemoCommentary;
    apDictionary : Result := MemoDictionary;
    apNotes      : Result := MemoNotes;
    else
      Result := nil;
  end;
end;

procedure TMainForm.EnableActions;
var
  B, L : boolean;
  x : integer;
begin
  B := PageControl.ActivePageIndex = apBible;
  L := PageControl.ActivePageIndex = apNotes;

  if B then x := 1 else x:= 0;

  ActionEditCopy.Enabled   := UnboundMemo.SelLength > x;
  ActionEditCut.Enabled    := L and (UnboundMemo.SelLength > 0);
  ActionEditDel.Enabled    := L and (UnboundMemo.SelLength > 0);
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

  ToolButtonSearch.Enabled := PageControl.ActivePageIndex <> apDictionary;
  pmSearch.Enabled := UnboundMemo.SelLength > x;
  pmDictionary.Enabled := UnboundMemo.SelLength > x;

  UpdateActionImage;
end;

procedure TMainForm.UpDownButtons;
var
  fp : TFontParams;
begin
  if PageControl.ActivePageIndex <> apNotes then Exit;

  with MemoNotes do
    begin
      fp := SelAttributes;

      ToolButtonBold.Down := fsBold in fp.Style;
      ToolButtonItalic.Down := fsItalic in fp.Style;
      ToolButtonUnderline.Down := fsUnderline in fp.Style;
      ToolButtonLink.Down := clNavy = fp.Color;

      case SelParaAlignment of
        paLeft: ToolButtonLeft.Down := True;
        paRight: ToolButtonRight.Down := True;
        paCenter: ToolButtonCenter.Down := True;
      end;

      {$ifdef windows} ToolButtonBullets.Down := SelParaNumbering.Style = pnBullet; {$endif}
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
  if page = PageControl.ActivePageIndex then Exit;
  PageControl.ActivePageIndex := page;
  PageControl.ActivePage.TabVisible := true;
  EnableActions;
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
  EnableActions;
  UpDownButtons;
  UpdateStatus('','');
//UnboundMemo.SetFocus; //*******************************************************************************************************************
  UnboundMemo.Repaint;
  if PageControl.ActivePageIndex = apCompare    then CmdCompare(PageControl);
  if PageControl.ActivePageIndex = apXref       then CmdXref(PageControl);
  if PageControl.ActivePageIndex = apCommentary then CmdCommentary(PageControl);
//if PageControl.ActivePageIndex = apDictionary then CmdDictionary(PageControl);
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

procedure TMainForm.ToolButtonSearchClick(Sender: TObject);
var
  Pos: TPoint;
begin
  Pos.x := Width - (SearchForm.Width div 2);
  Pos.y := PageControl.Top + 2;
  Pos := ClientToScreen(Pos);
  if SearchForm.ShowAtPos(Pos) = mrOk then LoadSearch(Edit.Text);
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
//                                       Loads
//----------------------------------------------------------------------------------------

procedure TMainForm.LoadChapter;
begin
  if Shelf.Count = 0 then Exit;
  MemoBible.Font.Assign(DefaultFont);
  MemoBible.LoadText(Load_Chapter, true);
  MakeChapterList(Bible.ChaptersCount(ActiveVerse));
  SelectPage(apBible);
end;

procedure TMainForm.LoadSearch(s: string);
var
  richtext : string;
  count : integer = 0;
  {$ifdef linux} const max = 2000; {$endif}
begin
  s := Trim(s);
  if Utf8Length(s) < 2 then Exit;
  if Shelf.Count = 0 then Exit;

  Cursor := crHourGlass;
  richtext := Load_Search(s, count);
  {$ifdef linux} if count > max then richtext := Show_Message(ls.Narrow); {$endif}
  MemoSearch.Font.Assign(DefaultFont);
  MemoSearch.LoadText(richtext);

  Cursor := crArrow;
  SelectPage(apSearch);
  UpdateStatus(ToStr(count) + ' ' + ls.found,'');
end;

procedure TMainForm.LoadXref;
begin
  if Shelf.Count = 0 then Exit;
  MemoXref.Font.Assign(DefaultFont);
  MemoXref.LoadText(Load_Xref);
  SelectPage(apXref);
end;

procedure TMainForm.LoadCommentary;
begin
  if Shelf.Count = 0 then Exit;
  MemoCommentary.Font.Assign(DefaultFont);
  MemoCommentary.LoadHtml(Load_Commentary);
  SelectPage(apCommentary);
end;

procedure TMainForm.LoadDictionary(s: string);
begin
  if Shelf.Count = 0 then Exit;
  if Trim(s) <> '' then
    begin
      MemoDictionary.Font.Assign(DefaultFont);
      MemoDictionary.LoadHtml(Load_Dictionary(s));
    end;
  SelectPage(apDictionary);
end;

procedure TMainForm.LoadStrong(s: string);
var text : string;
begin
  text := Load_Strong(s);
  if text = '' then Exit;
  NotifyForm.Title.Caption := ls.Strong;
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
  NotifyForm.Title.Caption := ls.Footnote;
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

