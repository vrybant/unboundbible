unit RichMemoEx;

interface

uses
  {$ifdef windows} Windows, RichEdit, Printers, OSPrinters, {$endif}
  {$ifdef linux} Gtk2, Glib2, Gdk2, {$endif}
  Forms, SysUtils, Classes, Graphics, Controls, ExtCtrls, RichMemo, LazUTF8;

type

  TRichMemoEx = class(TRichMemo)
  private
    FOnAttrChange: TNotifyEvent;
    function  GetAttributes: TFontParams;
    procedure SetAttributes(const value: TFontParams);
    procedure DoAttributesChange;
    {$ifdef linux} function GetTextView: PGtkTextView; {$endif}
    {$ifdef windows} function  GetModified: boolean; {$endif}
    {$ifdef windows} procedure SetModified(value: boolean); {$endif}
  protected
    procedure SetSel(x1,x2: integer);
    procedure GetSel(var x1,x2: integer);
    {$ifdef windows} function LineCount: integer; {$endif}
    {$ifdef windows} function LineIndex(x: longint): integer; {$endif}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    {$ifdef darwin} Modified : boolean; {$endif}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanUndo: boolean;
    function CanPaste: boolean; override;
    procedure HideCursor;
    procedure Hide_Selection;
    procedure Show_Selection;
    function Selected: boolean;
    procedure SelectAll;
    property SelAttributes: TFontParams read GetAttributes write SetAttributes;
    {$ifdef windows} function FindRightWordBreak(Pos: integer): integer; {$endif}
    {$ifdef windows} function GetTextRange(Pos, Length: Integer): string; {$endif}
    {$ifdef windows} property Modified: boolean read GetModified write SetModified; {$endif}
    {$ifdef linux} procedure CopyToClipboard; override; {$endif}
    {$ifdef linux} procedure CutToClipboard; override; {$endif}
    {$ifdef linux} procedure PasteFromClipboard; override; {$endif}
    function LoadRichText(Source: TStream): Boolean; override;
    function LoadRichText(Source: string): Boolean;
    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);
  published
    property OnAttrChange: TNotifyEvent read FOnAttrChange write FOnAttrChange;
  end;

implementation

function ToStr(value: longint): string;
begin
 System.Str(value, Result);
end;

function Utf8ToRTF(const s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
const
  endchar = {$ifdef linux} ' ' {$else} '?' {$endif};
begin
  Result := '';
  p := PChar(s);
  repeat
    unicode := UTF8CharacterToUnicode(p,CharLen);
    if unicode = 0 then Continue;
    if unicode < $80 then Result := Result + char(unicode)
                     else Result := Result + '\u' + ToStr(unicode) + endchar;

    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

constructor TRichMemoEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef unix} Modified := False; {$endif}
end;

destructor TRichMemoEx.Destroy;
begin
  inherited Destroy;
end;

{$ifdef windows}

function TRichMemoEx.LineCount: integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT,0,0);
end;

function TRichMemoEx.LineIndex(x: longint): integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX,x,0);
end;

function TRichMemoEx.GetModified: boolean;
begin
  Result := SendMessage(Handle, EM_GETMODIFY,  0, 0) <> 0;
end;

procedure TRichMemoEx.SetModified(value: boolean);
begin
  SendMessage(Handle, EM_SETMODIFY, Byte(value), 0);
end;

function TRichMemoEx.FindRightWordBreak(Pos: integer): integer;
begin
  Result := SendMessage(Handle, EM_FINDWORDBREAK, WB_MOVEWORDRIGHT, Pos);
end;

function TRichMemoEx.GetTextRange(Pos, Length: Integer): string;
var
  TextRange : RichEdit.TEXTRANGEW;
  w : WideString;
  res : LResult;
begin
  FillChar(TextRange{%H-}, sizeof(TextRange), 0);
  TextRange.chrg.cpMin := Pos;
  TextRange.chrg.cpMax := Pos + Length;
  SetLength(w, Length);
  TextRange.lpstrText := @w[1];
  res := SendMessage(Handle, EM_GETTEXTRANGE, 0, {%H-}Longint(@TextRange));
  Result := UTF8Encode(Copy(w, 1, res));
end;

{$endif}

procedure TRichMemoEx.HideCursor;
begin
   {$ifdef windows} HideCaret(Handle); {$endif}
   {$ifdef linux} gtk_text_view_set_cursor_visible(GetTextView, False); {$endif}
end;

procedure TRichMemoEx.Hide_Selection;
begin
   {$ifdef windows} SendMessage(Handle, EM_HIDESELECTION, 1, 0); {$endif}
end;

procedure TRichMemoEx.Show_Selection;
begin
   {$ifdef windows} SendMessage(Handle, EM_HIDESELECTION, 0, 0); {$endif}
end;

procedure TRichMemoEx.SetAttributes(const value: TFontParams);
begin
  SetTextAttributes(SelStart, SelLength, value);
  DoAttributesChange;
end;

function TRichMemoEx.GetAttributes: TFontParams;
begin
  GetTextAttributes(SelStart, Result{%H-});
end;

function TRichMemoEx.CanUndo: boolean;
begin
  {$ifdef windows}
  Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

function TRichMemoEx.CanPaste: boolean;
begin
  {$ifdef windows}
  Result := SendMessage(Handle, EM_CANPASTE,  0, 0) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

procedure TRichMemoEx.DoAttributesChange;
begin
  if Assigned(OnAttrChange) then OnAttrChange(self);
end;

procedure TRichMemoEx.SelectAll;
begin
  {$ifdef windows}
  SendMessage(Handle, EM_SETSEL, 0, -1);
  {$else}
  SetSel(0,High(Int16));
  {$endif}
end;

procedure TRichMemoEx.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  DoAttributesChange;
  {$ifdef unix} Modified := True; {$endif}
end;

procedure TRichMemoEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ReadOnly then HideCursor;
end;

procedure TRichMemoEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoAttributesChange;
  if ReadOnly then HideCursor;
end;

procedure TRichMemoEx.SetSel(x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_SETSEL, x1, x2);
  {$else}
  SelStart  := x1;
  SelLength := x2-x1;
  {$endif}
end;

procedure TRichMemoEx.GetSel(var x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_GETSEL, {%H-}integer(@x1), {%H-}integer(@x2));
  {$else}
  x1 := SelStart;
  x2 := SelStart + SelLength;
  {$endif}
end;

function TRichMemoEx.Selected: boolean;
begin
  Result := SelLength > 0;
end;

{$ifdef linux}

function TRichMemoEx.GetTextView: PGtkTextView;
var
  Widget, TextWidget: PGtkWidget;
  List: PGList;
begin
  Result := nil;
  Widget := {%H-}PGtkWidget(Handle);
  List := gtk_container_get_children(PGtkContainer(Widget));
  if not Assigned(List) then Exit;
  TextWidget := PGtkWidget(List^.Data);
  if not Assigned(TextWidget) then Exit;
  Result := PGtkTextView(TextWidget);
end;

procedure TRichMemoEx.CopyToClipboard;
var
  Clipboard: PGtkClipboard;
  Buffer: PGtkTextBuffer;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  Buffer := gtk_text_view_get_buffer(GetTextView);
  gtk_text_buffer_copy_clipboard(Buffer, Clipboard);
end;

procedure TRichMemoEx.CutToClipboard;
var
  Clipboard: PGtkClipboard;
  Buffer: PGtkTextBuffer;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  Buffer := gtk_text_view_get_buffer(GetTextView);
  gtk_text_buffer_cut_clipboard(Buffer, Clipboard, True);
end;

procedure TRichMemoEx.PasteFromClipboard;
var
  Clipboard: PGtkClipboard;
  Buffer: PGtkTextBuffer;
begin
  Clipboard := gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
  Buffer := gtk_text_view_get_buffer(GetTextView);
  gtk_text_buffer_paste_clipboard(Buffer, Clipboard, NULL, True);
end;

{$endif}

function TRichMemoEx.LoadRichText(Source: TStream): Boolean;
begin
  Source.Seek(0,soFromBeginning);
  Result := inherited LoadRichText(Source);
end;

function TRichMemoEx.LoadRichText(Source: string): Boolean;
var Stream : TMemoryStream;
begin
  Source := Utf8ToRTF(Source);
  Stream := TMemoryStream.Create;
  Stream.WriteBuffer(Pointer(Source)^, Length(Source));
  Result := LoadRichText(Stream);
  Stream.Free;
end;

procedure TRichMemoEx.LoadFromFile(const FileName : string);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.LoadFromFile(FileName);
  LoadRichText(Stream);
  Stream.Free;
end;

procedure TRichMemoEx.SaveToFile(const FileName : string);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  SaveRichText(Stream);
  Stream.Seek(0,soFromBeginning);
  Stream.SaveToFile(FileName);
  Stream.Free;
end;

end.

