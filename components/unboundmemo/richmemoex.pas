unit RichMemoEx;

interface

uses
   Forms, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  {$ifdef windows} Windows, Printers, OSPrinters, {$endif}
  {$ifdef windows} RichEdit, Win32RichMemoProc, {$endif}
  {$ifdef linux} rmGtk2ex, {$endif}
   RichMemo, RichMemoUtils, LazUTF8, LCLVersion;

type

  TRichMemoEx = class(TRichMemo)
  private
    FOnAttrChange: TNotifyEvent;
    function  GetAttributes: TFontParams;
    procedure SetAttributes(const value: TFontParams);
    procedure DoAttributesChange;
    {$ifdef windows} function  GetModified: boolean; {$endif}
    {$ifdef windows} procedure SetModified(value: boolean); {$endif}
  protected
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
    procedure HideCursor;
    procedure Hide_Selection;
    procedure Show_Selection;
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

function Utf8ToRTF(const s: string): string;

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
    {$if lcl_major >= 2}
      unicode := UTF8CodepointToUnicode(p,CharLen);
    {$else}
      unicode := UTF8CharacterToUnicode(p,CharLen);
    {$endif}
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
  w : UnicodeString;
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
   {$ifdef linux} Gtk2HideCursor(Handle); {$endif}
end;

procedure TRichMemoEx.Hide_Selection;
begin
   {$ifdef windows} SendMessage(Handle, EM_HIDESELECTION, 1, 0); {$endif}
end;

procedure TRichMemoEx.Show_Selection;
begin
   {$ifdef windows} SendMessage(Handle, EM_HIDESELECTION, 0, 0); {$endif}
end;

function TRichMemoEx.GetAttributes: TFontParams;
begin
  {$ifdef windows}
  RichEditManager.GetSelectedTextStyle(Handle, Result{%H-});
  {$else}
  GetTextAttributes(SelStart, Result{%H-});
  {$endif}
end;

procedure TRichMemoEx.SetAttributes(const value: TFontParams);
begin
  SetTextAttributes(SelStart, SelLength, value);
  DoAttributesChange;
end;

function TRichMemoEx.CanUndo: boolean;
begin
  {$ifdef windows}
  Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0;
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
  SelStart := 0;
  SelLength := MaxInt;
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

{$ifdef linux}

procedure TRichMemoEx.CopyToClipboard;
begin
  Gtk2Copy(Handle);
end;

procedure TRichMemoEx.CutToClipboard;
begin
  Gtk2Cut(Handle);
end;

procedure TRichMemoEx.PasteFromClipboard;
begin
  Gtk2Paste(Handle);
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
  Source := Utf8ToRTF(Source) + LineEnding;
  Stream := TMemoryStream.Create;
  Stream.WriteBuffer(Pointer(Source)^, Length(Source));
  Result := LoadRichText(Stream);
  Stream.Free;
end;

procedure TRichMemoEx.LoadFromFile(const FileName : string);
begin
  LoadRTFFile(self, FileName);
end;

procedure TRichMemoEx.SaveToFile(const FileName : string);
begin
  SaveRTFFile(self, FileName);
end;

end.

