unit RichMemoEx;

interface

uses
  Forms, SysUtils, Classes, Graphics, Controls, ExtCtrls,
  {$ifdef windows} Windows, Printers, OSPrinters, {$endif}
  {$ifdef windows} RichEdit, rmWinEx, {$endif}
  {$ifdef linux} rmGtk2ex, {$endif}
  RichMemo, RichMemoUtils, LazUTF8, UmLib;

type

  TRichMemoEx = class(TRichMemo)
  private
    {$ifdef windows} function  GetModified: boolean; {$endif}
    {$ifdef windows} procedure SetModified(value: boolean); {$endif}
  protected
    {$ifdef windows} function LineCount: integer; {$endif}
    {$ifdef windows} function LineIndex(x: longint): integer; {$endif}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanUndo: boolean;
    procedure HideCursor;
    procedure Hide_Selection;
    procedure Show_Selection;
    procedure SelectAll;
    function  SelAttributes: TFontParams;
    function SelParaAlignment: TParaAlignment;
    {$ifdef windows} function SelParaNumbering: TParaNumbering; {$endif}
    {$ifdef windows} function FindRightWordBreak(Pos: integer): integer; {$endif}
    {$ifdef windows} property Modified: boolean read GetModified write SetModified; {$endif}
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    procedure PasteFromClipboard; override;
    function LoadRichText(Source: TStream): Boolean; override;
    function LoadRichText(Source: string): Boolean;
    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);
  end;

implementation

function ToStr(value: longint): string;
begin
 System.Str(value, Result);
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

function TRichMemoEx.SelAttributes: TFontParams;
begin
  {$ifdef windows}
  Result := GetSelectedTextAttributes(Handle);
  {$else}
  GetTextAttributes(SelStart, Result{%H-});
  {$endif}
end;

function TRichMemoEx.SelParaAlignment: TParaAlignment;
begin
  {$ifdef windows}
  Result := GetSelectedParaAlignment(Handle);
  {$else}
  GetParaAlignment(SelStart, Result{%H-});
  {$endif}
end;

{$ifdef windows}
function TRichMemoEx.SelParaNumbering: TParaNumbering;
begin
  Result := GetSelectedParaNumbering(Handle);
end;
{$endif}

function TRichMemoEx.CanUndo: boolean;
begin
  {$ifdef windows}
  Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0;
  {$else}
  Result := True;
  {$endif}
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
  if ReadOnly then HideCursor;
end;


procedure TRichMemoEx.CopyToClipboard;
begin
  inherited;
  {$ifdef linux} Gtk2Copy(Handle); {$endif}
end;

procedure TRichMemoEx.CutToClipboard;
begin
  inherited;
  {$ifdef linux} Gtk2Cut(Handle); {$endif}
end;

procedure TRichMemoEx.PasteFromClipboard;
begin
  inherited;
  {$ifdef linux} Gtk2Paste(Handle); {$endif}
end;

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

