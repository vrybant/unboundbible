unit RichMemoEx;

interface

uses
  {$ifdef windows} Windows, Printers, OSPrinters, {$endif}
  Forms, SysUtils, Classes, Graphics, Controls, ExtCtrls, RichMemo;

type
  TRichMemoExtended = class(TRichMemo)
  private
    FOnSelChange: TNotifyEvent;
    function  GetAttributes: TFontParams;
    procedure SetAttributes(const value: TFontParams);
    procedure SelectionChange;  dynamic;
    {$ifdef windows} function  GetModified: boolean; {$endif}
    {$ifdef windows} procedure SetModified(value: boolean); {$endif}
  protected
    procedure SetSel(x1,x2: integer);
    procedure GetSel(var x1,x2: integer);
    {$ifdef windows}
    function LineCount: integer;
    function LineIndex(x: longint): integer;
    {$endif}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    {$ifdef unix} Modified : boolean; {$endif}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadRichText(Source: TStream): Boolean; override;
    function CanUndo: boolean;
    procedure HideCursor;
    function Selected: boolean;
    procedure SelectAll;
    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);
    property SelAttributes: TFontParams read GetAttributes write SetAttributes;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    {$ifdef windows} property Modified: boolean read GetModified write SetModified; {$endif}
  end;


implementation

constructor TRichMemoExtended.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef unix} Modified := False; {$endif}
end;

destructor TRichMemoExtended.Destroy;
begin
  inherited Destroy;
end;

{$ifdef windows}

function TRichMemoExtended.LineCount: integer;
begin
  Result := SendMessage(Handle, EM_GETLINECOUNT,0,0);
end;

function TRichMemoExtended.LineIndex(x: longint): integer;
begin
  Result := SendMessage(Handle, EM_LINEINDEX,x,0);
end;

function TRichMemoExtended.GetModified: boolean;
begin
  Result := SendMessage(Handle, EM_GETMODIFY,  0, 0) <> 0;
end;

procedure TRichMemoExtended.SetModified(value: boolean);
begin
  SendMessage(Handle, EM_SETMODIFY, Byte(value), 0);
end;

{$endif}

function TRichMemoExtended.LoadRichText(Source: TStream): Boolean;
begin
  Source.Seek(0,soFromBeginning);
  Result := inherited LoadRichText(Source);
end;

procedure TRichMemoExtended.SetAttributes(const value: TFontParams);
begin
  SetTextAttributes(SelStart, SelLength, value);
  SelectionChange;
end;

function TRichMemoExtended.GetAttributes: TFontParams;
begin
  GetTextAttributes(SelStart, Result{%H-});
end;

function TRichMemoExtended.CanUndo: boolean;
begin
  {$ifdef windows}
  Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

procedure TRichMemoExtended.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TRichMemoExtended.SelectAll;
begin
  {$ifdef windows}
  SendMessage(Handle, EM_SETSEL, 0, -1);
  {$else}
  SetSel(0,100000);
  {$endif}
end;

procedure TRichMemoExtended.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  SelectionChange;
  {$ifdef unix} Modified := True; {$endif}
end;

procedure TRichMemoExtended.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  SelectionChange;
end;

procedure TRichMemoExtended.SetSel(x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_SETSEL, x1, x2);
  {$else}
  SelStart  := x1;
  SelLength := x2-x1;
  {$endif}
end;

procedure TRichMemoExtended.GetSel(var x1,x2: integer);
begin
  {$ifdef windows}
  SendMessage(Handle, EM_GETSEL, {%H-}integer(@x1), {%H-}integer(@x2));
  {$else}
  x1 := SelStart;
  x2 := SelStart + SelLength;
  {$endif}
end;

function TRichMemoExtended.Selected: boolean;
begin
  Result := SelLength > 0;
end;

procedure TRichMemoExtended.HideCursor;
begin
  {$ifdef windows} HideCaret(Handle); {$endif}
end;

procedure TRichMemoExtended.LoadFromFile(const FileName : string);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.LoadFromFile(FileName);
  LoadRichText(Stream);
  Stream.Free;
end;

procedure TRichMemoExtended.SaveToFile(const FileName : string);
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

