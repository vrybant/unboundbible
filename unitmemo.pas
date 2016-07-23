unit UnitMemo;

{$mode delphi}{$H+}

interface

uses
  {$ifdef mswindows} Windows, Printers, OSPrinters, {$endif}
  Forms, SysUtils, Classes, Graphics, Controls, ExtCtrls, RichMemo;

type
  TRichMemoEx = class(TRichMemo)
  private
    FOnSelChange: TNotifyEvent;
    function  GetAttributes: TFontParams;
    procedure SetAttributes(const value: TFontParams);
    procedure SelectionChange;  dynamic;
    {$ifdef mswindows} function  GetModified: boolean; {$endif}
    {$ifdef mswindows} procedure SetModified(value: boolean); {$endif}
  protected
    {$ifdef mswindows}
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
    procedure SetSel(x1,x2: integer);
    procedure GetSel(var x1,x2: integer);
    procedure SelectAll; // override;
    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);
    property SelAttributes: TFontParams read GetAttributes write SetAttributes;
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    {$ifdef mswindows} property Modified: boolean read GetModified write SetModified; {$endif}
  end;


implementation

constructor TRichMemoEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef unix} Modified := False; {$endif}
end;

destructor TRichMemoEx.Destroy;
begin
  inherited Destroy;
end;

{$ifdef mswindows}

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

{$endif}

function TRichMemoEx.LoadRichText(Source: TStream): Boolean;
begin
  Source.Seek(0,soFromBeginning);
  Result := inherited LoadRichText(Source);
end;

procedure TRichMemoEx.SetAttributes(const value: TFontParams);
begin
  SetTextAttributes(SelStart, SelLength, value);
  SelectionChange;
end;

function TRichMemoEx.GetAttributes: TFontParams;
begin
  GetTextAttributes(SelStart, Result);
end;

function TRichMemoEx.CanUndo: boolean;
begin
  {$ifdef mswindows}
  Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

procedure TRichMemoEx.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TRichMemoEx.SelectAll;
begin
  {$ifdef mswindows}
  SendMessage(Handle, EM_SETSEL, 0, -1);
  {$else}
  SetSel(0,100000);
  {$endif}
end;

procedure TRichMemoEx.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  SelectionChange;
  {$ifdef unix} Modified := True; {$endif}
end;

procedure TRichMemoEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  SelectionChange;
end;

procedure TRichMemoEx.SetSel(x1,x2: integer);
begin
  {$ifdef mswindows}
  SendMessage(Handle, EM_SETSEL, x1, x2);
  {$else}
  SelStart  := x1;
  SelLength := x2-x1;
  {$endif}
end;

procedure TRichMemoEx.GetSel(var x1,x2: integer);
begin
  {$ifdef mswindows}
  SendMessage(Handle, EM_GETSEL, {%H-}integer(@x1), {%H-}integer(@x2));
  {$else}
  x1 := SelStart;
  x2 := SelStart + SelLength;
  {$endif}
end;

procedure TRichMemoEx.HideCursor;
begin
  {$ifdef mswindows} HideCaret(Handle); {$endif}
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

