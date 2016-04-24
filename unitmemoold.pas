unit UnitMemo;

{$mode delphi}{$H+}

interface

uses
  {$ifdef mswindows} Windows, {$endif}
  Forms, Messages, SysUtils, Classes, Graphics, Controls,
  RichMemo, ComCtrls, ExtCtrls, LMessages;

type
    TNumberingStyle = (nsNone, nsBullet);

  {$ifdef mswindows}
  TParaAttributes = class(TPersistent)
  private
    RichEdit: TRichMemo;
    procedure GetAttributes(var Paragraph: TParaFormat);
    function GetAlignment: TAlignment;
    function GetFirstIndent: Longint;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetNumbering: TNumberingStyle;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    procedure InitPara(var Paragraph: TParaFormat);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAttributes(var Paragraph: TParaFormat);
    procedure SetFirstIndent(Value: Longint);
    procedure SetLeftIndent(Value: Longint);
    procedure SetRightIndent(Value: Longint);
    procedure SetNumbering(Value: TNumberingStyle);
    procedure SetTab(Index: Byte; Value: Longint);
    procedure SetTabCount(Value: Integer);
  public
    constructor Create(AOwner: TRichMemo);
    procedure Assign(Source: TPersistent); override;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Longint read GetFirstIndent write SetFirstIndent;
    property LeftIndent: Longint read GetLeftIndent write SetLeftIndent;
    property Numbering: TNumberingStyle read GetNumbering write SetNumbering;
    property RightIndent: Longint read GetRightIndent write SetRightIndent;
    property Tab[Index: Byte]: Longint read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
  end;
  {$endif}

  TRichMemoPlus = class(TRichMemo)
  private
    {$ifdef mswindows} FParagraph: TParaAttributes; {$endif}
    FOnSelChange: TNotifyEvent;
    function  GetAttributes: TFontParams;
    procedure SetAttributes(const value: TFontParams);
    procedure SelectionChange;  dynamic;
    function  GetModified: boolean;
    procedure SetModified(value: boolean);
  protected
    {$ifdef mswindows} function GetSelText: string; override; {$endif}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
     Modified : boolean;
    {$ifdef mswindows} property Paragraph: TParaAttributes read FParagraph; {$endif}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelAttributes: TFontParams read GetAttributes write SetAttributes;
    function CharFromPos(p: TPoint): integer;
    function LoadRichText(Source: TStream): Boolean; override;
    function CanUndo: boolean;
    function CanPaste: boolean;
    procedure HideCursor;
    procedure SetSel(x1,x2: integer);
    procedure GetSel(var x1,x2: integer);
    procedure SelectAll; // override;
    procedure LoadFromFile(const FileName : string);
    procedure SaveToFile(const FileName : string);
    property OnSelectionChange: TNotifyEvent read FOnSelChange write FOnSelChange;
    property Modified: boolean read GetModified write SetModified;
  end;
	
implementation

uses
  LCLProc; // UTF8Length()

  //=================================================================================================
  //                                     TParaAttributes
  //=================================================================================================

  {$ifdef mswindows}

  constructor TParaAttributes.Create(AOwner: TRichMemo);
  begin
    inherited Create;
    RichEdit := AOwner;
  end;

  procedure TParaAttributes.InitPara(var Paragraph: TParaFormat);
  begin
    FillChar(Paragraph, SizeOf(TParaFormat), 0);
    Paragraph.cbSize := SizeOf(TParaFormat);
  end;

  procedure TParaAttributes.GetAttributes(var Paragraph: TParaFormat);
  begin
    InitPara(Paragraph);
    if RichEdit.HandleAllocated then
      SendMessage(RichEdit.Handle, EM_GETPARAFORMAT, 0, LPARAM(@Paragraph));
  end;

  procedure TParaAttributes.SetAttributes(var Paragraph: TParaFormat);
  begin
    RichEdit.HandleNeeded; { we REALLY need the handle for BiDi }
    if RichEdit.HandleAllocated then
    begin
      if RichEdit.UseRightToLeftAlignment then
        if Paragraph.wAlignment = PFA_LEFT then
          Paragraph.wAlignment := PFA_RIGHT
        else if Paragraph.wAlignment = PFA_RIGHT then
          Paragraph.wAlignment := PFA_LEFT;
      SendMessage(RichEdit.Handle, EM_SETPARAFORMAT, 0, LPARAM(@Paragraph));
    end;
  end;

  function TParaAttributes.GetAlignment: TAlignment;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := TAlignment(Paragraph.wAlignment - 1);
  end;

  procedure TParaAttributes.SetAlignment(Value: TAlignment);
  var
    Paragraph: TParaFormat;
  begin
    InitPara(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_ALIGNMENT;
      wAlignment := Ord(Value) + 1;
    end;
    SetAttributes(Paragraph);
  end;

  function TParaAttributes.GetNumbering: TNumberingStyle;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := TNumberingStyle(Paragraph.wNumbering);
  end;

  procedure TParaAttributes.SetNumbering(Value: TNumberingStyle);
  var
    Paragraph: TParaFormat;
  begin
    case Value of
      nsBullet: if LeftIndent < 10 then LeftIndent := 10;
      nsNone: LeftIndent := 0;
    end;
    InitPara(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_NUMBERING;
      wNumbering := Ord(Value);
    end;
    SetAttributes(Paragraph);
  end;

  function TParaAttributes.GetFirstIndent: Longint;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := Paragraph.dxStartIndent div 20
  end;

  procedure TParaAttributes.SetFirstIndent(Value: Longint);
  var
    Paragraph: TParaFormat;
  begin
    InitPara(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_STARTINDENT;
      dxStartIndent := Value * 20;
    end;
    SetAttributes(Paragraph);
  end;

  function TParaAttributes.GetLeftIndent: Longint;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := Paragraph.dxOffset div 20;
  end;

  procedure TParaAttributes.SetLeftIndent(Value: Longint);
  var
    Paragraph: TParaFormat;
  begin
    InitPara(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_OFFSET;
      dxOffset := Value * 20;
    end;
    SetAttributes(Paragraph);
  end;

  function TParaAttributes.GetRightIndent: Longint;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := Paragraph.dxRightIndent div 20;
  end;

  procedure TParaAttributes.SetRightIndent(Value: Longint);
  var
    Paragraph: TParaFormat;
  begin
    InitPara(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_RIGHTINDENT;
      dxRightIndent := Value * 20;
    end;
    SetAttributes(Paragraph);
  end;

  function TParaAttributes.GetTab(Index: Byte): Longint;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := Paragraph.rgxTabs[Index] div 20;
  end;

  procedure TParaAttributes.SetTab(Index: Byte; Value: Longint);
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    with Paragraph do
    begin
      rgxTabs[Index] := Value * 20;
      dwMask := PFM_TABSTOPS;
      if cTabCount < Index then cTabCount := Index;
      SetAttributes(Paragraph);
    end;
  end;

  function TParaAttributes.GetTabCount: Integer;
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    Result := Paragraph.cTabCount;
  end;

  procedure TParaAttributes.SetTabCount(Value: Integer);
  var
    Paragraph: TParaFormat;
  begin
    GetAttributes(Paragraph);
    with Paragraph do
    begin
      dwMask := PFM_TABSTOPS;
      cTabCount := Value;
      SetAttributes(Paragraph);
    end;
  end;

  procedure TParaAttributes.Assign(Source: TPersistent);
  var
    I: Integer;
  begin
    if Source is TParaAttributes then
    begin
      Alignment   := TParaAttributes(Source).Alignment;
      FirstIndent := TParaAttributes(Source).FirstIndent;
      LeftIndent  := TParaAttributes(Source).LeftIndent;
      RightIndent := TParaAttributes(Source).RightIndent;
      Numbering   := TParaAttributes(Source).Numbering;
      for I := 0 to MAX_TAB_STOPS - 1 do
        Tab[I] := TParaAttributes(Source).Tab[I];
    end
    else inherited Assign(Source);
  end;

  {$endif}

//=================================================================================================
//                                     TRichMemoPlus
//=================================================================================================

constructor TRichMemoPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef mswindows} FParagraph := TParaAttributes.Create(Self); {$endif}
  Modified := False;
end;

destructor TRichMemoPlus.Destroy;
begin
  {$ifdef mswindows} FParagraph.Free; {$endif}
  inherited Destroy;
end;

{$ifdef mswindows}
function TRichMemoPlus.GetSelText: string; // standart SelText does'nt work correctly
var
  w : WideString;
  length: integer;
begin
  SetLength(w, GetSelLength + 1);
  length := SendMessage(Handle, EM_GETSELTEXT, 0, Longint(PWideChar(w)));
  SetLength(w, length);
  Result := UTF8Encode(w);
end;
{$endif}

//-------------------------------------------------------------------------------------------------

function TRichMemoPlus.LoadRichText(Source: TStream): Boolean;
begin
  Source.Seek(0,soFromBeginning);
  Result := inherited LoadRichText(Source);
end;

procedure TRichMemoPlus.SetAttributes(const value: TFontParams);
begin
  SetTextAttributes(SelStart, SelLength, value);
  SelectionChange;
end;

function TRichMemoPlus.GetAttributes: TFontParams;
begin
  GetTextAttributes(SelStart, Result);
end;

function TRichMemoPlus.CharFromPos(p: TPoint): integer;
begin
  Result := SendMessage(Handle, EM_CHARFROMPOS, 0, longint(@p));
end;

function TRichMemoPlus.GetModified: boolean;
begin
  Result := False; // Modified;

  {$ifdef mswindows}
  Result := SendMessage(Handle, EM_GETMODIFY,  0, 0) <> 0;
  {$endif}
end;

procedure TRichMemoPlus.SetModified(value: boolean);
begin
  {$ifdef mswindows} SendMessage(Handle, EM_SETMODIFY, Byte(value), 0); {$endif}
end;

function TRichMemoPlus.CanUndo: boolean;
begin
  Result := Modified;
  {$ifdef mswindows} Result := SendMessage(Handle, EM_CANUNDO,  0, 0) <> 0; {$endif}
end;

function TRichMemoPlus.CanPaste: boolean;
begin
  Result := True;
  {$ifdef mswindows} Result := SendMessage(Handle, EM_CANPASTE, 0, 0) <> 0; {$endif}
end;

procedure TRichMemoPlus.SelectionChange;
begin
  if Assigned(OnSelectionChange) then OnSelectionChange(Self);
end;

procedure TRichMemoPlus.SelectAll;
begin
  {$ifdef mswindows} SendMessage(Handle, EM_SETSEL, 0, -1); {$endif}
end;

procedure TRichMemoPlus.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  SelectionChange;
end;

procedure TRichMemoPlus.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  SelectionChange;
end;

procedure TRichMemoPlus.SetSel(x1,x2: integer);
begin
  {$ifdef mswindows}
  SendMessage(Handle, EM_SETSEL, x1, x2);
  Exit;
  {$endif}

  SelStart  := x1;
  SelLength := x2-x1;
end;

procedure TRichMemoPlus.GetSel(var x1,x2: integer);
begin
  {$ifdef mswindows}
  SendMessage(Handle, EM_GETSEL, integer(@x1), integer(@x2));
  Exit;
  {$endif}

  x1 := SelStart;
  x2 := SelStart + SelLength;
end;

procedure TRichMemoPlus.HideCursor;
begin
  {$ifdef mswindows} HideCaret(Handle); {$endif}
end;

procedure TRichMemoPlus.LoadFromFile(const FileName : string);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.LoadFromFile(Utf8Decode(FileName));
  LoadRichText(Stream);
  Stream.Free;
end;

procedure TRichMemoPlus.SaveToFile(const FileName : string);
var
  Stream : TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  SaveRichText(Stream);
  Stream.Seek(0,soFromBeginning);
  Stream.SaveToFile(Utf8Decode(FileName));
  Stream.Free;
end;

end.

