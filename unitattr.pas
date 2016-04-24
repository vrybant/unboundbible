unit UnitAttr;

{$mode delphi}{$H+}

interface

uses
  Windows, Classes, RichMemo;

type
  TNumberingStyle = (nsNone, nsBullet);

  TParaAttributes = class(TPersistent)
  private
    RichEdit: TRichMemo;
    Paragraph: TParaFormat;
    procedure GetAttributes;
    function GetAlignment: TAlignment;
    function GetFirstIndent: Longint;
    function GetLeftIndent: Longint;
    function GetRightIndent: Longint;
    function GetNumbering: TNumberingStyle;
    function GetTab(Index: Byte): Longint;
    function GetTabCount: Integer;
    procedure InitParagraph;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAttributes;
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

implementation

  //=================================================================================================
  //                                     TParaAttributes
  //=================================================================================================

  constructor TParaAttributes.Create(AOwner: TRichMemo);
  begin
    inherited Create;
    RichEdit := AOwner;
    InitParagraph;
  end;

  procedure TParaAttributes.InitParagraph;
  begin
    FillChar(Paragraph, SizeOf(TParaFormat), 0);
    Paragraph.cbSize := SizeOf(TParaFormat);
  end;

  procedure TParaAttributes.GetAttributes;
  begin
    if RichEdit.HandleAllocated then
      SendMessage(RichEdit.Handle, EM_GETPARAFORMAT, 0, {%H-}LPARAM(@Paragraph));
  end;

  procedure TParaAttributes.SetAttributes;
  begin
    RichEdit.HandleNeeded; { we REALLY need the handle for BiDi }
    if RichEdit.HandleAllocated then
    begin
      if RichEdit.UseRightToLeftAlignment then
        if Paragraph.wAlignment = PFA_LEFT then
          Paragraph.wAlignment := PFA_RIGHT
        else if Paragraph.wAlignment = PFA_RIGHT then
          Paragraph.wAlignment := PFA_LEFT;
      SendMessage(RichEdit.Handle, EM_SETPARAFORMAT, 0, {%H-}LPARAM(@Paragraph));
    end;
  end;

  function TParaAttributes.GetAlignment: TAlignment;
  begin
    GetAttributes;
    Result := TAlignment(Paragraph.wAlignment - 1);
  end;

  procedure TParaAttributes.SetAlignment(Value: TAlignment);
  begin
    with Paragraph do
    begin
      dwMask := PFM_ALIGNMENT;
      wAlignment := Ord(Value) + 1;
    end;
    SetAttributes;
  end;

  function TParaAttributes.GetNumbering: TNumberingStyle;
  begin
    GetAttributes;
    Result := TNumberingStyle(Paragraph.wNumbering);
  end;

  procedure TParaAttributes.SetNumbering(Value: TNumberingStyle);
  begin
    case Value of
      nsBullet: if LeftIndent < 10 then LeftIndent := 10;
      nsNone: LeftIndent := 0;
    end;
    with Paragraph do
    begin
      dwMask := PFM_NUMBERING;
      wNumbering := Ord(Value);
    end;
    SetAttributes;
  end;

  function TParaAttributes.GetFirstIndent: Longint;
  begin
    GetAttributes;
    Result := Paragraph.dxStartIndent div 20
  end;

  procedure TParaAttributes.SetFirstIndent(Value: Longint);
  begin
    with Paragraph do
    begin
      dwMask := PFM_STARTINDENT;
      dxStartIndent := Value * 20;
    end;
    SetAttributes;
  end;

  function TParaAttributes.GetLeftIndent: Longint;
  begin
    GetAttributes;
    Result := Paragraph.dxOffset div 20;
  end;

  procedure TParaAttributes.SetLeftIndent(Value: Longint);
  begin
    with Paragraph do
    begin
      dwMask := PFM_OFFSET;
      dxOffset := Value * 20;
    end;
    SetAttributes;
  end;

  function TParaAttributes.GetRightIndent: Longint;
  begin
    GetAttributes;
    Result := Paragraph.dxRightIndent div 20;
  end;

  procedure TParaAttributes.SetRightIndent(Value: Longint);
  begin
    with Paragraph do
    begin
      dwMask := PFM_RIGHTINDENT;
      dxRightIndent := Value * 20;
    end;
    SetAttributes;
  end;

  function TParaAttributes.GetTab(Index: Byte): Longint;
  begin
    GetAttributes;
    Result := Paragraph.rgxTabs[Index] div 20;
  end;

  procedure TParaAttributes.SetTab(Index: Byte; Value: Longint);
  begin
    GetAttributes;
    with Paragraph do
    begin
      rgxTabs[Index] := Value * 20;
      dwMask := PFM_TABSTOPS;
      if cTabCount < Index then cTabCount := Index;
      SetAttributes;
    end;
  end;

  function TParaAttributes.GetTabCount: Integer;
  begin
    GetAttributes;
    Result := Paragraph.cTabCount;
  end;

  procedure TParaAttributes.SetTabCount(Value: Integer);
  begin
    GetAttributes;
    with Paragraph do
    begin
      dwMask := PFM_TABSTOPS;
      cTabCount := Value;
      SetAttributes;
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

end.

