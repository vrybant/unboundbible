unit UnboundMemo;

interface

uses
  {$ifdef windows} Windows, {$endif}
  Forms, SysUtils, LResources, Classes, Graphics, Controls, ExtCtrls,
  LCLProc, LCLType, LazUTF8, RichMemo, RichMemoEx, UnitLib,
  {$ifdef windows} UmParseWin; {$else} UmParse; {$endif}

type
  TUnboundMemo = class(TRichMemoEx)
  protected
    procedure DoParagraphChange;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  private
    fLinkable : boolean;
    fParagraphic : boolean;
    fOnParagraphChange : TNotifyEvent;
    SelStartTemp : integer;
    SelLengthTemp : integer;
    function GetNumber(s: string): integer;
    function  GetColorLink: string;
    function  GetLink: string;
    function  GetParagraphNumber(Pos: integer; select: boolean): integer;
    procedure GetParagraphRange;
    function  GetStartSelection: integer;
    function  GetEndSelection: integer;
  public
    Hyperlink : string;
    ParagraphStart : integer;
    ParagraphCount : integer;
    constructor Create(AOwner: TComponent); override;
    function Foreground: integer;
    function Breaks: integer;
    function Paragraphs: integer;
    procedure SelectParagraph(n: integer);
    procedure SelectWord;
    procedure SelectAll; {$ifdef windows} override; {$endif}
    procedure SaveSelection;
    procedure RestoreSelection;
    procedure LoadText(Source: string; jtag: boolean = false);
    procedure LoadHtml(Source: string);
  published
    property Linkable    : boolean read fLinkable    write fLinkable    default False;
    property Paragraphic : boolean read fParagraphic write fParagraphic default False;
    property OnParagraphChange: TNotifyEvent read fOnParagraphChange write fOnParagraphChange;
  end;

const
  fgText     = 0;
  fgLink     = 1;
  fgStrong   = 2;
  fgFootnote = 3;

procedure Register;

implementation

constructor TUnboundMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Hyperlink := '';
  ParagraphStart := 0;
  ParagraphCount := 0;
  Cursor := crArrow;

  {$ifdef windows} if Font.Name = 'default' then Font.Name := 'Tahoma'; {$endif}
end;

procedure TUnboundMemo.DoParagraphChange;
begin
  if Assigned(fOnParagraphChange) then fOnParagraphChange(Self);
end;

function TUnboundMemo.GetNumber(s: string): integer;
begin
  if not Prefix(' ',s) then s := '';
  s := Trim(s);
  s := Copy(s,1,Pos(' ',s));
  Result := ToInt(s);
end;

function TUnboundMemo.Foreground: integer;
begin
  Result := fgText;
  if SelAttributes.Color = clSysNavy  then Result := fgLink else
  if SelAttributes.Color = clSysBrown then Result := fgStrong else
  if SelAttributes.Color = clSysTeal  then Result := fgFootnote;
end;

function TUnboundMemo.Breaks: integer;
var
  c : char;
begin
  Result := 0;
  SaveSelection;
  SetSel(0, SelStart);
  for c in SelText do
    if c = LineBreak then Result += 1;
  RestoreSelection;
end;

function TUnboundMemo.GetColorLink: string;
var
  fore : integer;
  n1,n2,x0,x1,x2 : integer;
  limit : integer;
begin
  if SelLength > 0 then Exit('');

  fore := Foreground;
  if fore = fgText then Exit('');
  GetSel(n1,n2);

  x0 := SelStart;
  x1 := x0;
  repeat
    dec(x1);
    SetSel( {$ifdef unix} x1,x1 {$else} x1+1,x1+1 {$endif} );
  until (Foreground <> fore) or (x1 < 0);

  inc(x1);
  if x1 < 0 then inc(x1);
  limit := Utf8Length(Text);

  x2 := x0;
  repeat
    inc(x2);
    SetSel( {$ifdef unix} x2,x2 {$else} x2+1,x2+1 {$endif} );
  until (Foreground <> fore) or (x2 > limit);

  SetSel(x1, x2); Result := RemoveLineBreaker(SelText);
  SetSel(n1, n2); Result := Trim(Result);
end;

function TUnboundMemo.GetLink: string;
begin
  if Foreground <> fgText then Result := GetColorLink;
end;

procedure TUnboundMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1 : integer = 0;
  x2 : integer = 0;
begin
  if Linkable then Hyperlink := GetLink else Hyperlink := '';

  if Paragraphic and (Button = mbLeft) then
    begin
      if not Hyperlink.isEmpty then GetSel(x1,x2);
      GetParagraphRange;
      DoParagraphChange;
      if not Hyperlink.isEmpty then SetSel(x1,x2);
    end;

  inherited;
end;

procedure TUnboundMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  {$ifdef windows}
  if Paragraphic then
    if ((Key in [VK_PRIOR, VK_NEXT]) and (Shift = [])) or (Shift <> []) then
      begin
        GetParagraphRange;
        DoParagraphChange;
      end;
  {$endif}

  if Paragraphic then
    if ((Key in [VK_UP, VK_DOWN]) and (Shift = [])) then
      begin
        if Key = VK_UP   then if ParagraphStart > 1 then ParagraphStart -= 1;
        if Key = VK_DOWN then if ParagraphStart < Paragraphs then ParagraphStart += 1;
        ParagraphCount := 1;

        SelectParagraph(ParagraphStart);
        DoParagraphChange;
      end;

  {$ifdef linux}
  if Paragraphic then
    if ((Key in [VK_HOME, VK_END]) and (Shift = [ssCtrl])) then
      begin
        if Key = VK_HOME then ParagraphStart := 1;
        if Key = VK_END  then ParagraphStart := Paragraphs;
        ParagraphCount := 1;

        SelectParagraph(ParagraphStart);
        DoParagraphChange;
      end;
  {$endif}

  {$ifdef windows}
    if Linkable and not ReadOnly and (Key = VK_CONTROL) then ShowCaret(Handle);
  {$endif}
end;

function TUnboundMemo.GetParagraphNumber(Pos: integer; select: boolean): integer;
var
  p : TParaRange;
  s : string;
begin
  GetParaRange(Pos, p);
  s := GetText(p.start, p.length);
  Result := GetNumber(s);
  if select then SetSel(p.start, p.start+1);
end;

procedure TUnboundMemo.GetParagraphRange;
var
  List : TStringArray;
begin
  if not SelText.Contains(LineBreak) then
    begin
      ParagraphStart := GetParagraphNumber(SelStart, SelLength = 0);
      ParagraphCount := 1;
      Exit;
    end;

  List := SelText.Split(LineBreak);

  if GetNumber(SelText) > 0 then
    begin
      ParagraphStart := GetNumber(SelText);
      ParagraphCount := List.Count;
      Exit;
    end;

  if List.Count > 1 then
    begin
      ParagraphStart := GetNumber(List[1]) - 1;
      ParagraphCount := List.Count;
    end;
end;

function TUnboundMemo.Paragraphs: integer;
var
  i : integer = 0;
  ps : string;
begin
  repeat
    i += 1;
    ps := iif(i = 1, '', LineBreak);
    ps += ' ' + ToStr(i) + ' ';
  until UTF8Pos(ps, Text) = 0;
  Result := i-1;
end;

procedure TUnboundMemo.SelectParagraph(n: integer);
var
  ps : string;
  x : integer;
begin
  ps := LineBreak + ' ' + ToStr(n) + ' ';
  x := UTF8Pos(ps, Text);
  if (x = 0) and (n <> 1) then Exit;
  SetSel(x, x+1);
end;

function TUnboundMemo.GetStartSelection: integer;
var
  i, temp : integer;
begin
  temp := SelStart;
     i := SelStart;

  SetSel(i-1,i);
  while (SelText <> ' ') and (i > 0)  do
    begin
      dec(i);
      SetSel(i-1,i);
    end;

  Result := i;
  SetSel(temp, temp);
end;

function TUnboundMemo.GetEndSelection: integer;
var
  i, len, temp : integer;
begin
  temp := SelStart;
     i := SelStart;
   len := i + 50;

  SetSel(i,i+1);
  while (Utf8LowerCase(SelText) <> Utf8UpperCase(SelText)) and (i < len) do
    begin
      inc(i);
      SetSel(i,i+1);
    end;

  Result := i;
  SetSel(temp, temp);
end;

procedure TUnboundMemo.SelectWord;
begin
  SetSel(GetStartSelection, GetEndSelection);
end;

procedure TUnboundMemo.SelectAll;
begin
  Hide_Selection;
  inherited;
  GetParagraphRange;
  DoParagraphChange;
  Show_Selection;
end;

procedure TUnboundMemo.SaveSelection;
begin
  SelStartTemp  := SelStart;
  SelLengthTemp := SelLength;
end;

procedure TUnboundMemo.RestoreSelection;
begin
  SelStart  := SelStartTemp;
  SelLength := SelLengthTemp;
end;

procedure TUnboundMemo.LoadText(Source: string; jtag: boolean = false);
begin
  if not jtag then
    begin
      Replace(Source, '<J>','');
      Replace(Source,'</J>','');
    end;
  {$ifdef windows}
    LoadRichText(ParseWin(Source, Font));
  {$else}
    Parse(Self, Source);
    if ReadOnly then HideCursor;
  {$endif}
end;

procedure TUnboundMemo.LoadHtml(Source: string);
begin
  {$ifdef windows}
    LoadRichText(ParseWin(Source, Font, true));
  {$else}
    Parse(Self, Source, true);
    if ReadOnly then HideCursor;
  {$endif}
end;

procedure Register;
begin
  {$I unboundmemoicon.lrs}
  RegisterComponents('Common Controls',[TUnboundMemo]);
end;

end.

