unit UnboundMemo;

interface

uses
  {$ifdef windows} Windows, {$endif} Forms, SysUtils, LResources,
  Classes, Graphics, Controls, ExtCtrls, LCLProc, LCLType, LazUTF8,
  RichMemo, RichMemoEx;

type
  TUnboundMemo = class(TRichMemoEx)
  protected
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp  (var Key: Word; Shift: TShiftState); override;
  private
    FLinkable : boolean;
    FParagraphic : boolean;
    SelStartTemp  : integer;
    SelLengthTemp : integer;
    {$ifdef unix} function  Colored: boolean; {$endif}
    function  GetColorLink: string;
    {$ifdef linux} function  IsStrong: boolean; {$endif}
    {$ifdef linux} function  GetStrongLink: string; {$endif}
    function  GetLink: string;
    function  GetParagraphNumber(Pos: integer; select: boolean): integer;
    procedure GetParagraphRange;
    function  GetStartSelection: integer;
    function  GetEndSelection: integer;
  public
    Hyperlink : string;
    ParagraphStart : integer;
    ParagraphCount : integer;
    {$ifdef linux} strongmode : boolean; {$endif}
    constructor Create(AOwner: TComponent); override;
    function Foreground: integer;
    procedure SelectParagraph(n : integer);
    procedure SelectWord;
    procedure SelectAll;
    procedure SaveSelection;
    procedure RestoreSelection;
    procedure LoadText(Source: string; jtag: boolean = false);
    procedure LoadHtml(Source: string);
  published
    property Linkable    : boolean read FLinkable    write FLinkable    default False;
    property Paragraphic : boolean read FParagraphic write FParagraphic default False;
  end;

const
  fgText     = 0;
  fgLink     = 1;
  fgStrong   = 2;
  fgFootnote = 3;

procedure Register;

implementation

uses ParseWin;

function ToInt(s: string): integer;
var v, r : integer;
begin
  s := Trim(s);
  Val(s, v, r);
  if r=0 then Result := v else Result := 0;
end;

function RemoveCRLF(s: string): string;
const
  CharLF = #10; // line feed
  CharCR = #13; // carriage return
begin
  s := StringReplace(s, CharLF, '', [rfReplaceAll]);
  s := StringReplace(s, CharCR, '', [rfReplaceAll]);
  Result := s;
end;

constructor TUnboundMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Hyperlink := '';
  ParagraphStart := 0;
  ParagraphCount := 0;
  SelStartTemp := 0;
  SelLengthTemp := 0;
  Cursor := crArrow;

  {$ifdef windows} if Font.Name = 'default' then Font.Name := 'Tahoma'; {$endif}
  {$ifdef linux} strongmode := false; {$endif}
end;

function TUnboundMemo.Foreground: integer;
const
  clBrown = TColor($336699); // apple brown
begin
  Result := fgText;

  case SelAttributes.Color of
    clNavy  : Result := fgLink;
    clBrown : Result := fgStrong;
    clTeal  : Result := fgFootnote;
  end;

end;

{$ifdef unix}
function TUnboundMemo.Colored: boolean;
begin
  Result := Foreground = fgLink;
end;
{$endif}

function TUnboundMemo.GetColorLink: string;
var
  fore : integer;
  x1,x2,x0 : integer;
  n1,n2 : integer;
begin
  Result := '';
  if SelLength > 0 then Exit;

  fore := Foreground;
  if fore = fgText then Exit;
  GetSel(n1{%H-},n2{%H-});

  x0 := SelStart;
  x1 := x0;
  repeat
    dec(x1);
    SetSel(x1, x1);
  until (Foreground <> fore) or (x1 < 0);

  inc(x1);
  if x1 < 0 then inc(x1);

  x2 := x0;
  repeat
    inc(x2);
    SetSel(x2, x2);
  until Foreground <> fore;

  SetSel(x1, x2); Result := RemoveCRLF(SelText);
  SetSel(n1, n2); Result := Trim(Result);
end;

{$ifdef linux}
function TUnboundMemo.IsStrong: boolean;
var c : char;
begin
  Result := false;
  if SelLength <> 1 then Exit;
  c := SelText[1];
  Result := (c in ['0'..'9']) or (c in ['H','9']);
end;

function TUnboundMemo.GetStrongLink: string;
var
  x1,x2,x0 : integer;
  n1,n2 : integer;
  quit : boolean;
begin
  Result := '';
  if SelLength > 0 then Exit;

  GetSel(n1{%H-},n2{%H-});
  SetSel(n1, n2+1);
  quit := not IsStrong;
  SetSel(n1, n2);
  if quit then Exit;

  x0 := SelStart;
  x1 := x0;
  repeat
    dec(x1);
    SetSel(x1, x1+1);
  until not IsStrong or (x1 < 0);

  inc(x1);
  if x1 < 0 then inc(x1);

  x2 := x0;
  repeat
    inc(x2);
    SetSel(x2, x2+1);
  until not IsStrong;

  SetSel(x1, x2); Result := RemoveCRLF(SelText);
  SetSel(n1, n2); Result := Trim(Result);
end;
{$endif}

function TUnboundMemo.GetLink: string;
begin
  if Foreground <> fgText then Result := GetColorLink;
  {$ifdef linux}
    if strongmode and (Foreground = fgText) then Result := GetStrongLink;
  {$endif}
end;

procedure TUnboundMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  x1 : integer = 0;
  x2 : integer = 0;
begin
  if Linkable then Hyperlink := GetLink else Hyperlink := '';

  if Paragraphic and (Button = mbLeft) then
    begin
      if Hyperlink <> '' then GetSel(x1,x2);
      GetParagraphRange;
      if Hyperlink <> '' then SetSel(x1,x2);
    end;

  inherited;
end;

procedure TUnboundMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$ifdef windows}
    if Linkable and not ReadOnly and (Key = VK_CONTROL) then ShowCaret(Handle);
  {$endif}
end;

{$ifdef windows}
function TUnboundMemo.GetParagraphNumber(Pos: integer; select: boolean): integer;
var
  x1, x2, ln : integer;
begin
  GetParaRange(Pos, x1{%H-}, ln{%H-});
  x2 := FindRightWordBreak(x1+1);
  Result := ToInt(GetTextRange(x1, x2-x1));
  if select then SetSel(x1,x1+1);
end;
{$endif}

{$ifdef unix}
function TUnboundMemo.GetParagraphNumber(Pos: integer; select: boolean): integer;
var
  x1, x2, ln : integer;
begin
  GetParaRange(Pos, x1{%H-}, ln{%H-});

  x2 := x1;
  repeat
    inc(x2);
    SetSel(x2, x2);
  until not Colored;

  SetSel(x1,x2); Result := ToInt(SelText);
  if select then SetSel(x1,x1+1);
end;
{$endif}

procedure TUnboundMemo.GetParagraphRange;
var
  ParagraphEnd : integer;
  x1, x2 : integer;
begin
  GetSel(x1{%H-},x2{%H-});

  ParagraphStart := GetParagraphNumber(x1, x1=x2);
  ParagraphCount := 1;

  if x1 <> x2 then
    begin
      ParagraphEnd := GetParagraphNumber(x2, false);
      ParagraphCount := ParagraphEnd - ParagraphStart + 1;
      {$ifdef unix} SetSel(x1,x2); {$endif}
    end;
end;

{$ifdef windows}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  w, line : string;
  i, len, x : integer;
begin
  HideSelection := False; // important

  w := ' ' + IntToStr(n) + ' ';
  len := length(w);

  for i:=0 to LineCount - 1 do
    begin
      line := Lines[i];

      if copy(line,1,len) = w then
         begin
           x := LineIndex(i);
           SetSel(x,x+1);
           HideCursor;
         end;
    end;

  ParagraphStart := n;
  ParagraphCount := 1;
end;
{$endif}

{$ifdef unix}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  i, x : integer;
  L : boolean;
begin
  L := False;
  x := 0;

  i := 0;
  while True do
    begin
      SetSel(i,i);
      if SelStart <> i then break;

      if Colored then
        begin
          if not L then
            begin
              inc(x);
              if x = n then
                begin
                  SetSel(i,i+1);
                  break;
                end;
            end;
          L := True;
        end;

      if not Colored then L := False;
      inc(i);
    end;

  SetFocus;
end;
{$endif}

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
  while (LowerCase(SelText) <> UpperCase(SelText)) and (i < len) do
    begin
      inc(i);
      SetSel(i,i+1);
    end;

  Result := i;
  SetSel(temp, temp);
end;

procedure TUnboundMemo.SelectWord;
begin
  SelStart  := GetStartSelection;
  SelLength := GetEndSelection - SelStart;
end;

procedure TUnboundMemo.SelectAll;
begin
  Hide_Selection;
  inherited;
  GetParagraphRange;
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
  LoadRichText(Parse(Source, Font, jtag, false));
end;

procedure TUnboundMemo.LoadHtml(Source: string);
begin
  LoadRichText(Parse(Source, Font, false, true));
end;

procedure Register;
begin
  {$I unboundmemoicon.lrs}
  RegisterComponents('Common Controls',[TUnboundMemo]);
end;

end.

