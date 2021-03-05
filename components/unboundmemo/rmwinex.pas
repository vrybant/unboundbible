unit rmWinEx;

interface

uses
  Windows, RichEdit, RichMemo, RichMemoUtils, Win32RichMemoProc;

function GetSelectedTextAttributes(Handle: THandle): TFontParams;
function isSelectedTextLink(Handle: THandle): Boolean;
function GetSelectedParaAlignment(Handle: THandle): TParaAlignment;
function GetSelectedParaNumbering(Handle: THandle): TParaNumbering;
function Win32GetTextRange(Handle: THandle; Pos, Length: integer): string;

implementation

function GetSelectedTextAttributes(Handle: THandle): TFontParams;
begin
  RichEditManager.GetSelectedTextStyle(Handle, Result);
end;

function GetTextUIStyle(Handle: THandle): TTextUIParam;
var
  fmt : TCHARFORMAT2;
begin
  FillChar(Result, SizeOf(Result), 0);
  FillChar(fmt, SizeOf(fmt), 0);
  fmt.cbSize := SizeOf(fmt);
  fmt.dwMask := CFM_LINK;
  SendMessage(Handle, EM_GETCHARFORMAT, SCF_SELECTION, PtrInt(@fmt));
  if fmt.dwEffects and CFE_LINK > 0 then Include(Result.features, uiLink);
end;

function isSelectedTextLink(Handle: THandle): Boolean;
begin
  Result := uiLink in GetTextUIStyle(Handle).features;
end;

function GetSelectedPara2(Handle: THandle): PARAFORMAT2;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(Result);
  SendMessagea(Handle, EM_GETPARAFORMAT, 0, PtrInt(@Result));
end;

function GetSelectedParaAlignment(Handle: THandle): TParaAlignment;
var
  pf : PARAFORMAT2;
begin
  Result := paLeft;
  pf := GetSelectedPara2(Handle);

  case pf.wAlignment of
    PFA_CENTER:  Result := paCenter;
    PFA_RIGHT:   Result := paRight;
    PFA_JUSTIFY: Result := paJustify;
  end;
end;

function GetSelectedParaNumbering(Handle: THandle): TParaNumbering;
var
  pf : PARAFORMAT2;
const
  TwipsInPoint = 20;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Style := pnNone;

  pf := GetSelectedPara2(Handle);

  case pf.wNumbering of
    1 : Result.Style := pnBullet;
    2 : Result.Style := pnNumber;
    3 : Result.Style := pnLowLetter;
    4 : Result.Style := pnLowRoman;
    5 : Result.Style := pnUpLetter;
    6 : Result.Style := pnUpRoman;
    7 : Result.Style := pnCustomChar;
  end;

  if Result.Style = pnCustomChar then Result.CustomChar := WideChar(pf.wNumberingStart);

  if pf.wNumberingStyle or PFNS_PLAIN  > 0 then Result.SepChar := SepNone;
  if pf.wNumberingStyle or PFNS_PERIOD > 0 then Result.SepChar := SepDot;
  if ((pf.wNumberingStyle and PFNS_SOMESEPCHAR)= 0) and (Result.Style<>pnNone) then Result.SepChar := SepPar;

  Result.Indent := pf.wNumberingTab/TwipsInPoint;
end;

function Win32GetTextRange(Handle: THandle; Pos, Length: integer): string;
var
  TextRange : RichEdit.TEXTRANGEW;
  w : UnicodeString;
  res : LResult;
begin
  FillChar(TextRange, sizeof(TextRange), 0);
  TextRange.chrg.cpMin := Pos;
  TextRange.chrg.cpMax := Pos + Length;
  SetLength(w, Length);
  TextRange.lpstrText := @w[1];
  res := SendMessage(Handle, EM_GETTEXTRANGE, 0, PtrInt(@TextRange));
  Result := UTF8Encode(Copy(w, 1, res));
end;

end.

