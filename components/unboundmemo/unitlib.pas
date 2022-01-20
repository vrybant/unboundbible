unit UnitLib;

{$mode delphi}{$modeswitch typehelpers}{$H+}

interface

uses
  {$ifdef windows} Windows, Windirs, {$endif}
  {$ifdef linux} LazLogger, {$endif}
  SysUtils, StrUtils, Classes, Variants, Graphics, Controls, Forms, Dialogs,
  LazUtf8, LCLProc, LCLVersion, ExtCtrls, ClipBrd, Process, UTF8Process;

type
  TIntegerArray = array of integer;
  TStringsArray = array of TStringArray;

type
  TStringArrayHelper = type Helper for TStringArray
  public
    function IsEmpty: Boolean;
    procedure Add(const Value: string);
    procedure Delete(index: integer);
    function Count: integer;
    function IndexOf(const s: string): integer;
    function Reverse: TStringArray;
  end;

// string's functions

function IsNumeral(c: char): boolean;
function IsAnsiLetter(c: char): boolean;
function Prefix(ps, st: string): boolean;
function Suffix(ps, st: string): boolean;
function ToInt(s: string): integer;
function ToStr(value: variant): string;
function ToBoolean(s: string): boolean;
function Capitalize(st: string): string;
function DoubleQuoted(s: string): string;
function CleanString(s: string): string;
function StringPos(subst: string; s: string): TIntegerArray;
procedure Replace(var s: string; const oldPattern, newPattern: string);
procedure CutStr(var s: string; StartSt, EndSt: string);
procedure RemoveDoubleSpaces(var s: string);
function RemoveTags(s: string): string;
function RemoveLineBreaker(s: string): string;
function iif(const condition: boolean; trueResult, falseResult: variant): variant;

// unicode

function Utf8ToRTF(const s: string): string;

// arrays

function ListToArray(const List: TStringList): TStringArray;
function XmlToList(s: string): TStringArray;

// сlipboard

{$ifdef windows} procedure RichTextToClipboard(source: string; text: string); {$endif}

// file's functions

function ExtractOnlyName(s: string): string;
function SharePath: string;
function DocumentsPath: string;
{$ifdef windows} function LocalAppDataPath: string; {$endif}
function TempFileName: string;
function GetFileList(const Path, Mask: string) : TStringArray;
procedure OpenFolder(path: string);
{$ifdef darwin} procedure PrintFile(FileName : string); {$endif}
function StringsFromFile(filename : string): TStringArray;
procedure StringsToFile(filename: string; Strings: TStringArray);

// language functions

function GetLanguageID: string;
function GetLanguageIDs: string;
function IsRightToLeft(language: string): boolean;

// graphics

function WidthInPixels(Font: TFont; s: string): integer;
function IsDarkTheme: boolean;

// system colors

function clSysBrown: TColor;
function clSysMaroon: TColor;
function clSysNavy: TColor;
function clSysTeal: TColor;

// debug

procedure Output(s: string); overload;
procedure Output(n: integer); overload;

const
  clBlueScreen = TColor($BB4700);
  clGreenScreen = TColor($40B100);
  clSysGray = clGray;
  clSysRed = clRed;

const
  Slash = DirectorySeparator;
  LineBreaker = {$ifdef windows}#13{$else}#10{$endif};
  AcuteChar = #$CC#$81;

implementation

var DarkTheme : boolean = False;

// helper

function TStringArrayHelper.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TStringArrayHelper.Count: integer;
begin
  Result := Length(Self);
end;

procedure TStringArrayHelper.Add(const Value: string);
begin
  SetLength(Self, Count+1);
  Self[Count-1] := Value;
end;

procedure TStringArrayHelper.Delete(index: integer);
var
  List : TStringArray = [];
  i : integer;
begin
  for i:=0 to Count-1 do
    if i <> index then List.Add(Self[i]);
  Self := List;
end;

function TStringArrayHelper.IndexOf(const s: string): integer;
begin
  Result := 0;
  while (Result < Count) and (Self[Result] <> s) do Result += 1;
  if Result=Count then Result := -1;
end;

function TStringArrayHelper.Reverse: TStringArray;
var
  i : integer;
begin
  Result := [];
  for i:= Count-1 downto 0 do
    Result.Add(Self[i]);
end;

// string's functions

function IsNumeral(c: char): boolean;
begin
  Result := c in ['0'..'9'];
end;

function IsAnsiLetter(c: char): boolean;
begin
  Result := ord(c) > 64;  // A-65
end;

function Prefix(ps, st: string): boolean;
begin
  Result := Pos(ps, st) = 1;
end;

function Suffix(ps, st: string): boolean;
begin
  Result := ps = Copy(st, Length(st) - Length(ps) + 1);
end;

function ToInt(s: string): integer;
var v, r : integer;
begin
  s := Trim(s);
  Val(s, v, r);
  if r=0 then Result := v else Result := 0;
end;

function ToStr(value: variant): string;
var lg : longint;
begin
  lg := value;
  System.Str(lg, Result);
  if VarIsBool(value) then Result := BoolToStr(value,True);
end;

function ToBoolean(s: string): boolean;
var v : boolean;
begin
  Result := false;
  if TryStrToBool(s,v) then Result := v;
end;

procedure RemoveDoubleSpaces(var s: string);
begin
  s := DelSpace1(s);
end;

function RemoveTags(s: string): string;
var
  l : boolean = true;
  i : integer;
begin
  Result := '';
  for i:=1 to Length(s) do
    begin
      if s[i]='<' then l := False;
      if l then Result += s[i];
      if s[i]='>' then l := True;
    end;
end;

function Capitalize(st: string): string;
var
  st_up : string;
begin
  st_up := UpperCase(st);
  st[1] := st_up[1];
  Result := st;
end;

function DoubleQuoted(s: string): string;
begin
  Result := '"' + s + '"';
end;

function CleanString(s: string): string;
var i: integer;
begin
  for i:=1 to Length(s) do
    if not IsAnsiLetter(s[i]) and not IsNumeral(s[i]) then s[i] := ' ';
  Result := s;
end;

function StringPos(subst: string; s: string): TIntegerArray;
var
  i,k,n : integer;
begin
  SetLength(Result, Length(s));

  k := 0;
  n := Pos(subst,s);

  while n > 0 do
    begin
      Result[k] := n;
      Inc(k);
      for i:=n to n+Length(subst)-1 do s[i] := ' ';
      n := Pos(subst,s);
    end;

  SetLength(Result,k);
end;

procedure Replace(var s: string; const oldPattern, newPattern: string);
begin
  s := StringReplace(s, oldPattern, newPattern, [rfReplaceAll]);
end;

procedure CutStr(var s: string; StartSt, EndSt: string);
var
  x1,x2,len : integer;
begin
  while s.Contains(StartSt) do
    begin
      x1 := Pos(StartSt,s);
      x2 := Pos(  EndSt,s);
      if x2 < x1 then Exit;
      len := x2-x1+Length(EndSt);
      Delete(s, x1, len);
    end;
end;

function RemoveLineBreaker(s: string): string;
begin
  Replace(s, #10, ''); // line feed
  Replace(s, #13, ''); // carriage return
  Result := s;
end;

function iif(const condition: boolean; trueResult, falseResult: variant): variant;
begin
  if condition then Result := trueResult else Result := falseResult;
end;

// unicode

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

// arrays

function ListToArray(const List: TStringList): TStringArray;
var i : integer;
begin
  SetLength(Result, List.Count);
  for i:=0 to List.Count-1 do Result[i] := List[i];
end;

function XmlToList(s: string): TStringArray;
begin
  Result := s.Replace('<', #10'<').Replace('>', '>'#10).Split(#10);
end;

// сlipboard

{$ifdef windows}
procedure RichStreamToClipboard(Stream: TMemoryStream; text: string);
var
  Clipboard : TClipboard;
     CF_RTF : Word;
begin
  Clipboard := TClipboard.Create ;
  Clipboard.AsText:= text;
  CF_RTF := RegisterClipboardFormat('Rich Text Format');
  Clipboard.AddFormat(CF_RTF,Stream);
  Clipboard.Free ;
end;

procedure RichTextToClipboard(source: string; text: string);
var
  Stream : TMemoryStream;
begin
  source := Utf8ToRTF(source) + LineEnding;
  Stream := TMemoryStream.Create;
  Stream.Seek(0,soFromBeginning);
  Stream.WriteBuffer(Pointer(source)^, Length(source));
  RichStreamToClipboard(Stream, text);
  Stream.Free;
end;
{$endif}

// file's functions

function ExtractOnlyName(s: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(s,''));
end;

function SharePath: string;
begin
  Result := Application.Location;
  {$ifdef linux}
    if Prefix('/usr',Result) then Result := '/usr/share/' + Application.Title + '/';
  {$endif}
end;

function DocumentsPath: string;
begin
  {$ifdef windows} Result := GetWindowsSpecialDir(CSIDL_PERSONAL); {$endif}
  {$ifdef unix} Result := GetUserDir + 'Documents'; {$endif}
end;

{$ifdef windows}
function LocalAppDataPath: string;
begin
  Result := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA);
end;
{$endif}

function TempFileName: string; // for printing
begin
  Result := GetTempDir + 'temp.rtf';
end;

function GetFileList(const Path, Mask: string): TStringArray;
var
  List : TStringList;
  SearchRec : TSearchRec;
  Res : integer;
  s : string;
begin
  List := TStringList.Create;
  Res  := SysUtils.FindFirst(Path + slash + Mask, faAnyFile, SearchRec);

  while Res=0 do
    begin
      s := Path + slash + SearchRec.Name;
      if (SearchRec.Attr and faDirectory) = 0 then List.Add(s);
      Res := FindNext(SearchRec);
    end;

  SysUtils.FindClose(SearchRec);
  Result := ListToArray(List);
  List.Free;
end;

procedure OpenFolder(path : string);
begin
 with TProcessUTF8.Create(nil) do
  try
    {$ifdef windows} Executable := 'explorer'; {$endif}
    {$ifdef darwin } Executable := 'open ';    {$endif}
    {$ifdef linux  } Executable := 'xdg-open'; {$endif}
    Parameters.Add(path);
    Options := [poUsePipes];
    try
      Execute;
    except
      on EProcess do ShowMessage('Oops! Looks like it can''t be opened.');
    end;
  finally
    Free;
  end;
end;

{$ifdef darwin}
procedure PrintFile(filename : string);
begin
  with TProcessUTF8.Create(nil) do
  try
    CommandLine {%H-}:='lp "' + filename + '"';
    Options := [poUsePipes]; // poWaitOnExit
    try
      Execute;
    except
      on EProcess do ShowMessage('Oops! Looks like it can''t be printed.');
    end;
  finally
    Free;
  end;
end;
{$endif}

function StringsFromFile(filename: string): TStringArray;
var
  List : TStringList;
begin
  List := TStringList.Create;
  List.LoadFromFile(filename);
  Result := ListToArray(List);
  List.Free;
end;

procedure StringsToFile(filename: string; Strings: TStringArray);
var
  List : TStringList;
  item : string;
begin
  List := TStringList.Create;
  for item in Strings do List.Add(item);
  List.SaveToFile(filename);
  List.Free;
end;

// language functions

function GetLanguageID: string;
begin
  LazGetShortLanguageID(Result{%H-});
end;

function GetLanguageIDs: string;
var s: string;
begin
  LazGetLanguageIDs(Result{%H-}, s{%H-});
  {$ifdef linux} Replace(Result,'.UTF-8',''); {$endif}
end;

function IsRightToLeft(language: string): boolean;
begin
   Result := false;
   if Prefix( 'he',language) or
      Prefix('ara',language) or
      Prefix( 'fa',language) then Result := true;
end;

// graphics

function WidthInPixels(Font: TFont; s: string): integer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font := Font;
    Result := Bitmap.Canvas.TextWidth(s);
  finally
    Bitmap.Free;
  end;
end;

function IsDarkTheme: boolean;
  function Grayscale(C: TColor): longint;
  begin
    C := ColorToRGB(C);
    Result := Red(C)*3 + Green(C)*6 + Blue(C);
  end;
begin
  Result:= Grayscale(clWindow) < Grayscale(clWindowText);
end;

// system colors

function clSysBrown: TColor;
begin
  Result := iif(DarkTheme, TColor($1F97FD), TColor($336699));
end;

function clSysMaroon: TColor;
begin
  Result := iif(DarkTheme, TColor($4242E5), clMaroon);
end;

function clSysNavy: TColor;
begin
  Result := iif(DarkTheme, TColor($FF9900), clNavy);
end;

function clSysTeal: TColor;
begin
  Result := iif(DarkTheme, TColor($CCCC66), clTeal);
end;

// debug

procedure Output(s: string);
begin
  Replace(s,#0,' ');
  {$ifdef windows} OutputDebugString(PChar(s)); {$endif}
  {$ifdef linux} DebugLn(s); {$endif}
end;

procedure Output(n: integer);
begin
  Output(ToStr(n));
end;

initialization

DarkTheme := IsDarkTheme;

end.

