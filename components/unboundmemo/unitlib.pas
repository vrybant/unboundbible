unit UnitLib;

{$mode delphi}{$H+}

interface

uses
  {$ifdef windows} Windows, Windirs, {$endif}
  {$ifdef linux} LazLogger, {$endif}
  SysUtils, StrUtils, Classes, Variants, Graphics, Controls, Forms, Dialogs,
  LazUtf8, LCLProc, LCLVersion, ExtCtrls, ClipBrd, Process, UTF8Process;

type
  TIntegerArray = array of integer;
   TStringArray = array of string;
  TStringsArray = array of TStringArray;

// string's functions

function IsNumeral(c: char): boolean;
function IsAnsiLetter(c: char): boolean;
function Prefix(ps, st: string): boolean;
function Suffix(ps, st: string): boolean;
function ToInt(s: string): integer;
function ToStr(value: variant): string;
function ToBoolean(s: string): boolean;
function Capitalize(st: string): string;
function DoubleQuotedStr(s: string): string;
function CleanString(s: string): string;
function StringPos(subst: string; s: string): TIntegerArray;
procedure Replace(var s: string; const oldPattern, newPattern: string);
procedure CutStr(var s: string; StartSt, EndSt: string);
procedure RemoveDoubleSpaces(var s: string);
function RemoveTags(s: string): string;
function RemoveCRLF(s: string): string;
function iif(condition: boolean; trueResult, falseResult: variant): variant;

// unicode

function Utf8ToRTF(const s: string): string;

// arrays

function StringToList(s: string; separator: char): TStringArray;
function ListToString(const List: TStringArray; separator: string = ''): string;
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
function IsRightToLeft(language: string): boolean;
function Cyrillic(language: string): boolean;

// graphics

function WidthInPixels(Font: TFont; s: string): integer;
function IsDarkTheme: boolean;

// debug

procedure Output(s: string); overload;
procedure Output(n: integer); overload;

const
  Slash = DirectorySeparator;

const
  clBlueScreen  = TColor($bb4700); // #0047bb
  clGreenScreen = TColor($40b100); // #00b140
  clBrown = TColor($336699);       // apple brown

implementation

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
  Result := Pos(ps, st) = Length(st) - Length(ps) + 1;
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
  if VarIsBool(value) then Result := iif(value, 'True', 'False')
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

function DoubleQuotedStr(s: string): string;
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
  while Pos(StartSt, s) > 0 do
    begin
      x1 := Pos(StartSt,s);
      x2 := Pos(  EndSt,s);
      if x2 < x1 then Exit;
      len := x2-x1+Length(EndSt);
      Delete(s, x1, len);
    end;
end;

function RemoveCRLF(s: string): string;
begin
  Replace(s, #10, ''); // line feed
  Replace(s, #13, ''); // carriage return
  Result := s;
end;

function iif(condition: boolean; trueResult, falseResult: variant): variant;
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

function StringToList(s: string; separator: char): TStringArray;
var
  Point : array of integer = [];
  index : integer = 0;
  n : integer = 0;
  i, len : integer;
begin
  SetLength(Result,Length(s)+2);
  SetLength(Point ,Length(s)+2);
  Point[0] := 0;

  for i:=1 to Length(s) do
    if s[i] = separator then
      begin
        Inc(n);
        Point[n] := i;
      end;

  Point[n+1] := Length(s)+1;

  for i:=0 to n do
    begin
      len := Point[i+1]-Point[i]-1;
      if len > 0 then
        begin
          Result[index] := Copy(s, Point[i]+1, len);
          Inc(index);
        end;
    end;

  SetLength(Result,index);
end;

function ListToString(const List: TStringArray; separator: string = ''): string;
var i : integer;
begin
  Result := '';
  for i:=0 to Length(List)-1 do
    begin
      Result += List[i];
      if i <> Length(List) then Result += separator;
    end;
end;

function ListToArray(const List: TStringList): TStringArray;
var i : integer;
begin
  SetLength(Result, List.Count);
  for i:=0 to List.Count-1 do
    Result[i] := List[i];
end;

function XmlToList(s: string): TStringArray;
var
  temp : string = '';
  i : integer = 0;
  c : char;
begin
  SetLength(Result,Length(s)+1);

  for c in s do
    begin
      if c = '<' then
        begin
          Result[i] := temp;
          inc(i);
          temp := '';
        end;

      temp := temp + c;

      if c = '>' then
        begin
          Result[i] := temp;
          inc(i);
          temp := '';
        end;
    end;

  if temp <> '' then
    begin
      Result[i] := temp;
      inc(i);
    end;

  SetLength(Result,i);
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

function IsRightToLeft(language: string): boolean;
begin
   Result := false;
   if Prefix('he',language) or
      Prefix('ar',language) or
      Prefix('fa',language) then Result := true;
end;

function Cyrillic(language: string): boolean;
begin
   Result := false;
   if Prefix('ru',language) or
      Prefix('uk',language) or
      Prefix('bg',language) then Result := true;
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
  function Grayscale(C: TColor): double;
  begin
    Result:= Red(C)*0.3 + Green(C)*0.59 + Blue(C)*0.11;
  end;
begin
  Result:= Grayscale(ColorToRGB(clWindow)) < Grayscale(ColorToRGB(clWindowText));
end;

// debug

procedure Output(s: string);
begin
  {$ifdef windows} OutputDebugString(PChar(s)); {$endif}
  {$ifdef linux} DebugLn(s); {$endif}
end;

procedure Output(n: integer);
begin
  Output(ToStr(n));
end;

end.

