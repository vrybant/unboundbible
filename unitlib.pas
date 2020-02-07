unit UnitLib;

interface

uses
  {$ifdef windows} Windirs, {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  LazUtf8, LCLProc, ExtCtrls, ClipBrd, Process, UmLib;

// string's functions

function IsNumeral(c: char): boolean;
function IsLetter(c: char): boolean;
function OneUpCase(st: string): string;
function DoubleQuotedStr(s: string): string;
function CleanString(s: string): string;
function StringPos(subst: string; s: string): TIntegerArray;
procedure CutStr(var s: string; StartSt, EndSt: string);
function StringToList(ch: Char; s: string): TStringArray;

// сlipboard's function

{$ifdef windows} procedure StringToClipboard(Source: string); {$endif}

// file's functions

function ExtractOnlyName(s: string): string;
function SharePath: string;
function DocumentsPath: string;
{$ifdef windows} function LocalAppDataPath: string; {$endif}
function TempFileName: string;
function GetFileList(const Path, Mask: string) : TStringArray;
procedure OpenFolder(path: string);
{$ifdef darwin} procedure PrintFile(FileName : string); {$endif}

// language functions

function GetLanguageID: string;
function IsRightToLeft(language: string): boolean;
function Cyrillic(language: string): boolean;

// system's functions

function WidthInPixels(Font: TFont; s: string): integer;

const
  Slash = DirectorySeparator;

implementation

// string's functions

function IsNumeral(c: char): boolean;
begin
  Result := c in ['0'..'9'];
end;

function IsLetter(c: char): boolean;
begin
  Result := ord(c) > 64;  // A-65
end;

function OneUpCase(st: string): string;
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
    if not IsLetter(s[i]) and not IsNumeral(s[i]) then s[i] := ' ';
  Result := s;
end;

function StringPos(subst: string; s: string): TIntegerArray;
var
  i,k,n : integer;
begin
  SetLength(Result,Length(s));

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

function StringToList(ch: Char; s: string): TStringArray;
var
  Point : array of integer;
  i, len : integer;
  index : integer = 0;
  n : integer = 0;
begin
  SetLength(Result,Length(s)+2);
  SetLength(Point ,Length(s)+2);
  Point[0] := 0;

  for i:=1 to Length(s) do
    if s[i] = ch then
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

// сlipboard's function

{$ifdef windows}
procedure StreamToClipboard(Stream: TMemoryStream);
var
  Clipboard : TClipBoard;
     CF_RTF : Word;
begin
  Clipboard := TClipboard.Create ;
  CF_RTF := RegisterClipboardFormat('Rich Text Format');
  Clipboard.AddFormat(CF_RTF,Stream);
  Clipboard.Free ;
end;

procedure StringToClipboard(Source: string);
var
  Stream : TMemoryStream;
begin
  Source := Utf8ToRTF(Source) + LineEnding;
  Stream := TMemoryStream.Create;
  Stream.Seek(0,soFromBeginning);
  Stream.WriteBuffer(Pointer(Source)^, Length(Source));
  StreamToClipboard(Stream);
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
 with TProcess.Create(nil) do
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
  with TProcess.Create(nil) do
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

// system's functions

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

end.

