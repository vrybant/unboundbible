unit UnitLib;

{-define debugmode}
{$ifdef unix} {$undef RussianEdition} {$endif}

interface

uses
  {$ifdef windows} Windows, Windirs, {$endif}
  {$ifdef debugmode and linux} LazLogger, {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  { FileUtil depreciated } LazFileUtils, LazUtf8, LCLProc,
  ExtCtrls, ClipBrd, Process;

type
  TStringArray  = array of string;
  TIntegerArray = array of integer;

  TRange = record
    from : integer;
    till : integer;
  end;

const
  AppName = 'Unbound Bible';
  TitleDirectory = 'titles';
  VersionInfo = '2.01';

// string's functions

function IsNumeral(c: char): boolean;
function IsLetter(c: char): boolean;
function Prefix(ps, st: string): boolean;
function OneUpCase(st: string): string;
function MyStrToInt(st: string): integer;
function CleanString(s: string): string;
function StringPos(subst: string; s: string): TIntegerArray;
procedure Replace(var s: string; const oldPattern, newPattern: String);
function StringToList(ch: Char; st: string): TStringArray;
function CleanTags(s: string): string;
procedure DeleteTags(var s: string);

// сlipboard's function

{$ifdef windows} procedure StreamToClipboard(Stream : TMemoryStream); {$endif}

// file's functions

function ExtractOnlyName(s: string): string;
procedure GetFileList(const Path: string; const List: TStrings; Ext: boolean);
function AppLocation: string;
function UserDocumentsPath: string;
function AppDataPath: string;
function ConfigFile: string;
function TempFileName: string;
procedure CreateDirectories;
procedure OpenFolder(path: string);
{$ifdef darwin} procedure PrintFile(FileName : string); {$endif}

// language functions

function GetDefaultLanguage: string;
function GetRightToLeft(language: string): boolean;

// system's functions

function WidthInPixels(s: string): integer;
procedure Output(s: string); overload;
procedure Output(n: integer); overload;


var
  CurrFont: TFont;
  facelang : string;

const
  Slash = DirectorySeparator;
  CRLF = #13 + #10;

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

function Prefix(ps, st: string): boolean;
begin
  Result := Pos(ps, st) = 1;
end;

function OneUpCase(st: string): string;
var
  st_up : string;
begin
  st_up := UpperCase(st);
  st[1] := st_up[1];
  Result := st;
end;

function MyStrToInt(st: string): integer;
var v, r : integer;
begin
  st := Trim(st);
  Val(st, v, r);
  if r=0 then Result := v else Result := 0;
end;

function CleanString(s: string): string;
var i: integer;
begin
  for i:=1 to Length(s) do
    if not IsLetter(s[i]) then s[i] := ' ';
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

procedure Replace(var s: string; const oldPattern, newPattern: string);
begin
  s := StringReplace(s, oldPattern, newPattern, [rfReplaceAll]);
end;

function StringToList(ch: Char; st: string): TStringArray;
var
  Point : array of integer;
  i, n, index, len : integer;
begin
  SetLength(Result,Length(st)+2);
  SetLength(Point ,Length(st)+2);
  Point[0] := 0;
  n := 0;

  for i:=1 to Length(st) do
    if st[i] = ch then
      begin
        Inc(n);
        Point[n] := i;
      end;

  Point[n+1] := Length(st)+1;
  index := 0;

  for i:=0 to n do
    begin
      len := Point[i+1]-Point[i]-1;
      if len > 0 then
        begin
          Result[index] := Copy(st, Point[i]+1, len);
          Inc(index);
        end;
    end;

  SetLength(Result,index);
end;

function CleanTags(s: string): string;
var
  i : integer;
  l : boolean;
begin
  l := True;
  for i:=1 to Length(s) do
    begin
      if s[i]='<' then l := False;
      if not l then s[i] := ' ';
      if s[i]='>' then l := True;
    end;
  Result := s;
end;

procedure DeleteTags(var s: string);
var
  result : string;
  i : integer;
  l : boolean;
begin
  result := '';
  l := True;
  for i:=1 to Length(s) do
    begin
      if s[i]='<' then l := False;
      if l then result := result + s[i];
      if s[i]='>' then l := True;
    end;
  s := result;
end;

// сlipboard's function

{$ifdef windows}
procedure StreamToClipboard(Stream : TMemoryStream);
var
  Clipboard : TClipBoard;
     CF_RTF : Word;
begin
  Clipboard := TClipboard.Create ;
  CF_RTF  := RegisterClipboardFormat('Rich Text Format');
  Clipboard.AddFormat(CF_RTF,Stream);
  Clipboard.Free ;
end;
{$endif}

// file's functions

function ExtractOnlyName(s: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(s,''));
end;

procedure GetFileList(const Path: string; const List: TStrings; Ext: boolean);
var
  SearchRec : TSearchRec;
  Res : integer;
  s : string;
begin
  Res  := SysUtils.FindFirst(Path, faAnyFile, SearchRec);

  while Res=0 do
    begin
      if Ext then s := SearchRec.Name
             else s := ExtractOnlyName(SearchRec.Name);

      if (SearchRec.Attr and faDirectory) = 0 then List.Add(s);
      Res := FindNext(SearchRec);
    end;

  SysUtils.FindClose(SearchRec);
end;

function AppLocation: string;
{$ifdef darwin} var n : integer; {$endif}
begin
  Result := Application.Location;

  {$ifdef linux}
    if Prefix('/usr',Result) then Result := '/usr/share/' + Application.Title + '/';
  {$endif}

  {$ifdef darwin}
  n := Pos('MacOS',Result);
  if n > 0 then Result := Copy(Result,1,n-1) + 'Resources';
  {$endif}
end;

function UserDocumentsPath: string;
begin
  {$ifdef windows} Result := GetWindowsSpecialDir(CSIDL_PERSONAL); {$endif}
  {$ifdef linux  } Result := GetEnvironmentVariableUTF8('HOME') + Slash; {$endif}
  {$ifdef darwin } Result := GetEnvironmentVariableUTF8('HOME') + Slash + 'Library'; {$endif}
end;

function AppDataPath: string;
begin
 Result := UserDocumentsPath + AppName;
end;

function ConfigFile: string;
begin
  Result := GetAppConfigDir(False) + 'config.ini';
end;

function TempFileName: string; // for printing
begin
  Result := AppDataPath + Slash + 'temp.rtf';
end;

procedure CreateDirectories;
var
  dir : string;
begin
  dir := AppDataPath;
  if not DirectoryExists(dir) then ForceDirectories(dir);

  {$ifdef darwin}
  dir := ExtractFilePath(GetAppConfigFile(False));
  if not DirectoryExists(dir) then ForceDirectories(dir);
  {$endif}
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

function GetDefaultLanguage: string;
begin
  Result := 'english';

  {$ifdef windows}
  case Lo(GetUserDefaultLangID) of
    LANG_RUSSIAN   : Result := 'russian';
    LANG_SPANISH   : Result := 'spanish';
    LANG_ITALIAN   : Result := 'italian';
    LANG_FINNISH   : Result := 'finnish';
//  LANG_POLISH    : Result := 'polish';
//  LANG_FRENCH    : Result := 'french';
//  LANG_GERMAN    : Result := 'german';
    LANG_UKRAINIAN : Result := 'ukrainian';
  end;
  {$endif}

  {$ifdef unix}
    {$ifdef RussianEdition} Result := 'russian'; {$endif}
  {$endif}
end;

function GetRightToLeft(language: string): boolean;
begin
   Result := false;
   if Prefix('he' ,language) or
      Prefix('ara',language) or
      Prefix('fa' ,language) then Result := true;
end;

// system's functions

function WidthInPixels(s: string): integer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font := CurrFont;
    Result := Bitmap.Canvas.TextWidth(s);
  finally
    Bitmap.Free;
  end;
end;

procedure Output(s: string);
begin
  {$ifdef debugmode}
    {$ifdef windows} OutputDebugString(PChar(s)); {$endif}
    {$ifdef linux} DebugLn(s); {$endif}
  {$endif}
end;

procedure Output(n: integer);
begin
  Output(IntToStr(n));
end;

initialization
  CurrFont := TFont.Create;
  CurrFont.Name := {$ifdef windows} 'Tahoma' {$else} 'default' {$endif};
  CurrFont.Size := {$ifdef darwin} 14 {$else} 12 {$endif};

finalization
  CurrFont.Free;

end.

