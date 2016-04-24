unit UnitLib;

{$mode delphi}{$H+}

interface

uses
  {$ifdef mswindows} Windows, ShFolder, {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ClipBrd, FileUtil, LCLProc, Process; // UTF8Process

type
  TOptions = record
    cvAbbr  : boolean;
    cvEnd   : boolean;
    cvDelim : boolean;
    cvNum   : boolean;
    cvWrap  : boolean;
  end;

const
  AppName = 'Unbound Bible Tools';
  VersionInfo = '1.0';
  {$ifdef unix} RussianEdition = False{True}; {$endif}

const
  BibleDirectory = 'bibles';
  TitleDirectory = 'titles';
  VerseDirectory = 'verse';
//CommentaryDirectory = 'commentaries';

function IsNumeral(c: char): boolean;
function IsLetter(c: char): boolean;
function Marks(st: string): string;
function OneUpCase(st: string): string;
//function ReplaceSub(str, sub1, sub2: String; del: byte): String;
function MyStrToInt(st: string): integer;
function MyStrToSingle(st: string): Single;
function CountPos(sub, st: string): integer;

function WideTrim(const S: WideString): WideString;

procedure MyDelete(var s : AnsiString; index, count : Integer);
procedure StreamWrite  (var Stream: TMemoryStream; s: string);
procedure StreamWriteLn(var Stream: TMemoryStream; s: string);
{$ifdef mswindows} procedure StreamToClipboard(Stream : TMemoryStream); {$endif}
procedure Replace(var S: AnsiString; const OldPattern, NewPattern: AnsiString);
procedure StrToList(const st: AnsiString; const List: TStringList);
procedure StrToListEx(ch: AnsiChar; const st: AnsiString; const List: TStringList);
procedure ListToStr(const List: TStringList; var st: AnsiString);
procedure ListToStrEx(ch: AnsiChar; List: TStringList; var st: AnsiString);

function  ExtractOnlyName(s: string): string;
procedure GetFileList(const Path: AnsiString; const List: TStrings; Ext: boolean);
//function  UnicodeToRTF(const w: WideString): AnsiString;
function  Utf8ToRTF(const s: string): string;
function WideLowerCaseFixed(s : WideString): WideString;

function GetDefaultLanguage: string;
function AppPath: WideString;
function UserDocumentsPath : string;
function AppDataPath : WideString;
function IniFileName : WideString;
function TempFileName: WideString;

procedure CreateDirectories;
procedure PrintFile(FileName : string);
procedure OpenFolder(path : string);

var
  CurrFont: TFont;
  Options : TOptions;
  facelang : string;

const
  Slash = DirectorySeparator;
  CRLF = #13 + #10;

implementation

function IsNumeral(c: char): boolean;
begin
  Result := c in ['0'..'9'];
end;

function IsLetter(c: char): boolean;
begin
  Result := ord(c) > 64;  // A-65
end;

function Marks(st: string): string;
begin
  Result := '"' + st + '"';
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
var
  v : integer;
  r : integer;
begin
  st := Trim(st);
  Val(st, v, r);

  if r=0 then Result := v
         else Result := 0;
end;

function MyStrToSingle(st: string): Single;
var
  v : Single;
begin
  st := Trim(st);
  if TryStrToFloat(st, v) then Result := v
                          else Result := 0;
end;

function CountPos(sub, st: string): integer;
var
  n : integer;
begin
    Result := 0;

    n := Pos(sub, st);

    while n > 0 do
      begin
        Delete(st, 1, n + Length(sub) - 1);
        n := Pos(sub, st);
        inc(Result);
      end;
end;

function ReplaceSub(str, sub1, sub2: String; del: byte): String;
var
  aPos: Integer;
  rslt: String;
begin
    aPos := Pos(sub1, str);

    rslt := '';
    while (aPos <> 0) do begin
      rslt := rslt + Copy(str, 1, aPos - 1) + sub2;
      Delete(str, 1, aPos + Length(sub1) - 1 + del);
      aPos := Pos(sub1, str);
    end;

    Result := rslt + str;
end;

procedure MyDelete(var s : AnsiString; index, count : Integer);
begin
  Delete(s,index,count);
end;

procedure StreamWrite(var Stream: TMemoryStream; s: string);
begin
  Stream.WriteBuffer(Pointer(s)^, Length(s));
end;

procedure StreamWriteLn(var Stream: TMemoryStream; s: string);
begin
  s := s + CRLF;
  Stream.WriteBuffer(Pointer(s)^, Length(s));
end;

{$ifdef mswindows}
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

procedure Replace(var S: AnsiString; const OldPattern, NewPattern: AnsiString);
begin
  S := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll]);
end;

procedure StrToListEx(ch: AnsiChar; const st: AnsiString; const List: TStringList);
var
  p : array[1..10000] of integer;
  i : integer;
  n : integer;
  s : string;
begin
  List.Clear;

  p[1] := 0;
  n := 1;

  for i:=1 to length(st) do
    if st[i] = ch then
      begin
        inc(n);
        p[n] := i;
      end;

  inc(n);
  p[n] := length(st) + 1;

  for i:=1 to n-1 do
    begin
      s := copy(st,p[i]+1,p[i+1]-p[i]-1);
      s := Trim(s);
      List.Add(s);
    end;
end;

procedure StrToList(const st: string; const List: TStringList);
begin
  StrToListEx(chr(09), st, List);
end;

procedure ListToStrEx(ch: AnsiChar; List: TStringList; var st: AnsiString);
var i : integer;
begin
  st := '';

  for i:=0 to List.Count-1 do
    begin
      if st <> '' then st := st + ch;
      st := st + List[i];
    end;
end;

procedure ListToStr(const List: TStringList; var st: AnsiString);
begin
  ListToStrEx(chr(09), List, st);
end;

function WideTrim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function ExtractOnlyName(s: string): string;
begin
  Result := ExtractFileName(ChangeFileExt(s,''));
end;

procedure GetFileList(const Path: AnsiString; const List: TStrings; Ext: boolean);
var
  SRec : TSearchRec;
   Res : integer;
     s : string;
begin
  Res  := SysUtils.FindFirst(Path, faAnyFile, SRec);

  while Res=0 do
    begin
      if Ext then s := SRec.Name
             else s := ExtractOnlyName(SRec.Name);

      List.Add(s);
      Res := FindNext(SRec);
    end;

  SysUtils.FindClose(SRec);
end;

function UnicodeToRTF(const w: WideString): AnsiString;
var
  wc : Cardinal;
   i : integer;
begin
  Result := '';
  for i:=1 to Length(w) do
    begin
      wc := Ord(w[i]);
      if wc < $80 then Result := Result + w[i]
                  else Result := Result + '\u' + IntToStr(wc) + '?';
    end;
end;

function Utf8ToRTF(const s: string): string;
begin
  Result := UnicodeToRTF(Utf8Decode(s));
end;

function AppPath: WideString;
{$ifdef darwin} var n : integer; {$endif}
begin
  Result := Application.Location;
  {$ifdef darwin}
  n := Pos('MacOS',Result);
  if n > 0 then
    Result := Copy(Result,1,n-1) + 'Resources';
  {$endif}
end;

 (*
 CSIDL_PERSONAL, { My Documents }
 CSIDL_APPDATA, { Application Data }
 CSIDL_COMMON_APPDATA, { All Users\Application Data }
 CSIDL_WINDOWS, { GetWindowsDirectory() }
 CSIDL_SYSTEM,  { GetSystemDirectory() }
 CSIDL_PROGRAM_FILES, { C:\Program Files }
 CSIDL_MYPICTURES, { My Pictures }
 CSIDL_PROGRAM_FILES_COMMON, { C:\Program Files\Common }
 CSIDL_COMMON_DOCUMENTS, { All Users\Documents }
 *)

{$ifdef mswindows}
function GetSpecialFolderPath(FolderID: Cardinal): WideString; // UTF16
var
  s : PChar;
begin
 Result := '';
 GetMem(s, Max_Path);
 try
   SHGetFolderPath(0, FolderID, 0, 0, s);
   Result := s;
 finally
   FreeMem(s, Max_Path);
 end;
end;
{$endif}

function GetDefaultLanguage: string;
begin
  Result := 'english';

  {$ifdef mswindows}
  case Lo(GetSystemDefaultLangID) of
    LANG_RUSSIAN   : Result := 'russian';
    LANG_SPANISH   : Result := 'spanish';
    LANG_ITALIAN   : Result := 'italian';
    LANG_FINNISH   : Result := 'finnish';
//  LANG_POLISH    : Result := 'polish';
//  LANG_FRENCH    : Result := 'french';
//  LANG_GERMAN    : Result := 'german';
//  LANG_UKRAINIAN : Result := 'ukrainian';
  end;
  {$endif}

  {$ifdef unix}
  if RussianEdition then {%H-}Result := 'russian';
  {$endif}
end;

function UserDocumentsPath : string; // UTF8
begin
{$ifdef mswindows}
  Result := GetSpecialFolderPath(CSIDL_PERSONAL);
  Result := UTF16ToUTF8(Result);
{$else}
  Result := GetEnvironmentVariableUTF8('HOME');
{$endif}
end;

function AppDataFolder : WideString;
begin
{$ifdef mswindows}
  Result := GetSpecialFolderPath(CSIDL_APPDATA);
{$else}
  Result := GetEnvironmentVariableUTF8('HOME') + Slash + 'Library';
{$endif}
end;

function AppDataPath : WideString;
begin
  Result := AppDataFolder + Slash + AppName;
end;

function IniFileName: WideString; // UTF16
begin
{$ifdef mswindows}
  Result := AppDataPath + Slash + 'config.ini';
{$else}
  Result := GetAppConfigFile(False);
{$endif}
end;

function TempFileName: WideString; // for printing
begin
  Result := AppDataPath + Slash + 'temp.rtf';
end;

procedure CreateDirectories;
var
  dir : WideString;
begin
  dir := AppDataPath + Slash + BibleDirectory;
  if not DirectoryExists(dir) then ForceDirectories(dir);

//  dir := AppDataPath + Slash + CommentaryDirectory;
//  if not DirectoryExists(dir) then ForceDirectories(dir);

  {$ifdef darwin}
  dir := ExtractFilePath(GetAppConfigFile(False));
  if not DirectoryExists(dir) then ForceDirectories(dir);
  {$endif}
end;

{$ifdef mswindows}
function WideLowerCaseFixed(s : WideString): WideString;
begin
  Result := WideLowerCase(s);
end;
{$endif}

// A-Z = 1040-1071 / Russian Alphabet
// a-z = 1072-1103

{$ifdef unix}
function WideLowerCaseFixed(s : WideString): WideString;
var
  w : WideString;
  i,n : integer;
begin
  Result := s;
  w := WideLowerCase(s);

  for i:=1 to length(s) do
    begin
      n := ord(s[i]);
      if (n >= 1040) and (n <= 1071) then Result[i] := WideChar(n+32);
      if (n  < 1040) or  (n >  1103) then Result[i] := w[i];
    end;
end;
{$endif}

procedure PrintFile(filename : string);
begin
  with TProcess.Create(nil) do
  try
    {$ifdef mswindows} CommandLine {%H-}:= 'print ' + marks(filename); {$endif}
       {$ifdef darwin} CommandLine {%H-}:=    'lp ' + marks(filename); {$endif}
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

{$ifdef mswindows}
procedure OpenFolder(path : string);
begin
   ShellExecute(0,'open',PChar(marks(path)),'','',SW_SHOW);
end;
{$endif}

{$ifdef unix}
procedure OpenFolder(path : string);
begin
  with TProcess.Create(nil) do
  try
    CommandLine {%H-}:= 'open ' + marks(path);
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
{$endif}

initialization
  CurrFont := TFont.Create;
  CurrFont.Name := {$ifdef mswindows} 'Tahoma' {$else} 'default' {$endif};
  CurrFont.Size := {$ifdef mswindows} 12 {$else} 14 {$endif};

finalization
  CurrFont.Free;

end.
