unit UmLib;

interface

uses
  {$ifdef windows} Windows, Windirs, {$endif}
  {$ifdef linux} LazLogger, {$endif}
  SysUtils, StrUtils, Classes, Graphics, Controls, Forms, Dialogs,
  LazUtf8, LCLProc, ExtCtrls, ClipBrd, Process;

type
  TStringArray  = array of string;

// string's functions

function Prefix(ps, st: string): boolean;
function ToInt(s: string): integer;
function ToStr(value: longint): string;
procedure Replace(var s: string; const oldPattern, newPattern: string);
procedure DelDoubleSpace(var s: string);
function RemoveTags(s: string): string;
function RemoveCRLF(s: string): string;
function ListToString(const List: TStringArray): string;
function XmlToList(s: string): TStringArray;

// сlipboard's function

//{$ifdef windows} procedure StringToClipboard(Source: string); {$endif}

// system's functions

procedure Output(s: string); overload;
procedure Output(n: integer); overload;

const
  clBrown = TColor($336699); // apple brown

implementation

// string's functions

function Prefix(ps, st: string): boolean;
begin
  Result := Pos(ps, st) = 1;
end;

function ToInt(s: string): integer;
var v, r : integer;
begin
  s := Trim(s);
  Val(s, v, r);
  if r=0 then Result := v else Result := 0;
end;

function ToStr(value: longint): string;
begin
 System.Str(value, Result);
end;

procedure Replace(var s: string; const oldPattern, newPattern: string);
begin
  s := StringReplace(s, oldPattern, newPattern, [rfReplaceAll]);
end;

procedure DelDoubleSpace(var s: string);
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

function RemoveCRLF(s: string): string;
begin
  Replace(s, #10, ''); // line feed
  Replace(s, #13, ''); // carriage return
  Result := s;
end;

function ListToString(const List: TStringArray): string;
var s : string;
begin
  Result := '';
  for s in List do Result += s;
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

