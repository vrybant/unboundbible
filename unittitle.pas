unit UnitTitle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTitle = class(TStringList)
  public
    constructor Create(language: string);
    function GetTitle(n: integer): string;
    function GetAbbr(n: integer): string;
  private
    fileName : string;
    function GetFileName(language: string): string;
    function GetTitleEx(n : integer; abbr: boolean): string;
    procedure LoadDataFromFile;
  end;


implementation

uses
  UnitLib;

constructor TTitle.Create(language: string);
begin
  inherited Create;
  fileName := GetFileName(language);
  LoadDataFromFile;
end;

function TTitle.GetFileName(language: string): string;
var
  List : TStringList;
  path : string;
  i : integer;
begin
  Result := 'english.txt';

  List := TStringList.Create;
  path := appPath + titleDirectory + slash + '*.txt';

  GetFileList(path, List, True);

  for i:= 0 to List.Count-1 do
    if Prefix(language, List[i]) then
      Result := List[i];

  List.Free;
end;

procedure TTitle.LoadDataFromFile;
var
  path : string;
  f : System.Text;
  s : string;
begin
  path := appPath + slash + titleDirectory + slash + fileName;

  if not FileExists(path) then Exit;

  AssignFile(f,path); Reset(f);

  while not eof(f) do
    begin
      Readln(f,s);
      if (Length(s) > 3) and (s[1] = chr($EF)) then System.Delete(s,1,3); // unicode sign
      self.Add(s);
    end;

  CloseFile(f);
end;

function TTitle.GetTitleEx(n : integer; abbr: boolean): string;
var
  List : TStringList;
   i,k : integer;
begin
  Result := IntToStr(n);

  if self.Count = 0 then Exit;
  if n = 0 then Exit;
  if abbr then k := 2 else k := 1;
  List := TStringList.Create;

  for i:=0 to self.Count-1 do
    begin
      StrToList(self[i], List);
      if List.Count > k then
        if MyStrToInt(List[0]) = n then Result := List[k];
    end;

  List.Free;
end;

function TTitle.GetTitle(n : integer): string;
begin
  Result := GetTitleEx(n, false);
end;

function TTitle.GetAbbr(n : integer): string;
begin
  Result := GetTitleEx(n, true);
end;

end.

