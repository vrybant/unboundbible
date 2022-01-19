unit UnitExport;

{$modeswitch typehelpers}

interface

uses
  Classes, Fgl, SysUtils,
  UnitModule, UnitBible, UnitCommentary, UnitDictionary, UnitTools, UnitUtils, UnitLib;

type
  TModuleExporter = type Helper for TModule
  private
    procedure InsertDetails(Source: TModule);
  end;

  TBibleExporter = type Helper for TBible
  private
    procedure InsertText(const List: TStringArray);
    procedure InsertBooks(Books: TFPGList<TBook>);
    procedure InsertFootnotes(const List: TStringArray);
    function GetMyswordFootnotes(const List: TStringArray): TStringArray;
    function GetFootnotes: TStringArray;
  public
    procedure Exporting(Bible: TBible);
  end;

  TCommentaryExporter = type Helper for TCommentary
    procedure InsertData(const List: TStringArray);
    procedure Exporting(Commentary: TCommentary);
  end;

  TDictionaryExporter = type Helper for TDictionary
    procedure Exporting(Source: TDictionary);
  end;

  TToolsExporter = type Helper for TTools
    procedure ExportBible(Source: TBible);
    procedure ExportCommentary(Source: TCommentary);
    procedure ExportDictionary(Source: TDictionary);
  end;

implementation

uses UnitConvert;

//=================================================================================================
//                                         Module
//=================================================================================================

procedure TModuleExporter.InsertDetails(Source: TModule);
var
  num : string = '';
    n : string = '';
begin
  if Source.numbering = 'ru' then
    begin
      num := ',"Numbering" TEXT';
      n := ',:n';
    end;
  try
    Connection.ExecuteDirect('CREATE TABLE "Details" ' +
      '("Title" TEXT,"Abbreviation" TEXT,"Information" TEXT,"Language" TEXT'
      + num + ',"Modified" TEXT);');
    try
      Query.SQL.Text := 'INSERT INTO Details VALUES (:t,:a,:i,:l' + n + ',:m);';
      Query.ParamByName('t').AsString := Source.name;
      Query.ParamByName('a').AsString := Source.abbreviation;
      Query.ParamByName('i').AsString := Source.info;
      Query.ParamByName('l').AsString := Source.language;
      if n <> '' then Query.ParamByName('n').AsString := numbering;
      Query.ParamByName('m').AsString := FormatDateTime('dd/mm/yyyy', Now);
      Query.ExecSQL;
      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

//=================================================================================================
//                                         Bible
//=================================================================================================

procedure TBibleExporter.InsertBooks(Books: TFPGList<TBook>);
var
  Book : TBook;
begin
  try
    Connection.ExecuteDirect('CREATE TABLE "Books"'+
      '("Number" INT, "Name" TEXT, "Abbreviation" TEXT);');
    try
      for Book in Books do
        begin
          Query.SQL.Text := 'INSERT INTO Books VALUES (:n,:t,:a);';
          Query.ParamByName('n').AsInteger := Book.number;
          Query.ParamByName('t').AsString  := Book.title;
          Query.ParamByName('a').AsString  := Book.abbr;
          Query.ExecSQL;
        end;
      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBibleExporter.InsertText(const List: TStringArray);
var
  A : TStringArray;
  s : string;
begin
  if Length(List) = 0 then Exit;
  try
    try
      Connection.ExecuteDirect('CREATE TABLE "Bible"'+
        '("Book" INT, "Chapter" INT, "Verse" INT, "Scripture" TEXT);');

      for s in List do
        begin
          A := s.Split(#0);
          if A.Count < 4 then Continue;
          Query.SQL.Text := 'INSERT INTO Bible VALUES (:b,:c,:v,:t);';
          Query.ParamByName('b').AsString := A[0]; // book
          Query.ParamByName('c').AsString := A[1]; // chapter
          Query.ParamByName('v').AsString := A[2]; // verse
          Query.ParamByName('t').AsString := A[3]; // text
          Query.ExecSQL;
        end;
      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TBibleExporter.InsertFootnotes(const List: TStringArray);
var
  A : TStringArray;
  s : string;
begin
  output(Length(List));
  if Length(List) = 0 then Exit;
  try
    try
      Connection.ExecuteDirect('CREATE TABLE "Footnotes"'+
        '("Book" INT, "Chapter" INT, "Verse" INT, "Marker" TEXT, "Text" TEXT);');

      for s in List do
        begin
          A := s.Split(#0);
          if A.Count < 5 then Continue;
          Query.SQL.Text := 'INSERT INTO Footnotes VALUES (:b,:c,:v,:m,:t);';
          Query.ParamByName('b').AsString := A[0]; // book
          Query.ParamByName('c').AsString := A[1]; // chapter
          Query.ParamByName('v').AsString := A[2]; // verse
          Query.ParamByName('m').AsString := A[3]; // marker
          Query.ParamByName('t').AsString := A[4]; // text
          Query.ExecSQL;
        end;

      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TBibleExporter.GetMyswordFootnotes(const List: TStringArray): TStringArray;
var
  Arr : TStringArray;
  Line, Item : string;
begin
  Result := [];
  for Line in List do
    if Line.Contains('<RF') then
      for Item in ExtractMyswordFootnotes(Line) do
        begin
          Arr := Line.Split(#0);
          if Arr.Count < 4 then Continue;
          Arr[3] := Item;
          Result.Add(''.Join(#0, Arr));
        end;
end;

function TBibleExporter.GetFootnotes: TStringArray;
begin
  if format = mysword then Result := GetMyswordFootnotes(GetAll(true));
  if format = mybible then Result := Tools.Commentaries.GetAllFootnotes(fileName);
end;

procedure TBibleExporter.Exporting(Bible: TBible);
begin
  Bible.LoadDatabase;
  InsertDetails(Bible);
  InsertBooks(Bible.Books);
  InsertText(Bible.GetAll);
  InsertFootnotes(Bible.GetFootnotes);
end;

//=================================================================================================
//                                      Commentary
//=================================================================================================

procedure TCommentaryExporter.InsertData(const List : TStringArray);
var
  A : TStringArray;
  s : string;
begin
  if Length(List) = 0 then Exit;
  try
    try
      Connection.ExecuteDirect('CREATE TABLE "Commentary"'+
        '("book" INT, "chapter" INT, "fromverse" INT, "toverse" INT, "data" TEXT);');

      for s in List do
        begin
          A := s.Split(#0);
          if A.Count < 5 then Continue;
          Query.SQL.Text := 'INSERT INTO Commentary VALUES (:b,:c,:f,:t,:d);';
          Query.ParamByName('b').AsString := A[0]; // book
          Query.ParamByName('c').AsString := A[1]; // chapter
          Query.ParamByName('f').AsString := A[2]; // fromverse
          Query.ParamByName('t').AsString := A[3]; // toverse
          Query.ParamByName('d').AsString := A[4]; // data
          Query.ExecSQL;
        end;
      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

procedure TCommentaryExporter.Exporting(Commentary : TCommentary);
begin
  InsertDetails(Commentary);
  InsertData(Commentary.GetAll);
end;

//=================================================================================================
//                                        Dictionary
//=================================================================================================

procedure TDictionaryExporter.Exporting(Source : TDictionary);
begin
  InsertDetails(Source);
end;

//=================================================================================================
//                                           Tools
//=================================================================================================

procedure TToolsExporter.ExportBible(Source: TBible);
var
  Bible : TBible;
  path : string;
begin
  path := DataPath + Slash + '_' + Source.filename + '.unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Bible := TBible.Create(path);
  Bible.Exporting(Source);
  Bible.Free;
end;

procedure TToolsExporter.ExportCommentary(Source: TCommentary);
var
  Commentary : TCommentary;
  path : string;
begin
  path := DataPath + Slash + '_' + Source.filename + '.unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Commentary := TCommentary.Create(path);
  Commentary.Exporting(Source);
  Commentary.Free;
end;

procedure TToolsExporter.ExportDictionary(Source: TDictionary);
var
  Dictionary : TDictionary;
  path : string;
begin
  path := DataPath + Slash + '_' + Source.filename + '.unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Dictionary := TDictionary.Create(path);
  Dictionary.Exporting(Source);
  Dictionary.Free;
end;

end.
