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
    procedure InsertContents(const Contents: TContentArray);
    procedure InsertBooks(Books: TFPGList<TBook>);
    procedure InsertFootnotes(const Footnotes: TStringArray);
    function GetMyswordFootnotes(const Contents: TContentArray): TStringArray;
    function GetFootnotes: TStringArray;
  public
    procedure Exporting(Source : TBible);
  end;

  TCommentaryExporter = type Helper for TCommentary
    procedure InsertData(const CommentaryArr : TStringArray);
    procedure Exporting(Source : TCommentary);
  end;

  TDictionaryExporter = type Helper for TDictionary
    procedure Exporting(Source : TDictionary);
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

procedure TBibleExporter.InsertContents(const Contents : TContentArray);
var
  Item : TContent;
begin
  try
    Connection.ExecuteDirect('CREATE TABLE "Bible"'+
      '("Book" INT, "Chapter" INT, "Verse" INT, "Scripture" TEXT);');
    try
      for item in Contents do
        begin
          Query.SQL.Text := 'INSERT INTO Bible VALUES (:b,:c,:v,:s);';
          Query.ParamByName('b').AsInteger := Item.verse.book;
          Query.ParamByName('c').AsInteger := Item.verse.chapter;
          Query.ParamByName('v').AsInteger := Item.verse.number;
          Query.ParamByName('s').AsString  := Item.text;
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

procedure TBibleExporter.InsertFootnotes(const Footnotes : TStringArray);
var
  Item : string;
begin
  if Length(Footnotes) = 0 then Exit;
  try
    Connection.ExecuteDirect('CREATE TABLE "Footnotes"'+
      '("Book" INT, "Chapter" INT, "Verse" INT, "Marker" TEXT, "Text" TEXT);');
    try
      for Item in Footnotes do
        begin
          (*
          Query.SQL.Text := 'INSERT INTO Footnotes VALUES (:b,:c,:v,:m,:t);';
          Query.ParamByName('b').AsInteger := Item.book;
          Query.ParamByName('c').AsInteger := Item.chapter;
          Query.ParamByName('v').AsInteger := Item.fromverse;
          Query.ParamByName('m').AsString  := Item.marker;
          Query.ParamByName('t').AsString  := Item.data;
          Query.ExecSQL;
          *)
        end;
      CommitTransaction;
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

function TBibleExporter.GetMyswordFootnotes(const Contents : TContentArray): TStringArray;
var
  Footnote : string;
  List : TStringArray;
  Content : TContent;
  s : string;
begin
  Result := [];

  for Content in Contents do
    if Content.text.Contains('<RF') then
      for s in ExtractMyswordFootnotes(Content.text) do
        begin
          List := s.Split(#0);
          if Length(List) < 2 then Continue;
(*
          Footnote.book      := Content.verse.book;
          Footnote.chapter   := Content.verse.chapter;
          Footnote.fromverse := Content.verse.number;
          Footnote.marker    := List[0];
          Footnote.data      := List[1];

          Result.Add(Footnote);
          *)
        end;
end;

function TBibleExporter.GetFootnotes: TStringArray;
begin
  if format = mysword then Result := GetMyswordFootnotes(GetAll(true));
  if format = mybible then Result := Tools.Commentaries.GetAll(fileName);
end;

procedure TBibleExporter.Exporting(Source : TBible);
begin
  Source.LoadDatabase;
  InsertDetails(Source);
  InsertBooks(Source.Books);
  InsertContents(Source.GetAll);
  InsertFootnotes(Source.GetFootnotes);
end;

//=================================================================================================
//                                      Commentary
//=================================================================================================

procedure TCommentaryExporter.InsertData(const CommentaryArr : TStringArray);
var
  Item : string;
begin
  if Length(CommentaryArr) = 0 then Exit;
  try
    Connection.ExecuteDirect('CREATE TABLE "Commentary"'+
      '("book" INT, "chapter" INT, "fromverse" INT, "toverse" TEXT, "data" TEXT);');
    try
      for Item in CommentaryArr do
        begin
          (*
          Query.SQL.Text := 'INSERT INTO Commentary VALUES (:b,:c,:f,:t,:d);';
          Query.ParamByName('b').AsInteger := Item.book;
          Query.ParamByName('c').AsInteger := Item.chapter;
          Query.ParamByName('f').AsInteger := Item.fromverse;
          Query.ParamByName('t').AsInteger := Item.toverse;
          Query.ParamByName('d').AsString  := Item.data;
          Query.ExecSQL;
          *)
        end;
      CommitTransaction;
    except
      //
      output('except');
    end;
  finally
    Query.Close;
  end;
end;

procedure TCommentaryExporter.Exporting(Source : TCommentary);
begin
  InsertDetails(Source);
  InsertData(Source.GetAll);
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
