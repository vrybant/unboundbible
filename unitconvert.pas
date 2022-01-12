unit UnitConvert;

{$modeswitch typehelpers}

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TFootnote = record
    verse : TVerse;
    text : string;
    marker : string;
  end;

  TFootnoteArray = array of TFootnote;

  TModuleConverter = type Helper for TModule
  private
    procedure AssignTo(Module: TModule);
    procedure CreateTables;
    procedure InsertDetails;
  end;

  TBibleConverter = type Helper for TBible
  private
    procedure CreateTables;
    procedure InsertContents(const Contents : TContentArray);
    procedure InsertBooks(Books: TFPGList<TBook>);
    procedure InsertFootnotes(const Footnotes : TFootnoteArray);
    function GetFootnotes(const Contents : TContentArray): TFootnoteArray;
  public
    procedure Convert;
  end;

implementation

uses UnitPrepare;

//=================================================================================================
//                                       TModuleConverter
//=================================================================================================

procedure TModuleConverter.AssignTo(Module: TModule);
begin
  Module.name := name;
  Module.abbreviation := abbreviation;
  Module.info := info;
  Module.language := language;
  Module.numbering := numbering;
  Module.modified := modified;
end;

procedure TModuleConverter.CreateTables;
var
  n : string = '';
begin
  if numbering = 'ru' then n := ',"Numbering" TEXT';
  try
    Connection.ExecuteDirect('CREATE TABLE "Details" ' +
      '("Title" TEXT,"Abbreviation" TEXT,"Information" TEXT,"Language" TEXT'
      + n + ',"Modified" TEXT);');
    CommitTransaction;
  except
    //
  end;
 end;

procedure TModuleConverter.InsertDetails;
var
  n : string = '';
begin
  if numbering = 'ru' then n := ',:n';
  try
    try
      Query.SQL.Text := 'INSERT INTO Details VALUES (:t,:a,:i,:l' + n + ',:m);';
      Query.ParamByName('t').AsString := name;
      Query.ParamByName('a').AsString := abbreviation;
      Query.ParamByName('i').AsString := info;
      Query.ParamByName('l').AsString := language;
      if n <> '' then Query.ParamByName('n').AsString := numbering;
      Query.ParamByName('m').AsString := modified;
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
//                                        TBibleConverter
//=================================================================================================

procedure TBibleConverter.CreateTables;
begin
  inherited;
  try
    Connection.ExecuteDirect('CREATE TABLE "Bible"'+
        '("Book" INT, "Chapter" INT, "Verse" INT, "Scripture" TEXT);');
    Connection.ExecuteDirect('CREATE TABLE "Books"'+
        '("Number" INT, "Name" TEXT, "Abbreviation" TEXT);');
    Connection.ExecuteDirect('CREATE TABLE "Footnotes"'+
        '("Book" INT, "Chapter" INT, "Verse" INT, "Marker" TEXT, "Text" TEXT);');
    CommitTransaction;
  except
    //
  end;
 end;

procedure TBibleConverter.InsertBooks(Books: TFPGList<TBook>);
var
  Book : TBook;
begin
  try
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

procedure TBibleConverter.InsertContents(const Contents : TContentArray);
var
  item : TContent;
begin
  try
    try
      for item in Contents do
        begin
          Query.SQL.Text := 'INSERT INTO Bible VALUES (:b,:c,:v,:s);';
          Query.ParamByName('b').AsInteger := item.verse.book;
          Query.ParamByName('c').AsInteger := item.verse.chapter;
          Query.ParamByName('v').AsInteger := item.verse.number;
          Query.ParamByName('s').AsString  := item.text;
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

procedure TBibleConverter.InsertFootnotes(const Footnotes : TFootnoteArray);
var
  item : TFootnote;
begin
  try
    try
      for item in Footnotes do
        begin
          Query.SQL.Text := 'INSERT INTO Footnotes VALUES (:b,:c,:v,:m,:t);';
          Query.ParamByName('b').AsInteger := item.verse.book;
          Query.ParamByName('c').AsInteger := item.verse.chapter;
          Query.ParamByName('v').AsInteger := item.verse.number;
          Query.ParamByName('m').AsString  := item.marker;
          Query.ParamByName('t').AsString  := item.text;
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

//   custem : <RF>This is a translators' note<Rf>
// extended : <RF q=a>This is a translators' note with link a<Rf>

function ExtractMyswordFootnotes(s: string): TStringArray;
var
  item, marker : string;
  r : string = '';
  l : boolean = false;
begin
  Result := [];
  for item in XmlToList(s) do
    begin
      if item = '<Rf>' then
        begin
          if l then Result.Add(Trim(r));
          l := false;
        end;

      if l then r += item;

      if Prefix('<RF',item) then
        begin
          marker := '#';

          if Prefix('<RF q=',item) then
            marker := item.Replace('<RF q=','').Replace('>','');

          r := marker + delimiter;
          l := true;
        end;
    end;
end;

function TBibleConverter.GetFootnotes(const Contents : TContentArray): TFootnoteArray;
var
  Footnote : TFootnote;
  List : TStringArray;
  Content : TContent;
  s : string;
  k : integer = 0;
begin
  SetLength(Result, Length(Contents));

  for Content in Contents do
    if Content.text.Contains('<RF') then
      begin

        for s in ExtractMyswordFootnotes(Content.text) do
          begin
            List := s.Split(delimiter);

            Footnote.verse  := Content.verse;
            Footnote.marker := List[0];
            Footnote.text   := List[1];

            Result[k] := Footnote;
            inc(k);
          end;

      end;

  SetLength(Result, k);
end;

procedure TBibleConverter.Convert;
var
  Module : TBible;
  path : string;
begin
  LoadDatabase;

  path := DataPath + Slash + '_' + filename + '._unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Module := TBible.Create(path, true);

  AssignTo(Module);
  Module.modified := FormatDateTime('dd/mm/yyyy', Now);

  Module.CreateTables;
  Module.InsertDetails;
  Module.InsertBooks(Books);
  Module.InsertContents(GetAll);
  Module.InsertFootnotes( Module.GetFootnotes(GetAll(true)) );

  Module.Free;
end;

end.
