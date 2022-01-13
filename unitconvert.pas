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
    procedure InsertDetails;
  end;

  TBibleConverter = type Helper for TBible
  private
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

procedure TModuleConverter.InsertDetails;
var
  num : string = '';
    n : string = '';
begin
  if numbering = 'ru' then
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

procedure TBibleConverter.InsertBooks(Books: TFPGList<TBook>);
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

procedure TBibleConverter.InsertContents(const Contents : TContentArray);
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

procedure TBibleConverter.InsertFootnotes(const Footnotes : TFootnoteArray);
var
  Item : TFootnote;
begin
  try
    Connection.ExecuteDirect('CREATE TABLE "Footnotes"'+
      '("Book" INT, "Chapter" INT, "Verse" INT, "Marker" TEXT, "Text" TEXT);');
    try
      for Item in Footnotes do
        begin
          Query.SQL.Text := 'INSERT INTO Footnotes VALUES (:b,:c,:v,:m,:t);';
          Query.ParamByName('b').AsInteger := Item.verse.book;
          Query.ParamByName('c').AsInteger := Item.verse.chapter;
          Query.ParamByName('v').AsInteger := Item.verse.number;
          Query.ParamByName('m').AsString  := Item.marker;
          Query.ParamByName('t').AsString  := Item.text;
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

function ExtractMyswordFootnotes(s: string): TStringArray;
var
  item : string;
  marker : string = '';
  r : string = '';
  l : boolean = false;
begin
  Result := [];
  for item in XmlToList(s) do
    begin
      if item = '<Rf>' then
        begin
          if l then Result.Add(marker + delimiter + Trim(r));
          r := '';
          l := false;
        end;

      if l then r += item;

      if Prefix('<RF',item) then
        begin
          marker := '**';
          if Prefix('<RF q=',item) then
            marker := item.Replace('<RF q=','').Replace('>','');
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

  path := DataPath + Slash + '_' + filename + '.unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Module := TBible.Create(path, true);

  AssignTo(Module);
  Module.modified := FormatDateTime('dd/mm/yyyy', Now);

  Module.InsertDetails;
  Module.InsertBooks(Books);
  Module.InsertContents(GetAll);
  Module.InsertFootnotes( Module.GetFootnotes(GetAll(true)) );

  Module.Free;
end;

end.
