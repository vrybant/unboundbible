unit UnitConvert;

{$modeswitch typehelpers}

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TModuleConverter = type Helper for TModule
  private
    procedure CreateTables;
    procedure InsertDetails;
  end;

type
  TBibleConverter = type Helper for TBible
  private
    procedure CreateTables;
    procedure InsertContent(Content : TContentArray);
    procedure InsertBooks(Books: TFPGList<TBook>);
  public
    procedure Convert;
  end;

implementation

//=================================================================================================
//                                       TModuleConverter
//=================================================================================================

procedure TModuleConverter.CreateTables;
begin
  try
    Connection.ExecuteDirect('CREATE TABLE "Details" '+
      '("Title" TEXT,"Abbreviation" TEXT,"Information" TEXT,"Language" TEXT);');
    CommitTransaction;
  except
    //
  end;
 end;

procedure TModuleConverter.InsertDetails;
begin
  try
    try
      Query.SQL.Text := 'INSERT INTO Details VALUES (:t,:a,:i,:l);';
      Query.ParamByName('t').AsString := name;
      Query.ParamByName('a').AsString := abbreviation;
      Query.ParamByName('i').AsString := info;
      Query.ParamByName('l').AsString := language;
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

procedure TBibleConverter.InsertContent(Content : TContentArray);
var
  line : TContent;
begin
  try
    try
      for line in Content do
        begin
          Query.SQL.Text := 'INSERT INTO Bible VALUES (:b,:c,:v,:s);';
          Query.ParamByName('b').AsInteger := line.verse.book;
          Query.ParamByName('c').AsInteger := line.verse.chapter;
          Query.ParamByName('v').AsInteger := line.verse.number;
          Query.ParamByName('s').AsString  := line.text;
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

procedure TBibleConverter.Convert;
var
  Module : TBible;
  path : string;
begin
  LoadDatabase;

  path := DataPath + Slash + '_output.bbl.unbound';

  if FileExists(path) then DeleteFile(path);
  if FileExists(path) then Exit;

  Module := TBible.Create(path, true);
  Module.CreateTables;

  Module.name := name;
  Module.abbreviation := abbreviation;
  Module.info := info;
  Module.language := language;

  Module.InsertDetails;
  Module.InsertBooks(Books);
  Module.InsertContent(GetAll);

  Module.Free;
end;

end.
