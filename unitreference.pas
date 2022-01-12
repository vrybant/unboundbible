unit UnitReference;

interface

uses
  Classes, Fgl, SysUtils, UnitModule, UnitBible, UnitUtils, UnitLib;

type
  TReferenceAlias = record
    xreferences, book, chapter, verse, xbook, xchapter, xfromverse, xtoverse, votes : string;
  end;

  TReference = class(TModule)
  private
    z : TReferenceAlias;
    function GetData(Verse: TVerse): TVerseArray;
  public
    constructor Create(filePath: string);
  end;

  TReferences = class(TFPGList<TReference>)
  private
    procedure Load;
    function ReferenceByLanguage(language: string): TReference;
  public
    constructor Create;
    function GetData(Verse: TVerse; language: string; out info: string): TVerseArray;
    procedure DeleteItem(Item: TReference);
    destructor Destroy; override;
  end;

implementation

const
  unboundAlias : TReferenceAlias = (
      xreferences : 'xreferences';
      book        : 'book';
      chapter     : 'chapter';
      verse       : 'verse';
      xbook       : 'xbook';
      xchapter    : 'xchapter';
      xfromverse  : 'xfromverse';
      xtoverse    : 'xtoverse';
      votes       : 'votes';
     );

    mybibleAlias : TReferenceAlias = (
      xreferences : 'cross_references';
      book        : 'book';
      chapter     : 'chapter';
      verse       : 'verse';
  //  toverse     : 'verse_end';
      xbook       : 'book_to';
      xchapter    : 'chapter_to';
      xfromverse  : 'verse_to_start';
      xtoverse    : 'verse_to_end';
      votes       : 'votes';
     );

//========================================================================================
//                                   TReference
//========================================================================================

constructor TReference.Create(filePath: string);
begin
  inherited Create(filePath);
  z := unboundAlias;
  if format = mybible then z := mybibleAlias;
  if connected and not TableExists(z.xreferences ) then connected := false;
end;

function TReference.GetData(Verse: TVerse): TVerseArray;
var
  V : TVerse;
  v_from, v_to : string;
  id, toverse, votes, count : integer;
begin
  Result := [];

  id := EncodeID(Verse.book);
  v_from := ToStr(Verse.number);
  v_to   := ToStr(Verse.number + Verse.count - 1);

  try
    try
        Query.SQL.Text := 'SELECT * FROM ' + z.xreferences +
          ' WHERE '  + z.book    + ' = '  + ToStr(id) +
            ' AND '  + z.chapter + ' = '  + ToStr(Verse.chapter) +
            ' AND (' + z.verse + ' BETWEEN ' + v_from + ' AND ' + v_to + ') ';

        Query.Open;
        Query.Last;
        SetLength(Result, Query.RecordCount);
        Query.First;

        count := 0;
        while not Query.Eof do
          try
            v.Init;
            try v.book    := Query.FieldByName(z.xbook     ).AsInteger; except end;
            try v.chapter := Query.FieldByName(z.xchapter  ).AsInteger; except end;
            try v.number  := Query.FieldByName(z.xfromverse).AsInteger; except end;
            try toverse   := Query.FieldByName(z.xtoverse  ).AsInteger; except end;
            try votes     := Query.FieldByName(z.votes     ).AsInteger; except end;

            if votes <= 1 then continue;

            v.book := DecodeID(v.book);
            if toverse = 0 then v.count := 1
               else v.count := toverse - v.number + 1;

            Result[count] := v;
            count += 1;
          finally
            Query.Next;
          end;

        SetLength(Result, count);
    except
      //
    end;
  finally
    Query.Close;
  end;
end;

//=================================================================================================
//                                       TReferences
//=================================================================================================

constructor TReferences.Create;
begin
  inherited;
  Load;
end;

procedure TReferences.Load;
var
  Reference : TReference;
  f : string;
begin
  for f in DatabaseList do
    if f.Contains('.xrefs.') then
      begin
        Reference := TReference.Create(f);
        if Reference.connected then Add(Reference) else Reference.Free;
      end;
end;

function TReferences.ReferenceByLanguage(language: string): TReference;
var
  Reference : TReference;
begin
  Result := nil;
  for Reference in Self do
    if Reference.language = language then Exit(Reference);
end;

function TReferences.GetData(Verse: TVerse; language: string; out info: string): TVerseArray;
var
  Reference : TReference;
begin
  info := '';

  Reference := ReferenceByLanguage(language);
  if Reference = nil then Reference := ReferenceByLanguage('en');
  if Reference = nil then Exit([]);

  Result := Reference.GetData(Verse);
  info := Reference.info;
end;

procedure TReferences.DeleteItem(Item: TReference);
begin
  Item.Delete;
  Item.Free;
  Delete(IndexOf(Item));
end;

destructor TReferences.Destroy;
var
  Reference : TReference;
begin
  for Reference in Self do Reference.Free;
  inherited Destroy;
end;

end.
