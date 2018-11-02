unit UnitType;

interface

uses
  Classes, SysUtils;

type
  TFileFormat = (unbound, mybible);

  TSearchOption = (caseSensitive, wholeWords);
  TSearchOptions = set of TSearchOption;

type
  TBibleAlias = record
    bible   : string;
    book    : string;
    chapter : string;
    verse   : string;
    text    : string;
  end;

  TCommentaryAlias = record
    commentary : string;
    id         : string;
    book       : string;
    chapter    : string;
    fromverse  : string;
    toverse    : string;
    data       : string;
  end;

  TDictionaryAlias = record
    dictionary : string;
    word       : string;
    data       : string;
  end;

 type
  TVerse = record
    book     : integer;
    chapter  : integer;
    number   : integer;
    count    : integer;
  end;

 TContent = record
    verse : TVerse;
    text : string;
  end;

 TContentArray = array of TContent;

 TCopyOptions = record
    cvAbbreviate  : boolean;
    cvEnumerated  : boolean;
    cvGuillemets  : boolean;
    cvParentheses : boolean;
    cvEnd         : boolean;
  end;

const
  unboundStringAlias : TBibleAlias = (
    bible   : 'Bible';
    book    : 'Book';
    chapter : 'Chapter';
    verse   : 'Verse';
    text    : 'Scripture';
    );

  mybibleStringAlias : TBibleAlias = (
    bible   : 'verses';
    book    : 'book_number';
    chapter : 'chapter';
    verse   : 'verse';
    text    : 'text';
    );

  unboundCommentaryAlias : TCommentaryAlias = (
    commentary : 'commentary';
    id         : 'id';
    book       : 'book';
    chapter    : 'chapter';
    fromverse  : 'fromverse';
    toverse    : 'toverse';
    data       : 'data';
    );

  mybibleCommentaryAlias : TCommentaryAlias = (
    commentary : 'commentaries';
    id         : 'id';
    book       : 'book_number';
    chapter    : 'chapter_number_from';
    fromverse  : 'verse_number_from';
//  chapter    : 'chapter_number_to';
    toverse    : 'verse_number_to';
//  marker     : 'marker';
    data       : 'text';
    );

  unboundDictionaryAlias : TDictionaryAlias = (
    dictionary : 'dictionary';
    word       : 'word';
    data       : 'data';
    );

  mybibleDictionaryAlias : TDictionaryAlias = (
    dictionary : 'dictionary';
    word       : 'topic';
    data       : 'definition';
  );

  noneVerse : TVerse = (
    book    : 0;
    chapter : 0;
    number  : 0;
    count   : 0;
    );

  minVerse : TVerse = (
    book    : 1;
    chapter : 1;
    number  : 1;
    count   : 1;
    );

var
  ActiveVerse : TVerse;
  Options : TCopyOptions;

const
  MaxBooks = 79;

var
  myBibleArray : array [1..MaxBooks] of integer = (
    010,020,030,040,050,060,070,080,090,100,110,120,130,140,150,160,190,220,230,240,
    250,260,290,300,310,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,
    480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,
    680,690,700,710,720,730,165,170,180,270,280,315,320,462,464,466,468,790,467
    );

  sortArrayEN : array [1..MaxBooks] of integer = (
    01,02,03,04,05,06,07,08,09,10,11,12,13,14,78,15,16,67,68,69,
    17,74,75,76,79,77,18,19,20,21,22,70,71,23,24,25,72,73,26,27,
    28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66
    );

  sortArrayRU : array [1..MaxBooks] of integer = (
    01,02,03,04,05,06,07,08,09,10,11,12,13,14,78,15,16,67,68,69,
    17,18,19,20,21,22,70,71,23,24,25,72,73,26,27,28,29,30,31,32,
    33,34,35,36,37,38,39,74,75,76,79,77,40,41,42,43,44,59,60,61,
    62,63,64,65,45,46,47,48,49,50,51,52,53,54,55,56,57,58,66
    );

  BibleHubArray : array [1..66] of string = (
    'genesis','exodus','leviticus','numbers','deuteronomy','joshua','judges','ruth','1_samuel','2_samuel',
    '1_kings','2_kings','1_chronicles','2_chronicles','ezra','nehemiah','esther','job','psalms','proverbs',
    'ecclesiastes','songs','isaiah','jeremiah','lamentations','ezekiel','daniel','hosea','joel','amos',
    'obadiah','jonah','micah','nahum','habakkuk','zephaniah','haggai','zechariah','malachi','matthew',
    'mark','luke','john','acts','romans','1_corinthians','2_corinthians','galatians','ephesians','philippians',
    'colossians','1_thessalonians','2_thessalonians','1_timothy','2_timothy','titus','philemon','hebrews',
    'james','1_peter','2_peter','1_john','2_john','3_john','jude','revelation'
    );

function IsNewTestament(n: integer): boolean;
function IsOldTestament(n: integer): boolean;
function IsApocrypha(n: integer): boolean;
function EncodeID(format: TFileFormat; id: integer): integer;
function DecodeID(format: TFileFormat; id: integer): integer;

implementation

function IsNewTestament(n: integer): boolean;
begin
  Result := (n >= 40) and (n <= 66);
end;

function IsOldTestament(n: integer): boolean;
begin
  Result := n < 40;
end;

function IsApocrypha(n: integer): boolean;
begin
  Result := n > 66;
end;

function EncodeID(format: TFileFormat; id: integer): integer;
begin
  Result := id;
  if format = mybible then
    if id > 0 then
      if id <= Length(myBibleArray) then
        Result := myBibleArray[id];
end;

function DecodeID(format: TFileFormat; id: integer): integer;
var i : integer;
begin
  Result := id;
  if format = mybible then
    if id > 0 then
      for i:=1 to Length(myBibleArray) do
        if id = myBibleArray[i] then
          begin
            Result := i;
            Exit;
          end;
end;

end.

