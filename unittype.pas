unit UnitType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFileFormat = (unbound, mybible);

  TSearchRange = record
    from : integer;
    to_ : integer;
  end;

  TSearchOption = (caseSensitive, wholeWords);
  TSearchOptions = set of TSearchOption;

type
  TStringAlias = record
    bible   : string;
    book    : string;
    chapter : string;
    verse   : string;
    text    : string;
//  details : string;
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

const
  unboundStringAlias : TStringAlias = (
    bible   : 'Bible';
    book    : 'Book';
    chapter : 'Chapter';
    verse   : 'Verse';
    text    : 'Scripture';
//  details : 'Details';
    );

  mybibleStringAlias : TStringAlias = (
    bible   : 'verses';
    book    : 'book_number';
    chapter : 'chapter';
    verse   : 'verse';
    text    : 'text';
//  details : 'info';
    );

  noneVerse : TVerse = (
    book    : 0;
    chapter : 0;
    number  : 0;
    count   : 0;
    );

var
  myBibleArray : array [1..80] of integer = (
    010,020,030,040,050,060,070,080,090,100,110,120,130,140,150,160,190,220,230,240,
    250,260,290,300,310,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,
    480,490,500,510,520,530,540,550,560,570,580,590,600,610,620,630,640,650,660,670,
    680,690,700,710,720,730,170,180,270,280,320,462,464,000,165,466,000,790,468,315
    );

implementation

end.

