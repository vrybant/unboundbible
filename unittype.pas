unit UnitType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFileFormat = (unbound, mybible);

  TStringAlias = record
    bible   : string;
    book    : string;
    chapter : string;
    verse   : string;
    text    : string;
    details : string;
  end;

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

implementation

end.

