unit UnitStream;

interface

uses
  SysUtils, Classes, Graphics, LazUTF8;

type
  TRichStream = class(TMemoryStream)
  protected
    procedure Write(s: string); overload;
  public
    RightToLeft : boolean;
    procedure Open;
    procedure Close;
    procedure WriteLn(s: string);
  end;

implementation

uses UnitLib;

function Utf8ToRTF(const s: string): string;
var
  p: PChar;
  unicode: Cardinal;
  CharLen: integer;
const
  endchar = {$ifdef linux} ' ' {$else} '?' {$endif};
begin
  Result := '';
  p := PChar(s);
  repeat
    unicode := UTF8CharacterToUnicode(p,CharLen);
    if unicode = 0 then Continue;
    if unicode < $80 then Result := Result + char(unicode)
                     else Result := Result + '\u' + ToStr(unicode) + endchar;

    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

procedure TRichStream.Open;
begin
  WriteLn('{\rtf1\ansi\ansicpg1251\cocoartf1187 ');

  WriteLn('{\fonttbl');
  WriteLn('{\f0 default;}');
  WriteLn('{\f1 ' + DefaultFont.Name + ';}');
  WriteLn('}');
  WriteLn('{\colortbl;');
  WriteLn('\red0\green0\blue0;'       ); // 1 black
  WriteLn('\red192\green0\blue0;'     ); // 2 red
  WriteLn('\red0\green0\blue128;'     ); // 3 navy
  WriteLn('\red0\green128\blue0;'     ); // 4 green
  WriteLn('\red128\green128\blue128;' ); // 5 gray
  WriteLn('\red0\green128\blue128;'   ); // 6 teal
  WriteLn('\red128\green0\blue0;'     ); // 7 maroon
  WriteLn('\red153\green102\blue51;'  ); // 8 brown
  WriteLn('\red139\green69\blue19;'   ); // 9 saddlebrown
//WriteLn('\red128\green0\blue128;'   ); // 0 purple
  WriteLn('}');

  Write('\f1\cf1');
  Write('\fs' + ToStr(DefaultFont.Size * 2));

  if RightToLeft then WriteLn('\rtlpar\qr');
  WriteLn(''); // important
end;

procedure TRichStream.Write(s: string);
begin
  s := Utf8ToRTF(s);
  Self.WriteBuffer(Pointer(s)^, Length(s));
end;

procedure TRichStream.WriteLn(s: string);
begin
  Write(s + LineEnding );
end;

procedure TRichStream.Close;
begin
  WriteLn('}');
  WriteLn('');
end;

end.


