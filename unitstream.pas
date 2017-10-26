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
    constructor Create;
    procedure Open;
    procedure Close;
    procedure WriteLn(s: string);
  end;

implementation

uses UnitLib;

const
  CRLF = #13 + #10;

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
                     else Result := Result + '\u' + IntToStr(unicode) + endchar;

    inc(p,CharLen);
  until (CharLen=0) or (unicode=0);
end;

constructor TRichStream.Create;
begin
  inherited Create;
end;

procedure TRichStream.Open;
begin
  WriteLn('{\rtf1\ansi\ansicpg1251\cocoartf1187 ');
  WriteLn('{\fonttbl\f0\fcharset0 ' + CurrFont.Name + ';}');
  WriteLn('{\colortbl;');
  WriteLn('\red0\green0\blue0;'       ); // 1 black
  WriteLn('\red192\green0\blue0;'     ); // 2 red
  WriteLn('\red0\green0\blue128;'     ); // 3 navy
  WriteLn('\red0\green128\blue0;'     ); // 4 green
  WriteLn('\red128\green128\blue128;}'); // 5 gray

  Write('\f0\cf1');
  Write('\fs' + IntToStr(CurrFont.Size * 2));

  if RightToLeft then WriteLn('\rtlpar');
  WriteLn(''); // important
end;

procedure TRichStream.Write(s: string);
begin
  s := Utf8ToRTF(s);
  Self.WriteBuffer(Pointer(s)^, Length(s));
end;

procedure TRichStream.WriteLn(s: string);
begin
  Write(s + CRLF);
end;

procedure TRichStream.Close;
begin
  WriteLn('}');
  WriteLn('');
end;

end.


