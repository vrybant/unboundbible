unit UnitMemo;

{*******************************************************}
{              SuperEdit Lazarus Component              }
{                       GNU GPL                         }
{               Vladimir Rybant Software                }
{                  vladimirrybant.org                   }
{*******************************************************}

interface

uses
  {$ifdef windows} Windows, {$endif} Forms, SysUtils,
  Classes, Graphics, Controls, ExtCtrls, LCLProc, LCLType,
  RichMemoEx, UnitLib;

type
  TUnboundMemo = class(TRichMemoEx)
  protected
    procedure CreateWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp  (var Key: Word; Shift: TShiftState); override;
  private
    SelStartTemp  : integer;
    SelLengthTemp : integer;
    function  Colored: boolean;
    function  GetLink: string;
    function  GetParagraphNumber: integer;
  public
    Stream : TMemoryStream;
    Hyperlink : boolean;
    Hypertext : string;
    constructor Create(AOwner: TComponent); override;
    function  GetRange: TRange;
    procedure SelectParagraph(n : integer);
    function  GetStartSelection: integer;
    function  GetEndSelection: integer;
    procedure SelectWord;
    procedure OpenStream;
    procedure CloseStream;
    procedure Write(s: string);
    procedure WriteLn(s: string);
    procedure SaveSelection;
    procedure RestoreSelection;
  end;

procedure SaveTitle(var m: TMemoryStream);
procedure SaveTail(var m: TMemoryStream);

implementation

procedure SaveTitle(var m: TMemoryStream);
begin
  StreamWriteLn(m,'{\rtf1\ansi\ansicpg1251\cocoartf1187 ');
  StreamWriteLn(m,'{\fonttbl\f0\fcharset0 '+ CurrFont.Name + ';}');
  StreamWriteLn(m,'{\colortbl;');
  StreamWriteLn(m,'\red0\green0\blue0;'       ); // 1 black
  StreamWriteLn(m,'\red255\green0\blue0;'     ); // 2 red
  StreamWriteLn(m,'\red0\green0\blue128;'     ); // 3 navy
  StreamWriteLn(m,'\red0\green128\blue0;'     ); // 4 green
  StreamWriteLn(m,'\red128\green128\blue128;}'); // 5 gray

  StreamWrite(m,'\f0\cf1');
  StreamWrite(m,'\fs' + IntToStr(CurrFont.Size * 2)); // font size

// if fsBold in CurrFont.Style then StreamWrite(m,'\b'); // bold
// if RightToLeft then StreamWrite(m,'\rtlpar') else StreamWrite(m,'\ltrpar');
// if Bible.RightToLeft then StreamWrite(m,'\qr') else StreamWrite(m,'\ql');

  StreamWriteLn(m,''); // important
end;

procedure SaveTail(var m: TMemoryStream);
begin
  StreamWriteLn(m,'}');
  StreamWriteLn(m,'');
end;

//=================================================================================================
//                                      TSuperEdit
//=================================================================================================

constructor TUnboundMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Hyperlink := False;
  Hypertext := '';
  Cursor := crArrow;
  SelStartTemp  := 0;
  SelLengthTemp := 0;
end;

procedure TUnboundMemo.CreateWnd;
begin
  inherited;
  if ReadOnly then HideCursor;
end;

function TUnboundMemo.Colored: boolean;
begin
  Result := SelAttributes.Color = clNavy
end;

function TUnboundMemo.GetLink: string;
var
  x1,x2,x0 : integer;
  n1,n2 : integer;
begin
  Result := '';
  if (SelLength > 0) or not Colored then Exit;
  GetSel(n1,n2);

  x0 := SelStart;
  x1 := x0;
  repeat
    dec(x1);
    SetSel(x1, x1);
  until not Colored or (x1 < 0);

  x2 := x0;
  repeat
    inc(x2);
    SetSel(x2, x2);
  until not Colored;

  inc(x1);
  {$ifdef windows} dec(x2); {$endif}

  SetSel(x1, x2); Result := SelText;
  SetSel(n1, n2);
end;

procedure TUnboundMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ReadOnly or (ssCtrl in Shift) then HideCursor;
end;

procedure TUnboundMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Hyperlink then Hypertext := GetLink else Hypertext := '';
  if ReadOnly or (ssCtrl in Shift) then HideCursor;
  inherited;
end;

procedure TUnboundMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ReadOnly or (ssCtrl in Shift) then HideCursor;
end;

procedure TUnboundMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  {$ifdef windows}
  if Hyperlink and not ReadOnly and (Key = VK_CONTROL) then ShowCaret(Handle);
  {$endif}
end;

function TUnboundMemo.GetParagraphNumber: integer;
var
  x0,x1,x2 : integer;
begin
  Result := 0;

  GetSel(x1,x0); // must be equal

  while not Colored and (x1 > 0) do
    begin
      dec(x1);
      SetSel(x1, x1);
    end;

  repeat
    dec(x1);
    SetSel(x1, x1);
  until not Colored or (x1 < 0);

  x2 := x1;
  repeat
    inc(x2);
    SetSel(x2, x2);
  until not Colored; // or (x2 > x0+5)

                      inc(x1);
// {$ifdef windows} dec(x2); {$endif}

  SetSel(x1,x2); Result := MyStrToInt(SelText);
  SetSel(x1,x1+1);
end;

function TUnboundMemo.GetRange: TRange;
var
  x1,x2 : integer;
begin
  GetSel(x1,x2);
  SetSel(x2,x2); Result.till := GetParagraphNumber;
  SetSel(x1,x1); Result.from := GetParagraphNumber;
  if x1 <> x2 then SetSel(x1,x2);
end;

{$ifdef windows}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  w, line : string;
  i, len, x : integer;
begin
  HideSelection := False; // important

  w := ' ' + IntToStr(n) + ' ';
  len := length(w);

  for i:=0 to LineCount - 1 do
    begin
      line := Lines[i];

      if copy(line,1,len) = w then
         begin
           x := LineIndex(i);
           SetSel(x,x+1);
           HideCursor;
         end;
    end;
end;
{$endif}

{$ifdef unix}
procedure TUnboundMemo.SelectParagraph(n : integer);
var
  i, x : integer;
  L : boolean;
begin
  L := False;
  x := 0;

  i := 0;
  while True do
    begin
      SetSel(i,i);
      if SelStart <> i then break;

      if Colored then
        begin
          if not L then
            begin
              inc(x);
              if x = n then
                begin
                  SetSel(i,i+1);
                  break;
                end;
            end;
          L := True;
        end;

      if not Colored then L := False;
      inc(i);
    end;

  SetFocus;
end;
{$endif}

function TUnboundMemo.GetStartSelection: integer;
var
  i, temp : integer;
begin
  temp := SelStart;
     i := SelStart;

  SetSel(i-1,i);
  while (SelText <> ' ') and (i > 0)  do
    begin
      dec(i);
      SetSel(i-1,i);
    end;

  Result := i;
  SetSel(temp, temp);
end;

function TUnboundMemo.GetEndSelection: integer;
var
  i, len, temp : integer;
begin
  temp := SelStart;
     i := SelStart;
   len := i + 50;

  SetSel(i,i+1);
  while (LowerCase(SelText) <> UpperCase(SelText)) and (i < len) do
    begin
      inc(i);
      SetSel(i,i+1);
    end;

  Result := i;
  SetSel(temp, temp);
end;

procedure TUnboundMemo.SelectWord;
begin
  SelStart  := GetStartSelection;
  SelLength := GetEndSelection - SelStart;
end;

procedure TUnboundMemo.OpenStream;
begin
  Stream := TMemoryStream.Create;
  SaveTitle(Stream);
end;

procedure TUnboundMemo.Write(s: string);
begin
  StreamWrite(Stream,Utf8ToRTF(s));
end;

procedure TUnboundMemo.WriteLn(s: string);
begin
  StreamWriteLn(Stream,Utf8ToRTF(s));
end;

procedure TUnboundMemo.CloseStream;
begin
  SaveTail(Stream);
  LoadRichText(Stream);
  Stream.free;
  SelStart := 0;
end;

procedure TUnboundMemo.SaveSelection;
begin
  SelStartTemp  := SelStart;
  SelLengthTemp := SelLength;
end;

procedure TUnboundMemo.RestoreSelection;
begin
  SelStart  := SelStartTemp;
  SelLength := SelLengthTemp;
end;

end.

