unit UmParse;

interface

uses
  Classes, SysUtils, StrUtils, Graphics, RichMemo, RichMemoEx;

procedure Parse(Memo: TRichMemoEx; Source: string; jtag: boolean = false; html: boolean = false);

implementation

uses UmLib;

procedure HtmlReplacement(var s: string);
begin
  Replace(s,'&nbsp;' ,' ');
  Replace(s,'&quot;' ,'"');
  Replace(s,'&ldquo;','«');
  Replace(s,'&rdquo;','»');
  Replace(s, #09     ,' ');

  Replace(s, '<em>', '<i>' );
  Replace(s,'</em>','</i>' );

  Replace(s, '<strong>', '<b>' );
  Replace(s,'</strong>','</b>' );

  Replace(s, '<p/>','<p>' );
  Replace(s,'<br/>','<br><tab>');
  Replace(s, '<td>','<br><tab>');
  Replace(s, '<tr>','<br><tab>');
  Replace(s,'</td>','<br><tab>');
  Replace(s,'</tr>','<br><tab>');

  if Pos('</p>',s) = 0 then Replace(s,'<p>','<br><tab>');
  Replace(s,'</p>','<br><tab>');

  DelDoubleSpace(s);
end;

function Apply(Tag: UnicodeString; var fp: TFontParams): boolean;
begin
  Result := true;

  if Tag = '<l>' then
    begin
      fp.Color := clNavy;
    end

  else if Tag = '<J>' then
    begin
      fp.Color := clMaroon;
    end

  else if Tag = '<b>' then
    begin
      fp.Style := fp.Style + [fsBold];
      fp.Color := clBrown;
    end

  else if Tag = '<i>' then
    begin
      fp.Style := fp.Style + [fsItalic];
      fp.Color := clGray;
    end

  else if Tag = '<S>' then
    begin
      fp.Color := clBrown;
      fp.VScriptPos := TVScriptPos.vpSuperScript;
    end

  else
    Result := false;
end;

procedure Parse(Memo: TRichMemoEx; Source: string; jtag: boolean = false; html: boolean = false);
var
  fp,fp0: TFontParams;
  StOrig : UnicodeString;
  Tag : UnicodeString = '';
  idx : integer = 0;
  IsTag : boolean = false;
  i : integer;

  function iLength(ClosedTag: UnicodeString): integer;
  var
    SubTag: UnicodeString = '';
    IsTag: boolean = false;
    n : integer;
  begin
    Result := 0;
    Insert('/', ClosedTag, 2);

    for n := i to Length(StOrig) do
      begin
        if StOrig[n] = '<' then IsTag := True;

        if IsTag then SubTag := SubTag + StOrig[n]
                 else Inc(Result);

        if StOrig[n] = '>' then
          begin
            if SubTag = ClosedTag then Exit;
            IsTag := False;
            SubTag := '';
          end;
      end;

    Result := 0;
  end;

begin
  fp0 := Memo.SelAttributes;
  fp0.Style := [];
  fp0.Color := clBlack;
  fp0.VScriptPos := TVScriptPos.vpNormal;

  Replace(Source,'<br>',char($0A));
  Replace(Source,'</p>',char($0A));

  //Replace(Source,'<br>',LineEnding);
  //Replace(Source,'</p>',LineEnding);

  StOrig := UnicodeString(Source);
  Memo.Clear;
  Memo.Text := String(RemoveTags(Source));

  for i := 1 to Length(StOrig) do
    begin
      if StOrig[i] = '<' then IsTag := True;

      if IsTag then Tag := Tag + StOrig[i]
               else inc(idx);

      if StOrig[i] = '>' then
        begin
          fp := fp0;
          if Apply(Tag, fp) then Memo.SetTextAttributes(idx, iLength(Tag), fp);
          IsTag := False;
          Tag := '';
        end;
    end;

  Memo.SelStart := 0;
end;

end.

