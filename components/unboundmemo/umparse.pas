unit UmParse;

interface

uses
  Classes, SysUtils, Graphics, RichMemo, RichMemoEx, UmLib;

procedure Parse(Memo: TRichMemoEx; Source: string; html: boolean = false);
procedure HtmlReplacement(var s: string);

implementation

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
var t : UnicodeString;
begin
  t := LowerCase(Tag);
  Result := true;

  if Tag = '<J>' then fp.Color := clMaroon else
  if Tag = '<l>' then fp.Color := clNavy   else
  if Tag = '<r>' then fp.Color := clRed    else
  if Tag = '<n>' then fp.Color := clGray   else

  if Tag = '<f>' then begin fp.Color := clTeal;  fp.VScriptPos := vpSuperScript end else
  if Tag = '<m>' then begin fp.Color := clGray;  fp.VScriptPos := vpSuperScript end else
  if Tag = '<S>' then begin fp.Color := clBrown; fp.VScriptPos := vpSuperScript end else

  if t =  '<i>' then begin fp.Color := clGray;  fp.Style += [fsItalic] end else
  if t = '<em>' then begin fp.Color := clGray;  fp.Style += [fsItalic] end else
  if t =  '<a>' then begin fp.Color := clGray                          end else
  if t =  '<b>' then begin fp.Color := clBrown; fp.Style += [fsBold]   end else
  if t =  '<h>' then begin fp.Color := clBrown; fp.Style += [fsBold]   end else

  Result := false;
end;

procedure Parse(Memo: TRichMemoEx; Source: string; html: boolean = false);
var
  fp, fp0: TFontParams;
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
        if IsTag then SubTag := SubTag + StOrig[n];

        if StOrig[n] = '>' then
          begin
            if SubTag = ClosedTag then Exit;
            IsTag := False;
            SubTag := '';
          end
        else
          if not IsTag then Inc(Result);
      end;

    Result := 0;
  end;

begin
  Memo.Clear;
  fp0 := Memo.SelAttributes;

  Replace(Source,'<br>',char($0A));
  Replace(Source,'</p>',char($0A));

  StOrig := UnicodeString(Source);
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

