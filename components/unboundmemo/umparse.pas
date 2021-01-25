unit UmParse;

interface

uses
  Classes, SysUtils, Graphics, RichMemo, RichMemoEx, UnitLib;

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

  Replace(s, '<strong>', '<b>');
  Replace(s,'</strong>','</b>');

  Replace(s, '<p/>','<p>' );
  Replace(s,'<br/>','<br>');

  Replace(s, '<td>','<br><tab>');
  Replace(s, '<tr>','<br><tab>');
  Replace(s,'</td>','<br><tab>');
  Replace(s,'</tr>','<br><tab>');

  if Pos('</p>',s) = 0 then Replace(s,'<p>','<br><tab>');
  Replace(s,'</p>','<br>');

  RemoveDoubleSpaces(s);
end;

procedure Replacement(var s: string);
begin
  Replace(s, '<br>',char($0A));
  Replace(s, '</p>',char($0A));
  Replace(s,'<tab>',char($09));

  Replace(s, '<S><r>' , '<R>');
  Replace(s,'</r></S>','</R>');
end;

function ApplyString(Tag: UnicodeString; var fp: TFontParams): boolean;
begin
  Result := true;

  if Tag = '<J>' then fp.Color := clSysMaroon else
  if Tag = '<h>' then fp.Style += [fsBold]    else
  if Tag = '<l>' then fp.Color := clSysNavy   else
  if Tag = '<n>' then fp.Color := clSysGray   else
  if Tag = '<r>' then fp.Color := clSysRed    else

  if Tag = '<f>' then begin fp.Color := clSysTeal;  fp.VScriptPos := vpSuperScript end else
  if Tag = '<m>' then begin fp.Color := clSysGray;  fp.VScriptPos := vpSuperScript end else
  if Tag = '<S>' then begin fp.Color := clSysBrown; fp.VScriptPos := vpSuperScript end else
  if Tag = '<R>' then begin fp.Color := clSysRed;   fp.VScriptPos := vpSuperScript end else

  Result := false;
end;

function ApplyHtml(Tag: UnicodeString; var fp: TFontParams): boolean;
begin
  Tag := LowerCase(Tag);
  Result := true;

  if Prefix('<a ', Tag{%H-}) then Tag := '<a>';

  if Tag = '<a>' then fp.Color := clSysGray else
  if Tag = '<h>' then fp.Style += [fsBold]  else
  if Tag = '<v>' then fp.Color := clSysGray else

  if Tag = '<b>' then begin fp.Color := clSysBrown; fp.Style += [fsBold]   end else
  if Tag = '<i>' then begin fp.Color := clSysGray;  fp.Style += [fsItalic] end else

  if Tag = '<sup>' then fp.VScriptPos := vpSuperScript else

  Result := false;
end;

function Apply(Tag: UnicodeString; var fp: TFontParams; html: boolean): boolean;
begin
  Result := ApplyHtml(Tag, fp);
  if html then Exit;
  if not Result then Result := ApplyString(Tag, fp);
end;

procedure Parse(Memo: TRichMemoEx; Source: string; html: boolean = false);
var
  fp, fp0: TFontParams;
  Original : UnicodeString;
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

    for n := i to Length(Original) do
      begin
        if Original[n] = '<' then IsTag := True;
        if IsTag then SubTag := SubTag + Original[n];

        if Original[n] = '>' then
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
  if html then HtmlReplacement(Source);
  Replacement(Source);

  Memo.Clear;
  fp0 := Memo.SelAttributes;

  Original := UnicodeString(Source);
  Memo.Text := String(RemoveTags(Source));

  for i := 1 to Length(Original) do
    begin
      if Original[i] = '<' then IsTag := True;

      if IsTag then Tag := Tag + Original[i]
               else inc(idx);

      if Original[i] = '>' then
        begin
          fp := fp0;
          if Apply(Tag, fp, html) then Memo.SetTextAttributes(idx, iLength(Tag), fp);
          IsTag := False;
          Tag := '';
        end;
    end;

  Memo.SelStart := 0;
end;

end.

