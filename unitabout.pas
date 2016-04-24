unit UnitAbout;

interface

uses Classes, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, SysUtils,
     LCLIntf;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    LabelFB: TLabel;
    LabelGPL: TLabel;
    LabelName: TLabel;
    LabelTeam: TLabel;
    LabelVersion: TLabel;
    ImageFacebook: TImage;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ImageFacebookClick(Sender: TObject);
  public
    procedure Translate;
  end;

var
  AboutBox: TAboutBox;

implementation

uses UnitLib, UnitLang;

{$R *.lfm}

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  {$ifdef darwin}
  OKButton.Visible := False;
  Height := 210;
  {$endif}
end;

procedure TAboutBox.Translate;
begin
  Caption := ' ' + T('Menu.About');
//OKButton.Caption := T('Button.OK');

//  if FaceLang = 'russian' then Exit;

  LabelVersion.Caption := 'Version 1.0';
  LabelTeam.Caption := ' Developer: Vladimir Rybant ';
end;

procedure TAboutBox.ImageFacebookClick(Sender: TObject);
begin
  OpenURL('http://www.facebook.com/unbound.bible.tools/');
end;

end.

