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
  {$ifdef linux} Width := 390; {$endif}
end;

procedure TAboutBox.Translate;
var
  Developer : string;
begin
  Developer := ' ' + 'Vladimir Rybant';

  if facelang = 'russian'   then Developer := 'Владимир Рыбант';
  if facelang = 'ukrainian' then Developer := 'Володимир Рiбант';

  Caption := ' ' + T('Menu.About');
  LabelVersion.Caption := T('About.Version') + ' ' + VersionInfo;
  LabelTeam.Caption := T('About.Developer') + ': ' + Developer;
end;

procedure TAboutBox.ImageFacebookClick(Sender: TObject);
begin
  OpenURL('http://www.facebook.com/unbound.bible/');
end;

end.

