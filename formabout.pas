unit FormAbout;

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

  if LocalLang = 'ru'   then Developer := 'Владимир Рыбант';
  if LocalLang = 'uk' then Developer := 'Володимир Рiбант';

  Caption := ' ' + T('About');
  LabelVersion.Caption := T('Version') + ' ' + VersionInfo;
  LabelTeam.Caption := T('Developer') + ': ' + Developer;
end;

procedure TAboutBox.ImageFacebookClick(Sender: TObject);
begin
  OpenURL('http://www.facebook.com/unbound.bible/');
end;

end.

