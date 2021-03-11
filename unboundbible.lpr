program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage,
  UnitTool, UnitModule, UnitShelf, UnitCommentary, UnitDictionary, UnitReference,
  FormMain, FormAbout, FormNotify, FormSearch, FormFavorite, FormCopy, FormDownload;

{$R *.res}

begin
  Application.Title := {$ifdef windows} 'Unbound Bible'; {$else} 'unboundbible'; {$endif}
  Application.Initialize;
  Application.CreateForm(TMainForm,     MainForm);
  Application.CreateForm(TAboutBox,     AboutBox);
  Application.CreateForm(TNotifyForm,   NotifyForm);
  Application.CreateForm(TSearchForm,   SearchForm);
  Application.CreateForm(TFavoriteForm, FavoriteForm);
  Application.CreateForm(TCopyForm,     CopyForm);
  Application.CreateForm(TDownloadForm, DownloadForm);
  MainForm.LocalizeApplication;
  Application.Run;
end.

