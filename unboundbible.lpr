program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage,
  FormMain, FormAbout, FormNotify, FormSearch, FormCopy, FormShelf, FormOptions,
  UnitTools;

{$R *.res}

begin
  if Tools.Bibles.IsEmpty then Exit;

  {$ifdef windows} Application.Title := 'Unbound Bible'; {$endif}
  {$ifdef linux}   Application.Title := 'unboundbible';  {$endif}
  Application.Initialize;

  Application.CreateForm(TMainForm,    MainForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TAboutBox,    AboutBox);
  Application.CreateForm(TNotifyForm,  NotifyForm);
  Application.CreateForm(TSearchForm,  SearchForm);
  Application.CreateForm(TCopyForm,    CopyForm);
  Application.CreateForm(TShelfForm,   ShelfForm);
  Application.Run;
end.

