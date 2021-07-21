program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage,
  FormMain, FormAbout, FormNotify, FormSearch, FormCompare, FormCopy, FormShelf,
  UnitTools;

{$R *.res}

begin
  if Tools.Bibles.IsEmpty then Exit;

  Application.Title := {$ifdef windows} 'Unbound Bible' {$else} 'unboundbible' {$endif};
  Application.Initialize;

  Application.CreateForm(TMainForm,    MainForm);
  Application.CreateForm(TAboutBox,    AboutBox);
  Application.CreateForm(TNotifyForm,  NotifyForm);
  Application.CreateForm(TSearchForm,  SearchForm);
  Application.CreateForm(TCompareForm, CompareForm);
  Application.CreateForm(TCopyForm,    CopyForm);
  Application.CreateForm(TShelfForm,   ShelfForm);

  Application.Run;
end.

