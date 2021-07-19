program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage, UnitTools, UnitModule,
  UnitBible, UnitCommentary, UnitDictionary, UnitReference, FormMain, FormAbout,
  FormNotify, FormSearch, FormCompare, FormCopy, FormShelf;

{$R *.res}

begin
  Application.Title := {$ifdef windows} 'Unbound Bible' {$else} 'unboundbible' {$endif};
  Application.Initialize;

  if Bibles.IsEmpty then Exit;

  Application.CreateForm(TMainForm,    MainForm);
  Application.CreateForm(TAboutBox,    AboutBox);
  Application.CreateForm(TNotifyForm,  NotifyForm);
  Application.CreateForm(TSearchForm,  SearchForm);
  Application.CreateForm(TCompareForm, CompareForm);
  Application.CreateForm(TCopyForm,    CopyForm);
  Application.CreateForm(TShelfForm,   ShelfForm);

  MainForm.LocalizeApplication;
  Application.Run;
end.

