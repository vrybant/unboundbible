program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage,
  UnitShelf, UnitCommentary,
  UnitMain, UnitLang, UnitAbout, UnitSearch, UnitTool, UnitCompare,
  UnitInfo, UnitCopy, UnitTrans, UnitStream, UnitParse;

{$R *.res}

begin
  Application.Title := {$ifdef windows} 'Unbound Bible'; {$else} 'unboundbible'; {$endif}
  Application.Initialize;
  Application.CreateForm(TMainForm,      MainForm);
  Application.CreateForm(TAboutBox,      AboutBox);
  Application.CreateForm(TSearchForm,    SearchForm);
  Application.CreateForm(TCompareForm,   CompareForm);
  Application.CreateForm(TFormInfo,      FormInfo);
  Application.CreateForm(TFormCopy,      FormCopy);
  Application.CreateForm(TFormTranslate, FormTranslate);
  MainForm.TranslateAll;
  Application.Run;
end.

