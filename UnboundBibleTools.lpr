program UnboundBibleTools;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage, UnitShelf,
  UnitMain, UnitLang, UnitAbout, UnitInfo, UnitSearch, UnitTool, UnitCompare,
  UnitOptions, UnitCopy, UnitTrans, UnitTip, UnitStream;

{$R *.res}

begin
  Application.Title:='Unbound Bible';
  Application.Initialize;
  Application.CreateForm(TMainForm,      MainForm);
  Application.CreateForm(TAboutBox,      AboutBox);
  Application.CreateForm(TInfoBox,       InfoBox);
  Application.CreateForm(TSearchForm,    SearchForm);
  Application.CreateForm(TCompareForm,   CompareForm);
  Application.CreateForm(TFormOptions,   FormOptions);
  Application.CreateForm(TFormCopy,      FormCopy);
  Application.CreateForm(TFormTip,       FormTip);
  Application.CreateForm(TFormTranslate, FormTranslate);
  MainForm.TranslateAll;
  Application.Run;
end.

