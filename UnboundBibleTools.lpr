program UnboundBibleTools;

{$mode objfpc}{$H+}

uses
  {$ifdef unux}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef unix} printer4lazarus, {$endif}
  Interfaces, // this includes the LCL widgetset
  Graphics, Forms, UnitMain, richmemopackage, UnitEdit, UnitShelf, UnitLang,
  UnitAbout, UnitInfo, UnitLib, UnitSearch, UnitTool, UnitCompare, UnitClass,
  UnitOptions, UnitCopy, UnitMemo, unittrans;

{$R *.res}

begin
  Application.Title:='Unbound Bible Tools';
  Application.Initialize;
  Application.CreateForm(TMainForm,    MainForm);
  Application.CreateForm(TAboutBox,    AboutBox);
  Application.CreateForm(TInfoBox,     InfoBox);
  Application.CreateForm(TSearchForm,  SearchForm);
  Application.CreateForm(TCompareForm, CompareForm);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormCopy,    FormCopy);
  Application.CreateForm(TFormTranslate, FormTranslate);
  MainForm.TranslateAll;
  Application.Run;
end.
