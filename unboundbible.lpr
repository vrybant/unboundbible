program unboundbible;

uses
  {$ifdef unix}{$ifdef UseCThreads} cthreads, {$endif}{$endif}
  {$ifdef darwin} printer4lazarus, {$endif}
  Interfaces, Graphics, Forms, richmemopackage,
  UnitLang, UnitParse, UnitTool, UnitModule, UnitShelf, UnitCommentary, UnitDictionary,
  FormMain, FormAbout, FormNotify, FormSearch, FormCompare, FormCopy, FormTranslate,
  FormCommentary, FormDownload;

{$R *.res}

begin
  Application.Title := {$ifdef windows} 'Unbound Bible'; {$else} 'unboundbible'; {$endif}
  Application.Initialize;
  Application.CreateForm(TMainForm,       MainForm);
  Application.CreateForm(TAboutBox,       AboutBox);
  Application.CreateForm(TNotifyForm,     NotifyForm );
  Application.CreateForm(TSearchForm,     SearchForm );
  Application.CreateForm(TCompareForm,    CompareForm);
  Application.CreateForm(TCopyForm,       CopyForm);
  Application.CreateForm(TTranslateForm,  TranslateForm);
  Application.CreateForm(TCommentaryForm, CommentaryForm);
  Application.CreateForm(TDownloadForm,   DownloadForm);
  TranslateAll;
  Application.Run;
end.

