unit UnitSearch;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLType, UnitLib, UnitType;

type
  TSearchForm = class(TForm)
    OKButton: TButton;
    Edit: TEdit;
    RadioGroupRange: TRadioGroup;
    GroupBoxOption: TGroupBox;
    CheckBoxCase: TCheckBox;
    CheckBoxWhole: TCheckBox;
    procedure EditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    procedure Translate;
  end;

var
  SearchForm: TSearchForm;

function CurrentSearchOptions: TSearchOptions;
function CurrentSearchRange: TRange;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

const
  rbEntireBible  = 0;
  rbOldTestament = 1;
  rbNewTestament = 2;
  rbGospels      = 3;
  rbEpistles     = 4;
  rbOpenedBook   = 5;

const
  rgEntireBible  : TRange = (from: -1; till: -1);
  rgOldTestament : TRange = (from:  1; till: 39);
  rgNewTestament : TRange = (from: 40; till: 66);
  rgGospels      : TRange = (from: 40; till: 43);
  rgEpistles     : TRange = (from: 45; till: 66);

function CurrentSearchOptions: TSearchOptions;
begin
  Result := [];
  if SearchForm.CheckBoxCase .Checked then Result := Result + [wholeWords];
  if SearchForm.CheckBoxWhole.Checked then Result := Result + [caseSensitive];
end;

function CurrentSearchRange: TRange;
begin
  Result := rgEntireBible;
  case SearchForm.RadioGroupRange.ItemIndex of
    rbOldTestament : Result := rgOldTestament;
    rbNewTestament : Result := rgNewTestament;
    rbGospels      : Result := rgGospels;
    rbEpistles     : Result := rgEpistles;
    rbOpenedBook   :
      begin
        Result.from := ActiveVerse.book;
        Result.till := ActiveVerse.book;
      end;
  end;
end;

procedure TSearchForm.Translate;
begin
  Caption := ' ' + T('Search.Caption');

  RadioGroupRange.Caption := T('Search.Range'  ) + ' ';
  GroupBoxOption .Caption := T('Search.Options') + ' ';

  RadioGroupRange.Items[0] := T('Search.Entire'  );
  RadioGroupRange.Items[1] := T('Search.Old'     );
  RadioGroupRange.Items[2] := T('Search.New'     );
  RadioGroupRange.Items[3] := T('Search.Gospels' );
  RadioGroupRange.Items[4] := T('Search.Epistles');
  RadioGroupRange.Items[5] := T('Search.Current' );

  CheckBoxWhole.Caption := T('Search.Whole');
  CheckBoxCase .Caption := T('Search.Case' );

  OKButton     .Caption := T('Search.Button');
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin
  OKButton.Enabled := False;
end;

procedure TSearchForm.EditChange(Sender: TObject);
begin
  OKButton.Enabled := Length(Trim(Edit.Text)) > 2;
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then Close;
end;

procedure TSearchForm.FormActivate(Sender: TObject);
begin
  if Bible.FileType = 'text' then
    begin
      RadioGroupRange.Enabled := False;
      RadioGroupRange.ItemIndex := rbEntireBible;
    end
  else
    RadioGroupRange.Enabled := True;
end;

end.

