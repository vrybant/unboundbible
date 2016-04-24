unit UnitSearch;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, LCLType;

type
  TSearchForm = class(TForm)
    OKButton: TButton;
    Edit: TEdit;
    RadioGroupRange: TRadioGroup;
    RadioGroupType: TRadioGroup;
    GroupBoxOption: TGroupBox;
    CheckBoxCase: TCheckBox;
    CheckBoxWhole: TCheckBox;
    procedure EditChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    procedure Translate;
    function Range(n, CurrBook: integer): boolean;
  end;

var
  SearchForm: TSearchForm;

implementation

uses UnitShelf, UnitLang;

{$R *.lfm}

const
  rbEntireBible  = 0;
  rbOldTestament = 1;
  rbNewTestament = 2;
  rbGospels      = 3;
  rbEpistles     = 4;
  rbCurrentBook  = 5;

procedure TSearchForm.Translate;
begin
  Caption := ' ' + T('Search.Caption');

  RadioGroupType .Caption := T('Search.Type'   ) + ' ';
  RadioGroupRange.Caption := T('Search.Range'  ) + ' ';
  GroupBoxOption .Caption := T('Search.Options') + ' ';

  RadioGroupType.Items[0] := T('Search.Any'   );
  RadioGroupType.Items[1] := T('Search.Every' );
  RadioGroupType.Items[2] := T('Search.Phrase');

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

function TSearchForm.Range(n, CurrBook: integer): boolean;
begin
  Result := False;
  case RadioGroupRange.ItemIndex of
    rbEntireBible  :                                 Result := True;
    rbOldTestament : if (n >=  1) and (n <= 39) then Result := True;
    rbNewTestament : if (n >= 40) and (n <= 66) then Result := True;
    rbGospels      : if (n >= 40) and (n <= 43) then Result := True;
    rbEpistles     : if (n >= 45) and (n <= 65) then Result := True;
    rbCurrentBook  : if  n = CurrBook           then Result := True;
  end;
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

