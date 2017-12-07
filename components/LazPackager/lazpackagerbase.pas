{ LazDebian contains base class with common functionality
  for all packagers.

  Copyright (C) 2012 Bernd Kreuss prof7bit@gmail.com

  This source is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  A copy of the GNU General Public License is available on the World Wide
  Web at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by
  writing to the Free Software Foundation, Inc., 59 Temple Place - Suite
  330, Boston, MA 02111-1307, USA.
}

unit LazPackagerBase;

{$mode objfpc}{$H+}

interface

const
  TAB = #9;
  LF = #10;

  CONFNAME_BASE = 'lazpackager';

  DEFAULT_EXPORT
    = '?CP? *.lpi ?TEMPFOLDER?/' + LF
    + '?CP? *.lpr ?TEMPFOLDER?/' + LF
    + '?CP? *.pas ?TEMPFOLDER?/' + LF
    + '?CP? *.lfm ?TEMPFOLDER?/' + LF
    + '?CP? *.ico ?TEMPFOLDER?/' + LF
    ;

type
  { TPackagerBase all packagers will inherit from this base class.
    A packager will inherit and use these methods to load/save
    settings to the project file, get needed names and paths, etc.
    and also add even more specific functionality for example to
    deal with debian specific control files, etc. it will usually
    create a shell script from all this information and run it.
    See TPackagerDebian for an example }
  TPackagerBase = class
  public
    AuthorCopyright: String;
    Description: String;
    DescriptionLong: String;
    Maintainer: String;
    MaintainerEmail: String;
    PackageName: String;
    ExportCommands: String;
    constructor Create;
    destructor Destroy; override;
    procedure Save; virtual;
    procedure Load; virtual;
    function GetOriginalProjectVersion: String;
    function FillTemplate(Template: String): String; virtual;
    procedure DoMakePackage(Binary, Sign, Upload: Boolean); virtual; abstract;
  protected
    procedure SaveValue(Namespace, Key, Value: String);
    function LoadValue(Namespace, Key, DefaultValue: String): String;
    function GetCopyCommand: String;
    function GetBuildScriptName: String; virtual; abstract;
    function GetBuildScriptInterpreter: String; virtual;
    procedure RunBuildScript(Data: PtrInt);
    procedure RunBuildScriptAsync;
    function GetExecutableFilenameRelative: String;
    function GetProjectFilenameRelative: String;
    function GetOrigFolderNameOnly: String;
    function GetTempPathAbsolute: String;
    function GetProjectPathAbsolute: String;
  end;

procedure CreateFile(FullPathName, Contents: String);
procedure ReplaceMany(var T: String; R: array of String);

implementation
uses
  Classes,
  sysutils,
  Forms,
  LCLProc,
  FileUtil,
  LazIDEIntf,
  ProjectResourcesIntf,
  MacroIntf,
  //W32VersionInfo,
  IDEExternToolIntf;

type

  { TMyAbstractProjectResources }
  TMyAbstractProjectResources = class(TAbstractProjectResources)
    // need this to call a protected class function
    class function GetList: TList;
  end;

  TFileProductVersion = array[0..3] of word;

  {This should have the same memory layout as the original one}
  TSameLayoutAsTProjectVersionInfo = class(TAbstractProjectResource)
    FAutoIncrementBuild: boolean;
    FHexCharSet: string;
    FHexLang: string;
    FStringTable: TObject;
    FUseVersionInfo: boolean;
    FVersion: TFileProductVersion;
    // more fields follow but we are not
    // interested anymore, only need FVersion
  end;


procedure CreateFile(FullPathName, Contents: String);
var
  S: TFileStream;
begin
  try
    S := TFileStream.Create(FullPathName, fmCreate);
    S.Write(Contents[1], Length(Contents));
  finally
    S.Free;
  end;
end;

procedure ReplaceMany(var T: String; R: array of String);
var
  I,J: Integer;
begin
  for I := 0 to High(R) div 2 do begin
    J := I shl 1;
    T := StringReplace(T, R[J], R[J+1], [rfReplaceAll]);
  end;
end;

{ TMyAbstractProjectResources }

class function TMyAbstractProjectResources.GetList: TList;
begin
  Result := GetRegisteredResources;
end;

{ TPackagerBase }

constructor TPackagerBase.Create;
begin
  Load;
end;

destructor TPackagerBase.Destroy;
begin
  inherited Destroy;
end;

procedure TPackagerBase.Save;
begin
  SaveValue(CONFNAME_BASE, 'copyright', AuthorCopyright);
  SaveValue(CONFNAME_BASE, 'description', Description);
  SaveValue(CONFNAME_BASE, 'description_long', DescriptionLong);
  SaveValue(CONFNAME_BASE, 'maintainer', Maintainer);
  SaveValue(CONFNAME_BASE, 'maintainer_email', MaintainerEmail);
  SaveValue(CONFNAME_BASE, 'package_name', PackageName);
  SaveValue(CONFNAME_BASE, 'export_cmd', ExportCommands);
end;

procedure TPackagerBase.Load;
begin
  AuthorCopyright := LoadValue(CONFNAME_BASE, 'copyright', '2012 Jane Doe');
  Description := LoadValue(CONFNAME_BASE, 'description', 'this is a program');
  DescriptionLong := LoadValue(CONFNAME_BASE, 'description_long', 'long description may not be empty!');
  Maintainer := LoadValue(CONFNAME_BASE, 'maintainer', 'John Doe');
  MaintainerEmail := LoadValue(CONFNAME_BASE, 'maintainer_email', 'john_doe@example.invalid');
  PackageName := LoadValue(CONFNAME_BASE, 'package_name', 'debian-package-name');
  ExportCommands := LoadValue(CONFNAME_BASE, 'export_cmd', DEFAULT_EXPORT);
end;

function TPackagerBase.GetOriginalProjectVersion: String;
var
  ResList: TAbstractProjectResources;
  Resource: TAbstractProjectResource;
  ResClass: TAbstractProjectResourceClass;
  ResClassList: TList;
  P: Pointer;
  VerInfo: TSameLayoutAsTProjectVersionInfo;

begin
  ResList := LazarusIDE.ActiveProject.Resources as TAbstractProjectResources;
  ResClassList := TMyAbstractProjectResources.GetList;
  for P in ResClassList do begin
    ResClass := TAbstractProjectResourceClass(p);
    Resource := ResList.Resource[ResClass];
    if Resource.ClassName = 'TProjectVersionInfo' then begin
      {$warning please give me a proper interface in the IDE to replace this ugly hack}
      VerInfo := TSameLayoutAsTProjectVersionInfo(Resource);
      Result := Format('%d.%d.%d.%d', [VerInfo.FVersion[0],
                                       VerInfo.FVersion[1],
                                       VerInfo.FVersion[2],
                                       VerInfo.FVersion[3]]);
      break;
    end;
  end;
end;

function TPackagerBase.FillTemplate(Template: String): String;
var
  Version: String;

begin
  Template := ConvertLineEndings(Template);
  ReplaceMany(Template, ['?COPYRIGHT?',         AuthorCopyright
                        ,'?DESCRIPTION?',       Description
                        ,'?DESCRIPTION_LONG?',  DescriptionLong
                        ,'?MAINTAINER?',        Maintainer
                        ,'?MAINTAINER_EMAIL?',  MaintainerEmail
                        ,'?PACKAGE_NAME?',      PackageName
                        ,'?VERSION?',           GetOriginalProjectVersion
                        ,'?EXECUTABLE?',        GetExecutableFilenameRelative
                        ,'?PROJECT?',           GetProjectFilenameRelative
                        ,'?TEMPFOLDER?',        GetTempPathAbsolute
                        ,'?CP?',                GetCopyCommand
                        ]);
  Result := Template;
end;

procedure TPackagerBase.SaveValue(Namespace, Key, Value: String);
begin
  LazarusIDE.ActiveProject.CustomData.Values[Namespace + '/' + Key] := Value;
  LazarusIDE.ActiveProject.Modified := True;
end;

function TPackagerBase.LoadValue(Namespace, Key, DefaultValue: String): String;
begin
  Result := LazarusIDE.ActiveProject.CustomData.Values[Namespace + '/' + Key];
  if Result = '' then begin
    Result := DefaultValue;
  end;
end;

function TPackagerBase.GetCopyCommand: String;
begin
  {$ifdef windows}
  Result := '$(Make)';
  IDEMacros.SubstituteMacros(Result);
  Result := ConcatPaths([ExtractFilePath(Result), 'cp.exe']);
  {$else}
  Result := 'cp';
  {$endif}
end;

function TPackagerBase.GetBuildScriptInterpreter: String;
begin
  // individual packagers may override this, especially a
  // packager for windows might want to use cmd.exe for
  // its build script instead of sh.
  Result := '/bin/sh';
end;

procedure TPackagerBase.RunBuildScript(Data: PtrInt);
var
  Tool: TIDEExternalToolOptions;
begin
  Tool := TIDEExternalToolOptions.Create;
  Tool.Filename := GetBuildScriptInterpreter;
  Tool.CmdLineParams := GetBuildScriptName;
  Tool.WorkingDirectory := GetProjectPathAbsolute;
  Tool.ShowAllOutput := True;
  RunExternalTool(Tool);
  Tool.Free;
  Self.Free;
end;

procedure TPackagerBase.RunBuildScriptAsync;
begin
  Application.QueueAsyncCall(@Self.RunBuildScript, 0);
end;

function TPackagerBase.GetExecutableFilenameRelative: String;
begin
  Result:='$(TargetFile)';
  if not IDEMacros.SubstituteMacros(Result) then
    raise Exception.Create('unable to retrieve target file of project');
  Result := CreateRelativePath(Result, GetProjectPathAbsolute);
end;

function TPackagerBase.GetProjectFilenameRelative: String;
begin
  Result := LazarusIDE.ActiveProject.ProjectInfoFile;
  Result := CreateRelativePath(Result, GetProjectPathAbsolute);
end;

function TPackagerBase.GetOrigFolderNameOnly: String;
begin
  Result := Format('%s-%s', [PackageName, GetOriginalProjectVersion]);
end;

function TPackagerBase.GetTempPathAbsolute: String;
begin
  Result := ConcatPaths([GetTempDir, GetOrigFolderNameOnly]);
end;

function TPackagerBase.GetProjectPathAbsolute: String;
begin
  Result := ExtractFileDir(LazarusIDE.ActiveProject.ProjectInfoFile);
end;


end.

