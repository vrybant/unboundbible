{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazPackager;

interface

uses
  frmLazPackagerOptionsDeb, LazPackagerMain, LazPackagerBase, 
  frmLazPackagerPreview, frmLazPackagerMakeDeb, LazPackagerDebian, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazPackagerMain', @LazPackagerMain.Register);
end;

initialization
  RegisterPackage('LazPackager', @Register);
end.
