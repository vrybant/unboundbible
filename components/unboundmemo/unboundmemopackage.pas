{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit unboundmemopackage;

interface

uses
  UnboundMemo, RichStream, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UnboundMemo', @UnboundMemo.Register);
end;

initialization
  RegisterPackage('unboundmemopackage', @Register);
end.
