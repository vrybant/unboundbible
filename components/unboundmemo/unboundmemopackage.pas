{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit unboundmemopackage;

{$warn 5023 off : no warning about unused units}
interface

uses
  UnboundMemo, UmParseWin, UmLib, rmGtk2ex, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UnboundMemo', @UnboundMemo.Register);
end;

initialization
  RegisterPackage('unboundmemopackage', @Register);
end.
