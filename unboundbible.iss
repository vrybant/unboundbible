 
; -- unboundbible.iss --
                             
#define MyAppName "Unbound Bible"
#define MyAppVerName "Unbound Bible 3"
#define MyAppVersion "3.5"
#define MyAppCopyright "GNU GPL"
#define MyAppPublisher "Vladimir Rybant"
#define MyAppURL "http://vladimirrybant.org"
#define MyAppExeName "unboundbible.exe"
#define MyAppOutput "unboundbible"

[Setup]
 AppName={#MyAppName}
 AppVerName={#MyAppVerName}                     
 AppVersion={#MyAppVersion}
 AppCopyright={#MyAppCopyright}         
 AppPublisher={#MyAppPublisher}
 AppPublisherURL={#MyAppURL}
 AppSupportURL={#MyAppURL}
 AppUpdatesURL={#MyAppURL}

 DefaultDirName={commonpf}\{#MyAppName}
 DefaultGroupName={#MyAppName}
 DisableWelcomePage=no
 DisableStartupPrompt=yes
 DisableProgramGroupPage=yes
 OutputBaseFilename={#MyAppOutput}_{#MyAppVersion}
 UninstallDisplayIcon={app}\{#MyAppExeName}
 OutputDir=. 
             
 Compression=bzip
 SolidCompression=yes

[Dirs]
;Name: "{%USERPROFILE}\{#MyAppName}";

[Files]
;Source: "bibles\*"           ; DestDir: "{%USERPROFILE}\{#MyAppName}"
 Source: "bibles\*"           ; DestDir: "{app}\bibles"
 Source: "localization\*"     ; DestDir: "{app}\localization"
 Source: "titles\*"           ; DestDir: "{app}\titles" 
 Source: "{#MyAppExeName}"    ; DestDir: "{app}" ; Flags: ignoreversion
 Source: "sqlite3.dll"        ; DestDir: "{app}"

[Icons]
;Name: "{commondesktop}\{#MyAppName}"                         ; Filename: "{app}\{#MyAppExeName}" ; WorkingDir: "{app}"; Tasks: desktopicon
 Name: "{commondesktop}\{#MyAppName}"                         ; Filename: "{app}\{#MyAppExeName}" ; WorkingDir: "{app}" 
 Name: "{commonprograms}\{#MyAppName}\{#MyAppName}"           ; Filename: "{app}\{#MyAppExeName}" ; WorkingDir: "{app}"
 Name: "{commonprograms}\{#MyAppName}\Uninstall {#MyAppName}" ; Filename: "{app}\unins000.exe"    ; WorkingDir: "{app}"

[Run]                       
 Filename: "{app}\{#MyAppExeName}" ; Description: "{cm:LaunchProgram,{#MyAppName}}"; Flags: postinstall nowait skipifsilent

[UninstallDelete]
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\kjv.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\kjv+.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\rst+.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\rstw.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\ubio.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\strong.dct.unbound"
 Type: files          ; Name: "{%USERPROFILE}\{#MyAppName}\strongru.dct.unbound"
 Type: dirifempty     ; Name: "{%USERPROFILE}\{#MyAppName}"
 Type: filesandordirs ; Name: "{localappdata}\{#MyAppName}"
 Type: dirifempty     ; Name: "{app}"

[Tasks]
;Name: desktopicon; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
                 
[Languages]
 Name: en; MessagesFile: "compiler:Default.isl"
;Name: am; MessagesFile: "compiler:Languages\Armenian.islu"
;Name: br; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"
;Name: ct; MessagesFile: "compiler:Languages\Catalan.isl"
;Name: cr; MessagesFile: "compiler:Languages\Corsican.isl"
;Name: cz; MessagesFile: "compiler:Languages\Czech.isl"
;Name: dk; MessagesFile: "compiler:Languages\Danish.isl"
;Name: nl; MessagesFile: "compiler:Languages\Dutch.isl"
 Name: fl; MessagesFile: "compiler:Languages\Finnish.isl"
 Name: fr; MessagesFile: "compiler:Languages\French.isl"
 Name: de; MessagesFile: "compiler:Languages\German.isl"
;Name: gr; MessagesFile: "compiler:Languages\Greek.isl"
 Name: il; MessagesFile: "compiler:Languages\Hebrew.isl"
;Name: hu; MessagesFile: "compiler:Languages\Hungarian.isl"
 Name: it; MessagesFile: "compiler:Languages\Italian.isl"
;Name: jp; MessagesFile: "compiler:Languages\Japanese.isl"
;Name: np; MessagesFile: "compiler:Languages\Nepali.islu"
;Name: no; MessagesFile: "compiler:Languages\Norwegian.isl"
 Name: pl; MessagesFile: "compiler:Languages\Polish.isl"
 Name: pt; MessagesFile: "compiler:Languages\Portuguese.isl"
 Name: ru; MessagesFile: "compiler:Languages\Russian.isl"
;Name: sg; MessagesFile: "compiler:Languages\ScottishGaelic.isl"
;Name: rs; MessagesFile: "compiler:Languages\SerbianCyrillic.isl"
;Name: rl; MessagesFile: "compiler:Languages\SerbianLatin.isl"
;Name: si; MessagesFile: "compiler:Languages\Slovenian.isl"
 Name: es; MessagesFile: "compiler:Languages\Spanish.isl"
;Name: tr; MessagesFile: "compiler:Languages\Turkish.isl"
 Name: ua; MessagesFile: "compiler:Languages\Ukrainian.isl"
