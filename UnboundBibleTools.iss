; -- Verse.iss --

[Setup]
 AppName=Unbound Bible Tools 1.0
 AppVerName=Unbound Bible Tools 1.0
 AppVersion=1.0
 AppCopyright=GNU GPL 
 AppPublisher=Vladimir Rybant 
 AppPublisherURL=http://vladimirrybant.org
 AppSupportURL=http://vladimirrybant.org

 DefaultDirName={pf}\Unbound Bible Tools
 DefaultGroupName=Unbound Bible Tools
 DisableStartupPrompt=yes
 DisableProgramGroupPage=yes
 OutputBaseFilename=ub-setup
 UninstallDisplayIcon={app}\UnboundBibleTools.exe
;place result in the file in the source directory
 OutputDir=. 
             
 Compression=bzip
 SolidCompression=yes

[Languages]
 Name: en; MessagesFile: "compiler:Default.isl"
 Name: ru; MessagesFile: "compiler:Languages\Russian.isl"
 Name: de; MessagesFile: "compiler:Languages\German.isl"
 Name: fr; MessagesFile: "compiler:Languages\French.isl"
 Name: fi; MessagesFile: "compiler:Languages\Finnish.isl"
;Name: it; MessagesFile: "compiler:Languages\Italian.isl"
;Name: se; MessagesFile: "compiler:Languages\Swedish.isl"
;Name: no; MessagesFile: "compiler:Languages\Norwegian.isl"
;Name: dk; MessagesFile: "compiler:Languages\Danish.isl"
;Name: cs; MessagesFile: "compiler:Languages\ChineseSimp.isl"

[CustomMessages]
 en.LaunchProgram=Launch application
 ru.LaunchProgram=Запустить программу
 de.LaunchProgram=Unbound Bible Tools starten
 fr.LaunchProgram=Exécuter Unbound Bible Tools
 fi.LaunchProgram=Käynnistä Unbound Bible Tools
;it.LaunchProgram=Avvia Unbound Bible Tools
;cs.LaunchProgram=运行 Unbound Bible Tools

[Dirs]
 Name: "{userappdata}\Unbound Bible Tools";

[Files]
;Source: "bibles\*"                     ; DestDir: "{app}\bibles"
 Source: "bibles\english-kjv.txt"       ; DestDir: "{app}\bibles"
 Source: "bibles\russian.txt"           ; DestDir: "{app}\bibles"
 Source: "bibles\greek-nt.txt"          ; DestDir: "{app}\bibles"
 Source: "localization\*"               ; DestDir: "{app}\localization"
 Source: "titles\*"                     ; DestDir: "{app}\titles"
 Source: "UnboundBibleTools.exe"        ; DestDir: "{app}"

[Icons]
 Name: "{commondesktop}\Unbound Bible Tools"                      ; Filename: "{app}\UnboundBibleTools.exe" ; WorkingDir: "{app}"; 
 Name: "{commonprograms}\Unbound Bible Tools\Unbound Bible Tools" ; Filename: "{app}\UnboundBibleTools.exe" ; WorkingDir: "{app}"
 Name: "{commonprograms}\Unbound Bible Tools\Uninstall Unbound Bible Tools" ; Filename: "{app}\unins000.exe"; WorkingDir: "{app}"

[Run]                       
 Filename: "{app}\UnboundBibleTools.exe" ; Description: "{cm:LaunchProgram}"; Flags: postinstall nowait skipifsilent

[UninstallDelete]
 Type: files      ; Name: "{userappdata}\Unbound Bible Tools\config.ini"
 Type: dirifempty ; Name: "{userappdata}\Unbound Bible Tools\bibles"
 Type: dirifempty ; Name: "{userappdata}\Unbound Bible Tools"
 Type: dirifempty ; Name: "{app}"
 Type: dirifempty ; Name: "{commonprograms}\Unbound Bible Tools"

[Tasks]
;Name: installall; Description: All users; GroupDescription: Install for:; Flags: exclusive
;Name: installuser; Description: The current user only; GroupDescription: Install for:; Flags: exclusive unchecked

[Files]
;Source: data.xml; DestDir: {commonappdata}\AppDir; Flags: onlyifdoesntexist; Tasks: installall
;Source: data.xml; DestDir: {userappdata}\AppDir; Flags: onlyifdoesntexist; Tasks: installuser
