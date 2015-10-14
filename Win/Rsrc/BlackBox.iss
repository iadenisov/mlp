[Setup]
AppName=Multylayer Perceptron
AppVerName={#AppVerName}
AppPublisher=Siberian Federal University
AppPublisherURL=http://molpit.org
AppVersion={#AppVersion}
VersionInfoVersion={#VersionInfoVersion}
AppCopyright=Copyright (c) 2015 Ivan Denisov, MOLPIT.
Compression=bzip
SolidCompression=yes
PrivilegesRequired=poweruser
DefaultDirName={pf}\MLP {#AppVersion}
UsePreviousAppDir=no
SetupIconFile=Win\Rsrc\Applogo.ico
UninstallDisplayIcon={uninstallexe}
LicenseFile=LICENSE.txt
DisableProgramGroupPage=yes

[Icons]
Name: "{userdesktop}\MLP {#AppVersion}"; Filename: "{app}\MLP.exe"; WorkingDir: "{app}"

[Files]
Source: "*"; Excludes: "*~,odc*,Output,LICENSE.txt"; DestDir: "{app}\"; Flags: replacesameversion recursesubdirs
