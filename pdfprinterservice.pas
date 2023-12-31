unit pdfprinterservice;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs,
  Vcl.ExtCtrls, Winapi.TlHelp32, Winapi.ShellAPI, Win.Registry, Winapi.WinSvc, Winapi.PsAPI;

const
  WTS_CURRENT_SERVER_HANDLE = 0;

type
  WTS_CONNECTSTATE_CLASS = (
    WTSActive,              // User logged on and fully active
    WTSConnected,           // User logged on but disconnected
    WTSConnectQuery,        // Trying to connect
    WTSShadow,              // Shadowing another user's session
    WTSDisconnected,        // User logged on but disconnected
    WTSIdle,                // User logged on but idle
    WTSListen,              // Waiting for a connection
    WTSReset,               // Session is being reset
    WTSDown,                // Session is down due to an error
    WTSInit);               // Session is in the process of being initialized

  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
  private
    { Private declarations }
    FStatusHandle: SERVICE_STATUS_HANDLE;
    FNotificationHandle: HWND;
    procedure WndProc(var Msg: TMessage);
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  Service1: TService1;

function WTSQuerySessionInformation(hServer: THandle; SessionId: DWORD;
  WTSInfoClass: DWORD; var pBuffer: Pointer; var BytesReturned: DWORD): Boolean; stdcall; external 'wtsapi32.dll';
function WTSQueryUserToken(SessionId: Cardinal; phToken: PHandle): LONGBOOL; stdcall;
  external 'wtsapi32.dll';
function CreateEnvironmentBlock(var lpEnvironment: PVoid; hToken: THANDLE;
  bInherit: Boolean): LONGBOOL; stdcall;
  external 'userenv.dll';
function DestroyEnvironmentBlock(lpEnvironment: PVoid): LONGBOOL; stdcall;
  external 'userenv.dll';

implementation

{$R *.dfm}

function IsDefaultShellExplorer: Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon', False) then
    begin
      Result := (UpperCase(Registry.ReadString('Shell')) = 'EXPLORER.EXE');
      Registry.CloseKey;
    end
    else
      Result := False; // Unable to read the registry key
  finally
    Registry.Free;
  end;
end;

function IsProgramRunningForCurrentUser(const ProgramPath: string): Boolean;
var
  SnapProcHandle: THandle;
  ProcEntry: TProcessEntry32;
  CurrentSessionID: DWORD;
  ProcHandle: THandle;
  ProcPID: DWORD;
  fullPath: string;
  exeName: string;
begin
  Result := False;


  // let's also check the taskbar availability as
  if IsDefaultShellExplorer then
  begin
    Result := FindWindow('Shell_TrayWnd', nil) <> 0;
    // if the default shell is explorer.exe we should wait for the taskbar to be available
    if not Result then
      Exit;
  end;

  CurrentSessionID := WTSGetActiveConsoleSessionId;

  if CurrentSessionID = 0 then
  begin
    Result := True; // if no logon user is present let's just assume the program is running :P
    Exit;
  end;


  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapProcHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      ProcEntry.dwSize := SizeOf(TProcessEntry32);

      exeName := ExtractFileName(ProgramPath);
      if Process32First(SnapProcHandle, ProcEntry) then
      begin
        repeat
          if SameText(exeName, ProcEntry.szExeFile) then
          begin
            ProcPID := ProcEntry.th32ProcessID;
            ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcPID);
            if ProcHandle <> 0 then
            begin
              SetLength(fullPath, MAX_PATH);
              if GetModuleFileNameEx(ProcHandle, 0, PChar(fullPath), MAX_PATH) > 0 then
              begin
                SetLength(fullPath, StrLen(PChar(fullPath)));
                if SameText(ProgramPath, fullPath) then
                begin
                  if ProcessIdToSessionId(ProcEntry.th32ProcessID, CurrentSessionID) then
                  begin
                    Result := True;
                    CloseHandle(SnapProcHandle);
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        until not Process32Next(SnapProcHandle, ProcEntry);
      end;

    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

end;

function CreateProcessOnUserDesktop(ACmdLine: string): Boolean;
var
  LToken: THandle;
  LDupToken: THandle;
  LSessionID: DWORD;
  LStartUpInfo: TStartupInfo;
  LPointer: PVoid;
  LProcessInfo: TProcessInformation;
  LDesktop: string;
begin
  ZeroMemory(@LStartUpInfo, SizeOf(TStartupInfo));
  ZeroMemory(@LProcessInfo, SizeOf(TProcessInformation));
  try
    LSessionID := WTSGetActiveConsoleSessionId;
    if LSessionID = 0 then Exit;
    

    WTSQueryUserToken(LSessionID, @LToken);

    DuplicateTokenEx(LToken, TOKEN_ASSIGN_PRIMARY or TOKEN_ALL_ACCESS,
      nil, SecurityIdentification, TokenPrimary, LDupToken);

    LStartUpInfo.cb := SizeOf(TStartupInfo);
    LDesktop := 'winsta0\default';
    LStartUpInfo.lpDesktop := PChar(LDesktop);
    LStartUpInfo.dwFlags := STARTF_USESHOWWINDOW;
    LStartUpInfo.wShowWindow := SW_SHOW;

    LPointer := nil;
    if CreateEnvironmentBlock(LPointer, LDupToken, False) then
    begin
      Result := CreateProcessAsUser(LDupToken,
        nil, PChar(ACmdLine), nil, nil, False,
        CREATE_UNICODE_ENVIRONMENT or DETACHED_PROCESS,
        LPointer, PChar(ExtractFilePath(ACmdLine)),
        LStartUpInfo,
        LProcessInfo
        );
      if Assigned(LPointer) then
        DestroyEnvironmentBlock(LPointer);
    end;

  finally
    CloseHandle(LToken);
    CloseHandle(LDupToken);
  end;
end;

function IsExeRunning(const sExeName : String): Boolean;
var
  SnapProcHandle: THandle;
  ProcEntry: TProcessEntry32;
  NextProc: Boolean;
begin
  result := False;

  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  if SnapProcHandle = INVALID_HANDLE_VALUE then
    exit;

  ProcEntry.dwSize := SizeOf(ProcEntry);
  NextProc := Process32First(SnapProcHandle, ProcEntry);
  while NextProc do begin
    if UpperCase(StrPas(ProcEntry.szExeFile)) = UpperCase(sExeName) then begin
      result := True;
      break;
    end;
    NextProc := Process32Next(SnapProcHandle, ProcEntry);
  end;
  CloseHandle(SnapProcHandle);
end;

function GetProcessList(ProcessList: TStrings): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  ProcessList.Clear;
  while Integer(ContinueLoop) <> 0 do
  begin
    ProcessList.Add(FProcessEntry32.szExeFile);
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
  Result := ProcessList.Count > 0;
end;

function CheckIfProcessIsRunning(const ProcessName, FullPath: string): Boolean;
var
  ProcessList: TStrings;
  I: Integer;
begin
  Result := False;
  ProcessList := TStrings.Create;
  try
    if GetProcessList(ProcessList) then
    begin
      for I := 0 to ProcessList.Count - 1 do
      begin
        if SameFileName(FullPath, ProcessList[I]) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  finally
    ProcessList.Free;
  end;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      Reg.WriteString('Description', 'PDF Printer Monitoring service that ensures the TCP loopback server is always running.');
      Reg.WriteString('ImagePath', AnsiQuotedStr(ParamStr(0), '"'));
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;
end;

procedure TService1.ServiceExecute(Sender: TService);
const
  SecBetweenRuns = 3;
var
  Count: Integer;
  ExeFullPath: string;
begin
  Count := 0;
  while not Terminated do
  begin
//    WaitForSingleObject(FStatusHandle, INFINITE);

    Inc(Count);
    if Count >= SecBetweenRuns then
    begin
      Count := 0;

      ExeFullPath := ExtractFilePath(ParamStr(0)) + 'virtualprinter.exe';
      // Check if the PDF Printer server is running
      if FileExists(ExeFullPath) then
      begin
        //if not IsExeRunning('virtualprinter.exe') then
        if not IsProgramRunningForCurrentUser(ExeFullPath) then
//          ShellExecute(0, 'OPEN', PChar(ExeFullPath), nil, PChar(ExtractFilePath(ParamStr(0))), SW_SHOWNORMAL);
          CreateProcessOnUserDesktop(ExeFullPath);
      end;
    end;
    Sleep(1000);
    ServiceThread.ProcessRequests(False);

  end;

end;

procedure TService1.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := True;
  FNotificationHandle := AllocateHWnd(WndProc);
  if not WTSRegisterSessionNotification(FNotificationHandle, NOTIFY_FOR_ALL_SESSIONS) then
  begin

  end;

end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  WTSUnRegisterSessionNotification(FNotificationHandle);
  Stopped := True;
end;


procedure TService1.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_WTSSESSION_CHANGE:
    begin
      case Msg.WParam of
        WTS_SESSION_LOGON:
          begin
            // Handle session logon event
            //if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE, Msg.LParam, wtscon) then

          end;
        WTS_SESSION_LOGOFF:
          begin
            // Handle session logoff event
          end;

        WTS_REMOTE_CONNECT:
          begin
            // Handle remote connect event
          end;
        WTS_REMOTE_DISCONNECT:
          begin
            // Handle remote disconnect event
          end;

        WTS_CONSOLE_CONNECT:
          begin
            // Handle console connect event
          end;

        WTS_CONSOLE_DISCONNECT:
          begin
            // Handle console disconnect event
          end;

      end;
    end;

  end;
end;

end.
