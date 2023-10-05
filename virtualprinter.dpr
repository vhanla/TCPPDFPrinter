program virtualprinter;

uses
  FastMM4,
  Vcl.Forms,
  Winapi.Windows,
  System.SysUtils,
  main in 'main.pas' {mainForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

var
  MutexHandle: THandle;
  MutexString: string;

begin
  MutexString := 'VHLPDFPrinter_' + IntToStr(WTSGetActiveConsoleSessionId);
  MutexHandle := CreateMutex(nil, True, PChar(MutexString));

  if (MutexHandle <> 0) and (GetLastError = ERROR_ALREADY_EXISTS) then
  begin
    CloseHandle(MutexHandle);
    Halt;
  end;


  ReportMemoryLeaksOnShutdown:= True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  TStyleManager.TrySetStyle('Windows11 Dark');
  Application.CreateForm(TmainForm, mainForm);
  Application.Run;
end.
