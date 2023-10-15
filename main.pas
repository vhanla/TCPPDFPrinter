unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DosCommand, Vcl.StdCtrls, IOUtils, Math, ActiveX, ShlObj, ShellAPI,
  ShLwApi, ComObj, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext, IdThread, IdGlobal, IdSocketHandle,
  IdThreadComponent, IdThreadSafe, IdIOHandlerSocket, Process, Printers,
  IdServerIOHandler, IdServerIOHandlerSocket, IdServerIOHandlerStack,
  SkiSys.GS_Api, SkiSys.GS_Converter, SkiSys.GS_ParameterConst, SkiSys.GS_gdevdsp,
  Vcl.ExtCtrls, Vcl.Menus, DarkModeApi;

type

  TmainForm = class(TForm)
    gbGhostscript: TGroupBox;
    Label1: TLabel;
    IdTCPServer1: TIdTCPServer;
    Memo1: TMemo;
    btnAddPrinter: TButton;
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    ShowPDFPrinter1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure btnAddPrinterClick(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Exit1Click(Sender: TObject);

  private
    { Private declarations }
    printerInstalled: Boolean;
    bigBuff: TMemoryStream;
    pdfConvert: TGS_PdfConverter;
    firstBuffer: Boolean;

    procedure SetCursorToEnd(AMemo: TMemo);
    procedure StdError(const AText: string);
    procedure StdIn(const AText: string);
    procedure StdOut(const AText: string);
    procedure ThreadFinished(Sender: TObject);
  public
    { Public declarations }
    function CheckGhostScriptOEMDriver: Boolean;
    procedure Log(const Msg: string);
    procedure AddPrinter(const Name, Host: string; Port: Integer = 9101; PrinterPortName: string = ''; MakeDefault: Boolean = False; Comment: string = '');
  end;

var
  mainForm: TmainForm;
  counter: Integer = 0;

implementation

{$R *.dfm}

uses
  Vcl.Themes, Vcl.Styles;

{ TmainForm }

function GetProgramFilesDir: string;
var
  path: array[0..MAX_PATH] of Char;
begin
  if Succeeded(SHGetFolderPath(0, CSIDL_PROGRAM_FILES, 0, 0, path)) then
    Result := path
  else
    Result := '';

end;

procedure RunProcessAndWait(const CommandLine: string);
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
end;

procedure TmainForm.AddPrinter(const Name, Host: string; Port: Integer;
  PrinterPortName: string; MakeDefault: Boolean; Comment: string);
var
  PortStr: string;
  OutPut: AnsiString;
begin
  PortStr := IntToStr(Port);

  if PrinterPortName = '' then
    PrinterPortName := Host + ':' + PortStr;

  RunCommand('cscript',
    ['c:\Windows\System32\Printing_Admin_Scripts\en-US\prnport.vbs',
     '-md', '-a', '-o', 'raw', '-r', '"'+PrinterPortName+'"', '-h', Host, '-n', PortStr
    ],
  OutPut, [poNoConsole]);
  Memo1.Lines.Add(string(OutPut));

  RunCommand('rundll32',
    ['printui.dll,PrintUIEntry',
//     '/if', '/b', '"'+Name+'"', '/r', '"'+PrinterPortName+'"', '/m', '"Microsoft PS Class Driver"', '/Z'
     '/if', '/b', '"'+Name+'"', '/r', '"'+PrinterPortName+'"', '/m', '"Ghostscript PDF"', '/Z'
    ],
  OutPut, [poNoConsole]);
  Memo1.Lines.Add(string(OutPut));

end;

{ TODO : Fails if 32bit and doesn't detect programfiles which is weird }
procedure TmainForm.btnAddPrinterClick(Sender: TObject);
begin
  AddPrinter('Print to PDF', '127.0.0.1', 9001, 'PRINT2PDFPORT');
  btnAddPrinter.Visible := False;
end;

function TmainForm.CheckGhostScriptOEMDriver: Boolean;
var
  OutPut: AnsiString;
  TS: TStringList;
  I: Integer;
begin
  Result := False;
  RunCommand('pnputil',
    ['/enum-drivers', '/class', '"Printer"'
    ],
  OutPut, [poNoConsole]);

  TS := TStringList.Create;
  try
    TS.Text := (string(OutPut));
    for I := 0 to TS.Count - 1 do
    begin
      if TS[I].Contains('oem') and TS[I].Contains('.inf') then
      begin
        if I+1 <= TS.Count then
        begin
          if TS[I+1].Contains('ghostpdf.inf') then
          begin
            Result := True;
            Break;
          end;
        end;
      end;
    end;

  finally
    TS.Free;
  end;

end;

procedure TmainForm.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TmainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  mainForm.Hide;
end;

procedure TmainForm.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  if not IsDarkMode then
  begin
    TStyleManager.TrySetStyle('Windows');
  end
  else
    AllowDarkModeForApp(True);

  if not CheckGhostScriptOEMDriver then
    Label1.Caption := 'No GhostScript OEM Driver installed to create a Printer';


  printerInstalled := False;
  for I := 0 to Printer.Printers.Count - 1 do
  begin
    if Printer.Printers[I] = 'Print to PDF' then
    begin
      printerInstalled := True;
      Break;
    end;
  end;

  if not printerInstalled then
    btnAddPrinter.Visible := True;

  pdfConvert := TGS_PdfConverter.Create;
  pdfConvert.OnStdError := StdError;
  pdfConvert.OnStdIn := StdIn;
  pdfConvert.OnStdOut := StdOut;
  pdfConvert.OnAfterExecute := ThreadFinished;


  IdTCPServer1.DefaultPort := 9001;
  IdTCPServer1.Active := True;
end;

procedure TmainForm.FormDestroy(Sender: TObject);
begin
  if (PDFConvert <> nil) then
    FreeAndNil(PDFConvert);
  IdTCPServer1.Active := False;
end;

procedure TmainForm.IdTCPServer1Connect(AContext: TIdContext);
begin
  Memo1.Lines.Clear;
  Log('Starting printing job...');
  bigBuff := TMemoryStream.Create;
  firstBuffer := True;
end;

procedure TmainForm.IdTCPServer1Disconnect(AContext: TIdContext);
begin
  if Assigned(bigBuff) then
  begin
    Log('Printing JOB finished... creating PDF!');
    bigBuff.SaveToFile('PrintedJobs\I_printed_this.ps');
//    pdfConvert.Params.Device := DISPLAY_DEVICE_NAME;
    PDFConvert.Params.Device := DEVICES_HIGH_LEVEL[pdfwrite];
    pdfConvert.Params.SubsetFonts := False;
    pdfConvert.Params.EmbededFonts := True;
//    pdfConvert.Params.ICCProfile := 'default_cmyk.icc';
//    pdfConvert.Params.PDFAOutputConditionIdentifier := 'CMYK';
//    pdfConvert.Params.ColorConversionStrategy := ccsCMYK;
    pdfConvert.UserParams.Clear;
//    PDFConvert.UserParams.Add('-sProcessColorModel=DeviceCMYK');
    PDFConvert.OnAfterExecute := ThreadFinished;
    pdfConvert.ToPdf('PrintedJobs\I_printed_this.ps', 'PrintedJobs\I_printed_this.pdf', True);
    bigBuff.Free;
  end;
end;

procedure TmainForm.IdTCPServer1Execute(AContext: TIdContext);
var
  PrintThread: TIdThread;
  Buffer: TIdBytes;
  FileStream: TFileStream;
  MemoryStream, ms: TMemoryStream;
  TS: TStringList;
  parseTitle: Boolean;
  I: Integer;
begin
  parseTitle := False;
  if firstBuffer and not AContext.Connection.IOHandler.InputBufferIsEmpty then
  begin
    firstBuffer := False;
    parseTitle := True;
  end;

  ms := TMemoryStream.Create;
  try
    bigBuff.Position := bigBuff.Size; // move to the end of the stream
    AContext.Connection.IOHandler.InputBufferToStream(ms);
    if parseTitle then
    begin
      TS := TStringList.Create;
      try
        ms.Position := 0;
        TS.LoadFromStream(ms);
        var short := TS.Count - 1;
        if short > 4 then
          short := 4;
        for I := 0 to short do
        begin
          if TS[I].StartsWith('%%Title:') then
          begin
            Label1.Caption := ChangeFileExt(Copy(TS[I], 9, Length(TS[I]) - 8), '.pdf');
            Break;
          end;
        end;

      finally
        TS.Free;
      end;
    end;
    ms.Position := 0;
    bigBuff.CopyFrom(ms, ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TmainForm.Log(const Msg: string);
begin
  Memo1.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
end;

procedure TmainForm.SetCursorToEnd(AMemo: TMemo);
begin
  Exit;
// set the cursor at the end of the memo (auto scroll)
  if mainForm.Visible and Assigned(AMemo) then
  with AMemo do
  begin
    SelStart := Perform(EM_LINEINDEX, Lines.Count - 1, 0);
    SelLength := 0;
    Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TmainForm.StdError(const AText: string);
begin
  Log(AText);
  SetCursorToEnd(Memo1);
end;

procedure TmainForm.StdIn(const AText: string);
begin
  Log(AText);
  SetCursorToEnd(Memo1);
end;

procedure TmainForm.StdOut(const AText: string);
begin
  Log(AText);
  SetCursorToEnd(Memo1);
end;

procedure TmainForm.ThreadFinished(Sender: TObject);
begin
  if (pdfConvert.LastErrorCode < 0) then
    Log(pdfConvert.LastErrors);
  ShellExecute(0, 'OPEN',PChar(ExtractFilePath(ParamStr(0)) + 'PrintedJobs\I_printed_this.pdf'), nil, nil, SW_SHOWNORMAL);
  Log('PDF generation done!');
end;

procedure TmainForm.TrayIcon1DblClick(Sender: TObject);
begin
  mainForm.Show;
end;


end.
