unit DelphiLog;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls;

type
  THost = record
    ClassName: string;
    WindowName: string;
  end;

  TOutputType = (otConsole, otFile, otConsoleandFile);

type

  IDelphiLog = interface ['{D8E4F10C-4C6E-4395-8F0D-B3FEEBC79119}']
    /// <summary>
    /// Write message to DelphiLog application.
    /// </summary>
    /// <param name="sMessage">The message to write.</param>
    procedure WriteMessage(sMessage: string);
    function  GetLogCount: Integer;
    procedure SetLogCount(const Value: Integer);

    property LogCount: Integer read GetLogCount write SetLogCount;
  end;


  TConsoleLog = class(TInterfacedObject, IDelphiLog)
  private
    Host: THost;
    FShowErrorMessages: Boolean;
    FLogCount: Integer;
    procedure SendData(const copyDataStruct: TCopyDataStruct);
    function GetLogCount: Integer;
    procedure SetLogCount(const Value: Integer);
  public
    procedure WriteMessage(sMessage: string);

    property ShowErrorMessages: Boolean read fShowErrorMessages write fShowErrorMessages;
    property LogCount: Integer read GetLogCount write SetLogCount;

    constructor Create(showErrorMessage: Boolean);
  end;

  TFileLog = class(TInterfacedObject, IDelphiLog)
  private
    LogBuffer: TStringList;
    FLogBufferCount: Integer;
    FLogFilePath: string;
    FLogCount: Integer;
    procedure WritetoFile;
    function GetLogCount: Integer;
    procedure SetLogCount(const Value: Integer);
  public
    procedure WriteMessage(sMessage: string);
    procedure DeleteLogFile;

    property LogBufferCount: Integer read fLogBufferCount write fLogBufferCount;
    property LogFilePath: string read fLogFilePath write fLogFilePath;
    property LogCount: Integer read GetLogCount write SetLogCount;

//    constructor Create; overload;
    constructor Create(logFilePath: String); //overload;
    destructor Close;
  end;


  TDelphiLog = class(TInterfacedObject, IDelphiLog)
  private
    FEnabled: Boolean;
    FShowErrorMessages: Boolean;
    FTOutputType: TOutputType;
    FConsoleLog: TConsoleLog;
    FFileLog: TFileLog;
    FLogFilePath: string;
    FLogCount: Integer;
    function getConsoleLog: TConsoleLog;
    function getFileLog: TFileLog;
    function getLogFileBufferCount: Integer;
    procedure setLogFileBufferCount(const Value: Integer);
    function GetLogCount: Integer;
    procedure SetLogCount(const Value: Integer);
  public
    procedure WriteMessage(sMessage: string);
    property Enabled: Boolean read fEnabled write fEnabled;
    // Show error messages on client.
    property ShowErrorMessages: Boolean read fShowErrorMessages write fShowErrorMessages;
    property OutputType: TOutputType read fTOutputType write fTOutputType;
    property ConsoleLog: TConsoleLog read getConsoleLog write fConsoleLog;
    property FileLog: TFileLog read getFileLog write fFileLog;
    property LogFilePath: string read fLogFilePath write fLogFilePath;
    property LogFileBufferCount: Integer read getLogFileBufferCount write setLogFileBufferCount;
    property LogCount: Integer read GetLogCount write SetLogCount;

    constructor Create;
    destructor Close;
  end;






implementation

{ TDelphiLog }

/// <summary>
/// Send data to DelphiLog application.
/// </summary>
/// <param name="copyDataStruct">The TCopyDataStruct is defined in the Winapi.Windows.pas
/// unit and wraps the tagCOPYDATASTRUCT structure that contains the data to be passed.</param>
destructor TDelphiLog.Close;
begin
  if fConsoleLog <> nil then fConsoleLog.Free;
  if fFileLog <> nil then fFileLog.Free;
end;

constructor TDelphiLog.Create;
begin
  Self.Enabled := True;
  Self.ShowErrorMessages := True;
  Self.OutputType := otConsole;
end;


function TDelphiLog.getConsoleLog: TConsoleLog;
begin
  if fConsoleLog = nil then
  begin
    fConsoleLog := TConsoleLog.Create(Self.ShowErrorMessages);
  end;

  Result := fConsoleLog;
end;

function TDelphiLog.getFileLog: TFileLog;
begin
  if fFileLog = nil then
  begin
    fFileLog := TFileLog.Create(LogFilePath);
  end;

  Result := fFileLog;
end;

function TDelphiLog.GetLogCount: Integer;
begin
  Result := FLogCount;
end;

function TDelphiLog.getLogFileBufferCount: Integer;
begin
  if(Self.fFileLog = nil) then Result := 1 else Result := Self.fFileLog.fLogBufferCount;
end;

procedure TDelphiLog.SetLogCount(const Value: Integer);
begin
  FLogCount := Value;
end;

procedure TDelphiLog.setLogFileBufferCount(const Value: Integer);
begin
  Self.FileLog.fLogBufferCount := Value;
end;

procedure TDelphiLog.WriteMessage(sMessage: string);
var
  slogmessage: String;
  sdatetime: string;
begin
  if not Self.Enabled then
    Exit;

  sdatetime := DateTimeToStr(Now);
  slogmessage := Format('[%s] %s', [sdatetime, sMessage]);

  case OutputType of
    otConsole: Self.ConsoleLog.WriteMessage(slogmessage);
    otFile: Self.FileLog.WriteMessage(slogmessage);
    otConsoleandFile:
    begin
       Self.ConsoleLog.WriteMessage(slogmessage);
       Self.FileLog.WriteMessage(slogmessage);
    end;
  end;
end;

{ TConsoleLog }

constructor TConsoleLog.Create(showErrorMessage: Boolean);
begin
  Self.ShowErrorMessages := showErrorMessage;
end;

function TConsoleLog.GetLogCount: Integer;
begin
  Result := FLogCount;
end;

procedure TConsoleLog.SendData(const copyDataStruct: TCopyDataStruct);
var
  receiverHandle: THandle;
  res: integer;
begin
  receiverHandle := FindWindow(PChar(Host.ClassName), PChar(Host.WindowName));
  if (receiverHandle = 0) then
  begin
    if (Self.ShowErrorMessages) then ShowMessage('Copy Data Receiver NOT found!');
    Exit;
  end;

  res := SendMessage(receiverHandle, WM_COPYDATA, integer(Application.Handle),
  integer(@copyDataStruct));
end;

procedure TConsoleLog.SetLogCount(const Value: Integer);
begin
  FLogCount := Value;
end;

procedure TConsoleLog.WriteMessage(sMessage: string);
var
  copyDataStruct: TCopyDataStruct;
begin
  copyDataStruct.dwData := 0; // use it to identify the message contents
  copyDataStruct.cbData := (1 + Length(sMessage)) * SizeOf(Char);
  copyDataStruct.lpData := PChar(sMessage);
  Self.Host.ClassName := 'TfrmMain';
  Self.Host.WindowName := 'Delphi Log-Host';
  Self.SendData(copyDataStruct);
end;

{ TFileLog }


destructor TFileLog.Close;
begin
  WritetoFile;
  Self.LogBuffer.Free;
end;

constructor TFileLog.Create(logFilePath: String);
begin
  Self.LogBufferCount := 1;
  Self.LogFilePath := logFilePath;
end;

procedure TFileLog.DeleteLogFile;
begin
  DeleteFile(Self.LogFilePath);
end;

function TFileLog.GetLogCount: Integer;
begin
  Result := FLogCount;
end;

procedure TFileLog.SetLogCount(const Value: Integer);
begin
  FLogCount := Value;
end;

procedure TFileLog.WriteMessage(sMessage: string);
begin
  if  LogBuffer = nil then  LogBuffer := TStringList.Create;

  LogBuffer.Append(sMessage);

  if (LogBufferCount <= LogBuffer.Count) then
  begin
    WritetoFile;
  end;

end;

procedure TFileLog.WritetoFile;
var logfile: TextFile;
    i: Integer;

begin
  if (LogBuffer = nil) then Exit;

  AssignFile(logfile, Self.LogFilePath);
  if (FileExists(Self.LogFilePath)) then Append(logfile) else Rewrite(logfile);

  for i := 0 to LogBuffer.Count-1 do
  begin
    Writeln(logfile, LogBuffer[i]);
  end;

  LogBuffer.Clear;

  CloseFile(logfile);

end;

end.
