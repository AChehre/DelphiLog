unit CheDevSystem;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
 TCheDevSystem = class
 public
  class function GetComputerNetName: string;
  class function GetUserFromWindows: string;
  class function GetFileSize(FileName: string): Int64;
 end;


implementation

{ TCheDevSystem }

class function TCheDevSystem.GetComputerNetName: string;
var
  buffer: array[0..255] of char;
  size: dword;
begin
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer
  else
    Result := ''
end;


class function TCheDevSystem.GetFileSize(FileName: string): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  if NOT GetFileAttributesEx(PAnsiChar(FileName), GetFileExInfoStandard, @info) then
    EXIT;

  result := info.nFileSizeLow or (info.nFileSizeHigh shl 32);
end;

class function TCheDevSystem.GetUserFromWindows: string;
var
   UserName : string;
   UserNameLen : Dword;
Begin
   UserNameLen := 255;
   SetLength(userName, UserNameLen) ;
   If GetUserName(PChar(UserName), UserNameLen) Then
     Result := Copy(UserName,1,UserNameLen - 1)
   Else
     Result := 'Unknown';
End;

end.
 