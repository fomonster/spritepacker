unit ConstantsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  ViewTexSize=128;
type
  TObjectMethod=procedure of object;
var
  path:String;

  OpenSaveFilePath:String;

  OpenPicturePath:String;

  RenderTextureWidth:Integer;
  RenderTextureHeight:Integer;

  IsDocSaved:boolean;

  ss_seed:Cardinal = 131322131;

  function irandom(var seed:Cardinal;const max:Integer):Integer;
  function frandom(var seed:Cardinal):Single;
  function GetWord(var s:String):String;
  function RndColor:Cardinal;
  procedure SSMsg(s:String);
  function ReadTagInt(var s:String;var n:Integer):boolean;
  function IsDirectoryWriteable(const AName: string): Boolean;

implementation

procedure SSMsg(s:String);
begin
  //MessageBox(0,PChar(s),PChar('Space Sim Message'),0);
end;

function IsDirectoryWriteable(const AName: string): Boolean;
var
  FileName: String;
  H: THandle;
begin
  FileName := IncludeTrailingPathDelimiter(AName) + 'chk.tmp';
  H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(H);
end;

function GetWord(var s:String):String;
  var
    i,j:Integer;
    ss:String;
begin
  s:=trim(s)+' ';
  j:=pos('/',s);
  i:=pos(' ',s);
  if (i<j) or (j<=0) then begin
    result:=copy(s,1,i-1);
    delete(s,1,i);
  end else begin
    result:=copy(s,1,j-1);
    delete(s,1,j);
  end;
end;

function RndColor:Cardinal;
begin
  result:=random($ffffff);
end;

function frandom(var seed:Cardinal):Single; // -1..1
var
  r:Single;
begin
  {$R-} // Отключаем проверку на переполнение
  {$Q-}
  seed:=((seed shr 11) * seed * 15731 + 7789221) * seed + 8376312589;
  PCardinal(@r)^:=(seed and $007fffff) or $40000000;
  result:=r-3.0;
  {$Q+}
  {$R+} // Отключаем проверку на переполнение
end;

function irandom(var seed:Cardinal;const max:Integer):Integer; // 0..max-1
var
  r:Single;
  sd:Cardinal;
begin
  {$R-} // Отключаем проверку на переполнение
  {$Q-} // Отключаем проверку на переполнение
  seed:=seed * 543234 + 7874234234;
  seed:=round($FFFFFFFF * sin(seed xor $AAAAAAAA)+56872342);
  seed:=seed * 456455 + 9978234232;
  result:=seed mod max;
  {$R+} // Отключаем проверку на переполнение
  {$Q+} // Отключаем проверку на переполнение
end;

function ReadTagInt(var s:String;var n:Integer):boolean;
  var
    si:String;
    i:Integer;
begin
  result:=false;
  if length(s)<=0 then exit;
  i:=1;
  while not (s[i] in ['0'..'9']) do begin
    if i>length(s) then exit;
    inc(i);
  end;
  si:='';
  while (s[i] in ['0'..'9']) do begin
    si:=si+s[i];
    inc(i);
  end;
  n:=StrToIntDef(si,0);
  delete(s,1,i);
  result:=true;
end;

end.

