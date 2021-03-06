{*******************************************************************************

  Проект: SpritePacker
  Автор: Фомин С.С.


  Назначение модуля:


*******************************************************************************}
unit SpritePackerThreadUnit;

{$mode objfpc}{$H+}

interface
{******************************************************************************}
uses

  Classes, SysUtils;
{******************************************************************************}
type
  {******************************************************************************}


  {******************************************************************************}
  TSpritePackerThread = class(TThread)
  private

    //index: integer;
    //FSize: integer;
    IsNeedStopThread, IsThreadStopped: boolean;
    //IsNeedChange:Boolean;

    procedure ThreadDone(Sender: TObject);
  protected
    procedure Execute; override;
  public

    IsNeedStop:boolean;
    IsStopped:boolean;

    owner: TClass;

    constructor Create(IsSuspended: boolean);


  end;
  {******************************************************************************}

implementation
{******************************************************************************}
Uses SpritePackerUnit, SpritePackerNodeUnit;


constructor TSpritePackerThread.Create(IsSuspended: boolean);
begin
  IsStopped := true;
  IsNeedStop :=false;
  IsNeedStopThread := False;
  IsThreadStopped := False;
  //  OnTerminate:=ThreadDone;
  FreeOnTerminate := True;
  Priority := tpNormal;
  //  Suspend;
  inherited Create(IsSuspended);
end;



procedure TSpritePackerThread.Execute;
//var
  //i:    integer;
begin
  while not Terminated do
  begin



  end;
end;

procedure TSpritePackerThread.ThreadDone(Sender: TObject);
begin

end;

end.

