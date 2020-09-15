unit AboutFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, SSVector, ConstantsUnit, Windows;

type

  { TAboutForm }

  TStar=record
    position:TVector;
  end;

  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Label1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label7Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    stars:array of TStar;
    angle:Single;

    procedure InitStars;
    procedure RenderStars;
    procedure FrameMoveStars(DeltaTime:Single);
    procedure DoneStars;

  end; 

var
  AboutForm: TAboutForm;

implementation

{ TAboutForm }

procedure TAboutForm.InitStars;
  var
    i:Integer;
begin
  SetLength(stars,1000);
  for i:=0 to High(stars) do begin
    stars[i].position.Eqv(frandom(ss_seed)*100,frandom(ss_seed)*100,frandom(ss_seed)*100);
  end;
end;

procedure TAboutForm.RenderStars;
  var
    matProj,matModel,m1,m2:TMatrix;
    v:TVector;
    i:Integer;
begin
  Image1.Canvas.Pen.Color:=0;
  Image1.Canvas.Brush.Color:=0;
  Image1.Canvas.Rectangle(0,0,Image1.Width,Image1.Height);

  Image1.Canvas.Pen.Color:=$FFFFFF;
  matProj.SetProjectionMatrix(90, Image1.Width/Image1.Height,0,100000);
  m1.LookAt(gV(0,0,-100),gV(0,0,0),gV(0,1,0));
  m2.setYRotation(angle);
  matModel:=m1.multLeft(m2);

  for i:=0 to High(stars) do begin
    v:=Project(stars[i].position,matModel,matProj,0,0,Image1.Width,Image1.Height);
    Image1.Canvas.Pixels[round(v.x),round(v.y)]:=$FFFFFF;
  end;

end;

procedure TAboutForm.FrameMoveStars(DeltaTime:Single);
begin
  angle:=angle+0.01;
end;

procedure TAboutForm.DoneStars;
begin

end;

procedure TAboutForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Timer1.Enabled:=false;
  DoneStars;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  //
  InitStars;
  Timer1.Enabled:=true;
  DoubleBuffered:=true;
  Panel1.DoubleBuffered:=true;
end;

procedure TAboutForm.Label1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Close;
end;

procedure TAboutForm.Label7Click(Sender: TObject);
begin
  //
end;

procedure TAboutForm.Label8Click(Sender: TObject);
begin
  //
end;

procedure TAboutForm.Timer1Timer(Sender: TObject);
begin
  //
  FrameMoveStars(0.01);
  RenderStars;
end;

initialization
  {$I AboutFormUnit.lrs}

end.

