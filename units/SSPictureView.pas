{*******************************************************************************
  Компонент для отображения элементов планирования
  Автор: Фомин С.С.

*******************************************************************************}
unit SSPictureView;

{******************************************************************************}
interface

{******************************************************************************}
uses
  {******************************************************************************}
  {$IFDEF FPC}
  //  LCLIntf, LMessages, Types,
  {$ELSE}
  {$ENDIF}
  LMessages,
  Windows, Messages,
  Forms, Controls, Graphics,
  SysUtils, Classes, ExtCtrls,
  SSPictureViewScroll, Dialogs, SSPictureUnit,
  RGBGraphics;

const
  PV_MODE_NONE = 0;
  PV_MODE_MOVE = 1;
  PV_MODE_ZOOMANDMOVE = 2;
  PV_MODE_ZOOM = 3;

  PV_MODE_PIXELS = 4;  //
  PV_MODE_SMOOTHLINE = 5; //
//  PV_MODE_BRUSH = 5;

{******************************************************************************}
type
  {******************************************************************************}
  TPictureView = class;
  {******************************************************************************}
  // Функции событий
  TMouseDownEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer) of object;
  TMouseUpEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer) of object;
  TDblClickEvent = procedure(Sender: TObject) of object;
  TMouseMoveEvent = procedure(Sender: TObject; X, Y: integer) of object;
  TRightClickEvent = procedure(Sender: TObject; X, Y: integer) of object;

  // Константы
  TBackgroundStyle = (bsNoBitmap, bsStretched, bsTiled, bsTiledAndScrolled,
    bsTiledHorisontalAnsScrolled);

  {******************************************************************************}
  TPictureView = class(TPictureViewScroller)
  private
  public
    AWidth, AHeight: integer;
    OldWidth, OldHeight: integer;
    dx, dy: integer;
    canv: TCanvas;

    Mode:Integer;
    IsLeftMouseDown:Boolean;
    LeftMouseDownX:Integer;
    LeftMouseDownY:Integer;
    LeftMouseDownCenterX:Single;
    LeftMouseDownCenterY:Single;
    LeftMouseDownScale:Single;

    FOnMouseDown: TMouseDownEvent;
    FOnMouseUp: TMouseUpEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnDblClick: TDblClickEvent;
    FOnSelect: TNotifyEvent;
    FOnResized: TNotifyEvent;

    CurrentPicture: TSSPicture;

    DefaultFontName: ansistring;
    DefaultFontSize: integer;
    DefaultFontStyle: TFontStyles;

    {**************************************************************************}

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
    procedure PreCalculate;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;


    procedure OnScrolled; override;
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure DblClick; override;

    procedure Loaded; override;

    {**************************************************************************}

    procedure SetMode_None;
    procedure SetMode_Move;
    procedure SetMode_ZoomAndMove;
    procedure SetMode_Zoom;
    procedure SetMode_ZoomIn;
    procedure SetMode_ZoomOut;

    procedure SetMode_Pixels;

    {**************************************************************************}

    procedure TargetZoom(TargetX,TargetY:Integer;NewScale:Single);
    procedure TargetMove(TargetX,TargetY:Integer);

    {**************************************************************************}
    procedure CoordsViewToPicture(ViewX,ViewY:Integer;var PicX,PicY:Integer);overload;
    procedure CoordsViewToPicture(ViewX,ViewY:Integer;var PicX,PicY:Single);overload;

    {**************************************************************************}
    procedure RecalculateScrollBars;

  public
    {**************************************************************************}


  published

    property PopupMenu;
    property OnClick;

    property OnMouseDown: TMouseDownEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseUpEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;

    property OnDblClick: TDblClickEvent read FOnDblClick write FOnDblClick;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;

  end;

{******************************************************************************}

{******************************************************************************}
implementation

{******************************************************************************}
//    Uses MainForm;
{*******************************************************************************
  class TPictureView
*******************************************************************************}
// Конструктор
constructor TPictureView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  IsLeftMouseDown:=false;

  Align := alClient;

  AWidth := 2000;
  AHeight := 2000;


  FOnMouseDown := nil;
  FOnMouseUp := nil;
  FOnMouseMove := nil;
  FOnDblClick := nil;
  FOnSelect := nil;
  FOnResized := nil;

  Mode:=0;
end;
// Деструктор
destructor TPictureView.Destroy;
begin

  inherited Destroy;

end;

{******************************************************************************}
procedure TPictureView.Paint;
var
  bm: TRGB32Bitmap;
  x, y, yh: integer;
  ct, dt: TDateTime;
  i, j, no: integer;
  //ClientRect :TRect;
  s, s1: string;
begin
  // Создаем буфер для прорисовки
  dx := HPos + Canvas.ClipRect.Left;
  dy := VPos + Canvas.ClipRect.Top;

  bm := TRGB32Bitmap.Create(Canvas.ClipRect.Right - Canvas.ClipRect.Left + 1,Canvas.ClipRect.Bottom - Canvas.ClipRect.Top + 1);

  if CurrentPicture <> nil then begin
    CurrentPicture.View_Render(bm,ClientRect,  Canvas.ClipRect);
  end else bm.Canvas.Fill($ffffff);

  // Заканчиваем прорисовку
  bm.Canvas.DrawTo(Canvas,Canvas.ClipRect.Left, Canvas.ClipRect.Top);
  bm.Free;
end;
{******************************************************************************}

{******************************************************************************}
procedure TPictureView.PreCalculate;
var
  r: TRect;
begin
  // Прекалькуляция
  r := Canvas.ClipRect;
  dy := -VPos;
  Dec(r.Top, dy);
  Dec(r.Bottom, dy);
  Dec(dy, Canvas.ClipRect.Top);
  dx := -HPos - Canvas.ClipRect.Left;
end;
{******************************************************************************}
procedure TPictureView.RecalculateScrollBars;
begin
  if CurrentPicture = nil then exit;

  CurrentPicture.View_Calculate;

  if CurrentPicture.DrawWidth + ClientWidth > ClientWidth then begin
    HPos:=Round(CurrentPicture.CenterX*(CurrentPicture.DrawWidth));
  end else HPos:=0;

  if CurrentPicture.DrawHeight + ClientHeight > ClientHeight then begin
    VPos:=Round(CurrentPicture.CenterY*(CurrentPicture.DrawHeight));
  end else VPos:=0;

  UpdateScrollBars(CurrentPicture.DrawWidth+ClientWidth, CurrentPicture.DrawHeight+ClientHeight);
  Repaint;
end;
{******************************************************************************}
procedure TPictureView.WMSize(var Message: TWMSize);
begin
  if Assigned(FOnResized) then FOnResized(Self);
  RecalculateScrollBars;
end;

{******************************************************************************}
procedure TPictureView.TargetZoom(TargetX,TargetY:Integer;NewScale:Single);
var
  PictureMouseX,PictureMouseY,NewPictureMouseX,NewPictureMouseY,dxx,dyy:Single;
  NewCenterX,NewCenterY:Single;
begin

  PictureMouseX:=(TargetX-(ClientWidth/2-CurrentPicture.DrawWidth/2))/(CurrentPicture.Scale*CurrentPicture.Width);
  PictureMouseY:=(TargetY-(ClientHeight/2-CurrentPicture.DrawHeight/2))/(CurrentPicture.Scale*CurrentPicture.Height);
  CurrentPicture.Scale := NewScale;
  CurrentPicture.View_Calculate;

  NewPictureMouseX:=(TargetX-(ClientWidth/2-CurrentPicture.DrawWidth/2))/(CurrentPicture.Scale*CurrentPicture.Width);
  NewPictureMouseY:=(TargetY-(ClientHeight/2-CurrentPicture.DrawHeight/2))/(CurrentPicture.Scale*CurrentPicture.Height);
  NewCenterX:=CurrentPicture.CenterX-NewPictureMouseX+PictureMouseX;
  NewCenterY:=CurrentPicture.CenterY-NewPictureMouseY+PictureMouseY;

  if ( NewCenterX < 0 ) then NewCenterX:=0;
  if ( NewCenterX > 1 ) then NewCenterX:=1;
  if ( NewCenterY < 0 ) then NewCenterY:=0;
  if ( NewCenterY > 1 ) then NewCenterY:=1;

  CurrentPicture.View_SetPosition(NewCenterX,NewCenterY);
  RecalculateScrollBars;

end;
{******************************************************************************}
procedure TPictureView.TargetMove(TargetX,TargetY:Integer);
  var
    NewCenterX,NewCenterY:Single;
begin
  NewCenterX:=LeftMouseDownCenterX-(TargetX-LeftMouseDownX)/(CurrentPicture.Scale*CurrentPicture.Width);
  NewCenterY:=LeftMouseDownCenterY-(TargetY-LeftMouseDownY)/(CurrentPicture.Scale*CurrentPicture.Height);
  if ( NewCenterX < 0 ) then NewCenterX:=0;
  if ( NewCenterX > 1 ) then NewCenterX:=1;
  if ( NewCenterY < 0 ) then NewCenterY:=0;
  if ( NewCenterY > 1 ) then NewCenterY:=1;
  CurrentPicture.View_SetPosition(NewCenterX,NewCenterY);
  RecalculateScrollBars;
end;
{******************************************************************************}
procedure TPictureView.WMMouseWheel(var Message: TLMMouseEvent);
  var
    d:Single;
begin
  if CurrentPicture = nil then exit;

  if Mode = PV_MODE_MOVE then begin


  end else if Mode = PV_MODE_ZOOM then begin

    if Message.WheelDelta < 0 then d:=-1
    else if Message.WheelDelta > 0 then d:=1;

    TargetZoom(Message.X,Message.Y,CurrentPicture.Scale + CurrentPicture.Scale * d*0.1);

  end else if Mode = PV_MODE_ZOOMANDMOVE then begin

    if Message.WheelDelta < 0 then d:=-1
    else if Message.WheelDelta > 0 then d:=1;

    TargetZoom(Message.X,Message.Y,CurrentPicture.Scale + CurrentPicture.Scale * d*0.1);

  end else if Mode = PV_MODE_PIXELS then begin

      if Message.WheelDelta < 0 then d:=-1
      else if Message.WheelDelta > 0 then d:=1;

      TargetZoom(Message.X,Message.Y,CurrentPicture.Scale + CurrentPicture.Scale * d*0.1);


  end else if Mode = PV_MODE_NONE then begin

  end;

end;

{******************************************************************************}
procedure TPictureView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  r1: TRect;
begin
  Message.Result := 1;
  if (OldWidth < ClientWidth) or (OldHeight < ClientHeight) then
  begin
//    GetClipBox(Message.DC, r1);
    Paint;
//    DrawBackground(Message.DC, r1, ClientWidth, ClientHeight);
  end;
  OldWidth := ClientWidth;
  OldHeight := ClientHeight;
end;

{******************************************************************************}
procedure TPictureView.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(self);
  inherited DblClick;
end;

{******************************************************************************}
procedure TPictureView.Click;
begin
  SetFocus;
  inherited;
end;

{******************************************************************************}
procedure TPictureView.Loaded;
begin
  inherited Loaded;
end;
{******************************************************************************}

procedure TPictureView.CoordsViewToPicture(ViewX,ViewY:Integer;var PicX,PicY:Integer);
begin
  PicX:=trunc((ViewX-((ClientRect.left+ClientRect.right) div 2-CurrentPicture.DrawX))/CurrentPicture.Scale);
  PicY:=trunc((ViewY-((ClientRect.top+ClientRect.bottom) div 2-CurrentPicture.DrawY))/CurrentPicture.Scale);
end;

procedure TPictureView.CoordsViewToPicture(ViewX,ViewY:Integer;var PicX,PicY:Single);
begin
  PicX:=((ViewX-((ClientRect.left+ClientRect.right) div 2-CurrentPicture.DrawX))/CurrentPicture.Scale);
  PicY:=((ViewY-((ClientRect.top+ClientRect.bottom) div 2-CurrentPicture.DrawY))/CurrentPicture.Scale);
end;

{******************************************************************************}
procedure TPictureView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
  var
    tx,ty:Integer;
begin
  if Button = mbLeft then begin

    IsLeftMouseDown:=true;
    LeftMouseDownX:=X;
    LeftMouseDownY:=Y;

    if CurrentPicture<>nil then begin
      LeftMouseDownCenterX:=CurrentPicture.CenterX;
      LeftMouseDownCenterY:=CurrentPicture.CenterY;
      LeftMouseDownScale:=CurrentPicture.Scale;
    end;

  end;

  if Mode = PV_MODE_MOVE then begin


  end else if Mode = PV_MODE_ZOOMANDMOVE then begin


  end else if Mode = PV_MODE_ZOOM then begin


  end else if Mode = PV_MODE_PIXELS then begin

    (CurrentPicture.bitmap.Pixels)^:=$ff000000;

    // Рисование линии
    if CurrentPicture <> nil then begin

      CoordsViewToPicture(X,Y,tx,ty);

      if ( tx >= 0 ) and ( ty >= 0 ) and ( ty < CurrentPicture.Width ) and ( ty < CurrentPicture.Height ) then begin

        (CurrentPicture.bitmap.Pixels+CurrentPicture.bitmap.Width*ty+tx)^:=$ff000000;

        Repaint;
      end;


    end;

  end else if Mode = PV_MODE_NONE then begin


  end;

  PreCalculate;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(self, Button, Shift, HPos + x, VPos + y);
  inherited MouseDown(Button, Shift, X, Y);
end;

{******************************************************************************}
procedure TPictureView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  i, j: integer;
  IsNeedRepaint: boolean;
begin
  if Button = mbLeft then begin
    IsLeftMouseDown:=false;
  end;
  if Mode = PV_MODE_MOVE then begin


  end else if Mode = PV_MODE_ZOOM then begin

  end else if Mode = PV_MODE_ZOOMANDMOVE then begin

  end else if Mode = PV_MODE_PIXELS then begin



  end else if Mode = PV_MODE_NONE then begin

  end;

  PreCalculate;
  inherited MouseUp(Button, Shift, X, Y);
end;
{******************************************************************************}
procedure TPictureView.MouseMove(Shift: TShiftState; X, Y: integer);
  var
    tx,ty:Integer;
var
  i, j: integer;
  IsNeedRepaint: boolean;
  IsWasMouseIn: boolean;

  d,PictureMouseX,PictureMouseY,NewPictureMouseX,NewPictureMouseY,dxx,dyy:Single;
  NX,NY:Integer;
begin
  if Mode = PV_MODE_MOVE then begin

    if IsLeftMouseDown then begin
      if CurrentPicture <> nil then begin
        TargetMove(X,Y);
      end;
    end;

  end else if Mode = PV_MODE_ZOOMANDMOVE then begin

    if IsLeftMouseDown then begin
      if CurrentPicture <> nil then begin
        TargetMove(X,Y);
      end;
    end;

  end else if Mode = PV_MODE_ZOOM then begin

    if IsLeftMouseDown then begin
      if CurrentPicture <> nil then begin

        TargetZoom(LeftMouseDownX,LeftMouseDownY,LeftMouseDownScale+(X-LeftMouseDownX)/128);

      end;
    end;

  end else if Mode = PV_MODE_PIXELS then begin

    // Рисование линии
    if CurrentPicture <> nil then begin

      if IsLeftMouseDown then begin

        CoordsViewToPicture(X,Y,tx,ty);

        if ( tx >= 0 ) and ( ty >= 0 ) and ( ty < CurrentPicture.Width ) and ( ty < CurrentPicture.Height ) then begin

          (CurrentPicture.bitmap.Pixels+CurrentPicture.bitmap.Width*ty+tx)^:=$ff000000;

          Repaint;
        end;

      end;
    end;

  end else if Mode = PV_MODE_NONE then begin



  end;

  PreCalculate;
  inherited MouseMove(Shift, X, Y);
end;
{******************************************************************************}
procedure TPictureView.OnScrolled;
  var
    ddx,ddy:Integer;
begin
  if CurrentPicture <> nil then
  begin
    ddx:=(CurrentPicture.DrawWidth {+ ClientWidth});
    ddy:=(CurrentPicture.DrawHeight {+ ClientHeight});
    if ddx > 0 then CurrentPicture.CenterX:=(HPos{-ClientWidth div 2})/ddx;
    if ddy > 0 then CurrentPicture.CenterY:=(VPos{-Clientheight div 2})/ddy;
    Repaint;
  end;
  inherited;
end;
{******************************************************************************}
procedure TPictureView.SetMode_None;
begin
  Mode:=PV_MODE_NONE;
end;

procedure TPictureView.SetMode_Move;
begin
  Mode:=PV_MODE_MOVE;
end;

procedure TPictureView.SetMode_ZoomAndMove;
begin
  Mode:=PV_MODE_ZOOMANDMOVE;
end;

procedure TPictureView.SetMode_Zoom;
begin
  Mode:=PV_MODE_ZOOM;
end;

procedure TPictureView.SetMode_ZoomIn;
begin


end;

procedure TPictureView.SetMode_ZoomOut;
begin


end;

procedure TPictureView.SetMode_Pixels;
begin
  Mode:=PV_MODE_PIXELS;
end;

{******************************************************************************}
end.
{******************************************************************************}
