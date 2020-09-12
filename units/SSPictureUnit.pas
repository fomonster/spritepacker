{*******************************************************************************

  1. Каждая картинка имеет список слоев.
  2. В каждом слое список эффектов. (может быть пустой).
  3. В каждом слое Результирующая картинка собрана блоками
     Именно она смешивается по прозрачности.
  5. Каждый эффект вляет на результирующую картинку.

*******************************************************************************}
unit SSPictureUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  RGBGraphics, Graphics, SSGenericListUnit;

const
  LAYER_BLOCK_SIZE_SHIFT = 7;

  LAYER_BLOCK_SIZE = 128;
  LAYER_BLOCK_SIZE_M1 = 127;

  LAYER_BLOCKS = 256;
  LAYER_BLOCKS_M1 = 255;

  LAYER_MIN = -(LAYER_BLOCKS/2)*LAYER_BLOCK_SIZE;
  LAYER_MAX = (LAYER_BLOCKS/2)*LAYER_BLOCK_SIZE-1;


type

TSSLayer = class;
TSSPicture = class;
{*******************************************************************************

*******************************************************************************}
// Эффекты к Layer-у
TSSEffect = class(TVectorItem)

  layer:TSSLayer;

  constructor Create;
  destructor Destroy;

end;

TSSEffectList = specialize TGenericVector<TSSEffect>;

// Маска, которая используется для выделения части картинки
TSSMask = class



  constructor Create;
  destructor Destroy;

end;

PLayerPixel = ^TLayerPixel;
TLayerPixel = packed object
  r,g,b,a:Word;
end;

PLayerBlock = ^TLayerBlock;
TLayerBlock = packed object
  IsChanged:Boolean;
  rgba:array [0..LAYER_BLOCK_SIZE_M1,0..LAYER_BLOCK_SIZE_M1] of TLayerPixel;
end;

PLayerBitmapData=^TLayerBitmapData;
TLayerBitmapData=array [0..LAYER_BLOCKS_M1,0..LAYER_BLOCKS_M1] of PLayerBlock;

{*******************************************************************************

*******************************************************************************}
//
TSSLayer = class(TVectorItem)

  IsMouseOver:Boolean;

  Transparency:Single;

  IsNeedUpdate:boolean;
  X,Y:Integer;
  picture:TSSPicture;

  Effects:TSSEffectList;

  Data:TLayerBitmapData;

  ViewRect:TRect; // Используется для отображения на панели слоев


  constructor Create;
  destructor Destroy;

  // Установка знаения пиксела
  procedure SetPixel(_x,_y:Integer;_r,_g,_b,_a:Word);overload;
  procedure SetPixel(_x,_y:Integer;_p:TLayerPixel);overload;
  procedure SetPixel(_x,_y:Integer;_p:PLayerPixel);overload;
  function GetPixel(_x,_y:Integer):PLayerPixel;overload;
  procedure GetPixel(_x,_y:Integer;var _r,_g,_b,_a:Word);overload;
  procedure GetPixel(_x,_y:Integer;var _p:TLayerPixel);overload;

//  procedure SetPixel(x,y:Integer;_r,_g,_b,_a:Single);overload;
//  procedure SetPixel(x,y:Integer;_r,_g,_b,_a:Single);overload;

  procedure SetLayerOffset(_x,_y:Integer);

  procedure InitBlock(_hx,_hy:Integer);
  procedure DoneBlock(_hx,_hy:Integer);

  procedure Update;
end;

TSSLayersList = specialize TGenericVector<TSSLayer>;
{*******************************************************************************

*******************************************************************************}
TSSPicture = class(TVectorItem)

  bitmap:TRGB32BitMap;

  Scale:Single; // Масштаб
  CenterX,CenterY:Single; // Координаты центра экрана на картинке (если 0..1 то в картинке)
  DrawX,DrawY:Integer; // Положение левого верхнего угла картинки с учетом масштаба без Border
  DrawWidth,DrawHeight:Integer; // Отображаемые размеры c учетом масштаба и + Border
  Border:Integer;  // Отступ слева сверху справа и снизу
  Width,Height:Integer;  // Реальные размеры картинки

  Layers:TSSLayersList;


  ViewLayerBorder:Integer;
  ViewLayerHeight:Integer;

  SelectedLayer:TSSLayer; // Выбранный слой

  rbm_AfterSelectedLayer:TRGB32BitMap;
  rbm_BeforeSelectedLayer:TRGB32BitMap;

  {****************************************************************************}
  // Создание уничтожение
  constructor Create;
  destructor Destroy;

  {****************************************************************************}
  // Инициализация
  procedure SetSize(_width,_height:Integer);

  {****************************************************************************}
  // Отображение на экране
  procedure View_Calculate;
//  procedure View_SetScale(_scale:Single);
  procedure View_SetPosition(_x,_y:Single);
  procedure View_ScreenToPicture(screenx,screeny:Integer;var picx,picy:Integer);
  procedure View_Render(BitmapCanvas:TRGB32Bitmap;ClientRect,ClipRect:TRect);

  {****************************************************************************}
  // Слои
  procedure Layer_Select(_index:Integer);
  procedure Layer_AddEmpty;
  procedure Layer_Render(canv:TCanvas;dx,dy:Integer);
  function Layer_HitTest(_x,_y:Integer):Integer;
  function Layer_Calculate(ClientRect:TRect):Integer;

  {****************************************************************************}
  //
  procedure InitRBM(_rbm:TRGB32BitMap);
  procedure BlendRBM(_rbm1,_rbm2,_destrbm:TRGB32BitMap);
  procedure BlendLayerToRBM(_layer:TSSLayer;_destrbm:TRGB32BitMap);
  procedure BlendLayerAndRBMToRBM(_layer:TSSLayer;_rbm,_destrbm:TRGB32BitMap);

  {****************************************************************************}
  //


end;

TSSPictureList = specialize TGenericVector<TSSPicture>;
{*******************************************************************************

*******************************************************************************}
implementation
{*******************************************************************************


*******************************************************************************}
  var
    DefaultLayerPixel:TLayerPixel;
{*******************************************************************************
  SSPicture
*******************************************************************************}
constructor TSSPicture.Create;
begin
  inherited;
  Layers:=TSSLayersList.Create;

  bitmap:=nil;
  rbm_AfterSelectedLayer:=nil;
  rbm_BeforeSelectedLayer:=nil;

  Border:=0;
  Scale:=1;
  CenterX:=0.5;
  CenterY:=0.5;

  ViewLayerBorder:=1;
  ViewLayerHeight:=68;
end;

destructor TSSPicture.Destroy;
begin
  Layers.Destroy;
  if ( bitmap <> nil ) then bitmap.Free;
  if ( rbm_AfterSelectedLayer <> nil ) then rbm_AfterSelectedLayer.Free;
  if ( rbm_BeforeSelectedLayer <> nil ) then rbm_BeforeSelectedLayer.Free;
  inherited;
end;

procedure TSSPicture.SetSize(_width,_height:Integer);
begin
  Width:=_width;
  Height:=_height;
  // Результриующая картинка для прорисовки на экран
  if ( bitmap <> nil ) then bitmap.Free;
  bitmap:=TRGB32Bitmap.Create(Width,Height);
  InitRBM(bitmap);
  // Картинка всех слоев после выделенного
  if ( rbm_AfterSelectedLayer <> nil ) then rbm_AfterSelectedLayer.Free;
  rbm_AfterSelectedLayer:=TRGB32Bitmap.Create(Width,Height);
  InitRBM(rbm_AfterSelectedLayer);
  // Картинка всех слоев до выделенного
  if ( rbm_BeforeSelectedLayer <> nil ) then rbm_BeforeSelectedLayer.Free;
  rbm_BeforeSelectedLayer:=TRGB32Bitmap.Create(Width,Height);
  InitRBM(rbm_BeforeSelectedLayer);

end;

// Устанавливает положение центра экрана 0,0 - левый верхний угол картинки,
// 0.5,0.5 - центр картинки
procedure TSSPicture.View_SetPosition(_x,_y:Single);
begin
  CenterX:=_x;
  CenterY:=_y;
end;

procedure TSSPicture.View_Calculate;
begin
  if ( Width > 0 ) and ( Scale < 2/Width ) then Scale := 2/Width;
  if ( Height > 0 ) and ( Scale < 2/Height ) then Scale := 2/Height;
  DrawWidth:=round(Scale*Width);
  DrawHeight:=round(Scale*Height);
  DrawX:=round(CenterX*Scale*Width);
  DrawY:=round(CenterY*Scale*Height);
end;

procedure TSSPicture.View_ScreenToPicture(screenx,screeny:Integer;var picx,picy:Integer);
begin
  picx:=round((screenx-DrawX-DrawWidth div 2)/Scale);
  picy:=round((screeny-DrawY-DrawHeight div 2)/Scale);
end;

// Прорисовка картинки с учетем масштабирования
//
procedure TSSPicture.View_Render(BitmapCanvas:TRGB32Bitmap;ClientRect,ClipRect:TRect);
var
  s:AnsiString;
  c1, c2: integer;
  _left,_top,_leftT,_topT:Integer;
  i,j:Integer;
  canvrbm:TRGB32Bitmap;
  Canvas:TRGB32Canvas;
  rb:TRGB32Bitmap;
  //r:TRect;
  ClipRectWidth,ClipRectHeight:Integer;
  ClipRectWidthM1,ClipRectHeightM1,DDW,DDH,SrcWidth,SrcHeight,DrWidth,DrHeight:Integer; // ClipRect
  x,y:Integer;
  ix,iy:LongInt;
  fx,fy:Single;
  c,a,na,r,g,b,cf,cfy,srcadry,dstadry:DWord;
  //dstP,srcP:Pointer;
  dstW,dstH:Integer;
begin
  // Canvas.Width - Canvas.Height - размер области в которой нужно рисовать
  // Canvas левый верхний угол имеет координаты (0,0) им соответствует положение на экране
  // ClipRect.Left, ClipRect.Top
  // Поэтому рисуем со смещением (-ClipRect.Left, -ClipRect.Top)
  // dx,dy - смещение скроллинга
  Canvas:=BitmapCanvas.Canvas;

  View_Calculate;

  _left:=(ClientRect.left+ClientRect.right) div 2-DrawX-ClipRect.Left;
  _top:=(ClientRect.top+ClientRect.bottom) div 2-DrawY-ClipRect.Top;


  DrWidth:=DrawWidth;
  DrHeight:=DrawHeight;

  SrcWidth:=Width;
  SrcHeight:=Height;

  DDW:=Width*DrawWidth;
  DDH:=Height*DrawHeight;

  // Упрощение
  _leftT:=_left*Width;
  _topT:=_top*Height;

  _left:=_left;
  _top:=_top;

  ClipRectWidthM1:=ClipRect.Right-ClipRect.Left;
  ClipRectHeightM1:=ClipRect.Bottom-ClipRect.Top;
  ClipRectWidth:=ClipRectWidthM1+1;
  ClipRectHeight:=ClipRectHeightM1+1;

  dstW:=BitmapCanvas.Width;
  dstH:=BitmapCanvas.Height;
  //dstP:=BitmapCanvas.Pixels;

  //srcP:=bitmap.Pixels;
   {
  {$ASMMODE intel}
  asm
    push eax
    push ebx
    push ecx
    push edx
    push esi
    push edi

    mov eax,$ff0000

    mov esi,dstP

    mov ebx,ClipRectHeight
@loop1:
    push ebx

      mov eax,ClipRectHeight
      sub eax,ebx
      mov ecx,eax
      imul eax,SrcHeight
      sub eax,_topT
      js @loop3 // <
      cmp eax,DDH
      jge @loop3 // >=

      // srcadry:=bitmap.Width*(iy div DrawHeight);
      xor edx,edx
      div DrHeight
      mul SrcWidth;
      shl eax,2
      mov edi,srcP
      add edi,eax

      sub ecx,_top

      mov ebx,ClipRectWidth
  @loop2:
      push ebx
      push edi
      push ecx

        mov eax,ClipRectWidth
        sub eax,ebx
        mov ebx,eax
        imul eax,SrcWidth
        sub eax,_leftT
        js @loop6 // <
        cmp eax,DDW
        jge @loop6 // >=

        // Адрес источника цвета
        xor edx,edx
        div DrWidth
        shl eax,2
        add edi,eax

        // Цвет пиксела
        xor dx,dx
        mov dl,[edi+3] // alpha
        inc dl
        jz @loop8
        dec dl

        // al = alphaboxcolor
        sub ebx,_left // в ebx = (x-_left)
        xor ecx,ebx
        and cl,$10
        mov cx,$af
        jz @loop10
        add cl,$50
      @loop10:

        mov ax,$ff
        sub al,dl
        imul cx,ax

        // Blend
        // cx - box with alpha
        // bx - alpha

        //blue
        xor bx,bx
        mov bl,[edi];
        imul bx,dx
        add bx,cx
        mov [esi],bh

        // green
        xor bx,bx
        mov bl,[edi+1];
        imul bx,dx
        add bx,cx
        mov [esi+1],bh

        // red
        xor bx,bx
        mov bl,[edi+2];
        imul bx,dx
        add bx,cx
        mov [esi+2],bh

        add esi,4

        jmp @loop7
@loop8:
        mov eax,[edi]
        mov [esi],eax
        add esi,4

        jmp @loop7
      @loop6:

        mov eax,$ffffff
        mov [esi],eax
        add esi,4

      @loop7:

      pop ecx
      pop edi
      pop ebx
      dec ebx
      jnz @loop2

      jmp @loop4
@loop3:

      mov edx,ClipRectWidth
  @loop5:
      mov eax,$ffffff
      mov [esi],eax
      add esi,4
      dec edx
      jnz @loop5

@loop4:
    pop ebx
    dec ebx
    jnz @loop1

    pop edi
    pop esi
    pop edx
    pop ecx
    pop ebx
    pop eax
  end;
     }


 // Общий смысл асмовой вставки:
  for y:=0 to ClipRectHeightM1 do begin
    iy:=y*Height-_topT;
    if ((iy>=0) and (iy<DDH)) then begin
      cfy:=(y-_top);

      for x:=0 to ClipRectWidthM1 do begin
        ix:=x*Width-_leftT;
        if ((ix>=0) and (ix<DDW)) then begin
          c:=bitmap.Get32PixelUnsafe(ix div DrawWidth, iy div DrawHeight); // Берем исходный цвет
          if c < $ff000000 then begin // если с прозрачностью

            // Кубики
            a:=c shr 24;  // выделяем прозрачность

            cf:=$af+$5*(((x-_left) xor cfy) and $10);
            cf:=cf*(255-a) and $ff00;
            cf:=cf or (cf shl 8) or (cf shl 16);

            // Смешение по прозрачности
            BitmapCanvas.Set32PixelUnsafe(x, y,  ((
                  (((c and $0000ff)*a) and $0000ff00) or
                  (((c and $00ff00)*a) and $00ff0000) or
                  (((c and $ff0000)*a) and $ff000000)
                 ) + cf) shr 8);

          end else begin
            BitmapCanvas.Set32PixelUnsafe(x, y, c);
          end;

        end else BitmapCanvas.Set32PixelUnsafe(x, y, $ffffffff);
      end;
    end else for x:=0 to ClipRectWidthM1 do BitmapCanvas.Set32PixelUnsafe(x, y, $ffffffff);
  end;

end;
{*******************************************************************************
  Layers
*******************************************************************************}
procedure TSSPicture.Layer_Select(_index:Integer);
  var
    i:Integer;
begin
  SelectedLayer:=nil;
  if ( (_index<0) or (_index>=Layers.Length) ) then exit;
  for i:=0 to Layers.Length-1 do Layers[i].IsSelected:=false;

  SelectedLayer:=Layers[_index];
  SelectedLayer.IsSelected:=true;

  InitRBM(rbm_BeforeSelectedLayer);
  InitRBM(rbm_AfterSelectedLayer);

  // Соединяем слои для быстрого
  for i:=Layers.Length-1 downto _index+1 do
    BlendLayerToRBM(Layers[i],rbm_BeforeSelectedLayer);
  for i:=_index-1 downto 0 do
    BlendLayerToRBM(Layers[i],rbm_AfterSelectedLayer);
end;
{******************************************************************************}
procedure TSSPicture.Layer_AddEmpty;
begin
  Layers.Add;
end;
{******************************************************************************}
procedure TSSPicture.Layer_Render(canv:TCanvas;dx,dy:Integer);
  var
    i:Integer;
    layer:TSSLayer;
    r:TRect;
begin

  for i:=0 to Layers.Length-1 do begin
    layer:=Layers[i];

    r.Left:=layer.ViewRect.Left-dx;
    r.Top:=layer.ViewRect.Top-dy;
    r.Right:=layer.ViewRect.Right-dx;
    r.Bottom:=layer.ViewRect.Bottom-dy;

    if layer.IsMouseOver then begin
      if layer.IsSelected then begin
        canv.Brush.Color:=$cfcfcf;
        canv.Pen.Color:=$9f9f9f;
      end else begin
        canv.Brush.Color:=$cfcfcf;
        canv.Pen.Color:=$bfbfbf;
      end;
    end else begin
      if layer.IsSelected then begin
        canv.Brush.Color:=$c9c9c9;
        canv.Pen.Color:=$9f9f9f;
      end else begin
        canv.Brush.Color:=$dfdfdf;
        canv.Pen.Color:=$bfbfbf;
      end;
    end;
    canv.Rectangle(r);

    canv.Brush.Color:=$cfcfcf;
    canv.Pen.Color:=$8f8f8f;
    canv.Rectangle(r.Left+2+18,r.Top+2,r.Left+2+63+18,r.Top+2+63);

   { layer.ViewRect.Left:=ViewLayerBorder;
    layer.ViewRect.Right:=ClientRect.Right-ClientRect.Left-2*ViewLayerBorder;
    layer.ViewRect.Top:=ViewLayerBorder+(ViewLayerHeight+ViewLayerBorder)*i;
    layer.ViewRect.Bottom:=ViewLayerBorder+(ViewLayerHeight+ViewLayerBorder)*i+ViewLayerHeight-1;}
  end;
end;

{******************************************************************************}
// Подсчет строк со слоями и форматирование
function TSSPicture.Layer_Calculate(ClientRect:TRect):Integer;
  var
    i:Integer;
    layer:TSSLayer;
begin

  for i:=0 to Layers.Length-1 do begin
    layer:=Layers[i];
    layer.ViewRect.Left:=ViewLayerBorder;
    layer.ViewRect.Right:=ClientRect.Right-ClientRect.Left-2*ViewLayerBorder;
    layer.ViewRect.Top:=ViewLayerBorder+(ViewLayerHeight+ViewLayerBorder)*i;
    layer.ViewRect.Bottom:=ViewLayerBorder+(ViewLayerHeight+ViewLayerBorder)*i+ViewLayerHeight-1;
  end;

  result:=2*ViewLayerBorder+(ViewLayerHeight+ViewLayerBorder)*Layers.Length;
end;
{******************************************************************************}
//
function TSSPicture.Layer_HitTest(_x,_y:Integer):Integer;
  var
    i:Integer;
begin
  for i:=0 to Layers.Length-1 do begin
    if ( (Layers[i].ViewRect.Left <= _x ) and (Layers[i].ViewRect.Right >= _x ) and (Layers[i].ViewRect.Top <= _y ) and (Layers[i].ViewRect.Bottom >= _y ) ) then begin
      result:=i;
      exit;
    end;
  end;
  result:=-1;
end;
{******************************************************************************}
procedure TSSPicture.InitRBM(_rbm:TRGB32BitMap);
  var
    i:Integer;
begin
  _rbm.Canvas.Fill(TColor($ff000000));
end;

// Блендинг картинок одинакового размера
procedure TSSPicture.BlendRBM(_rbm1,_rbm2,_destrbm:TRGB32BitMap);
  //var

begin

end;

procedure TSSPicture.BlendLayerToRBM(_layer:TSSLayer;_destrbm:TRGB32BitMap);
begin
 // BlendLayerAndRBMToRBM(_layer,_destrbm,_destrbm);
end;

procedure TSSPicture.BlendLayerAndRBMToRBM(_layer:TSSLayer;_rbm,_destrbm:TRGB32BitMap);
  var
    x,y,lx,ly,lx1,ly1,lx2,ly2:Integer;
    iy,ix,dstadry,srcadry:Integer;
    a,na:Integer;
    srcadr:DWord;
    c,cf:DWord;
    layerblock:PLayerBlock;
    px:PLayerPixel;
begin
  lx1:=_layer.X shr LAYER_BLOCK_SIZE_SHIFT;
  ly1:=_layer.Y shr LAYER_BLOCK_SIZE_SHIFT;
  lx2:=(_layer.X+Width-1) shr LAYER_BLOCK_SIZE_SHIFT;
  ly2:=(_layer.X+Height-1) shr LAYER_BLOCK_SIZE_SHIFT;

  // По всем блокам слоев
  for ly:=ly1 to ly2 do begin
    for lx:=lx1 to lx2 do begin
      layerblock:=_layer.Data[ly,lx];
      if layerblock<>nil then begin // Собственно то ради чего блоки в слое (проверка заполнен ли блок)
        for y:=0 to LAYER_BLOCK_SIZE_M1 do begin
          iy:=y+_layer.Y;
          if (iy>=0) and (iy<Height) then begin

            srcadry:=y*LAYER_BLOCK_SIZE;
            for x:=0 to LAYER_BLOCK_SIZE_M1 do begin
              ix:=x+_layer.X;
              if (ix>=0) and (ix<Width) then begin
                px:=@layerblock^.rgba[x,y];

                srcadr:=srcadry+x;
                c:=_rbm.Get32PixelPtr(ix, iy)^;
                a:=1;//layerblock^.rgba[x,y].a shr 8;
                na:=0;//255-a;
                cf:=((DWord(px^.b) shr 8)*a and $ff00) or (DWord(px^.g and $ff00)*a and $ff0000) or ((DWord(px^.g and $ff00) shl 8)*a and $ff000000);
                _destrbm.Set32Pixel(ix,  iy, (((((c and $0000ff)*na) and $0000ff00) or (((c and $00ff00)*na) and $00ff0000) or (((c and $ff0000)*na) and $ff000000))+cf) shr 8);
              end;
            end;
          end;
        end;
      end;
    end;
  end;

end;
{******************************************************************************}

{*******************************************************************************
  SSLayer
*******************************************************************************}
//
constructor TSSLayer.Create;
  var
    i,j:Integer;
begin
  inherited;
  Effects:=TSSEffectList.Create;
  X:=0;
  Y:=0;

  IsSelected:=false;
  IsMouseOver:=false;

  for i:=0 to LAYER_BLOCKS_M1 do
    for j:=0 to LAYER_BLOCKS_M1 do
      Data[i,j]:=nil;
end;

//
destructor TSSLayer.Destroy;
  var
    i,j:Integer;
begin
  Effects.Destroy;
  for i:=0 to LAYER_BLOCKS_M1 do
    for j:=0 to LAYER_BLOCKS_M1 do
      DoneBlock(i,j);
  inherited;
end;

//
function TSSLayer.GetPixel(_x,_y:Integer):PLayerPixel;
  var
    hx,hy:Integer;
    p:PLayerPixel;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) or ( _y < LAYER_MIN ) or ( _y > LAYER_MAX )) then begin
    result:=@DefaultLayerPixel;
    exit;
  end;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] <> nil
    then result:=@Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1]
    else result:=@DefaultLayerPixel;
end;

procedure TSSLayer.GetPixel(_x,_y:Integer;var _r,_g,_b,_a:Word);
  var
    hx,hy:Integer;
    p:PLayerPixel;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) or ( _y < LAYER_MIN ) or ( _y > LAYER_MAX )) then begin
    _r:=0; _g:=0; _b:=0; _a:=0;
    exit;
  end;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] <> nil
    then begin
      p:=@Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1];
      _r:=p^.r; _g:=p^.g; _b:=p^.b; _a:=p^.a;
    end else begin
      _r:=0; _g:=0; _b:=0; _a:=0;
    end;
end;

procedure TSSLayer.GetPixel(_x,_y:Integer;var _p:TLayerPixel);
  var
    hx,hy:Integer;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) or ( _y < LAYER_MIN ) or ( _y > LAYER_MAX )) then begin
    _p.r:=0; _p.g:=0; _p.b:=0; _p.a:=0;
    exit;
  end;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] <> nil
    then begin
      _p:=Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1]
    end else begin
      _p.r:=0; _p.g:=0; _p.b:=0; _p.a:=0;
    end;
end;

//
procedure TSSLayer.SetPixel(_x,_y:Integer;_r,_g,_b,_a:Word);
  var
    hx,hy:Integer;
    p:PLayerPixel;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) ) then exit;
  if ( ( _y < LAYER_MIN ) or ( _y > LAYER_MAX ) ) then exit;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] = nil then InitBlock(hx,hy);
  Data[hx,hy]^.IsChanged:=true;
  p:=@Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1];
  p^.r:=_r;
  p^.g:=_g;
  p^.b:=_b;
  p^.a:=_a;
  IsNeedUpdate:=true;
end;

//
procedure TSSLayer.SetPixel(_x,_y:Integer;_p:TLayerPixel);
  var
    hx,hy:Integer;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) ) then exit;
  if ( ( _y < LAYER_MIN ) or ( _y > LAYER_MAX ) ) then exit;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] = nil then InitBlock(hx,hy);
  Data[hx,hy]^.IsChanged:=true;
  Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1]:=_p;
  IsNeedUpdate:=true;
end;

//
procedure TSSLayer.SetPixel(_x,_y:Integer;_p:PLayerPixel);
  var
    hx,hy:Integer;
    p:PLayerPixel;
begin
  _x:=_x-X;
  _y:=_y-Y;
  if ( ( _x < LAYER_MIN ) or ( _x > LAYER_MAX ) ) then exit;
  if ( ( _y < LAYER_MIN ) or ( _y > LAYER_MAX ) ) then exit;
  hx:=_x shr LAYER_BLOCK_SIZE_SHIFT;
  hy:=_y shr LAYER_BLOCK_SIZE_SHIFT;
  if Data[hx,hy] = nil then InitBlock(hx,hy);
  Data[hx,hy]^.IsChanged:=true;
  Data[hx,hy]^.rgba[_x and LAYER_BLOCK_SIZE_M1,_y and LAYER_BLOCK_SIZE_M1]:=_p^;
  IsNeedUpdate:=true;
end;


procedure TSSLayer.SetLayerOffset(_x,_y:Integer);
begin
  X:=_x;
  Y:=_y;
  IsNeedUpdate:=true;
end;

procedure TSSLayer.InitBlock(_hx,_hy:Integer);
begin
  Data[_hx,_hy]:=GetMem(SizeOf(TLayerBlock));
end;

procedure TSSLayer.DoneBlock(_hx,_hy:Integer);
begin
  Freememory(Data[_hx,_hy],SizeOf(TLayerBlock));
end;

//
procedure TSSLayer.Update;
begin


  IsNeedUpdate:=false;
end;
{******************************************************************************}

{*******************************************************************************
  SSEffect
*******************************************************************************}
constructor TSSEffect.Create;
begin
  inherited;

end;

destructor TSSEffect.Destroy;
begin

  inherited;
end;
{******************************************************************************}

{*******************************************************************************
  SSMask
*******************************************************************************}
constructor TSSMask.Create;
begin

end;

destructor TSSMask.Destroy;
begin

end;

initialization
  DefaultLayerPixel.r:=0;
  DefaultLayerPixel.g:=0;
  DefaultLayerPixel.b:=0;
  DefaultLayerPixel.a:=0;

finalization

{******************************************************************************}
end.
{******************************************************************************}

