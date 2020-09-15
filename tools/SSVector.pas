{*******************************************************************************

  Author: Fomin Sergey (fomonster@gmail.com)
  Date: 2009

  Vector and matrix operations for freepascal.
                        
  http://geometryalgorithms.com/Archive/algorithm_0104/algorithm_0104B.htm
  
  http://www.geometrictools.com/LibFoundation/Intersection/Intersection.html

  http://www.gamedev.ru/users/wat/articles/quaternions
  
                        
*******************************************************************************}
unit SSVector;

{$mode objfpc}{$H+}
{******************************************************************************}

interface

  uses  math;
  const
{******************************************************************************}
  fUs = 1.0/127.0;
{******************************************************************************}
  type
{******************************************************************************}
// TVector
  PVector=^TVector;
  TVector=packed object
    x,y,z:Single;
    constructor Init;overload;
    constructor Init(_x,_y,_z:Single);overload;
    procedure Translatef(dx,dy,dz:Single);
    procedure RotateXf(a:Single);
    procedure RotateYf(a:Single);
    procedure RotateZf(a:Single);
    function Normalize:Single;
    // операции присваивания
    procedure Eq(vector:TVector);overload;   // v=v1
    procedure Eqv(_x,_y,_z:Single); // v=(x,y,z)
    // операции с константами
    function MulCV(k:Single):TVector;  // v1=k*v
    function DivCV(k:Single):TVector;  // v1=v/k
    function AddCV(k:Single):TVector;  // v1=v+k
    function SubCV(k:Single):TVector;  // v1=v-k
    procedure MulC(k:Single);  // v=k*v
    procedure DivC(k:Single);  // v=v/k
    procedure AddC(k:Single);  // v=v+k
    procedure SubC(k:Single);  // v=v-k
    // бинарные операции
    function AddV(vector:TVector):TVector;  // v2=v+v1
    function SubV(vector:TVector):TVector;  // v2=v-v1
    // бинарные операции с самоприсваиванием
    procedure Add(vector:TVector);  // v=v+v1
    procedure Sub(vector:TVector);  // v=v-v1
    // унарные операции
    function M:TVector; // -v
    function D:TVector; // 1/v
    function GetNormalized:TVector; // Norm(v)
    function GetLength:Single;
    function IsMore(v:TVector):boolean;inline;
    function IsLess(v:TVector):boolean;inline;
    function IsMoreAndEqual(v:TVector):boolean;inline;
    function IsLessAndEqual(v:TVector):boolean;inline;
    function IsEqual(v:TVector):boolean;inline;
    function IsNotEqual(v:TVector):boolean;inline;
  end;
{******************************************************************************}

{******************************************************************************}
// TDoubleVector
  PDoubleVector=^TDoubleVector;
  TDoubleVector=packed object
    x,y,z:Double;
    constructor Init;overload;
    constructor Init(_x,_y,_z:Double);overload;
    procedure Translatef(dx,dy,dz:Double);
    procedure RotateXf(a:Double);
    procedure RotateYf(a:Double);
    procedure RotateZf(a:Double);
    function Normalize:Double;
    // операции присваивания
    procedure Eq(vector:TDoubleVector);overload;   // v=v1
    procedure Eqv(_x,_y,_z:Double); // v=(x,y,z)
    // операции с константами
    function MulCV(k:Double):TDoubleVector;  // v1=k*v
    function DivCV(k:Double):TDoubleVector;  // v1=v/k
    function AddCV(k:Double):TDoubleVector;  // v1=v+k
    function SubCV(k:Double):TDoubleVector;  // v1=v-k
    procedure MulC(k:Double);  // v=k*v
    procedure DivC(k:Double);  // v=v/k
    procedure AddC(k:Double);  // v=v+k
    procedure SubC(k:Double);  // v=v-k
    // бинарные операции
    function AddV(vector:TDoubleVector):TDoubleVector;  // v2=v+v1
    function SubV(vector:TDoubleVector):TDoubleVector;  // v2=v-v1
    // бинарные операции с самоприсваиванием
    procedure Add(vector:TDoubleVector);  // v=v+v1
    procedure Sub(vector:TDoubleVector);  // v=v-v1
    // унарные операции
    function M:TDoubleVector; // -v
    function D:TDoubleVector; // 1/v
    function GetNormalized:TDoubleVector; // Norm(v)
    function GetLength:Double;
    function IsMore(v:TDoubleVector):boolean;
    function IsLess(v:TDoubleVector):boolean;
    function IsMoreAndEqual(v:TDoubleVector):boolean;
    function IsLessAndEqual(v:TDoubleVector):boolean;


    procedure sV(v:TVector);
    function gV:TVector;
  end;
{******************************************************************************}
  PDoubleVectorArray = ^TDoubleVectorArray;
  TDoubleVectorArray = array [0..10000000] of TDoubleVector;
{******************************************************************************}

{******************************************************************************}
// TMatrix
  PMatrix=^TMatrix;
  TMatrix=packed object
    k11,k12,k13,k14,k21,k22,k23,k24,k31,k32,k33,k34,k41,k42,k43,k44:Single;
    procedure EqM(m:TMatrix);overload;
    constructor SetIdentity;
    procedure MulC(c:Single);

    procedure setTranslateLeft(x,y,z:Single);overload;
    procedure setTranslateLeft(_v:TVector);overload;
    procedure setTranslateRight(x,y,z:Single);overload;
    procedure setTranslateRight(_v:TVector);overload;
    procedure setScale(x,y,z:Single);
    procedure setXRotation(a:Single);
    procedure setYRotation(a:Single);
    procedure setZRotation(a:Single);
    procedure setOrbitraryRotation(x,y,z,a:Single);
    function multLeft(m:TMatrix):TMatrix; // Returns a Matrix whose value is the object multiplied by the passed matrix on the left.
    function multRight(m:TMatrix):TMatrix; // Returns a Matrix whose value is the object multiplied by the passed matrix on the right.
    function multVecMatrix(v:TVector):TVector; // Returns an Vec whose value is the object multiplied by the passed row vector.
    function multMatrixVec(v:TVector):TVector; // Returns an Vec whose value is the object multiplied by the passed column vector.
    procedure setTransform(tx,ty,tz,rx,ry,rz,ra,sx,sy,sz,sox,soy,soz,soa,cx,cy,cz:Single); // new method specified for VRML
    procedure Transpose;
    procedure Inverse3x3;
    procedure Inverse4x4;
    function GetDeterminant3x3:Single;
    function GetDeterminant4x4:Double;
    function GetPosition:TVector;
    procedure SetPosition(p:TVector);
    procedure NullPosition;
    procedure Roll(angle:Single);
    procedure Normalize;
    procedure ClearTranslations;
    
    // Для скелетной модели
    procedure SetRotationFromAngles(angles:TVector);
    procedure SetTranslation(trans:TVector);
    procedure InverseRotateVect(var pVect:TVector);
    procedure InverseTranslateVect(var pVect:TVector);
    
    // Для просмотра
    procedure SetViewMatrix(From, At, Worldup: TVector);
    procedure Perspective(fovy, aspect, zNear, zFar: Double);
    procedure LookAt(eye, center, worldup: TVector);
    procedure SetProjectionMatrix(fFOV, fAspect, fNearPlane, fFarPlane: Single);
    procedure SetTranslationMatrix(trans:TVector);
  end;
{******************************************************************************}

{******************************************************************************}
// TDoubleMatrix
  PDoubleMatrix=^TDoubleMatrix;
  TDoubleMatrix=packed object
    k11,k12,k13,k14,k21,k22,k23,k24,k31,k32,k33,k34,k41,k42,k43,k44:Double;
    procedure EqM(m:TDoubleMatrix);overload;
    constructor SetIdentity;
    procedure MulC(c:Double);

    procedure setTranslateLeft(x,y,z:Double);overload;
    procedure setTranslateLeft(_v:TDoubleVector);overload;
    procedure setTranslateRight(x,y,z:Double);overload;
    procedure setTranslateRight(_v:TDoubleVector);overload;
    procedure setScale(x,y,z:Double);
    procedure setXRotation(a:Double);
    procedure setYRotation(a:Double);
    procedure setZRotation(a:Double);
    procedure setOrbitraryRotation(x,y,z,a:Double);
    function multLeft(m:TDoubleMatrix):TDoubleMatrix; // Returns a Matrix whose value is the object multiplied by the passed matrix on the left.
    function multRight(m:TDoubleMatrix):TDoubleMatrix; // Returns a Matrix whose value is the object multiplied by the passed matrix on the right.
    function multVecMatrix(v:TDoubleVector):TDoubleVector; // Returns an Vec whose value is the object multiplied by the passed row vector.
    function multMatrixVec(v:TDoubleVector):TDoubleVector; // Returns an Vec whose value is the object multiplied by the passed column vector.
    procedure setTransform(tx,ty,tz,rx,ry,rz,ra,sx,sy,sz,sox,soy,soz,soa,cx,cy,cz:Double); // new method specified for VRML
    procedure Transpose;
    procedure Inverse3x3;
    procedure Inverse4x4;
    function GetDeterminant3x3:Double;
    function GetDeterminant4x4:Double;
    function GetPosition:TVector;
    procedure SetPosition(p:TVector);
    procedure NullPosition;
    procedure Roll(angle:Double);
    procedure Normalize;
    procedure ClearTranslations;

    // Для скелетной модели
    procedure SetRotationFromAngles(angles:TVector);
    procedure SetTranslation(trans:TDoubleVector);
    procedure InverseRotateVect(var pVect:TDoubleVector);
    procedure InverseTranslateVect(var pVect:TDoubleVector);

    // Для просмотра
    procedure SetViewMatrix(From, At, Worldup: TDoubleVector);
    procedure Perspective(fovy, aspect, zNear, zFar: Double);
    procedure LookAt(eye, center, worldup: TDoubleVector);
    procedure SetProjectionMatrix(fFOV, fAspect, fNearPlane, fFarPlane: Double);
    procedure SetTranslationMatrix(trans:TDoubleVector);

    procedure sM(m:TMatrix);
    function gM:TMatrix;
  end;
{******************************************************************************}

{******************************************************************************}
// TQuaternion
  PQuaternion=^TQuaternion;
  TQuaternion=object
    {**************************************************************************}
    x,y,z,w:Double;
    {**************************************************************************}
    constructor Init;
    destructor Done;
    {**************************************************************************}
    procedure Eqv(_x,_y,_z,_w:Double);
    {**************************************************************************}
    procedure Mul(q:TQuaternion);
    function MulQ(q:TQuaternion):TQuaternion;
    procedure Add(_q:TQuaternion);
    function AddQ(_q:TQuaternion):TQuaternion;
    procedure Inverse;
    procedure Conjugate;
    procedure Normalize;
    {**************************************************************************}
    function GetLength:Single;
    function GetInversed:TQuaternion;
    function GetConjugated:TQuaternion;
    function GetNormalized:TQuaternion;
    function GetAngle:Double;
    {**************************************************************************}
    procedure SetFromEyler(const yaw,pitch,roll:Double);overload;
    procedure SetFromEyler(const angles:TDoubleVector);overload;
    procedure SetFromEyler(const angles:TVector);overload;
    procedure SetFromSpherical(const latitude,longitude,angle:Double);
    procedure SetFromMatrix(const m:TMatrix);
    procedure SetFromAxisAngle(const Axis:TVector;const Angle:Double);overload;
    procedure SetFromAxisAngle(const Axis:TDoubleVector;const Angle:Double);overload;
    {**************************************************************************}
    function GetRotationMatrix:TMatrix;overload; // 4x4
    function GetRotationMatrix:TDoubleMatrix;overload; // 4x4
    {**************************************************************************}
    procedure SetRotation(var m:TMatrix);overload;  // 3x3
    procedure SetRotation(var m:TDoubleMatrix);overload;  // 3x3
    procedure SetRotationMatrix(var m:TMatrix);overload; // 4x4
    procedure SetRotationMatrix(var m:TDoubleMatrix);overload; // 4x4
    procedure SetToAxisAngle(var Axis:TVector;var Angle:Double);overload;
    procedure SetToAxisAngle(var Axis:TDoubleVector;var Angle:Double);overload;
    {**************************************************************************}
    procedure ShortestArc(FromV,ToV:TVector);overload;inline;
    procedure ShortestArc(FromV,ToV:TDoubleVector);overload;inline;
    procedure Interpolate(q1,q2:TQuaternion;d:Double);inline;
    {**************************************************************************}

  end;
{******************************************************************************}
  TLine = record // Отрезок линии
    A,B:TVector;
  end;

  TPlane = packed object
    n:TVector;
    d:Single;
    p:TVector;
    procedure Normalize;
    procedure SetFrom(normal,point:TVector);
    function IsIntersectionByLine(s:TLine;var _p:TVector):boolean;
  end;
{******************************************************************************}
  function gV(const x,y,z:Single):TVector;overload;   // Возвращаем вектор
  function gDV(const x,y,z:Single):TDoubleVector;overload;// Возвращаем вектор
  function gV(const v:TDoubleVector):TVector;overload;   // Возвращаем вектор
  function gDV(const v:TVector):TDoubleVector;overload; // Возвращаем вектор

  function MagnitudeVector(const v:TVector):Single;overload;inline; // длинна вектора
  function MagnitudeDoubleVector(const v:TDoubleVector):Double;overload;inline; // длинна вектора
  function QuadMagnitudeVector(const v:TVector):Single;inline;
  function QuadMagnitudeDoubleVector(const v:TDoubleVector):Double;inline;

  function CrossVector(const v1,v2:TVector):TVector;overload; // векторное произведение
  function CrossVector(const v1,v2:TDoubleVector):TDoubleVector;overload; // векторное произведение

  function DotVector(const v1,v2:TVector):Single;overload; // скалярное произведение векторов
  function DotVector(const v1,v2:TDoubleVector):Double;overload; // скалярное произведение векторов

  function NormalVector(const v1,v2,v3:TVector):TVector;overload; // Нормаль к плоскости по трем точкам
  function NormalVector(const v1,v2,v3:TDoubleVector):TDoubleVector;overload; // Нормаль к плоскости по трем точкам
  function NormalVector(const x1,y1,z1,x2,y2,z2,x3,y3,z3:Single):TVector;overload;
  function NormalVector(const x1,y1,z1,x2,y2,z2,x3,y3,z3:Double):TDoubleVector;overload;

  function Distance(const p1,p2:TVector):Single;overload; // расстояние между точками
  function Distance(const p1,p2:TDoubleVector):Double;overload; // расстояние между точками

  function Project(const v:TVector;const matModel,matProj:TMatrix;const left,top,width,height:Integer):TVector;overload;
  function Project(const v:TDoubleVector;const matModel,matProj:TDoubleMatrix;const left,top,width,height:Integer):TDoubleVector;overload;

  function Unproject(const inwin:TVector;const matModel,matProj:TMatrix;const left,top,width,height:Integer;const vnear,vfar:Single):TVector;overload;
  function Unproject(const inwin:TDoubleVector;const matModel,matProj:TDoubleMatrix;const left,top,width,height:Integer;const vnear,vfar:Double):TDoubleVector;overload;

  function InvertMatrix(const m:TMatrix):TMatrix;overload;
  function InvertMatrix(const m:TDoubleMatrix):TDoubleMatrix;overload;

  function PerspectiveZValue(const realZ:Single;const matProj:TMatrix):Single;overload;
  function PerspectiveZValue(const realZ:Double;const matProj:TDoubleMatrix):Double;overload;

  function AngleBetweenVectors(const v1,v2:TVector):Single;overload;// угол между векторами
  function AngleBetweenVectors(const v1,v2:TDoubleVector):Double;overload;// угол между векторами

  function ShortestArc(const FromV,ToV:TVector):TQuaternion;overload;
  function ShortestArc(const FromV,ToV:TDoubleVector):TQuaternion;overload;

  function PlaneDistance(const Normal,Point,ToPoint:TVector):Single;overload; // расстояние от точки до плоскости
  function PlaneDistance(const Normal,Point,ToPoint:TDoubleVector):Double;overload; // расстояние от точки до плоскости

{******************************************************************************}
  function S2B(s:Single):ShortInt;
  function B2S(b:ShortInt):Single;
{******************************************************************************}
  type

{******************************************************************************}
// TVectorList
PVectorArray = ^TVectorArray;
TVectorArray = array [0..100000000] of TVector;

TVectorList = class

private
  VectorArray:PVectorArray;

public
  count:Integer;

  constructor Create;
  destructor Destroy;

  function Get(index:Integer):PVector;
  procedure Put(index:Integer;const value:TVector);

  procedure Clear;
  procedure SetLength(NewLength:Integer);

  function Add:PVector;
  function Insert(index:Integer):PVector;
  function Push:PVector;
  function Pop:TVector;
  procedure Delete(index:Integer);

  property item[index : Integer]:PVector read Get;default;
end;
{******************************************************************************}
operator=(a,b:TVector):boolean;inline;
operator+(a,b:TVector):TVector;inline;
operator-(a,b:TVector):TVector;inline;
operator*(v1,v2:TVector):TVector;inline;
operator*(a:TVector;b:Single):TVector;inline;
operator/(a:TVector;b:Single):TVector;inline;

implementation
{******************************************************************************}
operator=(a,b:TVector):Boolean;inline;
begin
  result:=(a.x=b.x) and (a.y=b.y) and (a.z=b.z);
end;
{******************************************************************************}
operator+(a,b:TVector):TVector;inline;
begin
  result.x:=a.x+b.x;
  result.y:=a.y+b.y;
  result.z:=a.z+b.z;
end;
{******************************************************************************}
operator-(a,b:TVector):TVector;inline;
begin
  result.x:=a.x-b.x;
  result.y:=a.y-b.y;
  result.z:=a.z-b.z;
end;
{******************************************************************************}
operator*(v1,v2:TVector):TVector;inline;
begin
  result.x:=v1.y * v2.z - v1.z * v2.y;
  result.y:=v1.z * v2.x - v1.x * v2.z;
  result.z:=v1.x * v2.y - v1.y * v2.x;
end;
{******************************************************************************}
operator*(a:TVector;b:Single):TVector;inline;
begin
  result.x:=a.x*b;
  result.y:=a.y*b;
  result.z:=a.z*b;
end;
{******************************************************************************}
operator/(a:TVector;b:Single):TVector;inline;
begin
  if (b>0.00001)or(b<-0.00001) then begin
    result.x:=a.x/b;
    result.y:=a.y/b;
    result.z:=a.z/b;
  end else begin
    result.x:=0;
    result.y:=0;
    result.z:=0;
  end;
end;
{******************************************************************************}
// TVector
constructor TVector.Init;
begin
  x:=0; y:=0; z:=0;
end;
{******************************************************************************}
constructor TVector.Init(_x,_y,_z:Single);
begin
  x:=_x; y:=_y; z:=_z;
end;
{******************************************************************************}
procedure TVector.Translatef(dx,dy,dz:Single);
begin
  x:=x+dx; y:=y+dy; z:=z+dz;
end;
{******************************************************************************}
function TVector.GetLength:Single;
begin
  result:=sqrt(x*x+y*y+z*z);
end;
{procedure TVector.Add(v:TVector);
begin
  x:=x+v.x;
  y:=y+v.y;
  z:=z+v.z;
end;}
{******************************************************************************}
{procedure TVector.Sub(v:TVector);
begin
  x:=x-v.x;
  y:=y-v.y;
  z:=z-v.z;
end;}
{******************************************************************************}
{procedure TVector.MulC(c:Single);
begin
  x:=x*c;
  y:=y*c;
  z:=z*c;
end;}
{******************************************************************************}
procedure TVector.RotateXf(a:Single);
  var
    _x,_y,_z:Single;
begin
  _y:=y*cos(a)-z*sin(a);
  _z:=y*sin(a)+z*cos(a);
  y:=_y; z:=_z;
end;
{******************************************************************************}
procedure TVector.RotateYf(a:Single);
  var
    _x,_y,_z:Single;
begin
  _x:=x*cos(a)+z*sin(a);
  _z:=-x*sin(a)+z*cos(a);
  x:=_x; z:=_z;
end;
{******************************************************************************}
procedure TVector.RotateZf(a:Single);
  var
    _x,_y,_z:Single;
begin
  _x:=x*cos(a)-y*sin(a);
  _y:=x*sin(a)+y*cos(a);
  y:=_y; x:=_x;
end;
{******************************************************************************}
procedure TVector.Eq(vector:TVector);
begin
  x:=vector.x;
  y:=vector.y;
  z:=vector.z;
end;
{******************************************************************************}
{procedure TVector.Eq(vector:_D3DVECTOR);
begin
  x:=vector.x;
  y:=vector.y;
  z:=vector.z;
end;}
{******************************************************************************}
procedure TVector.Eqv(_x,_y,_z:Single);
begin
  x:=_x;
  y:=_y;
  z:=_z;
end;
{******************************************************************************}
procedure TVector.Add(vector:TVector);
begin
  x:=x+vector.x;
  y:=y+vector.y;
  z:=z+vector.z;
end;
{******************************************************************************}
procedure TVector.Sub(vector:TVector);
begin
  x:=x-vector.x;
  y:=y-vector.y;
  z:=z-vector.z;
end;
{******************************************************************************}
function TVector.M:TVector;
begin
  result.Eqv(-x,-y,-z);
end;
{******************************************************************************}
function TVector.D:TVector;
begin
  result.Eqv(1/x,1/y,1/z);
end;
{******************************************************************************}
{function TVector.G:D3DVECTOR;
begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
end;}
{******************************************************************************}
function TVector.AddV(vector:TVector):TVector;  // v2=v+v1
begin
  result.Eqv(x+vector.x,y+vector.y,z+vector.z);
end;
{******************************************************************************}
function TVector.SubV(vector:TVector):TVector;  // v2=v-v1
begin
  result.Eqv(x-vector.x,y-vector.y,z-vector.z);
end;
{******************************************************************************}
function TVector.MulCV(k:Single):TVector;  // v1=k*v
begin
  result.Eqv(x*k,y*k,z*k);
end;
{******************************************************************************}
function TVector.DivCV(k:Single):TVector;  // v1=v/k
begin
  result.Eqv(x/k,y/k,z/k);
end;
{******************************************************************************}
function TVector.AddCV(k:Single):TVector;  // v1=v+k
begin
  result.Eqv(x+k,y+k,z+k);
end;
{******************************************************************************}
function TVector.SubCV(k:Single):TVector;  // v1=v-k
begin
  result.Eqv(x-k,y-k,z-k);
end;
{******************************************************************************}
procedure TVector.MulC(k:Single);  // v=k*v
begin
  x:=x*k;
  y:=y*k;
  z:=z*k;
end;
{******************************************************************************}
procedure TVector.DivC(k:Single);  // v=v/k
begin
  x:=x/k;
  y:=y/k;
  z:=z/k;
end;
{******************************************************************************}
procedure TVector.AddC(k:Single);  // v=v+k
begin
  x:=x+k;
  y:=y+k;
  z:=z+k;
end;
{******************************************************************************}
procedure TVector.SubC(k:Single);  // v=v-k
begin
  x:=x-k;
  y:=y-k;
  z:=z-k;
end;
{******************************************************************************}
function TVector.Normalize:Single;
  var
    r:Double;
begin
  result:=x*x+y*y+z*z;
  if (result>=0.00000001) then begin
    result:=sqrt(result);
    r:=1/result;
    x:=x*r;
    y:=y*r;
    z:=z*r;
  end else begin
    result:=1;
    x:=1; y:=0; z:=0;
  end;
end;
{******************************************************************************}
function TVector.GetNormalized:TVector; // Norm(v)
  var
    r:Single;
begin
  r:=x*x+y*y+z*z;
  if (r>=0.0000001) then begin
    r:=1/sqrt(r);
    result.x:=x*r;
    result.y:=y*r;
    result.z:=z*r;
  end else begin
    result.x:=0;
    result.y:=1;
    result.z:=0;
  end;
end;
{******************************************************************************}
function TVector.IsMore(v:TVector):boolean;
begin
  result:=false;
  if x>v.x then begin result:=true; exit; end;
  if x<v.x then exit;
  if y>v.y then begin result:=true; exit; end;
  if y<v.y then exit;
  if z>v.z then begin result:=true; exit; end;
end;
{******************************************************************************}
function TVector.IsLess(v:TVector):boolean;
begin
  result:=(x<v.x) or (y<v.y) or (z<v.z);
end;
{******************************************************************************}
function TVector.IsMoreAndEqual(v:TVector):boolean;
begin
  result:=(x>=v.x) and (y>=v.y) and (z>=v.z);
end;
{******************************************************************************}
function TVector.IsLessAndEqual(v:TVector):boolean;
begin
  result:=(x<=v.x) and (y<=v.y) and (z<=v.z);
end;
{******************************************************************************}
function TVector.IsEqual(v:TVector):boolean;
begin
  result:=(x=v.x) and (y=v.y) and (z=v.z);
end;
{******************************************************************************}
function TVector.IsNotEqual(v:TVector):boolean;inline;
begin
  result:=(x<>v.x) or (y<>v.y) or (z<>v.z);
end;
{******************************************************************************}

{******************************************************************************}
// TDoubleVector
constructor TDoubleVector.Init;
begin
  x:=0; y:=0; z:=0;
end;
{******************************************************************************}
constructor TDoubleVector.Init(_x,_y,_z:Double);
begin
  x:=_x; y:=_y; z:=_z;
end;
{******************************************************************************}
procedure TDoubleVector.Translatef(dx,dy,dz:Double);
begin
  x:=x+dx; y:=y+dy; z:=z+dz;
end;
{******************************************************************************}
function TDoubleVector.GetLength:Double;
begin
  result:=sqrt(x*x+y*y+z*z);
end;
{procedure TDoubleVector.Add(v:TDoubleVector);
begin
  x:=x+v.x;
  y:=y+v.y;
  z:=z+v.z;
end;}
{******************************************************************************}
{procedure TDoubleVector.Sub(v:TDoubleVector);
begin
  x:=x-v.x;
  y:=y-v.y;
  z:=z-v.z;
end;}
{******************************************************************************}
{procedure TDoubleVector.MulC(c:Double);
begin
  x:=x*c;
  y:=y*c;
  z:=z*c;
end;}
{******************************************************************************}
procedure TDoubleVector.RotateXf(a:Double);
  var
    _x,_y,_z:Double;
begin
  _y:=y*cos(a)-z*sin(a);
  _z:=y*sin(a)+z*cos(a);
  y:=_y; z:=_z;
end;
{******************************************************************************}
procedure TDoubleVector.RotateYf(a:Double);
  var
    _x,_y,_z:Double;
begin
  _x:=x*cos(a)+z*sin(a);
  _z:=-x*sin(a)+z*cos(a);
  x:=_x; z:=_z;
end;
{******************************************************************************}
procedure TDoubleVector.RotateZf(a:Double);
  var
    _x,_y,_z:Double;
begin
  _x:=x*cos(a)-y*sin(a);
  _y:=x*sin(a)+y*cos(a);
  y:=_y; x:=_x;
end;
{******************************************************************************}
procedure TDoubleVector.Eq(vector:TDoubleVector);
begin
  x:=vector.x;
  y:=vector.y;
  z:=vector.z;
end;
{******************************************************************************}
{procedure TDoubleVector.Eq(vector:_D3DVECTOR);
begin
  x:=vector.x;
  y:=vector.y;
  z:=vector.z;
end;}
{******************************************************************************}
procedure TDoubleVector.Eqv(_x,_y,_z:Double);
begin
  x:=_x;
  y:=_y;
  z:=_z;
end;
{******************************************************************************}
procedure TDoubleVector.Add(vector:TDoubleVector);
begin
  x:=x+vector.x;
  y:=y+vector.y;
  z:=z+vector.z;
end;
{******************************************************************************}
procedure TDoubleVector.Sub(vector:TDoubleVector);
begin
  x:=x-vector.x;
  y:=y-vector.y;
  z:=z-vector.z;
end;
{******************************************************************************}
function TDoubleVector.M:TDoubleVector;
begin
  result.Eqv(-x,-y,-z);
end;
{******************************************************************************}
function TDoubleVector.D:TDoubleVector;
begin
  result.Eqv(1/x,1/y,1/z);
end;
{******************************************************************************}
{function TDoubleVector.G:D3DVECTOR;
begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
end;}
{******************************************************************************}
function TDoubleVector.AddV(vector:TDoubleVector):TDoubleVector;  // v2=v+v1
begin
  result.Eqv(x+vector.x,y+vector.y,z+vector.z);
end;
{******************************************************************************}
function TDoubleVector.SubV(vector:TDoubleVector):TDoubleVector;  // v2=v-v1
begin
  result.Eqv(x-vector.x,y-vector.y,z-vector.z);
end;
{******************************************************************************}
function TDoubleVector.MulCV(k:Double):TDoubleVector;  // v1=k*v
begin
  result.Eqv(x*k,y*k,z*k);
end;
{******************************************************************************}
function TDoubleVector.DivCV(k:Double):TDoubleVector;  // v1=v/k
begin
  result.Eqv(x/k,y/k,z/k);
end;
{******************************************************************************}
function TDoubleVector.AddCV(k:Double):TDoubleVector;  // v1=v+k
begin
  result.Eqv(x+k,y+k,z+k);
end;
{******************************************************************************}
function TDoubleVector.SubCV(k:Double):TDoubleVector;  // v1=v-k
begin
  result.Eqv(x-k,y-k,z-k);
end;
{******************************************************************************}
procedure TDoubleVector.MulC(k:Double);  // v=k*v
begin
  x:=x*k;
  y:=y*k;
  z:=z*k;
end;
{******************************************************************************}
procedure TDoubleVector.DivC(k:Double);  // v=v/k
begin
  x:=x/k;
  y:=y/k;
  z:=z/k;
end;
{******************************************************************************}
procedure TDoubleVector.AddC(k:Double);  // v=v+k
begin
  x:=x+k;
  y:=y+k;
  z:=z+k;
end;
{******************************************************************************}
procedure TDoubleVector.SubC(k:Double);  // v=v-k
begin
  x:=x-k;
  y:=y-k;
  z:=z-k;
end;
{******************************************************************************}
function TDoubleVector.Normalize:Double;
  var
    r:Double;
begin
  result:=x*x+y*y+z*z;
  if (result>=0.0000000000001) then begin
    result:=Sqrt(result);
    r:=1/result;
    x:=x*r;
    y:=y*r;
    z:=z*r;
  end else begin
    result:=1;
    x:=1; y:=0; z:=0;
  end;
end;
{******************************************************************************}
function TDoubleVector.GetNormalized:TDoubleVector; // Norm(v)
  var
    r:Double;
begin
  r:=x*x+y*y+z*z;
  if (r>=0.000000000001) then begin
    r:=1/sqrt(r);
    result.x:=x*r;
    result.y:=y*r;
    result.z:=z*r;
  end else begin
    result.x:=1;
    result.y:=0;
    result.z:=0;
  end;
end;
{******************************************************************************}
function TDoubleVector.IsMore(v:TDoubleVector):boolean;
begin
  result:=(x>v.x) or (y>v.y) or (z>v.z);
end;
{******************************************************************************}
function TDoubleVector.IsLess(v:TDoubleVector):boolean;
begin
  result:=(x<v.x) or (y<v.y) or (z<v.z);
end;
{******************************************************************************}
function TDoubleVector.IsMoreAndEqual(v:TDoubleVector):boolean;
begin
  result:=(x>=v.x) and (y>=v.y) and (z>=v.z);
end;
{******************************************************************************}
function TDoubleVector.IsLessAndEqual(v:TDoubleVector):boolean;
begin
  result:=(x<=v.x) and (y<=v.y) and (z<=v.z);
end;
{******************************************************************************}
procedure TDoubleVector.sV(v:TVector);
begin
  x:=v.x; y:=v.y; z:=v.z;
end;
{******************************************************************************}
function TDoubleVector.gV:TVector;
begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
end;
{******************************************************************************}

{******************************************************************************}
// TMatrix
procedure TMatrix.SetRotationFromAngles(angles:TVector);
  var
    cr,sr,cp,sp,cy,sy,
    srsp,crsp : single;
begin
  cr:= cos(angles.x);
  sr:= sin(angles.x);
  cp:= cos(angles.y);
  sp:= sin(angles.y);
  cy:= cos(angles.z);
  sy:= sin(angles.z);

  k11:= cp*cy;
  k12:= cp*sy;
  k13:= -sp;

  srsp:= sr*sp;
  crsp:= cr*sp;

  k21:= srsp*cy-cr*sy;
  k22:= srsp*sy+cr*cy;
  k23:= sr*cp;

  k31:= crsp*cy+sr*sy;
  k32:= crsp*sy-sr*cy;
  k33:= cr*cp;
end;
{******************************************************************************}
procedure TMatrix.SetTranslation(trans:TVector);
begin
  k41:=trans.x;
  k42:=trans.y;
  k43:=trans.z;
end;
{******************************************************************************}
procedure TMatrix.InverseRotateVect(var pVect:TVector);
var
  temp : TVector;
begin
  temp:=pVect;

  temp.x:= pVect.x*k11+pVect.y*k12+pVect.z*k13;
  temp.y:= pVect.x*k21+pVect.y*k22+pVect.z*k23;
  temp.z:= pVect.x*k31+pVect.y*k32+pVect.z*k33;

{  temp.x:= pVect.x*k11+pVect.y*k21+pVect.z*k31;
  temp.y:= pVect.x*k12+pVect.y*k22+pVect.z*k32;
  temp.z:= pVect.x*k13+pVect.y*k23+pVect.z*k33;}

  pVect:=temp;
end;

{******************************************************************************}
procedure TMatrix.InverseTranslateVect(var pVect:TVector);
var
  temp : TVector;
begin
     temp:=pVect;

     temp.x:= pVect.x-k41;
     temp.y:= pVect.y-k42;
     temp.z:= pVect.z-k43;

     pVect:=temp;
end;

{procedure TMatrix.SetRotationQuaternion(quat:TQuaternion);
begin
     m_matrix[0]:= ( 1.0 - 2.0*quat.m_quat[1]*quat.m_quat[1] - 2.0*quat.m_quat[2]*quat.m_quat[2]);
     m_matrix[1]:= ( 2.0*quat.m_quat[0]*quat.m_quat[1] + 2.0*quat.m_quat[3]*quat.m_quat[2] );
     m_matrix[2]:= ( 2.0*quat.m_quat[0]*quat.m_quat[2] - 2.0*quat.m_quat[3]*quat.m_quat[1] );

     m_matrix[4]:= ( 2.0*quat.m_quat[0]*quat.m_quat[1] - 2.0*quat.m_quat[3]*quat.m_quat[2] );
     m_matrix[5]:= ( 1.0 - 2.0*quat.m_quat[0]*quat.m_quat[0] - 2.0*quat.m_quat[2]*quat.m_quat[2] );
     m_matrix[6]:= ( 2.0*quat.m_quat[1]*quat.m_quat[2] + 2.0*quat.m_quat[3]*quat.m_quat[0] );

     m_matrix[8]:= ( 2.0*quat.m_quat[0]*quat.m_quat[2] + 2.0*quat.m_quat[3]*quat.m_quat[1] );
     m_matrix[9]:= ( 2.0*quat.m_quat[1]*quat.m_quat[2] - 2.0*quat.m_quat[3]*quat.m_quat[0] );
     m_matrix[10]:= ( 1.0 - 2.0*quat.m_quat[0]*quat.m_quat[0] - 2.0*quat.m_quat[1]*quat.m_quat[1] );
end;
}
{******************************************************************************}
constructor TMatrix.setIdentity;
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setTranslateLeft(x,y,z:Single);
begin
  k11:=1; k12:=0; k13:=0; k14:=x;
  k21:=0; k22:=1; k23:=0; k24:=y;
  k31:=0; k32:=0; k33:=1; k34:=z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setTranslateRight(x,y,z:Single);
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=x; k42:=y; k43:=z; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.Transpose;
  var
    m:TMatrix;
begin
  m.k11:=k11; m.k12:=k21; m.k13:=k31; m.k14:=k41;
  m.k21:=k12; m.k22:=k22; m.k23:=k32; m.k24:=k42;
  m.k31:=k13; m.k32:=k23; m.k33:=k33; m.k34:=k43;
  m.k41:=k14; m.k42:=k24; m.k43:=k34; m.k44:=k44;
  EqM(m);
end;
{******************************************************************************}
procedure TMatrix.EqM(m:TMatrix);
begin
  k11:=m.k11; k12:=m.k12; k13:=m.k13; k14:=m.k14;
  k21:=m.k21; k22:=m.k22; k23:=m.k23; k24:=m.k24;
  k31:=m.k31; k32:=m.k32; k33:=m.k33; k34:=m.k34;
  k41:=m.k41; k42:=m.k42; k43:=m.k43; k44:=m.k44;
end;
{******************************************************************************}
procedure TMatrix.MulC(c:Single);
begin
  k11:=k11*c;
  k12:=k12*c;
  k13:=k13*c;
  k14:=k14*c;
  k21:=k21*c;
  k22:=k22*c;
  k23:=k23*c;
  k24:=k24*c;
  k31:=k31*c;
  k32:=k32*c;
  k33:=k33*c;
  k34:=k34*c;
  k41:=k41*c;
  k42:=k42*c;
  k43:=k43*c;
  k44:=k44*c;
end;

{function TMatrix.GetM:_D3DMATRIX;
begin
  result._11:=k11;
  result._12:=k12;
  result._13:=k13;
  result._14:=k14;
  result._21:=k21;
  result._22:=k22;
  result._23:=k23;
  result._24:=k24;
  result._31:=k31;
  result._32:=k32;
  result._33:=k33;
  result._34:=k34;
  result._41:=k41;
  result._42:=k42;
  result._43:=k43;
  result._44:=k44;
end;}
{******************************************************************************}

{procedure TMatrix.EqM(m:_D3DMATRIX);
begin
  k11:=m._11; k12:=m._12; k13:=m._13; k14:=m._14;
  k21:=m._21; k22:=m._22; k23:=m._23; k24:=m._24;
  k31:=m._31; k32:=m._32; k33:=m._33; k34:=m._34;
  k41:=m._41; k42:=m._42; k43:=m._43; k44:=m._44;
end;}
{******************************************************************************}
procedure TMatrix.setTranslateLeft(_v:TVector);
begin
  k11:=1; k12:=0; k13:=0; k14:=_v.x;
  k21:=0; k22:=1; k23:=0; k24:=_v.y;
  k31:=0; k32:=0; k33:=1; k34:=_v.z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setTranslateRight(_v:TVector);
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=_v.x; k42:=_v.y; k43:=_v.z; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setScale(x,y,z:Single);
begin
  k11:=x; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=y; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=z; k34:=0;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setXRotation(a:Single);
begin
  k11:=1; k12:=0;      k13:=0;       k14:=0;
  k21:=0; k22:=cos(a); k23:=-sin(a); k24:=0;
  k31:=0; k32:=sin(a); k33:=cos(a);  k34:=0;
  k41:=0; k42:=0;      k43:=0;       k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setYRotation(a:Single);
begin
  k11:=cos(a);  k12:=0; k13:=sin(a); k14:=0;
  k21:=0;       k22:=1; k23:=0;      k24:=0;
  k31:=-sin(a); k32:=0; k33:=cos(a); k34:=0;
  k41:=0;       k42:=0; k43:=0;      k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setZRotation(a:Single);
begin
  k11:=cos(a);  k12:=-sin(a); k13:=0; k14:=0;
  k21:=sin(a);  k22:=cos(a);  k23:=0; k24:=0;
  k31:=0;       k32:=0;       k33:=1; k34:=0;
  k41:=0;       k42:=0;       k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.setOrbitraryRotation(x,y,z,a:Single);
  var
    c,s,t:Single;
begin
  c:=cos(a); s:=sin(a); t:=1-c;
  k11:=t*x*x+c;   k12:=t*x*y+s*z; k13:=t*x*z-s*y; k14:=0;
  k21:=t*x*y-s*z; k22:=t*y*y+c;   k23:=t*y*z+s*x; k24:=0;
  k31:=t*x*z+s*y; k32:=t*y*z-s*x; k33:=t*z*z+c;   k34:=0;
  k41:=0;         k42:=0;         k43:=0;         k44:=1;
end;

function TMatrix.multLeft(m:TMatrix):TMatrix;
begin
  result.k11:=m.k11*k11 + m.k12*k21 + m.k13*k31 + m.k14*k41;
  result.k12:=m.k11*k12 + m.k12*k22 + m.k13*k32 + m.k14*k42;
  result.k13:=m.k11*k13 + m.k12*k23 + m.k13*k33 + m.k14*k43;
  result.k14:=m.k11*k14 + m.k12*k24 + m.k13*k34 + m.k14*k44;
  
  result.k21:=m.k21*k11 + m.k22*k21 + m.k23*k31 + m.k24*k41;
  result.k22:=m.k21*k12 + m.k22*k22 + m.k23*k32 + m.k24*k42;
  result.k23:=m.k21*k13 + m.k22*k23 + m.k23*k33 + m.k24*k43;
  result.k24:=m.k21*k14 + m.k22*k24 + m.k23*k34 + m.k24*k44;
  
  result.k31:=m.k31*k11 + m.k32*k21 + m.k33*k31 + m.k34*k41;
  result.k32:=m.k31*k12 + m.k32*k22 + m.k33*k32 + m.k34*k42;
  result.k33:=m.k31*k13 + m.k32*k23 + m.k33*k33 + m.k34*k43;
  result.k34:=m.k31*k14 + m.k32*k24 + m.k33*k34 + m.k34*k44;
  
  result.k41:=m.k41*k11 + m.k42*k21 + m.k43*k31 + m.k44*k41;
  result.k42:=m.k41*k12 + m.k42*k22 + m.k43*k32 + m.k44*k42;
  result.k43:=m.k41*k13 + m.k42*k23 + m.k43*k33 + m.k44*k43;
  result.k44:=m.k41*k14 + m.k42*k24 + m.k43*k34 + m.k44*k44;
end;
{******************************************************************************}
function TMatrix.multRight(m:TMatrix):TMatrix;
begin
  result.k11:=k11*m.k11 + k12*m.k21 + k13*m.k31 + k14*m.k41;
  result.k12:=k11*m.k12 + k12*m.k22 + k13*m.k32 + k14*m.k42;
  result.k13:=k11*m.k13 + k12*m.k23 + k13*m.k33 + k14*m.k43;
  result.k14:=k11*m.k14 + k12*m.k24 + k13*m.k34 + k14*m.k44;
  result.k21:=k21*m.k11 + k22*m.k21 + k23*m.k31 + k24*m.k41;
  result.k22:=k21*m.k12 + k22*m.k22 + k23*m.k32 + k24*m.k42;
  result.k23:=k21*m.k13 + k22*m.k23 + k23*m.k33 + k24*m.k43;
  result.k24:=k21*m.k14 + k22*m.k24 + k23*m.k34 + k24*m.k44;
  result.k31:=k31*m.k11 + k32*m.k21 + k33*m.k31 + k34*m.k41;
  result.k32:=k31*m.k12 + k32*m.k22 + k33*m.k32 + k34*m.k42;
  result.k33:=k31*m.k13 + k32*m.k23 + k33*m.k33 + k34*m.k43;
  result.k34:=k31*m.k14 + k32*m.k24 + k33*m.k34 + k34*m.k44;
  result.k41:=k41*m.k11 + k42*m.k21 + k43*m.k31 + k44*m.k41;
  result.k42:=k41*m.k12 + k42*m.k22 + k43*m.k32 + k44*m.k42;
  result.k43:=k41*m.k13 + k42*m.k23 + k43*m.k33 + k44*m.k43;
  result.k44:=k41*m.k14 + k42*m.k24 + k43*m.k34 + k44*m.k44;
end;
{******************************************************************************}
function TMatrix.multMatrixVec(v:TVector):TVector;
  var
   r:TVector;
begin
  r.x:=k11*v.x+k12*v.y+k13*v.z+k14;
  r.y:=k21*v.x+k22*v.y+k23*v.z+k24;
  r.z:=k31*v.x+k32*v.y+k33*v.z+k34;
  result:=r;
end;
{******************************************************************************}
function TMatrix.multVecMatrix(v:TVector):TVector;
  var
   r:TVector;
begin
  r.x:=v.x*k11+v.y*k21+v.z*k31+k41;
  r.y:=v.x*k12+v.y*k22+v.z*k32+k42;
  r.z:=v.x*k13+v.y*k23+v.z*k33+k43;
  result:=r;
end;
{******************************************************************************}
procedure TMatrix.setTransform(tx,ty,tz,rx,ry,rz,ra,sx,sy,sz,sox,soy,soz,soa,cx,cy,cz:Single);
  var
    T,C,R,SR,S,o:TMatrix;
begin
  T.setTranslateLeft(tx,ty,tz);
  C.setIdentity; // cx,cy,cz
  R.setOrbitraryRotation(rx,ry,rz,-ra);
  SR.setIdentity; // sox,soy,soz,soa
  S.setScale(sx,sy,sz);
  o:=T.multRight(R.multRight(S));
  k11:=o.k11; k12:=o.k12; k13:=o.k13; k14:=o.k14;
  k21:=o.k21; k22:=o.k22; k23:=o.k23; k24:=o.k24;
  k31:=o.k31; k32:=o.k32; k33:=o.k33; k34:=o.k34;
  k41:=o.k41; k42:=o.k42; k43:=o.k43; k44:=o.k44;
end;
{******************************************************************************}
function TMatrix.GetPosition:TVector;
begin
  result.Eqv(k41,k42,k43);
end;
{******************************************************************************}
procedure TMatrix.NullPosition;
begin
  k41:=0;
  k42:=0;
  k43:=0;
  k44:=1;
end;
{******************************************************************************}
procedure TMatrix.Roll(angle:Single);
  var
    m:TMatrix;
begin
  m.setOrbitraryRotation(k31,k32,k33,angle);
  multLeft(m);
end;
{******************************************************************************}
procedure TMatrix.Normalize;
  var
    v1,v2,v3:TVector;
begin
  k14:=0; v1.Eqv(k11,k12,k13); v1.Normalize; k11:=v1.x; k12:=v1.y; k13:=v1.z;
  k24:=0; v2.Eqv(k21,k22,k23); v2.Normalize; k21:=v2.x; k22:=v2.y; k23:=v2.z;
  v3:=CrossVector(v1,v2);
  k31:=v3.x; k32:=v3.y; k33:=v3.z; k34:=0;
  v1:=CrossVector(v2,v3);
  k11:=v1.x; k12:=v1.y; k13:=v1.z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TMatrix.SetPosition(p:TVector);
begin
  k41:=p.x; k42:=p.y; k43:=p.z; k44:=1;
end;
{******************************************************************************}
//http://www.reactos.org/generated/doxygen/de/de5/project_8c-source.html#l00115
procedure TMatrix.Perspective(fovy, aspect, zNear, zFar: Double);
  var
    sine,cotangent,deltaZ,radians:Double;
begin
  SetIdentity;
  radians:=PI*(fovy/2)/180;
  deltaZ:=zFar-zNear;
  sine:=sin(radians);
  if (sine=0) or (deltaZ=0) or (aspect=0) then exit;
  cotangent:=cos(radians)/sine;
  k11:=cotangent/aspect;
  k22:=cotangent;
  k33:=-(zFar+zNear)/deltaZ;
  k34:=-1;
  k43:=-2*zNear*zFar/deltaZ;
  k44:=0;
end;
{******************************************************************************}
procedure TMatrix.SetProjectionMatrix(fFOV, fAspect, fNearPlane, fFarPlane: Single);
var
  Q,fCos,fSin:Single;
begin
  SetIdentity;
  fFOV:=fFOV*PI/180;
  if (abs(fFarPlane-fNearPlane) < 0.01 ) then exit;
  fCos := cos(fFOV/2);
  fSin := sin(fFOV/2);
  Q := (fFarPlane * fSin) / (fFarPlane - fNearPlane);
  k11 := fCos / fAspect;
  k22 := fCos;
  k33 := -Q;
  k34 := -fSin;
  k43 := Q*fNearPlane;
end;
{******************************************************************************}

procedure TMatrix.LookAt(eye, center, worldup: TVector);
  var
    forw,side,up:TVector;
    m:TMatrix;
begin
  forw:=(center.SubV(eye)).GetNormalized;
  side:=(CrossVector(forw,worldup)).GetNormalized.MulCV(-1);
  up:=worldup;//CrossVector(side,forw);
  m.SetIdentity;
  m.k11:=side.x; m.k12:=up.x; m.k13:=-forw.x;
  m.k21:=side.y; m.k22:=up.y; m.k23:=-forw.y;
  m.k31:=side.z; m.k32:=up.z; m.k33:=-forw.z;
  SetIdentity;
  SetTranslation(eye.MulCV(-1));
  self:=m.multLeft(self);
end;
{******************************************************************************}
procedure TMatrix.SetViewMatrix(From, At, Worldup: TVector);
var
  View: TVector;
  Length: Single;
  DotProduct: Single;
  Up: TVector;
  Right: TVector;
begin
  // Get the z basis vector, which points straight ahead. This is the
  // difference from the eyepoint to the lookat point.
  View := At.SubV(From);
  Length := View.GetLength;
  if (Length < 1e-6) then
  begin
    Exit;
  end;
  
  // Normalize the z basis vector
  View.DivC(Length);
  
  // Get the dot product, and calculate the projection of the z basis
  // vector onto the up vector. The projection is the y basis vector.
  
  DotProduct := DotVector(WorldUp,View);
  Up := WorldUp.SubV(View.MulCV(DotProduct));

  // If this vector has near-zero length because the input specified a
  // bogus up vector, let's try a default up vector
  Length := Up.GetLength;
  if (Length < 1e-6) then
  begin
    Up := gV(0,1,0).SubV(View.MulcV(View.y));
    // If we still have near-zero length, resort to a different axis.
    Length := Up.GetLength;
    if (Length < 1e-6) then
    begin
      Up := gV(0,0,1).SubV(View.MulCV(View.z));
      Length := Up.GetLength;
      if (Length < 1e-6) then
      begin
        Exit;
      end;
    end;
  end;
  // Normalize the y basis vector
  Up.DivC(Length);
  // The x basis vector is found simply with the cross product of the y
  // and z basis vectors
  Right := CrossVector(View,Up);
  // Start building the matrix. The first three rows contains the basis
  // vectors used to rotate the view to point at the lookat point

  k11 := Right.x;  k12 := Up.x;  k13 := View.x;  k14 := 0;
  k21 := Right.y;  k22 := Up.y;  k23 := View.y;  k24 := 0;
  k31 := Right.z;  k32 := Up.z;  k33 := View.z;  k34 := 0;
  // Do the translation values (rotations are still about the eyepoint)
  k41 := DotVector(From,Right);
  k42 := DotVector(From,Up);
  k43 := DotVector(From,View);
  k44 := 1;
end;
{******************************************************************************}
procedure TMatrix.SetTranslationMatrix(trans:TVector);
begin
  SetIdentity;
  SetTranslation(trans);
end;
{******************************************************************************}
procedure TMatrix.ClearTranslations;
begin
  k14:=0;
  k24:=0;
  k34:=0;
  k41:=0;
  k42:=0;
  k43:=0;
  k44:=1;
end;
{******************************************************************************}
function TMatrix.GetDeterminant3x3:Single;
begin
  result:=k11*(k22*k33-k32*k23);
  result:=result-k12*(k21*k33-k31*k23);
  result:=result+k13*(k21*k32-k31*k22);
end;
{******************************************************************************}
function TMatrix.GetDeterminant4x4:Double;
begin
   result:=
   k14 * k23 * k32 * k41-k13 * k24 * k32 * k41-k14 * k22 * k33 * k41+k12 * k24 * k33 * k41+
   k13 * k22 * k34 * k41-k12 * k23 * k34 * k41-k14 * k23 * k31 * k42+k13 * k24 * k31 * k42+
   k14 * k21 * k33 * k42-k11 * k24 * k33 * k42-k13 * k21 * k34 * k42+k11 * k23 * k34 * k42+
   k14 * k22 * k31 * k43-k12 * k24 * k31 * k43-k14 * k21 * k32 * k43+k11 * k24 * k32 * k43+
   k12 * k21 * k34 * k43-k11 * k22 * k34 * k43-k13 * k22 * k31 * k44+k12 * k23 * k31 * k44+
   k13 * k21 * k32 * k44-k11 * k23 * k32 * k44-k12 * k21 * k33 * k44+k11 * k22 * k33 * k44;
end;
{******************************************************************************}
procedure TMatrix.Inverse3x3;
  var
    m:TMatrix;
    d:Single;
begin
  d:=GetDeterminant3x3;
  if d=0 then exit;
  d:=1/d;
  m.EqM(self);
  m.k11:=(k22*k33-k32*k23)*d;
  m.k12:=(k32*k13-k12*k33)*d;
  m.k13:=(k12*k23-k22*k13)*d;
  m.k21:=(k31*k23-k21*k33)*d;
  m.k22:=(k11*k33-k13*k31)*d;
  m.k23:=(k21*k13-k11*k23)*d;
  m.k31:=(k21*k32-k31*k22)*d;
  m.k32:=(k31*k12-k11*k32)*d;
  m.k33:=(k11*k22-k12*k21)*d;
  EqM(m);
end;
{******************************************************************************}
//http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm
procedure TMatrix.Inverse4x4;
  var
    m:TMatrix;
    d:Double;
begin
  k11 := k23*k34*k42 - k24*k33*k42 + k24*k32*k43 - k22*k34*k43 - k23*k32*k44 + k22*k33*k44;
  k12 := k14*k33*k42 - k13*k34*k42 - k14*k32*k43 + k12*k34*k43 + k13*k32*k44 - k12*k33*k44;
  k13 := k13*k24*k42 - k14*k23*k42 + k14*k22*k43 - k12*k24*k43 - k13*k22*k44 + k12*k23*k44;
  k14 := k14*k23*k32 - k13*k24*k32 - k14*k22*k33 + k12*k24*k33 + k13*k22*k34 - k12*k23*k34;
  k21 := k24*k33*k41 - k23*k34*k41 - k24*k31*k43 + k21*k34*k43 + k23*k31*k44 - k21*k33*k44;
  k22 := k13*k34*k41 - k14*k33*k41 + k14*k31*k43 - k11*k34*k43 - k13*k31*k44 + k11*k33*k44;
  k23 := k14*k23*k41 - k13*k24*k41 - k14*k21*k43 + k11*k24*k43 + k13*k21*k44 - k11*k23*k44;
  k24 := k13*k24*k31 - k14*k23*k31 + k14*k21*k33 - k11*k24*k33 - k13*k21*k34 + k11*k23*k34;
  k31 := k22*k34*k41 - k24*k32*k41 + k24*k31*k42 - k21*k34*k42 - k22*k31*k44 + k21*k32*k44;
  k32 := k14*k32*k41 - k12*k34*k41 - k14*k31*k42 + k11*k34*k42 + k12*k31*k44 - k11*k32*k44;
  k33 := k12*k24*k41 - k14*k22*k41 + k14*k21*k42 - k11*k24*k42 - k12*k21*k44 + k11*k22*k44;
  k34 := k14*k22*k31 - k12*k24*k31 - k14*k21*k32 + k11*k24*k32 + k12*k21*k34 - k11*k22*k34;
  k41 := k23*k32*k41 - k22*k33*k41 - k23*k31*k42 + k21*k33*k42 + k22*k31*k43 - k21*k32*k43;
  k42 := k12*k33*k41 - k13*k32*k41 + k13*k31*k42 - k11*k33*k42 - k12*k31*k43 + k11*k32*k43;
  k43 := k13*k22*k41 - k12*k23*k41 - k13*k21*k42 + k11*k23*k42 + k12*k21*k43 - k11*k22*k43;
  k44 := k12*k23*k31 - k13*k22*k31 + k13*k21*k32 - k11*k23*k32 - k12*k21*k33 + k11*k22*k33;
  MulC(1/GetDeterminant4x4);
end;
{******************************************************************************}


{******************************************************************************}
// TDoubleMatrix
procedure TDoubleMatrix.SetRotationFromAngles(angles:TVector);
  var
    cr,sr,cp,sp,cy,sy,
    srsp,crsp : Double;
begin
  cr:= cos(angles.x);
  sr:= sin(angles.x);
  cp:= cos(angles.y);
  sp:= sin(angles.y);
  cy:= cos(angles.z);
  sy:= sin(angles.z);

  k11:= cp*cy;
  k12:= cp*sy;
  k13:= -sp;

  srsp:= sr*sp;
  crsp:= cr*sp;

  k21:= srsp*cy-cr*sy;
  k22:= srsp*sy+cr*cy;
  k23:= sr*cp;

  k31:= crsp*cy+sr*sy;
  k32:= crsp*sy-sr*cy;
  k33:= cr*cp;
end;
{******************************************************************************}
procedure TDoubleMatrix.SetTranslation(trans:TDoubleVector);
begin
  k41:=trans.x;
  k42:=trans.y;
  k43:=trans.z;
end;
{******************************************************************************}
procedure TDoubleMatrix.InverseRotateVect(var pVect:TDoubleVector);
var
  temp : TDoubleVector;
begin
  temp:=pVect;

  temp.x:= pVect.x*k11+pVect.y*k12+pVect.z*k13;
  temp.y:= pVect.x*k21+pVect.y*k22+pVect.z*k23;
  temp.z:= pVect.x*k31+pVect.y*k32+pVect.z*k33;

{  temp.x:= pVect.x*k11+pVect.y*k21+pVect.z*k31;
  temp.y:= pVect.x*k12+pVect.y*k22+pVect.z*k32;
  temp.z:= pVect.x*k13+pVect.y*k23+pVect.z*k33;}

  pVect:=temp;
end;

{******************************************************************************}
procedure TDoubleMatrix.InverseTranslateVect(var pVect:TDoubleVector);
var
  temp : TDoubleVector;
begin
     temp:=pVect;

     temp.x:= pVect.x-k41;
     temp.y:= pVect.y-k42;
     temp.z:= pVect.z-k43;

     pVect:=temp;
end;

{procedure TDoubleMatrix.SetRotationQuaternion(quat:TQuaternion);
begin
     m_matrix[0]:= ( 1.0 - 2.0*quat.m_quat[1]*quat.m_quat[1] - 2.0*quat.m_quat[2]*quat.m_quat[2]);
     m_matrix[1]:= ( 2.0*quat.m_quat[0]*quat.m_quat[1] + 2.0*quat.m_quat[3]*quat.m_quat[2] );
     m_matrix[2]:= ( 2.0*quat.m_quat[0]*quat.m_quat[2] - 2.0*quat.m_quat[3]*quat.m_quat[1] );

     m_matrix[4]:= ( 2.0*quat.m_quat[0]*quat.m_quat[1] - 2.0*quat.m_quat[3]*quat.m_quat[2] );
     m_matrix[5]:= ( 1.0 - 2.0*quat.m_quat[0]*quat.m_quat[0] - 2.0*quat.m_quat[2]*quat.m_quat[2] );
     m_matrix[6]:= ( 2.0*quat.m_quat[1]*quat.m_quat[2] + 2.0*quat.m_quat[3]*quat.m_quat[0] );

     m_matrix[8]:= ( 2.0*quat.m_quat[0]*quat.m_quat[2] + 2.0*quat.m_quat[3]*quat.m_quat[1] );
     m_matrix[9]:= ( 2.0*quat.m_quat[1]*quat.m_quat[2] - 2.0*quat.m_quat[3]*quat.m_quat[0] );
     m_matrix[10]:= ( 1.0 - 2.0*quat.m_quat[0]*quat.m_quat[0] - 2.0*quat.m_quat[1]*quat.m_quat[1] );
end;
}
{******************************************************************************}
constructor TDoubleMatrix.setIdentity;
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setTranslateLeft(x,y,z:Double);
begin
  k11:=1; k12:=0; k13:=0; k14:=x;
  k21:=0; k22:=1; k23:=0; k24:=y;
  k31:=0; k32:=0; k33:=1; k34:=z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setTranslateRight(x,y,z:Double);
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=x; k42:=y; k43:=z; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.Transpose;
  var
    m:TDoubleMatrix;
begin
  m.k11:=k11; m.k12:=k21; m.k13:=k31; m.k14:=k41;
  m.k21:=k12; m.k22:=k22; m.k23:=k32; m.k24:=k42;
  m.k31:=k13; m.k32:=k23; m.k33:=k33; m.k34:=k43;
  m.k41:=k14; m.k42:=k24; m.k43:=k34; m.k44:=k44;
  EqM(m);
end;
{******************************************************************************}
procedure TDoubleMatrix.EqM(m:TDoubleMatrix);
begin
  k11:=m.k11; k12:=m.k12; k13:=m.k13; k14:=m.k14;
  k21:=m.k21; k22:=m.k22; k23:=m.k23; k24:=m.k24;
  k31:=m.k31; k32:=m.k32; k33:=m.k33; k34:=m.k34;
  k41:=m.k41; k42:=m.k42; k43:=m.k43; k44:=m.k44;
end;
{******************************************************************************}
procedure TDoubleMatrix.MulC(c:Double);
begin
  k11:=k11*c;
  k12:=k12*c;
  k13:=k13*c;
  k14:=k14*c;
  k21:=k21*c;
  k22:=k22*c;
  k23:=k23*c;
  k24:=k24*c;
  k31:=k31*c;
  k32:=k32*c;
  k33:=k33*c;
  k34:=k34*c;
  k41:=k41*c;
  k42:=k42*c;
  k43:=k43*c;
  k44:=k44*c;
end;

{function TDoubleMatrix.GetM:_D3DMATRIX;
begin
  result._11:=k11;
  result._12:=k12;
  result._13:=k13;
  result._14:=k14;
  result._21:=k21;
  result._22:=k22;
  result._23:=k23;
  result._24:=k24;
  result._31:=k31;
  result._32:=k32;
  result._33:=k33;
  result._34:=k34;
  result._41:=k41;
  result._42:=k42;
  result._43:=k43;
  result._44:=k44;
end;}
{******************************************************************************}

{procedure TDoubleMatrix.EqM(m:_D3DMATRIX);
begin
  k11:=m._11; k12:=m._12; k13:=m._13; k14:=m._14;
  k21:=m._21; k22:=m._22; k23:=m._23; k24:=m._24;
  k31:=m._31; k32:=m._32; k33:=m._33; k34:=m._34;
  k41:=m._41; k42:=m._42; k43:=m._43; k44:=m._44;
end;}
{******************************************************************************}
procedure TDoubleMatrix.setTranslateLeft(_v:TDoubleVector);
begin
  k11:=1; k12:=0; k13:=0; k14:=_v.x;
  k21:=0; k22:=1; k23:=0; k24:=_v.y;
  k31:=0; k32:=0; k33:=1; k34:=_v.z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setTranslateRight(_v:TDoubleVector);
begin
  k11:=1; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=1; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=1; k34:=0;
  k41:=_v.x; k42:=_v.y; k43:=_v.z; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setScale(x,y,z:Double);
begin
  k11:=x; k12:=0; k13:=0; k14:=0;
  k21:=0; k22:=y; k23:=0; k24:=0;
  k31:=0; k32:=0; k33:=z; k34:=0;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setXRotation(a:Double);
begin
  k11:=1; k12:=0;      k13:=0;       k14:=0;
  k21:=0; k22:=cos(a); k23:=-sin(a); k24:=0;
  k31:=0; k32:=sin(a); k33:=cos(a);  k34:=0;
  k41:=0; k42:=0;      k43:=0;       k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setYRotation(a:Double);
begin
  k11:=cos(a);  k12:=0; k13:=sin(a); k14:=0;
  k21:=0;       k22:=1; k23:=0;      k24:=0;
  k31:=-sin(a); k32:=0; k33:=cos(a); k34:=0;
  k41:=0;       k42:=0; k43:=0;      k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setZRotation(a:Double);
begin
  k11:=cos(a);  k12:=-sin(a); k13:=0; k14:=0;
  k21:=sin(a);  k22:=cos(a);  k23:=0; k24:=0;
  k31:=0;       k32:=0;       k33:=1; k34:=0;
  k41:=0;       k42:=0;       k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.setOrbitraryRotation(x,y,z,a:Double);
  var
    c,s,t:Double;
begin
  c:=cos(a); s:=sin(a); t:=1-c;
  k11:=t*x*x+c;   k12:=t*x*y+s*z; k13:=t*x*z-s*y; k14:=0;
  k21:=t*x*y-s*z; k22:=t*y*y+c;   k23:=t*y*z+s*x; k24:=0;
  k31:=t*x*z+s*y; k32:=t*y*z-s*x; k33:=t*z*z+c;   k34:=0;
  k41:=0;         k42:=0;         k43:=0;         k44:=1;
end;

function TDoubleMatrix.multLeft(m:TDoubleMatrix):TDoubleMatrix;
begin
  result.k11:=m.k11*k11 + m.k12*k21 + m.k13*k31 + m.k14*k41;
  result.k12:=m.k11*k12 + m.k12*k22 + m.k13*k32 + m.k14*k42;
  result.k13:=m.k11*k13 + m.k12*k23 + m.k13*k33 + m.k14*k43;
  result.k14:=m.k11*k14 + m.k12*k24 + m.k13*k34 + m.k14*k44;

  result.k21:=m.k21*k11 + m.k22*k21 + m.k23*k31 + m.k24*k41;
  result.k22:=m.k21*k12 + m.k22*k22 + m.k23*k32 + m.k24*k42;
  result.k23:=m.k21*k13 + m.k22*k23 + m.k23*k33 + m.k24*k43;
  result.k24:=m.k21*k14 + m.k22*k24 + m.k23*k34 + m.k24*k44;

  result.k31:=m.k31*k11 + m.k32*k21 + m.k33*k31 + m.k34*k41;
  result.k32:=m.k31*k12 + m.k32*k22 + m.k33*k32 + m.k34*k42;
  result.k33:=m.k31*k13 + m.k32*k23 + m.k33*k33 + m.k34*k43;
  result.k34:=m.k31*k14 + m.k32*k24 + m.k33*k34 + m.k34*k44;

  result.k41:=m.k41*k11 + m.k42*k21 + m.k43*k31 + m.k44*k41;
  result.k42:=m.k41*k12 + m.k42*k22 + m.k43*k32 + m.k44*k42;
  result.k43:=m.k41*k13 + m.k42*k23 + m.k43*k33 + m.k44*k43;
  result.k44:=m.k41*k14 + m.k42*k24 + m.k43*k34 + m.k44*k44;
end;
{******************************************************************************}
function TDoubleMatrix.multRight(m:TDoubleMatrix):TDoubleMatrix;
begin
  result.k11:=k11*m.k11 + k12*m.k21 + k13*m.k31 + k14*m.k41;
  result.k12:=k11*m.k12 + k12*m.k22 + k13*m.k32 + k14*m.k42;
  result.k13:=k11*m.k13 + k12*m.k23 + k13*m.k33 + k14*m.k43;
  result.k14:=k11*m.k14 + k12*m.k24 + k13*m.k34 + k14*m.k44;
  result.k21:=k21*m.k11 + k22*m.k21 + k23*m.k31 + k24*m.k41;
  result.k22:=k21*m.k12 + k22*m.k22 + k23*m.k32 + k24*m.k42;
  result.k23:=k21*m.k13 + k22*m.k23 + k23*m.k33 + k24*m.k43;
  result.k24:=k21*m.k14 + k22*m.k24 + k23*m.k34 + k24*m.k44;
  result.k31:=k31*m.k11 + k32*m.k21 + k33*m.k31 + k34*m.k41;
  result.k32:=k31*m.k12 + k32*m.k22 + k33*m.k32 + k34*m.k42;
  result.k33:=k31*m.k13 + k32*m.k23 + k33*m.k33 + k34*m.k43;
  result.k34:=k31*m.k14 + k32*m.k24 + k33*m.k34 + k34*m.k44;
  result.k41:=k41*m.k11 + k42*m.k21 + k43*m.k31 + k44*m.k41;
  result.k42:=k41*m.k12 + k42*m.k22 + k43*m.k32 + k44*m.k42;
  result.k43:=k41*m.k13 + k42*m.k23 + k43*m.k33 + k44*m.k43;
  result.k44:=k41*m.k14 + k42*m.k24 + k43*m.k34 + k44*m.k44;
end;
{******************************************************************************}
function TDoubleMatrix.multMatrixVec(v:TDoubleVector):TDoubleVector;
  var
   r:TDoubleVector;
begin
  r.x:=k11*v.x+k12*v.y+k13*v.z+k14;
  r.y:=k21*v.x+k22*v.y+k23*v.z+k24;
  r.z:=k31*v.x+k32*v.y+k33*v.z+k34;
  result:=r;
end;
{******************************************************************************}
function TDoubleMatrix.multVecMatrix(v:TDoubleVector):TDoubleVector;
  var
   r:TDoubleVector;
begin
  r.x:=v.x*k11+v.y*k21+v.z*k31+k41;
  r.y:=v.x*k12+v.y*k22+v.z*k32+k42;
  r.z:=v.x*k13+v.y*k23+v.z*k33+k43;
  result:=r;
end;
{******************************************************************************}
procedure TDoubleMatrix.setTransform(tx,ty,tz,rx,ry,rz,ra,sx,sy,sz,sox,soy,soz,soa,cx,cy,cz:Double);
  var
    T,C,R,SR,S,o:TDoubleMatrix;
begin
  T.setTranslateLeft(tx,ty,tz);
  C.setIdentity; // cx,cy,cz
  R.setOrbitraryRotation(rx,ry,rz,-ra);
  SR.setIdentity; // sox,soy,soz,soa
  S.setScale(sx,sy,sz);
  o:=T.multRight(R.multRight(S));
  k11:=o.k11; k12:=o.k12; k13:=o.k13; k14:=o.k14;
  k21:=o.k21; k22:=o.k22; k23:=o.k23; k24:=o.k24;
  k31:=o.k31; k32:=o.k32; k33:=o.k33; k34:=o.k34;
  k41:=o.k41; k42:=o.k42; k43:=o.k43; k44:=o.k44;
end;
{******************************************************************************}
function TDoubleMatrix.GetPosition:TVector;
begin
  result.Eqv(k41,k42,k43);
end;
{******************************************************************************}
procedure TDoubleMatrix.NullPosition;
begin
  k41:=0;
  k42:=0;
  k43:=0;
  k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.Roll(angle:Double);
  var
    m:TDoubleMatrix;
begin
  m.setOrbitraryRotation(k31,k32,k33,angle);
  multLeft(m);
end;
{******************************************************************************}
procedure TDoubleMatrix.Normalize;
  var
    v1,v2,v3:TVector;
begin
  k14:=0; v1.Eqv(k11,k12,k13); v1.Normalize; k11:=v1.x; k12:=v1.y; k13:=v1.z;
  k24:=0; v2.Eqv(k21,k22,k23); v2.Normalize; k21:=v2.x; k22:=v2.y; k23:=v2.z;
  v3:=CrossVector(v1,v2);
  k31:=v3.x; k32:=v3.y; k33:=v3.z; k34:=0;
  v1:=CrossVector(v2,v3);
  k11:=v1.x; k12:=v1.y; k13:=v1.z;
  k41:=0; k42:=0; k43:=0; k44:=1;
end;
{******************************************************************************}
procedure TDoubleMatrix.SetPosition(p:TVector);
begin
  k41:=p.x; k42:=p.y; k43:=p.z; k44:=1;
end;
{******************************************************************************}
//http://www.reactos.org/generated/doxygen/de/de5/project_8c-source.html#l00115
procedure TDoubleMatrix.Perspective(fovy, aspect, zNear, zFar: Double);
  var
    sine,cotangent,deltaZ,radians:Double;
begin
  SetIdentity;
  radians:=PI*(fovy/2)/180;
  deltaZ:=zFar-zNear;
  sine:=sin(radians);
  if (sine=0) or (deltaZ=0) or (aspect=0) then exit;
  cotangent:=cos(radians)/sine;
  k11:=cotangent/aspect;
  k22:=cotangent;
  k33:=-(zFar+zNear)/deltaZ;
  k34:=-1;
  k43:=-2*zNear*zFar/deltaZ;
  k44:=0;
end;
{******************************************************************************}
procedure TDoubleMatrix.SetProjectionMatrix(fFOV, fAspect, fNearPlane, fFarPlane: Double);
var
  Q,fCos,fSin:Double;
begin
  SetIdentity;
  fFOV:=fFOV*PI/180;
  if (abs(fFarPlane-fNearPlane) < 0.01 ) then exit;
  fCos := cos(fFOV/2);
  fSin := sin(fFOV/2);
  Q := (fFarPlane * fSin) / (fFarPlane - fNearPlane);
  k11 := fCos / fAspect;
  k22 := fCos;
  k33 := -Q;
  k34 := -fSin;
  k43 := Q*fNearPlane;
end;
{******************************************************************************}

procedure TDoubleMatrix.LookAt(eye, center, worldup: TDoubleVector);
  var
    forw,side,up:TDoubleVector;
    m:TDoubleMatrix;
begin
  forw:=(center.SubV(eye)).GetNormalized;
  side:=(CrossVector(forw,worldup)).GetNormalized.MulCV(-1);
  up:=worldup;//CrossVector(side,forw);
  m.SetIdentity;
  m.k11:=side.x; m.k12:=up.x; m.k13:=-forw.x;
  m.k21:=side.y; m.k22:=up.y; m.k23:=-forw.y;
  m.k31:=side.z; m.k32:=up.z; m.k33:=-forw.z;
  SetIdentity;
  SetTranslation(eye.MulCV(-1));
  self:=m.multLeft(self);
end;
{******************************************************************************}
procedure TDoubleMatrix.SetViewMatrix(From, At, Worldup: TDoubleVector);
var
  View: TDoubleVector;
  Length: Double;
  DotProduct: Double;
  Up: TDoubleVector;
  Right: TDoubleVector;
begin
  // Get the z basis vector, which points straight ahead. This is the
  // difference from the eyepoint to the lookat point.
  View := At.SubV(From);
  Length := View.GetLength;
  if (Length < 1e-6) then
  begin
    Exit;
  end;

  // Normalize the z basis vector
  View.DivC(Length);

  // Get the dot product, and calculate the projection of the z basis
  // vector onto the up vector. The projection is the y basis vector.

  DotProduct := DotVector(WorldUp,View);
  Up := WorldUp.SubV(View.MulCV(DotProduct));

  // If this vector has near-zero length because the input specified a
  // bogus up vector, let's try a default up vector
  Length := Up.GetLength;
  if (Length < 1e-6) then
  begin
    Up := gDV(0,1,0).SubV(View.MulcV(View.y));
    // If we still have near-zero length, resort to a different axis.
    Length := Up.GetLength;
    if (Length < 1e-6) then
    begin
      Up := gDV(0,0,1).SubV(View.MulCV(View.z));
      Length := Up.GetLength;
      if (Length < 1e-6) then
      begin
        Exit;
      end;
    end;
  end;
  // Normalize the y basis vector
  Up.DivC(Length);
  // The x basis vector is found simply with the cross product of the y
  // and z basis vectors
  Right := CrossVector(View,Up);
  // Start building the matrix. The first three rows contains the basis
  // vectors used to rotate the view to point at the lookat point

  k11 := Right.x;  k12 := Up.x;  k13 := View.x;  k14 := 0;
  k21 := Right.y;  k22 := Up.y;  k23 := View.y;  k24 := 0;
  k31 := Right.z;  k32 := Up.z;  k33 := View.z;  k34 := 0;
  // Do the translation values (rotations are still about the eyepoint)
  k41 := DotVector(From,Right);
  k42 := DotVector(From,Up);
  k43 := DotVector(From,View);
  k44 := 1;
end;
{******************************************************************************}
procedure TDoubleMatrix.SetTranslationMatrix(trans:TDoubleVector);
begin
  SetIdentity;
  SetTranslation(trans);
end;
{******************************************************************************}
procedure TDoubleMatrix.ClearTranslations;
begin
  k14:=0;
  k24:=0;
  k34:=0;
  k41:=0;
  k42:=0;
  k43:=0;
  k44:=1;
end;
{******************************************************************************}
function TDoubleMatrix.GetDeterminant3x3:Double;
begin
  result:=k11*(k22*k33-k32*k23);
  result:=result-k12*(k21*k33-k31*k23);
  result:=result+k13*(k21*k32-k31*k22);
end;
{******************************************************************************}
function TDoubleMatrix.GetDeterminant4x4:Double;
begin
   result:=
   k14 * k23 * k32 * k41-k13 * k24 * k32 * k41-k14 * k22 * k33 * k41+k12 * k24 * k33 * k41+
   k13 * k22 * k34 * k41-k12 * k23 * k34 * k41-k14 * k23 * k31 * k42+k13 * k24 * k31 * k42+
   k14 * k21 * k33 * k42-k11 * k24 * k33 * k42-k13 * k21 * k34 * k42+k11 * k23 * k34 * k42+
   k14 * k22 * k31 * k43-k12 * k24 * k31 * k43-k14 * k21 * k32 * k43+k11 * k24 * k32 * k43+
   k12 * k21 * k34 * k43-k11 * k22 * k34 * k43-k13 * k22 * k31 * k44+k12 * k23 * k31 * k44+
   k13 * k21 * k32 * k44-k11 * k23 * k32 * k44-k12 * k21 * k33 * k44+k11 * k22 * k33 * k44;
end;
{******************************************************************************}
procedure TDoubleMatrix.Inverse3x3;
  var
    m:TDoubleMatrix;
    d:Double;
begin
  d:=GetDeterminant3x3;
  if d=0 then exit;
  d:=1/d;
  m.EqM(self);
  m.k11:=(k22*k33-k32*k23)*d;
  m.k12:=(k32*k13-k12*k33)*d;
  m.k13:=(k12*k23-k22*k13)*d;
  m.k21:=(k31*k23-k21*k33)*d;
  m.k22:=(k11*k33-k13*k31)*d;
  m.k23:=(k21*k13-k11*k23)*d;
  m.k31:=(k21*k32-k31*k22)*d;
  m.k32:=(k31*k12-k11*k32)*d;
  m.k33:=(k11*k22-k12*k21)*d;
  EqM(m);
end;
{******************************************************************************}
//http://www.euclideanspace.com/maths/algebra/matrix/functions/inverse/fourD/index.htm
procedure TDoubleMatrix.Inverse4x4;
  var
    m:TDoubleMatrix;
    d:Double;
begin
  k11 := k23*k34*k42 - k24*k33*k42 + k24*k32*k43 - k22*k34*k43 - k23*k32*k44 + k22*k33*k44;
  k12 := k14*k33*k42 - k13*k34*k42 - k14*k32*k43 + k12*k34*k43 + k13*k32*k44 - k12*k33*k44;
  k13 := k13*k24*k42 - k14*k23*k42 + k14*k22*k43 - k12*k24*k43 - k13*k22*k44 + k12*k23*k44;
  k14 := k14*k23*k32 - k13*k24*k32 - k14*k22*k33 + k12*k24*k33 + k13*k22*k34 - k12*k23*k34;
  k21 := k24*k33*k41 - k23*k34*k41 - k24*k31*k43 + k21*k34*k43 + k23*k31*k44 - k21*k33*k44;
  k22 := k13*k34*k41 - k14*k33*k41 + k14*k31*k43 - k11*k34*k43 - k13*k31*k44 + k11*k33*k44;
  k23 := k14*k23*k41 - k13*k24*k41 - k14*k21*k43 + k11*k24*k43 + k13*k21*k44 - k11*k23*k44;
  k24 := k13*k24*k31 - k14*k23*k31 + k14*k21*k33 - k11*k24*k33 - k13*k21*k34 + k11*k23*k34;
  k31 := k22*k34*k41 - k24*k32*k41 + k24*k31*k42 - k21*k34*k42 - k22*k31*k44 + k21*k32*k44;
  k32 := k14*k32*k41 - k12*k34*k41 - k14*k31*k42 + k11*k34*k42 + k12*k31*k44 - k11*k32*k44;
  k33 := k12*k24*k41 - k14*k22*k41 + k14*k21*k42 - k11*k24*k42 - k12*k21*k44 + k11*k22*k44;
  k34 := k14*k22*k31 - k12*k24*k31 - k14*k21*k32 + k11*k24*k32 + k12*k21*k34 - k11*k22*k34;
  k41 := k23*k32*k41 - k22*k33*k41 - k23*k31*k42 + k21*k33*k42 + k22*k31*k43 - k21*k32*k43;
  k42 := k12*k33*k41 - k13*k32*k41 + k13*k31*k42 - k11*k33*k42 - k12*k31*k43 + k11*k32*k43;
  k43 := k13*k22*k41 - k12*k23*k41 - k13*k21*k42 + k11*k23*k42 + k12*k21*k43 - k11*k22*k43;
  k44 := k12*k23*k31 - k13*k22*k31 + k13*k21*k32 - k11*k23*k32 - k12*k21*k33 + k11*k22*k33;
  MulC(1/GetDeterminant4x4);
end;
{******************************************************************************}
procedure TDoubleMatrix.sM(m:TMatrix);
begin
  k11:=m.k11; k12:=m.k12; k13:=m.k13; k14:=m.k14;
  k21:=m.k21; k22:=m.k22; k23:=m.k23; k24:=m.k24;
  k31:=m.k31; k32:=m.k32; k33:=m.k33; k34:=m.k34;
  k41:=m.k41; k42:=m.k42; k43:=m.k43; k44:=m.k44;
end;
{******************************************************************************}
function TDoubleMatrix.gM:TMatrix;
begin
  result.k11:=k11; result.k12:=k12; result.k13:=k13; result.k14:=k14;
  result.k21:=k21; result.k22:=k22; result.k23:=k23; result.k24:=k24;
  result.k31:=k31; result.k32:=k32; result.k33:=k33; result.k34:=k34;
  result.k41:=k41; result.k42:=k42; result.k43:=k43; result.k44:=k44;
end;
{******************************************************************************}


{******************************************************************************}
// TQuaternion
constructor TQuaternion.Init;
begin
  x:=0;
  y:=0;
  z:=0;
  w:=1;
end;
{******************************************************************************}
destructor TQuaternion.Done;
begin
  Init;
end;
{******************************************************************************}
function TQuaternion.GetLength:Single;
begin
  result:=sqrt(x*x+y*y+z*z+w*w);
end;
{******************************************************************************}
procedure TQuaternion.Add(_q:TQuaternion);
begin
  x:=x+_q.x;
  y:=y+_q.y;
  z:=z+_q.z;
  w:=w+_q.w;
end;
{******************************************************************************}
function TQuaternion.AddQ(_q:TQuaternion):TQuaternion;
begin
  result.x:=x+_q.x;
  result.y:=x+_q.y;
  result.z:=x+_q.z;
  result.w:=x+_q.w;
end;
{******************************************************************************}
procedure TQuaternion.Mul(q:TQuaternion);
  var
    A,B,C,D,E,F,G,H:Double;
begin
  A:=(w+x)*(q.w+q.x);
  B:=(z-y)*(q.y-q.z);
  C:=(x-w)*(q.y+q.z);
  D:=(y+z)*(q.x-q.w);
  E:=(x+z)*(q.x+q.y);
  F:=(x-z)*(q.x-q.y);
  G:=(w+y)*(q.w-q.z);
  H:=(w-y)*(q.w+q.z);
  w:= B+(-E-F+G+H)*0.5;
  x:= A-(E+F+G+H)*0.5;
  y:=-C+(E-F+G-H)*0.5;
  z:=-D+(E-F-G+H)*0.5;
end;
{******************************************************************************}
function TQuaternion.MulQ(q:TQuaternion):TQuaternion;
  var
    A,B,C,D,E,F,G,H:Double;
begin
  A:=(w+x)*(q.w+q.x);
  B:=(z-y)*(q.y-q.z);
  C:=(x-w)*(q.y+q.z);
  D:=(y+z)*(q.x-q.w);
  E:=(x+z)*(q.x+q.y);
  F:=(x-z)*(q.x-q.y);
  G:=(w+y)*(q.w-q.z);
  H:=(w-y)*(q.w+q.z);
  result.w:= B+(-E-F+G+H)*0.5;
  result.x:= A-(E+F+G+H)*0.5;
  result.y:=-C+(E-F+G-H)*0.5;
  result.z:=-D+(E-F-G+H)*0.5;
end;
{******************************************************************************}
procedure TQuaternion.Inverse;
begin
  w:=-w;
  x:=-x;
  y:=-y;
  z:=-z;
end;
{******************************************************************************}
function TQuaternion.GetInversed:TQuaternion;
begin
  result.w:=-w;
  result.x:=-x;
  result.y:=-y;
  result.z:=-z;
end;
{******************************************************************************}
procedure TQuaternion.Conjugate;
begin
  x:=-x;
  y:=-y;
  z:=-z;
end;
{******************************************************************************}
function TQuaternion.GetConjugated:TQuaternion;
begin
  result.w:=w;
  result.x:=-x;
  result.y:=-y;
  result.z:=-z;
end;
{******************************************************************************}
procedure TQuaternion.Normalize;
  var
    len:Double;
begin
  len:=GetLength;
  if len=0 then begin
    x:=0;
    y:=0;
    z:=0;
    w:=1;
  end else begin
    len:=1/len;
    x:=x*len;
    y:=y*len;
    z:=z*len;
    w:=w*len;
  end;
end;
{******************************************************************************}
function TQuaternion.GetNormalized:TQuaternion;
  var
    len:Double;
begin
  len:=GetLength;
  if len<>0 then begin
    result.x:=1;
    result.y:=0;
    result.z:=0;
    result.w:=0;
  end else begin
    len:=1/len;
    result.x:=x*len;
    result.y:=y*len;
    result.z:=z*len;
    result.w:=w*len;
  end;
end;
{******************************************************************************}
procedure TQuaternion.Eqv(_x,_y,_z,_w:Double);
begin
  x:=_x;
  y:=_y;
  z:=_z;
  w:=_w;
end;
{******************************************************************************}
procedure TQuaternion.SetFromEyler(const yaw,pitch,roll:Double);overload;
  var
    qy,qp,qr,q:TQuaternion;
begin
  qp.EqV(sin(pitch/2),0,0,cos(pitch/2));
  qy.EqV(0,sin(yaw/2),0,cos(yaw/2));
  qr.EqV(0,0,sin(roll/2),cos(roll/2));
  q:=(qy.MulQ(qp)).MulQ(qr);
  x:=q.x;
  y:=q.y;
  z:=q.z;
  w:=q.w;
end;
{******************************************************************************}
procedure TQuaternion.SetFromEyler(const angles:TVector);overload;
  var
    angle:Double;
    sr,sp,sy,cr,cp,cy:Double;
    crcp,srsp:Double;
begin
  angle:=angles.z*0.5;
  sy:=sin(angle);
  cy:=cos(angle);
  angle:= angles.y*0.5;
  sp:=sin(angle);
  cp:=cos(angle);
  angle:= angles.x*0.5;
  sr:=sin(angle);
  cr:=cos(angle);
  crcp:= cr*cp;
  srsp:= sr*sp;
  x:=sr*cp*cy-cr*sp*sy;
  y:=cr*sp*cy+sr*cp*sy;
  z:=crcp*sy-srsp*cy;
  w:=crcp*cy+srsp*sy;
end;
{******************************************************************************}
procedure TQuaternion.SetFromEyler(const angles:TDoubleVector);overload;
  var
    angle:Double;
    sr,sp,sy,cr,cp,cy:Double;
    crcp,srsp:Double;
begin
  angle:=angles.z*0.5;
  sy:=sin(angle);
  cy:=cos(angle);
  angle:= angles.y*0.5;
  sp:=sin(angle);
  cp:=cos(angle);
  angle:= angles.x*0.5;
  sr:=sin(angle);
  cr:=cos(angle);
  crcp:= cr*cp;
  srsp:= sr*sp;
  x:=sr*cp*cy-cr*sp*sy;
  y:=cr*sp*cy+sr*cp*sy;
  z:=crcp*sy-srsp*cy;
  w:=crcp*cy+srsp*sy;
end;
{******************************************************************************}
function TQuaternion.GetRotationMatrix:TMatrix;
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2,s:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  result.k11:=1.0 - (yy + zz);
  result.k12:=xy - wz;
  result.k13:=xz + wy;
  result.k14:=0;

  result.k21:=xy + wz;
  result.k22:=1.0 - (xx + zz);
  result.k23:=yz - wx;
  result.k24:=0;

  result.k31:=xz - wy;
  result.k32:=yz + wx;
  result.k33:=1.0 - (xx + yy);
  result.k34:=0;

  result.k41:=0;
  result.k42:=0;
  result.k43:=0;
  result.k44:=1;
end;
{******************************************************************************}
function TQuaternion.GetRotationMatrix:TDoubleMatrix;
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2,s:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  result.k11:=1.0 - (yy + zz);
  result.k12:=xy - wz;
  result.k13:=xz + wy;
  result.k14:=0;

  result.k21:=xy + wz;
  result.k22:=1.0 - (xx + zz);
  result.k23:=yz - wx;
  result.k24:=0;

  result.k31:=xz - wy;
  result.k32:=yz + wx;
  result.k33:=1.0 - (xx + yy);
  result.k34:=0;

  result.k41:=0;
  result.k42:=0;
  result.k43:=0;
  result.k44:=1;
end;

procedure  TQuaternion.SetRotation(var m:TMatrix);
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  m.k11:=1.0 - (yy + zz);
  m.k12:=xy - wz;
  m.k13:=xz + wy;

  m.k21:=xy + wz;
  m.k22:=1.0 - (xx + zz);
  m.k23:=yz - wx;

  m.k31:=xz - wy;
  m.k32:=yz + wx;
  m.k33:=1.0 - (xx + yy);

end;

procedure  TQuaternion.SetRotation(var m:TDoubleMatrix);
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  m.k11:=1.0 - (yy + zz);
  m.k12:=xy - wz;
  m.k13:=xz + wy;

  m.k21:=xy + wz;
  m.k22:=1.0 - (xx + zz);
  m.k23:=yz - wx;

  m.k31:=xz - wy;
  m.k32:=yz + wx;
  m.k33:=1.0 - (xx + yy);

end;

procedure  TQuaternion.SetRotationMatrix(var m:TMatrix);
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  m.k11:=1.0 - (yy + zz);
  m.k12:=xy - wz;
  m.k13:=xz + wy;
  m.k14:=0;

  m.k21:=xy + wz;
  m.k22:=1.0 - (xx + zz);
  m.k23:=yz - wx;
  m.k24:=0;

  m.k31:=xz - wy;
  m.k32:=yz + wx;
  m.k33:=1.0 - (xx + yy);
  m.k34:=0;


  m.k41:=0;
  m.k42:=0;
  m.k43:=0;
  m.k44:=1;

end;

procedure  TQuaternion.SetRotationMatrix(var m:TDoubleMatrix);
  var
    wx,wy,wz,xx,yy,yz,xy,xz,zz,x2,y2,z2:Double;
begin
  x2:=x+x;
  y2:=y+y;
  z2:=z+z;

  xx:=x*x2; xy:=x*y2; xz:=x*z2;
  yy:=y*y2; yz:=y*z2; zz:=z*z2;
  wx:=w*x2; wy:=w*y2; wz:=w*z2;

  m.k11:=1.0 - (yy + zz);
  m.k12:=xy - wz;
  m.k13:=xz + wy;
  m.k14:=0;

  m.k21:=xy + wz;
  m.k22:=1.0 - (xx + zz);
  m.k23:=yz - wx;
  m.k24:=0;

  m.k31:=xz - wy;
  m.k32:=yz + wx;
  m.k33:=1.0 - (xx + yy);
  m.k34:=0;


  m.k41:=0;
  m.k42:=0;
  m.k43:=0;
  m.k44:=1;

end;

procedure TQuaternion.SetFromMatrix(const m:TMatrix);
  var
    tr,s:Double;
begin
  tr:=m.k11+m.k22+m.k33;
  if tr>0 then begin
    s:=sqrt(tr+1);
    w:=s*0.5;
    s:=0.5/s;
    x:=(m.k32-m.k23)*s;
    y:=(m.k31-m.k31)*s;
    z:=(m.k21-m.k12)*s;
  end else begin



  end;
end;

{******************************************************************************}
procedure TQuaternion.SetFromAxisAngle(const Axis:TVector;const Angle:Double);overload;
  var
    sina,halfangle:Double;
begin
  Axis.Normalize;
  halfangle:=Angle*0.5;
  sina:=sin(halfangle);
  w:=cos(halfangle);
  x:=Axis.x*sina;
  y:=Axis.y*sina;
  z:=Axis.z*sina;
end;
{******************************************************************************}
procedure TQuaternion.SetToAxisAngle(var Axis:TVector;var Angle:Double);
  var
    vl,ivl:Double;
begin
  vl:=sqrt(x*x+y*y+z*z);
  if (vl>0.000001) then begin
    ivl:=1/vl;
    Axis.Eqv(x*ivl,y*ivl,z*ivl);
    if (w<0) then begin
      Angle:=2*arctan2(-vl,-w);
    end else begin
      Angle:=2*arctan2(vl,w);
    end;
  end else begin
    Axis.Eqv(0,0,0);
    Angle:=0;
  end
end;
{******************************************************************************}
procedure TQuaternion.SetFromAxisAngle(const Axis:TDoubleVector;const Angle:Double);overload;
  var
    sina,halfangle:Double;
begin
  Axis.Normalize;
  halfangle:=Angle*0.5;
  sina:=sin(halfangle);
  w:=cos(halfangle);
  x:=Axis.x*sina;
  y:=Axis.y*sina;
  z:=Axis.z*sina;
end;
{******************************************************************************}
function TQuaternion.GetAngle:Double;
  var
    vl,ivl:Double;
begin
  vl:=sqrt(x*x+y*y+z*z);
  if (vl>0.000001) then begin
    ivl:=1/vl;
//    Axis.Eqv(x*ivl,y*ivl,z*ivl);
    if (w<0) then begin
      result:=2*arctan2(-vl,-w);
    end else begin
      result:=2*arctan2(vl,w);
    end;
  end else begin
//    Axis.Eqv(0,0,0);
    result:=0;
  end
end;
{******************************************************************************}
procedure TQuaternion.SetToAxisAngle(var Axis:TDoubleVector;var Angle:Double);
  var
    vl,ivl:Double;
begin
  vl:=sqrt(x*x+y*y+z*z);
  if (vl>0.000001) then begin
    ivl:=1/vl;
    Axis.Eqv(x*ivl,y*ivl,z*ivl);
    if (w<0) then begin
      Angle:=2*arctan2(-vl,-w);
    end else begin
      Angle:=2*arctan2(vl,w);
    end;
  end else begin
    Axis.Eqv(0,0,0);
    Angle:=0;
  end
end;
{******************************************************************************}
procedure TQuaternion.SetFromSpherical(const latitude,longitude,angle:Double);
  var
    halfangle,sin_a:Double;
begin
  halfangle:=angle*0.5;
  sin_a:=sin(halfangle);
  x:=sin_a*cos(latitude)*sin(longitude);
  y:=sin_a*sin(latitude);
  z:=y*cos(longitude);
  w:=cos(halfangle);
end;
{******************************************************************************}
procedure TQuaternion.ShortestArc(FromV,ToV:TVector);
  var
    c:TVector;
begin
  c:=CrossVector(FromV,ToV);
  x:=c.x; y:=c.y; z:=c.z; w:=DotVector(FromV,ToV);
  Normalize;
  w:=w+1;
  if w<=0.00001 then begin
    if (FromV.z*FromV.z>FromV.x*FromV.x) then begin
      x:=0; y:=FromV.z; z:=-FromV.y;
    end else begin
      x:=FromV.y; y:=-FromV.x; z:=0;
    end;
  end;
  Normalize;
end;
{******************************************************************************}
procedure TQuaternion.ShortestArc(FromV,ToV:TDoubleVector);
  var
    c:TDoubleVector;
begin
  c:=CrossVector(FromV,ToV);
  x:=c.x; y:=c.y; z:=c.z; w:=DotVector(FromV,ToV);
  Normalize;
  w:=w+1;
  if w<=0.00001 then begin
    if (FromV.z*FromV.z>FromV.x*FromV.x) then begin
      x:=0; y:=FromV.z; z:=-FromV.y;
    end else begin
      x:=FromV.y; y:=-FromV.x; z:=0;
    end;
  end;
  Normalize;
end;
{******************************************************************************}
procedure TQuaternion.Interpolate(q1,q2:TQuaternion;d:Double);
  var
    cosom,sinom,omega,sclq1,sclq2:Double;
begin

  cosom:=q1.x*q2.x+q1.y*q2.y+q1.z*q2.z+q1.w*q2.w;
  if cosom <0 then
    q2:=q2.GetInversed;

  if (( 1.0-cosom ) > 0.00000001 ) then begin
    
{    omega:= myarccos(cosom);
    sinom:= 1/sin(omega);
    sclq1:= sin(( 1.0-d )*omega )*sinom;
    sclq2:= sin( d*omega )*sinom;}
                                                
      sclq1:= 1.0-d;
      sclq2:= d;

  end else begin
    
      sclq1:= 1.0-d;
      sclq2:= d;
    
  end;
    
  x:=sclq1*q1.x+sclq2*q2.x;
  y:=sclq1*q1.y+sclq2*q2.y;
  z:=sclq1*q1.z+sclq2*q2.z;
  w:=sclq1*q1.w+sclq2*q2.w;
  Normalize;

end;
{******************************************************************************}
procedure TPlane.Normalize;
  var
    r:Single;
begin
  r:=n.x*n.x+n.y*n.y+n.z*n.z;
  if r<=0 then exit;
  r:=sqrt(r);
  n.x:=n.x/r;
  n.y:=n.y/r;
  n.z:=n.z/r;
  d:=d/r;
end;
{******************************************************************************}
procedure TPlane.SetFrom(normal,point:TVector);
begin
  n:=normal;
  p:=point;
end;
{******************************************************************************}
function TPlane.IsIntersectionByLine(s:TLine;var _p:TVector):boolean;
  var
    u,w:TVector;
    _D,_N,sI:Single;
begin
  u:=S.B.SubV(S.A);
  w:=S.A.SubV(p);
  _D:= DotVector(n, u);
  _N:= -DotVector(n, w);
  if (abs(_D) < 0.0000000001) then begin // segment is parallel to plane
{        if (N == 0)                     // segment lies in plane
            return 2;
        else
            return 0;                   // no intersection
}
    result:=false;
    exit;
  end;
  // they are not parallel
  // compute intersect param
  sI:=_N/_D;
  if (sI < 0) or (sI > 1) then begin
    result:=false;
    exit;
  end;
  _p:=s.A.AddV(u.MulCV(sI));
  result:=true;
end;



{******************************************************************************}
// COMMON PROCEDURES
// Для простоты кода вводим процедуру, которая сразу вектор возвращает по трем координатам
function gV(const x,y,z:Single):TVector;
begin
  result.Eqv(x,y,z);
end;

// Для простоты кода вводим процедуру, которая сразу вектор возвращает по трем координатам
function gDV(const x,y,z:Single):TDoubleVector;
begin
  result.Eqv(x,y,z);
end;

// Возвращаем вектор
function gV(const v:TDoubleVector):TVector;
begin
  result.x:=v.x; result.y:=v.y; result.z:=v.z;
end;

// Возвращаем вектор двойной точности
function gDV(const v:TVector):TDoubleVector;
begin
  result.x:=v.x; result.y:=v.y; result.z:=v.z;
end;

// Длина вектора
function MagnitudeVector(const v:TVector):Single;inline; // длинна вектора
begin
  result:=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
end;

// Длина вектора двойной точности
function MagnitudeDoubleVector(const v:TDoubleVector):Double;inline; // длинна вектора
begin
  result:=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
end;

// Длина вектора
function QuadMagnitudeVector(const v:TVector):Single;inline; // длинна вектора
begin
  result:=v.x*v.x+v.y*v.y+v.z*v.z;
end;

// Длина вектора двойной точности
function QuadMagnitudeDoubleVector(const v:TDoubleVector):Double;inline; // длинна вектора
begin
  result:=v.x*v.x+v.y*v.y+v.z*v.z;
end;

// векторное произведение
function CrossVector(const v1,v2:TVector):TVector;
begin
  result.x:=v1.y * v2.z - v1.z * v2.y;
  result.y:=v1.z * v2.x - v1.x * v2.z;
  result.z:=v1.x * v2.y - v1.y * v2.x;
end;

// векторное произведение
function CrossVector(const v1,v2:TDoubleVector):TDoubleVector;
begin
  result.x:=v1.y * v2.z - v1.z * v2.y;
  result.y:=v1.z * v2.x - v1.x * v2.z;
  result.z:=v1.x * v2.y - v1.y * v2.x;
end;

// скалярное произведение векторов
function DotVector(const v1,v2:TVector):Single;
begin
  result:=v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

// скалярное произведение векторов
function DotVector(const v1,v2:TDoubleVector):Double;
begin
  result:=v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

// Нормаль к плоскости по трем точкам
function NormalVector(const v1,v2,v3:TVector):TVector;
begin
  result:=CrossVector(v1.SubV(v2),v2.SubV(v3)).GetNormalized;
end;

// Нормаль к плоскости по трем точкам
function NormalVector(const v1,v2,v3:TDoubleVector):TDoubleVector;
begin
  result:=CrossVector(v1.SubV(v2),v2.SubV(v3)).GetNormalized;
end;

function NormalVector(const x1,y1,z1,x2,y2,z2,x3,y3,z3:Single):TVector;
  var
    v1,v2:TVector;
begin
  v1.Eqv(x1-x2,y1-y2,z1-z2);
  v2.Eqv(x3-x2,y3-y2,z3-z2);
  result:=CrossVector(v1,v2).GetNormalized;
end;

function NormalVector(const x1,y1,z1,x2,y2,z2,x3,y3,z3:Double):TDoubleVector;
  var
    v1,v2:TDoubleVector;
begin
  v1.Eqv(x1-x2,y1-y2,z1-z2);
  v2.Eqv(x3-x2,y3-y2,z3-z2);
  result:=CrossVector(v1,v2).GetNormalized;
end;

// расстояние между точками
function Distance(const p1,p2:TVector):Single;
begin
  result:=sqrt((p2.x-p1.x)*(p2.x-p1.x)+(p2.y-p1.y)*(p2.y-p1.y)+(p2.z-p1.z)*(p2.z-p1.z));
end;

// расстояние между точками
function Distance(const p1,p2:TDoubleVector):Double;
begin
  result:=sqrt((p2.x-p1.x)*(p2.x-p1.x)+(p2.y-p1.y)*(p2.y-p1.y)+(p2.z-p1.z)*(p2.z-p1.z));
end;

// Проецирование вектора в пространстве в вектор на экране, с использованием матриц проекции и вида
function Project(const v:TVector;const matModel,matProj:TMatrix;const left,top,width,height:Integer):TVector;
  var
    w:Single;
    vr,vv:TVector;
begin
  w:=1;
  vr.x:=v.x*matModel.k11+v.y*matModel.k21+v.z*matModel.k31+w*matModel.k41;
  vr.y:=v.x*matModel.k12+v.y*matModel.k22+v.z*matModel.k32+w*matModel.k42;
  vr.z:=v.x*matModel.k13+v.y*matModel.k23+v.z*matModel.k33+w*matModel.k43;
  w:=   v.x*matModel.k14+v.y*matModel.k24+v.z*matModel.k34+w*matModel.k44;
  vv.x:=vr.x*matProj.k11+vr.y*matProj.k21+vr.z*matProj.k31+w*matProj.k41;
  vv.y:=vr.x*matProj.k12+vr.y*matProj.k22+vr.z*matProj.k32+w*matProj.k42;
  vv.z:=vr.x*matProj.k13+vr.y*matProj.k23+vr.z*matProj.k33+w*matProj.k43;
  w:=   vr.x*matProj.k14+vr.y*matProj.k24+vr.z*matProj.k34+w*matProj.k44;
  vv.x:=0.5*vv.x/w+0.5;
  vv.y:=0.5*vv.y/w+0.5;
  vv.z:=0.5*vv.z/w+0.5;
  vv.x:=vv.x*width+left;
  vv.y:=vv.y*height+top;
  result:=vv;
end;

// Проецирование вектора в пространстве в вектор на экране, с использованием матриц проекции и вида
function Project(const v:TDoubleVector;const matModel,matProj:TDoubleMatrix;const left,top,width,height:Integer):TDoubleVector;
  var
    w:Double;
    vr,vv:TDoubleVector;
begin
  w:=1;
  vr.x:=v.x*matModel.k11+v.y*matModel.k21+v.z*matModel.k31+w*matModel.k41;
  vr.y:=v.x*matModel.k12+v.y*matModel.k22+v.z*matModel.k32+w*matModel.k42;
  vr.z:=v.x*matModel.k13+v.y*matModel.k23+v.z*matModel.k33+w*matModel.k43;
  w:=   v.x*matModel.k14+v.y*matModel.k24+v.z*matModel.k34+w*matModel.k44;
  vv.x:=vr.x*matProj.k11+vr.y*matProj.k21+vr.z*matProj.k31+w*matProj.k41;
  vv.y:=vr.x*matProj.k12+vr.y*matProj.k22+vr.z*matProj.k32+w*matProj.k42;
  vv.z:=vr.x*matProj.k13+vr.y*matProj.k23+vr.z*matProj.k33+w*matProj.k43;
  w:=   vr.x*matProj.k14+vr.y*matProj.k24+vr.z*matProj.k34+w*matProj.k44;
  if abs(w)<0.0000001 then begin
    vv.x:=0.5;
    vv.y:=0.5;
    vv.z:=0.5;
  end else begin
    vv.x:=(vv.x+w)/(2*w);
    vv.y:=(vv.y+w)/(2*w);
    vv.z:=(vv.z+w)/(2*w);
  end;
  vv.x:=vv.x*width+left;
  vv.y:=vv.y*height+top;
  result:=vv;
end;

// Процедура обратная проецированию находит координаты вектора в пространстве по координатам с экрана
function Unproject(const inwin:TVector;const matModel,matProj:TMatrix;const left,top,width,height:Integer;const vnear,vfar:Single):TVector;
  var
    m:TMatrix;
    v,vr:TVector;
    w:Single;
begin
  m:=InvertMatrix(matProj.multLeft(matModel));

  v.x:=(inwin.x-Left)/Width;
  v.y:=(inwin.y-Top)/Height;
  v.z:=inwin.z;

  v.x:=v.x*2-1;
  v.y:=v.y*2-1;
  v.z:=v.z*2-1;
  w:=1;

  vr.x:=v.x*m.k11+v.y*m.k21+v.z*m.k31+w*m.k41;
  vr.y:=v.x*m.k12+v.y*m.k22+v.z*m.k32+w*m.k42;
  vr.z:=v.x*m.k13+v.y*m.k23+v.z*m.k33+w*m.k43;
  w:=   v.x*m.k14+v.y*m.k24+v.z*m.k34+w*m.k44;

  result.Eqv(vr.x/w,vr.y/w,vr.z/w);

end;

// Процедура обратная проецированию находит координаты вектора в пространстве по координатам с экрана
function Unproject(const inwin:TDoubleVector;const matModel,matProj:TDoubleMatrix;const left,top,width,height:Integer;const vnear,vfar:Double):TDoubleVector;
  var
    m:TDoubleMatrix;
    v,vr:TDoubleVector;
    w:Double;
begin
  m:=InvertMatrix(matProj.multLeft(matModel));

  v.x:=(inwin.x-Left)/Width;
  v.y:=(inwin.y-Top)/Height;
  v.z:=inwin.z;

  v.x:=v.x*2-1;
  v.y:=v.y*2-1;
  v.z:=v.z*2-1;
  w:=1;

  vr.x:=v.x*m.k11+v.y*m.k21+v.z*m.k31+w*m.k41;
  vr.y:=v.x*m.k12+v.y*m.k22+v.z*m.k32+w*m.k42;
  vr.z:=v.x*m.k13+v.y*m.k23+v.z*m.k33+w*m.k43;
  w:=   v.x*m.k14+v.y*m.k24+v.z*m.k34+w*m.k44;

  result.Eqv(vr.x/w,vr.y/w,vr.z/w);

end;

// Процедура нахождения обратной матрицы 4x4
function InvertMatrix(const m:TMatrix):TMatrix;
type
  PMatrr = ^TMatrr;
  TMatrr = array [0..3,0..3] of Single;
var
  i,j,k,swap:Integer;
  t:Single;
  inverse:TMatrix;
  src,dst:PMatrr;
begin
  src:=@m;
  inverse.SetIdentity;
  dst:=@inverse;
  for i:=0 to 3 do begin
    swap:=i;
    for j:=i+1 to 3 do
      if abs(src^[j][i])>abs(src^[i][i]) then swap:=j;
    if swap<>i then begin
      for k:=0 to 3 do begin
        t:=src^[i][k];
        src^[i][k]:=src^[swap][k];
        src^[swap][k]:=t;
        t:=dst^[i][k];
        dst^[i][k]:=dst^[swap][k];
        dst^[swap][k]:=t;
      end;
    end;
    if src^[i][i]=0 then exit;
    t:=src^[i][i];
    for k:=0 to 3 do begin
      src^[i][k]:=src^[i][k]/t;
      dst^[i][k]:=dst^[i][k]/t;
    end;
    for j:=0 to 3 do begin
      if i<>j then begin
        t:=src^[j][i];
        for k:=0 to 3 do begin
          src^[j][k]:=src^[j][k]-src^[i][k]*t;
          dst^[j][k]:=dst^[j][k]-dst^[i][k]*t;
        end;
      end;
    end;
  end;
  result:=inverse;
end;

// Процедура нахождения обратной матрицы 4x4
function InvertMatrix(const m:TDoubleMatrix):TDoubleMatrix;
type
  PMatrr = ^TMatrr;
  TMatrr = array [0..3,0..3] of Double;
var
  i,j,k,swap:Integer;
  t:Double;
  inverse:TDoubleMatrix;
  src,dst:PMatrr;
begin
  src:=@m;
  inverse.SetIdentity;
  dst:=@inverse;
  for i:=0 to 3 do begin
    swap:=i;
    for j:=i+1 to 3 do
      if abs(src^[j][i])>abs(src^[i][i]) then swap:=j;
    if swap<>i then begin
      for k:=0 to 3 do begin
        t:=src^[i][k];
        src^[i][k]:=src^[swap][k];
        src^[swap][k]:=t;
        t:=dst^[i][k];
        dst^[i][k]:=dst^[swap][k];
        dst^[swap][k]:=t;
      end;
    end;
    if src^[i][i]=0 then exit;
    t:=src^[i][i];
    for k:=0 to 3 do begin
      src^[i][k]:=src^[i][k]/t;
      dst^[i][k]:=dst^[i][k]/t;
    end;
    for j:=0 to 3 do begin
      if i<>j then begin
        t:=src^[j][i];
        for k:=0 to 3 do begin
          src^[j][k]:=src^[j][k]-src^[i][k]*t;
          dst^[j][k]:=dst^[j][k]-dst^[i][k]*t;
        end;
      end;
    end;
  end;
  result:=inverse;
end;

// Определяем Z коориднату в экранном буфере из реальной координаты
function PerspectiveZValue(const realZ:Single;const matProj:TMatrix):Single;
  var
    v:TVector;
begin
  v.Eqv(0,0,realZ);
  v:=matProj.multVecMatrix(v);
  result:=v.z;
end;

// Определяем Z коориднату в экранном буфере из реальной координаты
function PerspectiveZValue(const realZ:Double;const matProj:TDoubleMatrix):Double;
  var
    v:TDoubleVector;
begin
  v.Eqv(0,0,realZ);
  v:=matProj.multVecMatrix(v);
  result:=v.z;
end;

// угол между векторами
function AngleBetweenVectors(const v1,v2:TVector):Single;
begin
  result:=arccos(DotVector(v1.GetNormalized,v2.GetNormalized));
end;

// угол между векторами
function AngleBetweenVectors(const v1,v2:TDoubleVector):Double;
begin
  result:=arccos(DotVector(v1.GetNormalized,v2.GetNormalized));
end;

// Кратчайшая дуга поворота между двумя векторами
function ShortestArc(const FromV,ToV:TVector):TQuaternion;
  var
    c:TVector;
begin
  c:=CrossVector(FromV,ToV);
  result.x:=c.x; result.y:=c.y; result.z:=c.z; result.w:=DotVector(FromV,ToV);
  result.Normalize;
  result.w:=result.w+1;
  if result.w<=0.00001 then begin
    if (FromV.z*FromV.z>FromV.x*FromV.x) then begin
      result.x:=0; result.y:=FromV.z; result.z:=-FromV.y;
    end else begin
      result.x:=FromV.y; result.y:=-FromV.x; result.z:=0;
    end;
  end;
  result.Normalize;
end;

// Кратчайшая дуга поворота между двумя векторами
function ShortestArc(const FromV,ToV:TDoubleVector):TQuaternion;
  var
    c:TDoubleVector;
begin
  c:=CrossVector(FromV,ToV);
  result.x:=c.x; result.y:=c.y; result.z:=c.z; result.w:=DotVector(FromV,ToV);
  result.Normalize;
  result.w:=result.w+1;
  if result.w<=0.00001 then begin
    if (FromV.z*FromV.z>FromV.x*FromV.x) then begin
      result.x:=0; result.y:=FromV.z; result.z:=-FromV.y;
    end else begin
      result.x:=FromV.y; result.y:=-FromV.x; result.z:=0;
    end;
  end;
  result.Normalize;
end;

// расстояние от точки до плоскости
function PlaneDistance(const Normal,Point,ToPoint:TVector):Single;
begin
  result:=DotVector(Normal,ToPoint)-DotVector(Normal,Point);
end;

// расстояние от точки до плоскости
function PlaneDistance(const Normal,Point,ToPoint:TDoubleVector):Double;
begin
  result:=DotVector(Normal,ToPoint)-DotVector(Normal,Point);
end;
{******************************************************************************}

{******************************************************************************}
function B2S(b:ShortInt):Single;
begin
  result:=fUS*b;
end;

function S2B(s:Single):ShortInt;
begin
  result:=round(127*s);
end;
{******************************************************************************}

{******************************************************************************}
// TVectorList
constructor TVectorList.Create;
begin
  count:=0;
  VectorArray:=nil;
end;

destructor TVectorList.Destroy;
begin
  Clear;
end;

function TVectorList.Get(index:Integer):PVector;
begin
  if (VectorArray<>nil) and (index>=0) and (index<count)
    then result:=@VectorArray^[index]
    else result:=nil;
end;

procedure TVectorList.Put(index:Integer;const value:TVector);
begin
  if (index>=0) and (index<count)
    then VectorArray^[index]:=value;
end;

procedure TVectorList.Clear;
  var
    i:Integer;
begin
  count:=0;
  Freemem(VectorArray);
  VectorArray:=nil;
end;

procedure TVectorList.SetLength(NewLength:Integer);
  var
    i:Integer;
begin
  if NewLength = count then exit;
  if NewLength > 0 then begin
    ReAllocMem(VectorArray,sizeof(TVector) * NewLength);
    if NewLength > count then begin
      for i:=count to NewLength-1 do begin
        VectorArray^[i].Eqv(0,0,0);
      end;
    end;
    count:=NewLength;
  end else begin
    Clear;
  end;
end;

function TVectorList.Add:PVector;
begin
  SetLength(count+1);
  Result:=@VectorArray^[count-1];
end;

function TVectorList.Insert(index:Integer):PVector;
  var
    i:Integer;
begin
  if (index>=0) and (index<=count) then begin
    ReAllocMem(VectorArray,sizeof(TVector) * (count+1));
    if index < count then begin
      System.Move(VectorArray^[index],VectorArray^[index+1],(count-index)*sizeof(TVector));
    end;
    VectorArray^[index].Eqv(0,0,0);
    inc(count);
    result:=@VectorArray^[index];
  end else result:=nil;
end;

function TVectorList.Push:PVector;
begin
  SetLength(count+1);
  Result:=@VectorArray^[count-1];
end;

function TVectorList.Pop:TVector;
begin
  Result:=VectorArray^[count-1];
  Delete(count-1);
end;

procedure TVectorList.Delete(index:Integer);
  var
    i:Integer;
begin
  if (index>=0) and (index<count) then begin
    if index < count-1 then begin
      System.Move(VectorArray^[index+1],VectorArray^[index],(count-index-1)*sizeof(TVector));
    end;
    dec(count);
    ReAllocMem(VectorArray,sizeof(TVector) * count);
  end;
end;
{******************************************************************************}

{******************************************************************************}
end.
{******************************************************************************}

