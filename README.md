# C/C++ 개발자를 위한 Ada Programming 튜토리얼

본 문서는 아래 링크를 참조하여 작성되었습니다.

[https://www.adahome.com/Ammo/Cplpl2Ada.html](https://www.adahome.com/Ammo/Cplpl2Ada.html)

## Ada 기본

### 연산자

| Operator       | C/C++   | Ada |
|:--------------:|:-------:|:---:|
| Assignment     | =       | :=  |
| Equality       | ==      | =   |
| NonEquality    | !=      | /=  |
| PlusEquals     | +=      |     |
| SubtractEquals | -=      |     |
| MultiplyEquals | *=      |     |
| DivisionEquals | /=      |     |
| OrEquals       | &#124;= |     |
| AndEquals      | &=      |     |
| Modulus        | %       | mod |
| Remainder      |         | rem |
| AbsoluteValue  |         | abs |
| Exponentiation |         | **  |
| Range          |         | ..  |

### Ada 와 C 가 다른 특징

Ada 는 대소문자를 구분하지 않는다. 키워드 `begin`, `BEGIN`, `Begin` 은 모두 동일하게 인식됩니다.

또한, `'` 문자가 객체의 속성 접근자로 쓰인다.

```c
// in c programming language
int a = sizeof(int) * 8;
```

```ada
-- in ada programming language
a : Integer := Integer'Size;
```

### 변수생성

변수 생성은 아래와 같이 할 수 있다.

```c
// in c programming language
int i;
int a, b, c;
int j = 0;
int k, l = 1;
```

```ada
-- in ada programming language
i : Integer;
a, b, c : Integer;
j : Integer := 0;
k, l : Integer := 1;
```

상수 선언은 아래와 같이 할 수 있다.

```c
// in c programming language
const int days_per_week = 7;
```

```ada
-- in ada programming language
days_per_week : constant Integer := 7;
days_per_week : constant := 7;
```

상수 생성시 타입을 생략할 수 있다. 생략된 타입은 컴파일시 자동으로 선택된다.

### 새로운 타입 생성

Ada 는 강하게 타입을 체크합니다.

```c
// in c programming language
typedef int INT;
INT a;
int b;

a = b; // works, no problem
```

동일한 정수형이라 해도 타입이 다르면 값을 대입할 수 없습니다.

```ada
-- in ada programming language
type INT is new Integer;
a : INT;
b : Integer;

a := b; -- fails.
```

위에서 `new` 키워드를 이용해 새로운 타입을 생성한 것을 볼 수 있습니다.

`subtype` 키워드를 이용해 하위 타입을 생성할 수 있습니다.

```ada
subtype INT is Integer;
a : INT;
b : Integer;

a := b; -- works.
```

`subtype` 의 용도는 타입의 범위를 한정하기 위함입니다.(아래에서 자세히 설명합니다.)

### 기본 타입

Ada 에서는 기본적으로 `Integer` 를 제공합니다.

`Long_Integer, Short_Integer, Long_Long_Integer` 등은 컴파일러에 따라 제공될 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;      use Ada.Long_Integer_Text_IO;
with Ada.Short_Integer_Text_IO;     use Ada.Short_Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;

procedure Hello
is
    i : Integer := 0;
    j : Long_Integer := 1;
    k : Short_Integer := 2;
    l : Long_Long_Integer := 3;
begin
    -- Put(Integer'Image(i));
    -- New_Line(1);
    Put_Line("i =" & Integer'Image(i) & ", j =" & Long_Integer'Image(j));
    Put_Line("i =" & Short_Integer'Image(k) & ", j =" & Long_Long_Integer'Image(l));
end Hello;
```

Ada 는 언어 레벨에서 `unsigned integer` 를 제공하지 않습니다.

하지만, `Ada-95` 부터는 `System.Unsigned_Types` 를 제공합니다.

`Ada-95` 부터는 `modular type` 이 추가됩니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure Hello
is
    type BYTE is mod 2**8;
    i : BYTE := 255;
begin
    Put_Line("i =" & BYTE'Image(i));    -- 255
    i := i + 1;
    Put_Line("i =" & BYTE'Image(i));    -- 0
end Hello;
```

범위 안에서만 순환하므로 오버플로우/언더플로우 가 발생하지 않습니다.

Ada 에는 ASCII 문자만 가질 수 있는 `Character` 와 한글같은 비 ASCII 문자값을 가질 수 있는 `Wide_Character` 가 있습니다.

또한 `Boolean` 타입을 지원합니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Wide_Text_IO;              use Ada.Wide_Text_IO;

procedure WideText is
    ch : Character := 'A';

    str0 : String := "Hello, World!";
    str1 : Wide_String := "한글은 이렇게!";
    str2 : Wide_String := "中国.";

    b : Boolean;
begin
    Put_Line(Character'Image(ch));

    Put_Line(str0);
    Put_Line(str1);
    Put_Line(str2);

    b := TRUE;
    Put_Line(Boolean'Image(b));
end WideText;
```

### String

Ada 에서는 문자열 타입을 생성할 때 사이즈를 지정해야 합니다. 또한, 지정한 사이즈의 문자열만 입력받을 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Wide_Text_IO;              use Ada.Wide_Text_IO;

procedure test is
    str0 : String := "Hello, World!";
    --str1 : String;        -- compiler error
    str2 : String(1 .. 4);
begin
    Put_Line(str0);

    str2 := "aaa";          -- runtime error
    Put_Line(str2);
end test;
```

당황할 분들이 많으실건데... 가변형 문자열 입력받는 방법 또한 있으니 너무 걱정은 마시기 바랍니다.

### Float

Ada 에는 두가지 타입의 실수형 타입을 제공합니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Wide_Text_IO;              use Ada.Wide_Text_IO;

procedure test
is
    type FloatingPoint1 is new Float;
    type FloatingPoint2 is digits 2;
    x : FloatingPoint1;
    y : FloatingPoint2;
begin
    x := 1.0;
    y := 2.123456;
    Put_Line(FloatingPoint1'Image(x));
    Put_Line(FloatingPoint2'Image(y));
end test;
```

첫번째는 `Float` 이고, 두번째는 소수점 5자리까지의 고정 사이즈 실수형입니다.

### Enumeration and Range

아래와 같이 `Enum` 또는 `Range` 타입을 생성할 수 있습니다.

```ada
type Boolean is (FALSE, TRUE);

type Hours   is new Integer range 1 .. 12;
type Hours24 is range 0 .. 23;
type Minutes is range 1 .. 60;
```

정수형에서 파생된 타입이지만 범위를 벗어나는 값을 입력하려 하면 오류가 발생합니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type All_Days is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
    subtype Week_Days is All_Days range Monday .. Friday;
    subtype Weekend is All_Days range Saturday .. Sunday;
    Day : All_Days := Tuesday;
begin
    if Day in Week_Days then
        Put_Line("Let's work!");
    end if;

    if Day in Saturday .. Sunday then
        Put_Line("Enjoy your life!");
    end if;
end test;
```

`subtype` 을 이용해 `Enum` 또는 `Range` 값의 일부를 범위로 하는 새로운 타입을 생성할 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type All_Days is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
begin
    Put_Line(All_Days'Image(All_Days'Succ(Monday)));    -- successor
    Put_Line(All_Days'Image(All_Days'Pred(Tuesday)));   -- predecessor

    Put_Line(All_Days'Image(All_Days'Val(0)));

    Put_Line(All_Days'Image(All_Days'First));
    Put_Line(All_Days'Image(All_Days'Last));

    Put_Line(All_Days'Image(All_Days'Succ(All_Days'Pred(Tuesday))));
end test;
```

`Succ`, `Pred`, `Val`, `First`, `Last` 속성을 이용할 수 있습니다.

### Array

배열은 아래와 같이 생성 및 사용할 수 있다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    Name  : array (0 .. 30) of Character;
    Track : array (0 .. 2) of Integer;
    DblA  : array (0 .. 2,0 .. 9) of Integer;
    Init  : array (0 .. 2) of Integer := (0, 1, 2);

    type Name_Type is array (0 .. 30) of Character;
    a, b : Name_Type;

    type All_Days is (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
    Hours_Worked : array (All_Days range Monday .. Friday) of All_Days;

    type Vector is array (Integer range <>) of Float;

    Example : array (1 .. 10) of Integer;
begin
    Track(2) := 1;
    dbla(0,3) := 2;     -- case insensitive

    a := b;

    for i in Example'Range loop
        Put_Line(Integer'Image(i));
    end loop;
end test;
```

배열의 초기화는 아래와 같이 할 수 있다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    InitA : array (0 .. 3) of Integer := (0 .. 3 => 1);
    InitB : array (0 .. 3) of Integer := (0 => 1, others => 0);

    Large : array (0 .. 100) of Integer := (others => 0);
    Small : array (0 .. 3) of Integer;
begin
    Put_Line(Integer'Image(InitB(2)));

    Large := Large(51 .. 100) & Large(0..50);
end test;
```

### Record

C 언어의 구조체와 비슷합니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type struct_device is
        record
            major_number : Integer;
            minor_number : Integer;
            name         : String(1 .. 3);      -- start from 1 (not zero)
        end record;
    type Device is new struct_device;

    type struct_device_with_default is
        record
            major_number : Integer          := 0;
            minor_number : Integer          := 0;
            name         : String(1 .. 7)  := "unknown";
        end record;

    lp1 : Device := (1, 2, "lp1");
    lp2 : Device := (major_number => 1,
                     minor_number => 3,
                     name         => "lp2");
    tmp : Device;
begin
    tmp := (major_number => 255, minor_number =>10, name => "tmp");
    Put_Line(lp2.name);
end test;
```

### Access types (pointers)

포인터입니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

procedure test
is
    type Device_Event;
    type Device_Event_Access is access Device_Event;
    type Device_Event is
        record
            major_number : Integer := 0;
            minor_number : Integer := 0;
            event_ident  : Integer := 0;

            next         : Device_Event_Access := null;
            -- Note: the assignement to null is not required,
            -- Ada automatically initialises access types to
            -- null if no other value is specified.
        end record;

    Event_1 : Device_Event_Access;

    dev1, dev2 : Device_Event;
    pdv1, pdv2 : Device_Event_Access;

    procedure Free is new Ada.Unchecked_Deallocation (Device_Event, Device_Event_Access);
begin
    Event_1 := new Device_Event;
    Event_1.next := new Device_Event'(1, 2, 7, null);

    pdv1 := Event_1;
    pdv2 := Event_1.next;
    dev1 := dev2; -- all elements copied.
    pdv1 := pdv2; -- pdv1 now points to contents of pdv2.
    pdv1.all := pdv2.all; -- !!

    Free(Event_1.next);
    Free(Event_1);
end test;

```

포인터를 생성하기 전에 포인터가 가리키는 타입은 먼저 생성되어 있어야 합니다.

`new` 키워드로 생성된 객체의 액세스타입을 받습니다.

`'` 가 객체의 속성이 아닌 객체의 파라미터를 받기 위해 사용되고 있습니다.

`all` 키워드를 이용해 액세스 타입이 가르키는 데이타를 모두 복사합니다.

`Ada.Unchecked_Deallocation` 패키지를 이용해 생성한 객체를 해제해줍니다.
생성해제는 사용자가 생성한 객체만 하고 컴파일러에서 제공하는 라이브러리로 생성한 객체는 생성해제 해서는 안됩니다.
(이론적으로는 그렇습니다.)

### Ada 타입 심화

#### Casting

`Casting` 기능이 있습니다.(와우!! 살았다!!)

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type Thing is new Integer;

    an_Integer : Integer;
    a_Thing : Thing;
    a_Float : Float;
    a_Character : Character;
begin
    a_Thing := 1;
    a_Float := 1.1;
    a_Character := 'A';
    -- an_Integer := a_Thing;               -- illegal
    an_Integer := Integer(a_Thing);
    an_Integer := Integer(a_Float);
    -- an_Integer := Integer(a_Character);  -- illegal
end test;
```

캐스팅은 유사한 데이타 타입 사이에서만 가능합니다. 서로 다른 타입 사이의 캐스팅은 `Unchecked_Conversion` 을 이용해 가능하며 나중에 설명됩니다.

#### Procedure type

C 에 있는 함수 포인터 기능을 사용할 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type Callback_Func is access function(param_1 : in Integer;
                                          param_2 : in Integer)
                                         return Integer;
begin
    Put_Line("");
end test;
```

#### Discriminant type

타입생성시 파라미터를 부여할 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type Discriminated_Record (Size : Natural) is
        record
            A : String (1 .. Size);
        end record;

    subtype Length is Natural range 0 .. 1024;
    type Rec (Len : Length := 4) is
        record
            Data : String (1 .. Len);
    end record;

    My_Record1 : Discriminated_Record(10);
    My_Record2 : Rec;
begin
    My_Record1.A := "0123456789";
    My_Record2.Data := "0123";
end test;
```

파라미터에 디폴트 값을 부여할 수 있습니다. 런터임 오류를 방지하기 위해 디폴트값은 최대값이 설정되어 있어야 합니다.

#### Variant record

파라미터에 따라 레코드 타입의 변수를 다르게 생성할 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    type Transport_Type is (Sports, Family, Van);

    type Car(T_Type : Transport_Type) is
        record
            Name : String(1 .. 3);
            case T_Type is
               when Sports =>
                   Soft_Top      : Boolean;
               when Family =>
                   Number_Seats  : Integer;
                   Rear_Belts    : Boolean;
               when Van    =>
                   Cargo_Capacity: Integer;
            end case;
        end record;

    subtype Sports_Car is Car(Sports);
    subtype Family_Car is Car(T_Type => Family);
    subtype Small_Van  is Car(T_Type => Van);

    My_Car : Car(Sports);
begin
    My_Car.Name := "sky";
end test;
```

가변형 레코드에 대해 `subtype` 을 생성할 수 있습니다.

#### Exceptions

예외를 처리할 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;

procedure test
is
    parameter_out_of_range : Exception;
    i : Integer;
begin
    i := 100;
    if i > 50 then
        raise parameter_out_of_range;
    end if;
    Put_Line("");
exception
    when parameter_out_of_range => Put_Line("parameter_out_of_range");
end test;
```

#### 시스템 데이타를 위한 변수 타입

`Size` 속성을 이용해 타입의 비트 수를 지정할 수 있습니다.

열거형 타입에 정수값을 지정해 줄 수 있습니다.

생성된 변수의 메모리 주소를 지정해 줄 수 있습니다. 숫자의 표현은 `진수#숫자#` 형식으로 표현할 수 있습니다.
숫자에 `_` 을 입력해 가독성을 높일 수 있습니다.

```ada
with Ada.Text_IO;                   use Ada.Text_IO;
with System;                        use System;

procedure test
is
	type BYTE is range 0 .. 255;
	for  BYTE'Size use 8;

	type DEV_Activity is (READING, WRITING, IDLE);
	for  DEV_Activity use (READING => 1, WRITING => 2, IDLE => 3);

	type DEV_Available is new BYTE;
	Avail_Flag : DEV_Available;
	for Avail_Flag'Address use System'To_Address(16#00000340#);

	-- base#number#
	Is_Available : constant BYTE := 2#1000_0000#;
	Not_Available: constant BYTE := 2#0000_0000#;

	type DEV_Status is range 0 .. 15;

	type DeviceDetails is
		record
			status : DEV_Activity;
			rd_stat: DEV_Status;
			wr_stat: DEV_Status;
		end record;

	-- use two bytes, for status in first byte, for rd_stat/wr_stat in second byte.
	for DeviceDetails use
		record at mod 2;
			status  at 0 range 0 .. 7;
			rd_stat at 1 range 0 .. 3;
			wr_stat at 1 range 4 .. 7;
		end record;
begin
	Put_Line("");
end test;
```

마지막으로 `at mod 2` 와 깉은 방식으로 타입의 바이트 수를 지정해 줄 수 있습니다.
`at 0`, `at 1` 등을 입력해서 변수의 저장 위치를 지정해 줄 수 있습니다.

### C++ 문법과 대응하는 Ada 문법

#### Compound Statement

블럭은 아래와 같이 생성할 수 있습니다.

```c++
// in c programming language
{
  declarations
  statements
}
```

```ada
-- in ada programming language
declare
  declarations
begin
  statement
end;
```

#### if Statement

```c++
// in c programming language
if (expression) {
  statement
} else {
  statement
}
```

```ada
-- in ada programming language
if expression then
  statement
elsif expression then
  statement
else
  statement
end if;
```

#### switch Statement

Ada 에는 `break` 를 입력하지 않아도 됩니다.

```c++
// in c programming language
switch (expression)
{
  case value: statement
  default:    statement
}
```

```ada
-- in ada programming language
case expression is
  when value => statement
  when others => statement
end case;
```

범위 연산자를 `case` 문에 사용할 수 있습니다.

```c++
// in c programming language
switch (integer_value) {
case 1:
case 2:
case 3:
case 4:
  value_ok = 1;
  break;
case 5:
case 6:
case 7:
  break;
}
```

```ada
-- in ada programming language
case integer_value is
  when 1 .. 4    => value_ok := 1;
  when 5 | 6 | 7 => null;
end case;
```

처리해야할 내용이 없을 경우 `null` 을 지정해 컴파일 오류를 막을 수 있습니다.

### 반복 구문

```ada
loop
  statement
end loop;
```

```ada
while expression loop
  statement
end loop;
```

```ada
for ident in range loop
  statement
end loop;
```

```ada
for i in 1 .. 10 loop
  null;
end loop;
```

`reverse` 키워드를 이용해 거꾸로 루프를 돌 수 있습니다.

```ada
for i in reverse 1 .. 10 loop
  null;
end loop;
```

C++ 에서 `break` 키워드는 `exit` 이 같은 기능을 합니다. `continue` 에 대응하는 키워드는 없습니다.

```ada
while expression loop
  if expression2 then
    exit;
  end if;
end loop;

while expression loop
  exit when expression2;
end loop;
```

```ada
Main_Loop:
  while not End_Of_File(File_Handle) loop
    for Char_Index in Buffer'Range loop
      exit when Buffer(Char_Index) = NEW_LINE;
      exit Main_Loop when Buffer(Char_Index) = PERCENT;
    end loop;
  end loop Main_Loop;
```

### exception handling

`raise` 를 이용해 나머지 예외를 그대로 상위 프로시져로 전달합니다.

```ada
begin
  statement1
exception
  when ident => statement2
  when others => statement2
end;

begin
  function_call;
exception
  when the_one_we_want => handle_it;
  when others          => raise;
end;
```

### Procedure vs Function

`function` 은 리턴값을 가질수도 있고 없을 수도 있지만, `procedure` 는 리턴값을 가질 수 없습니다.

`function` 은 구문 안에서 사용될 수 있지만, `procedure` 는 구문안에서 사용될 수 없습니다.

`function` 은 아래와 같이 정의할 수 있습니다.

```c++
return_type func_name(parameters);
return_type func_name(parameters)
{
  declarations
  statement
}
```

```ada
function func_name(parameters) return return_type;
function func_name(parameters) return return_type is
  declarations
begin
  statement
end func_name
```

리턴 값이 없는 함수(Ada 에서는 프로시저)는 아래와 같이 생성할 수 있습니다.

```c++
void func_name(parameters);
```

```ada
procedure func_name(parameters);
```

리턴값을 가질 수는 없지만, 파라미터를 이용해 값을 받을 수 있습니다.

```ada
type int      is new Integer;
type int_star is access int;
procedure func1(by_value     : in     int);
procedure func2(by_address   : in out int_star);
procedure func3(by_reference : in out int);
```

파라미터가 없는 함수 또는 프로시저는 아래와 같이 정의할 수 있습니다.

```ada
procedure func_name;
function func_name return Integer;
```

아래와 같이 함수의 `overloading` 이 가능합니다.

```ada
function Day return All_Days;
function Day(a_date : in Date_Type) return All_Days;
```

아래와 같이 연산자 오버로딩도 가능합니다. C++ 과 다르게 클래스 없이도 생성 가능합니다.

```ada
function "+"(Left, Right : in Integer) return Integer;
```

오버로딩 가능한 연산자는 아래와 같습니다.

```ada
=	<	<=	>	>=
+	-	&	abs	not
*	/	mod	rem	**
and	or	xor
```

파라미터의 전달 방식은 `in`, `out` 키워드를 이용해 파라미터의 입출력 방식을 정하고 있습니다.

```ada
procedure proc(Parameter : in     Integer);
procedure proc(Parameter :    out Integer);
procedure proc(Parameter : in out Integer);
procedure proc(Parameter :        Integer);
```

파라미터에는 디폴트값을 지정해 줄 수 있습니다.

```ada
procedure Create
  (File : in out File_Type;
   Mode : in     File_Mode := Inout_File;
   Name : in     String    := "";
   Form : in     String    := "");
```

프로시저는 내장 프로시저를 가질 수 있습니다.

```ada
procedure Sort(Sort_This : in out An_Array)
is
  procedure Swap(Item_1, Item_2 : in out Array_Type)
  is
  begin
  end Swap;
begin
end Sort;
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

```ada
```

## Under Translation & Writing


































































































.
