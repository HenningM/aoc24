with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day01 is
   package Integer_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Integer);
   use Integer_Vectors;
   package A_Sorter is new Generic_Sorting;
   Input_Str  : String (1 .. 80);
   First_List : Vector;
   Second_List : Vector;
   I : Integer;
   First_Str : String (1 .. 10);
   Second_Str : String (1 .. 10);
   First_Num_Pos : Natural;
   Last_Num_Pos : Natural;
   Last_Num_Len : Natural;
   First : Integer;
   Second : Integer;
   Last : Integer;
   Sum : Integer := 0;
   Similarity : Integer := 0;
   Space : constant Character_Set :=
     To_Set (" ");
begin
   while not End_Of_File (Standard_Input) loop
      Get_Line (Input_Str, Last);
      Find_Token (Input_Str, Space, Inside, First_Num_Pos, Last_Num_Pos);
      First_Str (1 .. First_Num_Pos) := Input_Str (1 .. First_Num_Pos);
      I := Integer'Value (First_Str (1 .. First_Num_Pos));
      First_List.Append (I);
      Last_Num_Pos := First_Num_Pos + 3;
      Last_Num_Len := Last - Last_Num_Pos + 1;
      Second_Str (1 .. Last_Num_Len) := Input_Str (Last_Num_Pos .. Last);
      I := Integer'Value (Second_Str (1 .. Last_Num_Len));
      Second_List.Append (I);
   end loop;
   A_Sorter.Sort (First_List);
   A_Sorter.Sort (Second_List);

   --  Part 1
   for I in 0 .. First_List.Last_Index loop
      First := First_List (I);
      Last := Second_List (I);
      Sum := Sum + abs (First - Last);
   end loop;
   Put (Sum, 0);
   New_Line;

   -- Part 2
   for I in 0 .. First_List.Last_Index loop
      First := First_List (I);
      Last := 0;
      for J in Second_List.First_Index .. Second_List.Last_Index loop
         Second := Second_List (J);
         if First = Second then
            Last := Last + 1;
         end if;
      end loop;
      Similarity := Similarity + First * Last;
   end loop;
   Put (Similarity, 0);
   New_Line;
end Day01;
