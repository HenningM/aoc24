Dim in As String

Open Cons For Input As #1

Sub splitString(ByVal source As String, destination(Any) As String, ByVal delimiter As UByte)
    Do
        Dim As Integer position = InStr(1, source, Chr(delimiter))
        ReDim Preserve destination(UBound(destination) + 1)
        If position = 0 Then
            destination(UBound(destination)) = source
            Exit Do
        End If
        destination(UBound(destination)) = Left(source, position - 1)
        source = Mid(source, position + 1)
    Loop
End Sub

Function IsLineSafe(lineArr(Any) As String) As Boolean
    Dim prevDiff As Integer
    prevDiff = 0
    For position As Integer = LBound(lineArr) To UBound(lineArr) - 1
        Dim value As Integer
        Dim nextValue As Integer
        Dim diff As Integer
        value = ValInt(lineArr(position))
        nextValue = ValInt(lineArr(position + 1))
        diff = nextValue - value
        If Abs(diff) < 4 And Abs(diff) > 0 And ((prevDiff <= 0 and diff < 0) or (prevDiff >= 0 and diff > 0)) Then
          prevDiff = diff
        Else
          Return False
        End If
    Next
    Return True
End Function

Function IsLineSafeWithTolerance(lineArr(Any) As String) As Boolean
    If IsLineSafe(lineArr()) Then
      Return True
    End If

    For posIgnore As Integer = LBound(lineArr) To UBound(lineArr)
      Dim As String testArr(UBound(lineArr) - 1)
      For position As Integer = LBound(lineArr) To UBound(lineArr)
        If position > posIgnore Then
          testArr(position - 1) = lineArr(position)
        ElseIf position < posIgnore Then
          testArr(position) = lineArr(position)
        End If
      Next
      If IsLineSafe(testArr()) Then
        Return True
      End If
    Next
    Return False
End Function

Dim sum as Integer
Dim sumPartTwo as Integer
sum = 0
sumPartTwo = 0
Do Until eof(1)
  Dim isSafe As Boolean
  Dim As String testArr(Any)
  Line Input #1,in
  splitString(in, testArr(), Asc(" "))
  isSafe = IsLineSafe(testArr())
  If isSafe Then
    sum += 1
  End If
  isSafe = IsLineSafeWithTolerance(testArr())
  If isSafe Then
    sumPartTwo += 1
  End If
Loop
Print sum
Print sumPartTwo
