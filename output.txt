; ModuleID = 'SwiftLite'
source_filename = "SwiftLite"

%Fraction = type <{ i32, i32, i32 (%Fraction*, i32)*, i32 (%Fraction*, i32)*, i32 (%Fraction*)*, i32 (%Fraction*)*, %Fraction* (%Fraction*)*, %Fraction* (%Fraction*)*, i32 (%Fraction*)* }>

@strfmt = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@intfmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@charfmt = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1
@lit = private unnamed_addr constant [2 x i8] c"/\00", align 1
@x = global %Fraction* null
@y = global %Fraction* null

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %Fraction0 = call %Fraction* @Fraction0(i32 5, i32 3)
  %Fraction01 = call %Fraction* @Fraction0(i32 6, i32 3)
  %addFrac_result = call %Fraction* @addFrac(%Fraction* %Fraction0, %Fraction* %Fraction01)
  store %Fraction* %addFrac_result, %Fraction** @x, align 8
  %x = load %Fraction*, %Fraction** @x, align 8
  %Fraction02 = call %Fraction* @Fraction0(i32 1, i32 1)
  %subFrac_result = call %Fraction* @subFrac(%Fraction* %x, %Fraction* %Fraction02)
  store %Fraction* %subFrac_result, %Fraction** @y, align 8
  %obj = load %Fraction*, %Fraction** @x, align 8
  %method = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 8
  %print = load i32 (%Fraction*)*, i32 (%Fraction*)** %method, align 8
  %call = call i32 %print(%Fraction* %obj)
  %obj3 = load %Fraction*, %Fraction** @y, align 8
  %method4 = getelementptr inbounds %Fraction, %Fraction* %obj3, i32 0, i32 8
  %print5 = load i32 (%Fraction*)*, i32 (%Fraction*)** %method4, align 8
  %call6 = call i32 %print5(%Fraction* %obj3)
  ret i32 0
}

define { i32, i8* }* @concat({ i32, i8* }* %s1, { i32, i8* }* %s2) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i32, i8* }* getelementptr ({ i32, i8* }, { i32, i8* }* null, i32 1) to i32))
  %cc = bitcast i8* %malloccall to { i32, i8* }*
  %len1 = getelementptr inbounds { i32, i8* }, { i32, i8* }* %s1, i32 0, i32 0
  %len11 = load i32, i32* %len1, align 4
  %len12 = getelementptr inbounds { i32, i8* }, { i32, i8* }* %s2, i32 0, i32 0
  %len2 = load i32, i32* %len12, align 4
  %p1 = getelementptr inbounds { i32, i8* }, { i32, i8* }* %s1, i32 0, i32 1
  %p13 = load i8*, i8** %p1, align 8
  %p2 = getelementptr inbounds { i32, i8* }, { i32, i8* }* %s2, i32 0, i32 1
  %p24 = load i8*, i8** %p2, align 8
  %tmp = add i32 %len11, %len2
  %len = getelementptr inbounds { i32, i8* }, { i32, i8* }* %cc, i32 0, i32 0
  store i32 %tmp, i32* %len, align 4
  %chars = getelementptr inbounds { i32, i8* }, { i32, i8* }* %cc, i32 0, i32 1
  %tmp5 = add i32 %tmp, 1
  %mallocsize = mul i32 %tmp5, ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i32)
  %arr = tail call i8* @malloc(i32 %mallocsize)
  %count = alloca i32, align 4
  store i32 0, i32* %count, align 4
  br label %for

for:                                              ; preds = %for_body, %entry
  %comp = load i32, i32* %count, align 4
  %check = icmp slt i32 %comp, %len11
  br i1 %check, label %for_body, label %merge

for_body:                                         ; preds = %for
  %count7 = load i32, i32* %count, align 4
  %c = getelementptr i8, i8* %arr, i32 %count7
  %c1 = getelementptr i8, i8* %p13, i32 %count7
  %tmp8 = load i8, i8* %c1, align 1
  store i8 %tmp8, i8* %c, align 1
  %tmp9 = add i32 %count7, 1
  store i32 %tmp9, i32* %count, align 4
  br label %for

merge:                                            ; preds = %for
  %count10 = alloca i32, align 4
  store i32 0, i32* %count10, align 4
  br label %for11

for11:                                            ; preds = %for_body12, %merge
  %comp20 = load i32, i32* %count10, align 4
  %check21 = icmp slt i32 %comp20, %len2
  br i1 %check21, label %for_body12, label %merge22

for_body12:                                       ; preds = %for11
  %count13 = load i32, i32* %count, align 4
  %count14 = load i32, i32* %count10, align 4
  %c15 = getelementptr i8, i8* %arr, i32 %count13
  %c116 = getelementptr i8, i8* %p24, i32 %count14
  %tmp17 = load i8, i8* %c116, align 1
  store i8 %tmp17, i8* %c15, align 1
  %tmp18 = add i32 %count13, 1
  store i32 %tmp18, i32* %count, align 4
  %tmp19 = add i32 %count14, 1
  store i32 %tmp19, i32* %count10, align 4
  br label %for11

merge22:                                          ; preds = %for11
  %end = getelementptr i8, i8* %arr, i32 %tmp
  store i8 0, i8* %end, align 1
  store i8* %arr, i8** %chars, align 8
  ret { i32, i8* }* %cc
}

declare noalias i8* @malloc(i32)

define { i32, i32* }* @range(i32 %s1, i32 %s2) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i32, i32* }* getelementptr ({ i32, i32* }, { i32, i32* }* null, i32 1) to i32))
  %arr = bitcast i8* %malloccall to { i32, i32* }*
  %tmp = sub i32 %s2, %s1
  %tmp1 = add i32 %tmp, 1
  %mallocsize = mul i32 %tmp1, ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32)
  %malloccall2 = tail call i8* @malloc(i32 %mallocsize)
  %tmp3 = bitcast i8* %malloccall2 to i32*
  %tmp4 = alloca i32, align 4
  %tmp5 = alloca i32, align 4
  store i32 0, i32* %tmp5, align 4
  store i32 %s1, i32* %tmp4, align 4
  br label %while

while:                                            ; preds = %while_body, %entry
  %i9 = load i32, i32* %tmp5, align 4
  %tmp10 = icmp slt i32 %i9, %tmp1
  br i1 %tmp10, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i = load i32, i32* %tmp5, align 4
  %x = load i32, i32* %tmp4, align 4
  %tmp6 = getelementptr i32, i32* %tmp3, i32 %i
  store i32 %x, i32* %tmp6, align 4
  %tmp7 = add i32 %x, 1
  store i32 %tmp7, i32* %tmp4, align 4
  %tmp8 = add i32 %i, 1
  store i32 %tmp8, i32* %tmp5, align 4
  br label %while

merge:                                            ; preds = %while
  %len_gep = getelementptr inbounds { i32, i32* }, { i32, i32* }* %arr, i32 0, i32 0
  %arr_gep = getelementptr inbounds { i32, i32* }, { i32, i32* }* %arr, i32 0, i32 1
  store i32 %tmp1, i32* %len_gep, align 4
  store i32* %tmp3, i32** %arr_gep, align 8
  ret { i32, i32* }* %arr
}

define i32 @gcd(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, i32* %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, i32* %b2, align 4
  %b3 = load i32, i32* %b2, align 4
  %tmp = icmp eq i32 %b3, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else
  %a5 = load i32, i32* %a1, align 4
  %b6 = load i32, i32* %b2, align 4
  %tmp7 = urem i32 %a5, %b6
  %x = alloca i32, align 4
  store i32 %tmp7, i32* %x, align 4
  %b8 = load i32, i32* %b2, align 4
  %x9 = load i32, i32* %x, align 4
  %gcd_result = call i32 @gcd(i32 %b8, i32 %x9)
  ret i32 %gcd_result

then:                                             ; preds = %entry
  %a4 = load i32, i32* %a1, align 4
  ret i32 %a4

else:                                             ; preds = %entry
  br label %merge
}

define i32 @lcm(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, i32* %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, i32* %b2, align 4
  %a3 = load i32, i32* %a1, align 4
  %a4 = load i32, i32* %a1, align 4
  %b5 = load i32, i32* %b2, align 4
  %gcd_result = call i32 @gcd(i32 %a4, i32 %b5)
  %tmp = sdiv i32 %a3, %gcd_result
  %b6 = load i32, i32* %b2, align 4
  %tmp7 = mul i32 %tmp, %b6
  ret i32 %tmp7
}

define i32 @setNum(%Fraction* %self, i32 %num1) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %num12 = alloca i32, align 4
  store i32 %num1, i32* %num12, align 4
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 0
  %self4 = load %Fraction*, %Fraction** %self1, align 8
  %f5 = getelementptr inbounds %Fraction, %Fraction* %self4, i32 0, i32 1
  %num16 = load i32, i32* %num12, align 4
  store i32 %num16, i32* %f, align 4
  ret i32 0
}

define i32 @setDen(%Fraction* %self, i32 %den1) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %den12 = alloca i32, align 4
  store i32 %den1, i32* %den12, align 4
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 0
  %self4 = load %Fraction*, %Fraction** %self1, align 8
  %f5 = getelementptr inbounds %Fraction, %Fraction* %self4, i32 0, i32 1
  %den16 = load i32, i32* %den12, align 4
  store i32 %den16, i32* %f5, align 4
  ret i32 0
}

define i32 @getNum(%Fraction* %self) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %self2 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self2, i32 0, i32 0
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f4 = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 1
  %self5 = load %Fraction*, %Fraction** %self1, align 8
  %num = getelementptr inbounds %Fraction, %Fraction* %self5, i32 0, i32 0
  %num6 = load i32, i32* %num, align 4
  ret i32 %num6
}

define i32 @getDen(%Fraction* %self) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %self2 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self2, i32 0, i32 0
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f4 = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 1
  %self5 = load %Fraction*, %Fraction** %self1, align 8
  %den = getelementptr inbounds %Fraction, %Fraction* %self5, i32 0, i32 1
  %den6 = load i32, i32* %den, align 4
  ret i32 %den6
}

define %Fraction* @negate(%Fraction* %self) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %self2 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self2, i32 0, i32 0
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f4 = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 1
  %num = load i32, i32* %f, align 4
  %tmp = sub i32 0, %num
  store i32 %tmp, i32* %f, align 4
  %self5 = load %Fraction*, %Fraction** %self1, align 8
  ret %Fraction* %self5
}

define %Fraction* @reduce(%Fraction* %self) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %self2 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self2, i32 0, i32 0
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f4 = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 1
  %num = load i32, i32* %f, align 4
  %tmp = icmp eq i32 %num, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  %self19 = load %Fraction*, %Fraction** %self1, align 8
  ret %Fraction* %self19

then:                                             ; preds = %entry
  %self5 = load %Fraction*, %Fraction** %self1, align 8
  %den = getelementptr inbounds %Fraction, %Fraction* %self5, i32 0, i32 1
  store i32 1, i32* %den, align 4
  br label %merge

else:                                             ; preds = %entry
  %num6 = load i32, i32* %f, align 4
  %den7 = load i32, i32* %f4, align 4
  %gcd_result = call i32 @gcd(i32 %num6, i32 %den7)
  %tmp8 = alloca i32, align 4
  store i32 %gcd_result, i32* %tmp8, align 4
  %num9 = load i32, i32* %f, align 4
  %tmp10 = load i32, i32* %tmp8, align 4
  %tmp11 = sdiv i32 %num9, %tmp10
  %self12 = load %Fraction*, %Fraction** %self1, align 8
  %num13 = getelementptr inbounds %Fraction, %Fraction* %self12, i32 0, i32 0
  store i32 %tmp11, i32* %num13, align 4
  %den14 = load i32, i32* %f4, align 4
  %tmp15 = load i32, i32* %tmp8, align 4
  %tmp16 = sdiv i32 %den14, %tmp15
  %self17 = load %Fraction*, %Fraction** %self1, align 8
  %den18 = getelementptr inbounds %Fraction, %Fraction* %self17, i32 0, i32 1
  store i32 %tmp16, i32* %den18, align 4
  br label %merge
}

define i32 @print(%Fraction* %self) {
entry:
  %self1 = alloca %Fraction*, align 8
  store %Fraction* %self, %Fraction** %self1, align 8
  %self2 = load %Fraction*, %Fraction** %self1, align 8
  %f = getelementptr inbounds %Fraction, %Fraction* %self2, i32 0, i32 0
  %self3 = load %Fraction*, %Fraction** %self1, align 8
  %f4 = getelementptr inbounds %Fraction, %Fraction* %self3, i32 0, i32 1
  %num = load i32, i32* %f, align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @intfmt, i32 0, i32 0), i32 %num)
  %malloccall = tail call i8* @malloc(i32 ptrtoint ({ i32, i8* }* getelementptr ({ i32, i8* }, { i32, i8* }* null, i32 1) to i32))
  %arr = bitcast i8* %malloccall to { i32, i8* }*
  %len = getelementptr inbounds { i32, i8* }, { i32, i8* }* %arr, i32 0, i32 0
  %a = getelementptr inbounds { i32, i8* }, { i32, i8* }* %arr, i32 0, i32 1
  store i32 1, i32* %len, align 4
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @lit, i32 0, i32 0), i8** %a, align 8
  %str = getelementptr inbounds { i32, i8* }, { i32, i8* }* %arr, i32 0, i32 1
  %s = load i8*, i8** %str, align 8
  %printf5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @strfmt, i32 0, i32 0), i8* %s)
  %den = load i32, i32* %f4, align 4
  %printf6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @intfmt, i32 0, i32 0), i32 %den)
  ret i32 0
}

define %Fraction* @Fraction0(i32 %num, i32 %den) {
entry:
  %num1 = alloca i32, align 4
  store i32 %num, i32* %num1, align 4
  %den2 = alloca i32, align 4
  store i32 %den, i32* %den2, align 4
  %malloccall = tail call i8* @malloc(i32 ptrtoint (%Fraction* getelementptr (%Fraction, %Fraction* null, i32 1) to i32))
  %obj = bitcast i8* %malloccall to %Fraction*
  %f = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 0
  store i32 0, i32* %f, align 4
  %f3 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 1
  store i32 0, i32* %f3, align 4
  %m = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 2
  store i32 (%Fraction*, i32)* @setNum, i32 (%Fraction*, i32)** %m, align 8
  %m4 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 3
  store i32 (%Fraction*, i32)* @setDen, i32 (%Fraction*, i32)** %m4, align 8
  %m5 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 4
  store i32 (%Fraction*)* @getNum, i32 (%Fraction*)** %m5, align 8
  %m6 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 5
  store i32 (%Fraction*)* @getDen, i32 (%Fraction*)** %m6, align 8
  %m7 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 6
  store %Fraction* (%Fraction*)* @negate, %Fraction* (%Fraction*)** %m7, align 8
  %m8 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 7
  store %Fraction* (%Fraction*)* @reduce, %Fraction* (%Fraction*)** %m8, align 8
  %m9 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 8
  store i32 (%Fraction*)* @print, i32 (%Fraction*)** %m9, align 8
  %malloccall10 = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %self = bitcast i8* %malloccall10 to %Fraction**
  store %Fraction* %obj, %Fraction** %self, align 8
  %f11 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 0
  %f12 = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 1
  %den13 = load i32, i32* %den2, align 4
  %tmp = icmp slt i32 %den13, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  ret %Fraction* %obj

then:                                             ; preds = %entry
  %den14 = load i32, i32* %den2, align 4
  %tmp15 = sub i32 0, %den14
  %self16 = load %Fraction*, %Fraction** %self, align 8
  %den17 = getelementptr inbounds %Fraction, %Fraction* %self16, i32 0, i32 1
  store i32 %tmp15, i32* %den17, align 4
  %num18 = load i32, i32* %num1, align 4
  %tmp19 = sub i32 0, %num18
  %self20 = load %Fraction*, %Fraction** %self, align 8
  %num21 = getelementptr inbounds %Fraction, %Fraction* %self20, i32 0, i32 0
  store i32 %tmp19, i32* %num21, align 4
  br label %merge

else:                                             ; preds = %entry
  %num22 = load i32, i32* %num1, align 4
  %self23 = load %Fraction*, %Fraction** %self, align 8
  %num24 = getelementptr inbounds %Fraction, %Fraction* %self23, i32 0, i32 0
  store i32 %num22, i32* %num24, align 4
  %den25 = load i32, i32* %den2, align 4
  %self26 = load %Fraction*, %Fraction** %self, align 8
  %den27 = getelementptr inbounds %Fraction, %Fraction* %self26, i32 0, i32 1
  store i32 %den25, i32* %den27, align 4
  br label %merge
}

define %Fraction* @addFrac(%Fraction* %x, %Fraction* %y) {
entry:
  %x1 = alloca %Fraction*, align 8
  store %Fraction* %x, %Fraction** %x1, align 8
  %y2 = alloca %Fraction*, align 8
  store %Fraction* %y, %Fraction** %y2, align 8
  %obj = load %Fraction*, %Fraction** %x1, align 8
  %method = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 4
  %getNum = load i32 (%Fraction*)*, i32 (%Fraction*)** %method, align 8
  %call = call i32 %getNum(%Fraction* %obj)
  %numx = alloca i32, align 4
  store i32 %call, i32* %numx, align 4
  %obj3 = load %Fraction*, %Fraction** %x1, align 8
  %method4 = getelementptr inbounds %Fraction, %Fraction* %obj3, i32 0, i32 5
  %getDen = load i32 (%Fraction*)*, i32 (%Fraction*)** %method4, align 8
  %call5 = call i32 %getDen(%Fraction* %obj3)
  %denx = alloca i32, align 4
  store i32 %call5, i32* %denx, align 4
  %obj6 = load %Fraction*, %Fraction** %y2, align 8
  %method7 = getelementptr inbounds %Fraction, %Fraction* %obj6, i32 0, i32 4
  %getNum8 = load i32 (%Fraction*)*, i32 (%Fraction*)** %method7, align 8
  %call9 = call i32 %getNum8(%Fraction* %obj6)
  %numy = alloca i32, align 4
  store i32 %call9, i32* %numy, align 4
  %obj10 = load %Fraction*, %Fraction** %y2, align 8
  %method11 = getelementptr inbounds %Fraction, %Fraction* %obj10, i32 0, i32 5
  %getDen12 = load i32 (%Fraction*)*, i32 (%Fraction*)** %method11, align 8
  %call13 = call i32 %getDen12(%Fraction* %obj10)
  %deny = alloca i32, align 4
  store i32 %call13, i32* %deny, align 4
  %deny14 = load i32, i32* %deny, align 4
  %denx15 = load i32, i32* %denx, align 4
  %lcm_result = call i32 @lcm(i32 %deny14, i32 %denx15)
  %lcd = alloca i32, align 4
  store i32 %lcm_result, i32* %lcd, align 4
  %lcd16 = load i32, i32* %lcd, align 4
  %deny17 = load i32, i32* %deny, align 4
  %tmp = sdiv i32 %lcd16, %deny17
  %numy18 = load i32, i32* %numy, align 4
  %tmp19 = mul i32 %tmp, %numy18
  %lcd20 = load i32, i32* %lcd, align 4
  %denx21 = load i32, i32* %denx, align 4
  %tmp22 = sdiv i32 %lcd20, %denx21
  %numx23 = load i32, i32* %numx, align 4
  %tmp24 = mul i32 %tmp22, %numx23
  %tmp25 = add i32 %tmp19, %tmp24
  %tmp26 = alloca i32, align 4
  store i32 %tmp25, i32* %tmp26, align 4
  %tmp27 = load i32, i32* %tmp26, align 4
  %lcd28 = load i32, i32* %lcd, align 4
  %Fraction0 = call %Fraction* @Fraction0(i32 %tmp27, i32 %lcd28)
  %frac = alloca %Fraction*, align 8
  store %Fraction* %Fraction0, %Fraction** %frac, align 8
  %obj29 = load %Fraction*, %Fraction** %frac, align 8
  %method30 = getelementptr inbounds %Fraction, %Fraction* %obj29, i32 0, i32 7
  %reduce = load %Fraction* (%Fraction*)*, %Fraction* (%Fraction*)** %method30, align 8
  %call31 = call %Fraction* %reduce(%Fraction* %obj29)
  ret %Fraction* %call31
}

define %Fraction* @subFrac(%Fraction* %x, %Fraction* %y) {
entry:
  %x1 = alloca %Fraction*, align 8
  store %Fraction* %x, %Fraction** %x1, align 8
  %y2 = alloca %Fraction*, align 8
  store %Fraction* %y, %Fraction** %y2, align 8
  %x3 = load %Fraction*, %Fraction** %x1, align 8
  %obj = load %Fraction*, %Fraction** %y2, align 8
  %method = getelementptr inbounds %Fraction, %Fraction* %obj, i32 0, i32 6
  %negate = load %Fraction* (%Fraction*)*, %Fraction* (%Fraction*)** %method, align 8
  %call = call %Fraction* %negate(%Fraction* %obj)
  %addFrac_result = call %Fraction* @addFrac(%Fraction* %x3, %Fraction* %call)
  ret %Fraction* %addFrac_result
}
