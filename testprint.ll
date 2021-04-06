; ModuleID = 'SwiftLite'
source_filename = "SwiftLite"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@lit = private unnamed_addr constant [12 x i8] c"Hello_world\00", align 1
@lit.1 = private unnamed_addr constant [12 x i8] c"Hello_world\00", align 1
@lit.2 = private unnamed_addr constant [12 x i8] c"Hello_world\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @lit, i32 0, i32 0))
  %printf1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @lit.1, i32 0, i32 0))
  %printf2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @lit.2, i32 0, i32 0))
  ret i32 0
}
