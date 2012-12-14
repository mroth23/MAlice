.class public Myclass
.super java/lang/Object
.field public _scanner Ljava/util/Scanner;
.method public <init>()V
 .limit stack 100
 .limit locals 100
aload_0
dup
invokespecial java/lang/Object/<init>()V
aload_0
new java/util/Scanner
dup
getstatic java/lang/System.in Ljava/io/InputStream;
invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V
putfield Myclass/_scanner Ljava/util/Scanner;
invokevirtual Myclass/hatta()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 100
 .limit locals 100
 new Myclass
dup
invokespecial Myclass/<init>()V
astore_0
return
.end method
.method public printElement([II)V
.limit locals 3
.limit stack 6
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "arr["
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "] = "
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
aload_1
iload_2
iaload
invokevirtual java/io/PrintStream/print(I)V
return
.end method
.method public reduction([II)I
.limit locals 5
.limit stack 4
iconst_0
istore_3
iconst_0
istore 4
_label0:
iload 4
iload_2
if_icmpeq _label1
iload_3
aload_1
iload 4
iaload
iadd
istore_3
iload 4
iconst_1
iadd
istore 4
goto _label0
_label1:
iload_3
ireturn
.end method
.method public _m_initialise_9(Ljava/util/concurrent/atomic/AtomicReference;[I)V
.limit locals 4
.limit stack 7
iconst_0
istore_3
_label4:
iload_3
aload_1
invokevirtual java/util/concurrent/atomic/AtomicReference.get()Ljava/lang/Object;
checkcast java/lang/Integer
invokevirtual java/lang/Integer.intValue()I
if_icmpeq _label5
aload_2
iload_3
dup
iconst_2
ldc 10
iconst_3
idiv
ineg
iload_3
ldc 500
iadd
isub
iadd
imul
iastore
iload_3
iconst_1
iadd
istore_3
goto _label4
_label5:
return
.end method
.method public hatta()V
.limit locals 6
.limit stack 16
ldc 10
dup
istore_2
newarray int
astore_3
aload_0
new java/util/concurrent/atomic/AtomicReference
dup
dup
new java/lang/Integer
dup
iload_2
invokespecial java/lang/Integer/<init>(I)V
invokespecial java/util/concurrent/atomic/AtomicReference/<init>(Ljava/lang/Object;)V
astore_1
aload_3
invokevirtual Myclass/_m_initialise_9(Ljava/util/concurrent/atomic/AtomicReference;[I)V
aload_1
invokevirtual java/util/concurrent/atomic/AtomicReference.get()Ljava/lang/Object;
checkcast java/lang/Integer
invokevirtual java/lang/Integer.intValue()I
istore_2
aload_0
aload_3
iload_2
invokevirtual Myclass/reduction([II)I
istore 4
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Summing the elements of the array gives "
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload 4
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "\n"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "The array has "
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc " elements\n"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Choose an element to print in the range 0.."
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
iconst_1
isub
invokevirtual java/io/PrintStream/print(I)V
aload_0
getfield Myclass/_scanner Ljava/util/Scanner;
invokevirtual java/util/Scanner/nextInt()I
dup
istore 5
iconst_0
if_icmplt _label9
iconst_0
goto _label10
_label9:
iconst_1
_label10:
iload 5
iload_2
iconst_1
isub
if_icmpgt _label11
iconst_0
goto _label12
_label11:
iconst_1
_label12:
ior
ifeq _label13
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Chosen element out of range"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
goto _label8
_label13:
aload_0
aload_3
iload 5
invokevirtual Myclass/printElement([II)V
_label8:
return
.end method
