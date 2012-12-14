.class public Myclass
.super java/lang/Object
.field public __global_limit I
.field public __global_arr [I
.method public <init>()V
 .limit stack 100
 .limit locals 100
aload_0
dup
invokespecial java/lang/Object/<init>()V
aload_0
ldc 10
putfield Myclass/__global_limit I
aload_0
aload_0
getfield Myclass/__global_limit I
newarray int
putfield Myclass/__global_arr [I
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
.method public _m_output_3()I
.limit stack 100
.limit locals 4
iconst_0
istore_3
iconst_0
istore_2
_label0:
iload_2
aload_0
getfield Myclass/__global_limit I
if_icmpeq _label2
iconst_0
goto _label3
_label2:
iconst_1
_label3:
ifne _label1
aload_0
getfield Myclass/__global_arr [I
iload_2
iaload
iconst_1
if_icmpeq _label5
iconst_0
goto _label6
_label5:
iconst_1
_label6:
ifeq _label7
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc " is prime\n"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
iload_3
iconst_1
iadd
istore_3
goto _label4
_label7:
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc " is NOT prime\n"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
_label4:
iload_2
iconst_1
iadd
istore_2
goto _label0
_label1:
iload_3
ireturn
.end method
.method public _m_findPrimes_2()V
.limit stack 100
.limit locals 4
iconst_2
istore_2
_label8:
iload_2
iload_2
imul
aload_0
getfield Myclass/__global_limit I
if_icmpge _label10
iconst_0
goto _label11
_label10:
iconst_1
_label11:
ifne _label9
aload_0
getfield Myclass/__global_arr [I
iload_2
iaload
iconst_1
if_icmpeq _label13
iconst_0
goto _label14
_label13:
iconst_1
_label14:
ifeq _label15
iload_2
istore_3
_label16:
iload_2
iload_3
imul
aload_0
getfield Myclass/__global_limit I
if_icmpge _label18
iconst_0
goto _label19
_label18:
iconst_1
_label19:
ifne _label17
aload_0
getfield Myclass/__global_arr [I
iload_2
iload_3
imul
iconst_0
iastore
iload_3
iconst_1
iadd
istore_3
goto _label16
_label17:
goto _label12
_label15:
_label12:
iload_2
iconst_1
iadd
istore_2
goto _label8
_label9:
aload_0
invokevirtual Myclass/_m_output_3()I
istore_3
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "There are "
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_3
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc " primes in the first "
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
aload_0
getfield Myclass/__global_limit I
invokevirtual java/io/PrintStream/print(I)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc " natural numbers"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
return
.end method
.method public _m_initialise_0()V
.limit stack 100
.limit locals 3
iconst_0
istore_2
_label16:
iload_2
aload_0
getfield Myclass/__global_limit I
if_icmpeq _label18
iconst_0
goto _label19
_label18:
iconst_1
_label19:
ifne _label17
iload_2
iconst_0
if_icmpeq _label21
iconst_0
goto _label22
_label21:
iconst_1
_label22:
iload_2
iconst_1
if_icmpeq _label23
iconst_0
goto _label24
_label23:
iconst_1
_label24:
ior
ifeq _label25
aload_0
getfield Myclass/__global_arr [I
iload_2
iconst_0
iastore
goto _label20
_label25:
aload_0
getfield Myclass/__global_arr [I
iload_2
iconst_1
iastore
_label20:
iload_2
iconst_1
iadd
istore_2
goto _label16
_label17:
return
.end method
.method public hatta()V
.limit stack 100
.limit locals 1
aload_0
invokevirtual Myclass/_m_initialise_0()V
aload_0
invokevirtual Myclass/_m_findPrimes_2()V
return
.end method
.method public _throwConditionError()V
.limit stack 100
.limit locals 1
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "No return matched in function."
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
iconst_1
invokestatic java/lang/System/exit(I)V
return
.end method
