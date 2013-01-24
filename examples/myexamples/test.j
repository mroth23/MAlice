.class public test
.super java/lang/Object
.method public <init>()V
.limit locals 1
.limit stack 2
aload_0
dup
invokespecial java/lang/Object/<init>()V
invokevirtual test/hatta()V
return
.end method
.method public static main([Ljava/lang/String;)V
.limit stack 2
 .limit locals 1
 new test
dup
invokespecial test/<init>()V
astore_0
return
.end method
.method public _m_f_2(Ljava/util/concurrent/atomic/AtomicReference;[Ljava/lang/String;)V
.limit locals 4
.limit stack 6
iconst_0
istore_3
aload_1
new java/lang/Integer
dup
iload_3
invokespecial java/lang/Integer/<init>(I)V
invokevirtual java/util/concurrent/atomic/AtomicReference.set(Ljava/lang/Object;)V
aload_2
iconst_0
ldc "world"
aastore
return
.end method
.method public hatta()V
.limit locals 4
.limit stack 10
iconst_1
istore_2
ldc 10
anewarray java/lang/String
dup
astore_3
iconst_0
ldc "hello"
aastore
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
invokevirtual test/_m_f_2(Ljava/util/concurrent/atomic/AtomicReference;[Ljava/lang/String;)V
aload_1
invokevirtual java/util/concurrent/atomic/AtomicReference.get()Ljava/lang/Object;
checkcast java/lang/Integer
invokevirtual java/lang/Integer.intValue()I
istore_2
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_2
ifeq _label0
ldc "True"
goto _label1
_label0:
ldc "False"
_label1:
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
aload_3
iconst_0
iaload
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Done"
invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
return
.end method
