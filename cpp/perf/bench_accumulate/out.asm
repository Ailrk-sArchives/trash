
./a.out:     file format elf64-x86-64


Disassembly of section .init:

0000000000001000 <_init>:
    1000:	f3 0f 1e fa          	endbr64 
    1004:	48 83 ec 08          	sub    $0x8,%rsp
    1008:	48 8b 05 c1 2f 00 00 	mov    0x2fc1(%rip),%rax        # 3fd0 <__gmon_start__>
    100f:	48 85 c0             	test   %rax,%rax
    1012:	74 02                	je     1016 <_init+0x16>
    1014:	ff d0                	call   *%rax
    1016:	48 83 c4 08          	add    $0x8,%rsp
    101a:	c3                   	ret    

Disassembly of section .plt:

0000000000001020 <benchmark::State::FinishKeepRunning()@plt-0x10>:
    1020:	ff 35 e2 2f 00 00    	push   0x2fe2(%rip)        # 4008 <_GLOBAL_OFFSET_TABLE_+0x8>
    1026:	ff 25 e4 2f 00 00    	jmp    *0x2fe4(%rip)        # 4010 <_GLOBAL_OFFSET_TABLE_+0x10>
    102c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000001030 <benchmark::State::FinishKeepRunning()@plt>:
    1030:	ff 25 e2 2f 00 00    	jmp    *0x2fe2(%rip)        # 4018 <benchmark::State::FinishKeepRunning()>
    1036:	68 00 00 00 00       	push   $0x0
    103b:	e9 e0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001040 <benchmark::internal::RegisterBenchmarkInternal(benchmark::internal::Benchmark*)@plt>:
    1040:	ff 25 da 2f 00 00    	jmp    *0x2fda(%rip)        # 4020 <benchmark::internal::RegisterBenchmarkInternal(benchmark::internal::Benchmark*)>
    1046:	68 01 00 00 00       	push   $0x1
    104b:	e9 d0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001050 <rand@plt>:
    1050:	ff 25 d2 2f 00 00    	jmp    *0x2fd2(%rip)        # 4028 <rand@GLIBC_2.2.5>
    1056:	68 02 00 00 00       	push   $0x2
    105b:	e9 c0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001060 <benchmark::Initialize(int*, char**)@plt>:
    1060:	ff 25 ca 2f 00 00    	jmp    *0x2fca(%rip)        # 4030 <benchmark::Initialize(int*, char**)>
    1066:	68 03 00 00 00       	push   $0x3
    106b:	e9 b0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001070 <benchmark::internal::Benchmark::Benchmark(char const*)@plt>:
    1070:	ff 25 c2 2f 00 00    	jmp    *0x2fc2(%rip)        # 4038 <benchmark::internal::Benchmark::Benchmark(char const*)>
    1076:	68 04 00 00 00       	push   $0x4
    107b:	e9 a0 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001080 <std::__throw_length_error(char const*)@plt>:
    1080:	ff 25 ba 2f 00 00    	jmp    *0x2fba(%rip)        # 4040 <std::__throw_length_error(char const*)@GLIBCXX_3.4>
    1086:	68 05 00 00 00       	push   $0x5
    108b:	e9 90 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001090 <memset@plt>:
    1090:	ff 25 b2 2f 00 00    	jmp    *0x2fb2(%rip)        # 4048 <memset@GLIBC_2.2.5>
    1096:	68 06 00 00 00       	push   $0x6
    109b:	e9 80 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010a0 <__assert_fail@plt>:
    10a0:	ff 25 aa 2f 00 00    	jmp    *0x2faa(%rip)        # 4050 <__assert_fail@GLIBC_2.2.5>
    10a6:	68 07 00 00 00       	push   $0x7
    10ab:	e9 70 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010b0 <benchmark::ReportUnrecognizedArguments(int, char**)@plt>:
    10b0:	ff 25 a2 2f 00 00    	jmp    *0x2fa2(%rip)        # 4058 <benchmark::ReportUnrecognizedArguments(int, char**)>
    10b6:	68 08 00 00 00       	push   $0x8
    10bb:	e9 60 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010c0 <__cxa_atexit@plt>:
    10c0:	ff 25 9a 2f 00 00    	jmp    *0x2f9a(%rip)        # 4060 <__cxa_atexit@GLIBC_2.2.5>
    10c6:	68 09 00 00 00       	push   $0x9
    10cb:	e9 50 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010d0 <operator new(unsigned long)@plt>:
    10d0:	ff 25 92 2f 00 00    	jmp    *0x2f92(%rip)        # 4068 <operator new(unsigned long)@GLIBCXX_3.4>
    10d6:	68 0a 00 00 00       	push   $0xa
    10db:	e9 40 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010e0 <operator delete(void*, unsigned long)@plt>:
    10e0:	ff 25 8a 2f 00 00    	jmp    *0x2f8a(%rip)        # 4070 <operator delete(void*, unsigned long)@CXXABI_1.3.9>
    10e6:	68 0b 00 00 00       	push   $0xb
    10eb:	e9 30 ff ff ff       	jmp    1020 <_init+0x20>

00000000000010f0 <benchmark::internal::InitializeStreams()@plt>:
    10f0:	ff 25 82 2f 00 00    	jmp    *0x2f82(%rip)        # 4078 <benchmark::internal::InitializeStreams()>
    10f6:	68 0c 00 00 00       	push   $0xc
    10fb:	e9 20 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001100 <benchmark::internal::Benchmark::DenseRange(long, long, int)@plt>:
    1100:	ff 25 7a 2f 00 00    	jmp    *0x2f7a(%rip)        # 4080 <benchmark::internal::Benchmark::DenseRange(long, long, int)>
    1106:	68 0d 00 00 00       	push   $0xd
    110b:	e9 10 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001110 <benchmark::RunSpecifiedBenchmarks()@plt>:
    1110:	ff 25 72 2f 00 00    	jmp    *0x2f72(%rip)        # 4088 <benchmark::RunSpecifiedBenchmarks()>
    1116:	68 0e 00 00 00       	push   $0xe
    111b:	e9 00 ff ff ff       	jmp    1020 <_init+0x20>

0000000000001120 <benchmark::State::StartKeepRunning()@plt>:
    1120:	ff 25 6a 2f 00 00    	jmp    *0x2f6a(%rip)        # 4090 <benchmark::State::StartKeepRunning()>
    1126:	68 0f 00 00 00       	push   $0xf
    112b:	e9 f0 fe ff ff       	jmp    1020 <_init+0x20>

0000000000001130 <std::ios_base::Init::Init()@plt>:
    1130:	ff 25 62 2f 00 00    	jmp    *0x2f62(%rip)        # 4098 <std::ios_base::Init::Init()@GLIBCXX_3.4>
    1136:	68 10 00 00 00       	push   $0x10
    113b:	e9 e0 fe ff ff       	jmp    1020 <_init+0x20>

0000000000001140 <benchmark::internal::Benchmark::Unit(benchmark::TimeUnit)@plt>:
    1140:	ff 25 5a 2f 00 00    	jmp    *0x2f5a(%rip)        # 40a0 <benchmark::internal::Benchmark::Unit(benchmark::TimeUnit)>
    1146:	68 11 00 00 00       	push   $0x11
    114b:	e9 d0 fe ff ff       	jmp    1020 <_init+0x20>

0000000000001150 <_Unwind_Resume@plt>:
    1150:	ff 25 52 2f 00 00    	jmp    *0x2f52(%rip)        # 40a8 <_Unwind_Resume@GCC_3.0>
    1156:	68 12 00 00 00       	push   $0x12
    115b:	e9 c0 fe ff ff       	jmp    1020 <_init+0x20>

Disassembly of section .text:

0000000000001160 <accumulate_bench(benchmark::State&) [clone .cold]>:
    1160:	48 89 de             	mov    %rbx,%rsi
    1163:	4c 29 e6             	sub    %r12,%rsi
    1166:	4d 85 e4             	test   %r12,%r12
    1169:	74 08                	je     1173 <accumulate_bench(benchmark::State&) [clone .cold]+0x13>
    116b:	4c 89 e7             	mov    %r12,%rdi
    116e:	e8 6d ff ff ff       	call   10e0 <operator delete(void*, unsigned long)@plt>
    1173:	48 89 ef             	mov    %rbp,%rdi
    1176:	e8 d5 ff ff ff       	call   1150 <_Unwind_Resume@plt>

000000000000117b <_GLOBAL__sub_I_main.cold>:
    117b:	48 89 ef             	mov    %rbp,%rdi
    117e:	be c8 00 00 00       	mov    $0xc8,%esi
    1183:	e8 58 ff ff ff       	call   10e0 <operator delete(void*, unsigned long)@plt>
    1188:	4c 89 e7             	mov    %r12,%rdi
    118b:	e8 c0 ff ff ff       	call   1150 <_Unwind_Resume@plt>

0000000000001190 <main>:
    1190:	55                   	push   %rbp
    1191:	48 89 f5             	mov    %rsi,%rbp
    1194:	48 83 ec 10          	sub    $0x10,%rsp
    1198:	89 7c 24 0c          	mov    %edi,0xc(%rsp)
    119c:	48 8d 7c 24 0c       	lea    0xc(%rsp),%rdi
    11a1:	e8 ba fe ff ff       	call   1060 <benchmark::Initialize(int*, char**)@plt>
    11a6:	8b 7c 24 0c          	mov    0xc(%rsp),%edi
    11aa:	48 89 ee             	mov    %rbp,%rsi
    11ad:	e8 fe fe ff ff       	call   10b0 <benchmark::ReportUnrecognizedArguments(int, char**)@plt>
    11b2:	41 89 c0             	mov    %eax,%r8d
    11b5:	b8 01 00 00 00       	mov    $0x1,%eax
    11ba:	45 84 c0             	test   %r8b,%r8b
    11bd:	75 07                	jne    11c6 <main+0x36>
    11bf:	e8 4c ff ff ff       	call   1110 <benchmark::RunSpecifiedBenchmarks()@plt>
    11c4:	31 c0                	xor    %eax,%eax
    11c6:	48 83 c4 10          	add    $0x10,%rsp
    11ca:	5d                   	pop    %rbp
    11cb:	c3                   	ret    
    11cc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000000011d0 <_GLOBAL__sub_I_main>:
    11d0:	41 54                	push   %r12
    11d2:	55                   	push   %rbp
    11d3:	48 83 ec 08          	sub    $0x8,%rsp
    11d7:	e8 14 ff ff ff       	call   10f0 <benchmark::internal::InitializeStreams()@plt>
    11dc:	48 8d 3d e6 2e 00 00 	lea    0x2ee6(%rip),%rdi        # 40c9 <std::__ioinit>
    11e3:	e8 48 ff ff ff       	call   1130 <std::ios_base::Init::Init()@plt>
    11e8:	48 8b 3d f1 2d 00 00 	mov    0x2df1(%rip),%rdi        # 3fe0 <std::ios_base::Init::~Init()@GLIBCXX_3.4>
    11ef:	48 8d 35 d3 2e 00 00 	lea    0x2ed3(%rip),%rsi        # 40c9 <std::__ioinit>
    11f6:	48 8d 15 bb 2e 00 00 	lea    0x2ebb(%rip),%rdx        # 40b8 <__dso_handle>
    11fd:	e8 be fe ff ff       	call   10c0 <__cxa_atexit@plt>
    1202:	bf c8 00 00 00       	mov    $0xc8,%edi
    1207:	e8 c4 fe ff ff       	call   10d0 <operator new(unsigned long)@plt>
    120c:	48 8d 35 9a 0e 00 00 	lea    0xe9a(%rip),%rsi        # 20ad <_IO_stdin_used+0xad>
    1213:	48 89 c7             	mov    %rax,%rdi
    1216:	48 89 c5             	mov    %rax,%rbp
    1219:	e8 52 fe ff ff       	call   1070 <benchmark::internal::Benchmark::Benchmark(char const*)@plt>
    121e:	48 8d 05 4b 2b 00 00 	lea    0x2b4b(%rip),%rax        # 3d70 <vtable for benchmark::internal::FunctionBenchmark+0x10>
    1225:	48 89 ef             	mov    %rbp,%rdi
    1228:	48 89 45 00          	mov    %rax,0x0(%rbp)
    122c:	48 8d 05 4d 01 00 00 	lea    0x14d(%rip),%rax        # 1380 <accumulate_bench(benchmark::State&)>
    1233:	48 89 85 c0 00 00 00 	mov    %rax,0xc0(%rbp)
    123a:	e8 01 fe ff ff       	call   1040 <benchmark::internal::RegisterBenchmarkInternal(benchmark::internal::Benchmark*)@plt>
    123f:	be 14 00 00 00       	mov    $0x14,%esi
    1244:	b9 01 00 00 00       	mov    $0x1,%ecx
    1249:	ba 19 00 00 00       	mov    $0x19,%edx
    124e:	48 89 c7             	mov    %rax,%rdi
    1251:	e8 aa fe ff ff       	call   1100 <benchmark::internal::Benchmark::DenseRange(long, long, int)@plt>
    1256:	48 83 c4 08          	add    $0x8,%rsp
    125a:	be 01 00 00 00       	mov    $0x1,%esi
    125f:	5d                   	pop    %rbp
    1260:	48 89 c7             	mov    %rax,%rdi
    1263:	41 5c                	pop    %r12
    1265:	e9 d6 fe ff ff       	jmp    1140 <benchmark::internal::Benchmark::Unit(benchmark::TimeUnit)@plt>
    126a:	49 89 c4             	mov    %rax,%r12
    126d:	e9 09 ff ff ff       	jmp    117b <_GLOBAL__sub_I_main.cold>
    1272:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
    1279:	00 00 00 
    127c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000001280 <_start>:
    1280:	f3 0f 1e fa          	endbr64 
    1284:	31 ed                	xor    %ebp,%ebp
    1286:	49 89 d1             	mov    %rdx,%r9
    1289:	5e                   	pop    %rsi
    128a:	48 89 e2             	mov    %rsp,%rdx
    128d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
    1291:	50                   	push   %rax
    1292:	54                   	push   %rsp
    1293:	4c 8d 05 56 03 00 00 	lea    0x356(%rip),%r8        # 15f0 <__libc_csu_fini>
    129a:	48 8d 0d df 02 00 00 	lea    0x2df(%rip),%rcx        # 1580 <__libc_csu_init>
    12a1:	48 8d 3d e8 fe ff ff 	lea    -0x118(%rip),%rdi        # 1190 <main>
    12a8:	ff 15 1a 2d 00 00    	call   *0x2d1a(%rip)        # 3fc8 <__libc_start_main@GLIBC_2.2.5>
    12ae:	f4                   	hlt    
    12af:	90                   	nop

00000000000012b0 <deregister_tm_clones>:
    12b0:	48 8d 3d 11 2e 00 00 	lea    0x2e11(%rip),%rdi        # 40c8 <__TMC_END__>
    12b7:	48 8d 05 0a 2e 00 00 	lea    0x2e0a(%rip),%rax        # 40c8 <__TMC_END__>
    12be:	48 39 f8             	cmp    %rdi,%rax
    12c1:	74 15                	je     12d8 <deregister_tm_clones+0x28>
    12c3:	48 8b 05 f6 2c 00 00 	mov    0x2cf6(%rip),%rax        # 3fc0 <_ITM_deregisterTMCloneTable>
    12ca:	48 85 c0             	test   %rax,%rax
    12cd:	74 09                	je     12d8 <deregister_tm_clones+0x28>
    12cf:	ff e0                	jmp    *%rax
    12d1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
    12d8:	c3                   	ret    
    12d9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000000012e0 <register_tm_clones>:
    12e0:	48 8d 3d e1 2d 00 00 	lea    0x2de1(%rip),%rdi        # 40c8 <__TMC_END__>
    12e7:	48 8d 35 da 2d 00 00 	lea    0x2dda(%rip),%rsi        # 40c8 <__TMC_END__>
    12ee:	48 29 fe             	sub    %rdi,%rsi
    12f1:	48 89 f0             	mov    %rsi,%rax
    12f4:	48 c1 ee 3f          	shr    $0x3f,%rsi
    12f8:	48 c1 f8 03          	sar    $0x3,%rax
    12fc:	48 01 c6             	add    %rax,%rsi
    12ff:	48 d1 fe             	sar    %rsi
    1302:	74 14                	je     1318 <register_tm_clones+0x38>
    1304:	48 8b 05 cd 2c 00 00 	mov    0x2ccd(%rip),%rax        # 3fd8 <_ITM_registerTMCloneTable>
    130b:	48 85 c0             	test   %rax,%rax
    130e:	74 08                	je     1318 <register_tm_clones+0x38>
    1310:	ff e0                	jmp    *%rax
    1312:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
    1318:	c3                   	ret    
    1319:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000001320 <__do_global_dtors_aux>:
    1320:	f3 0f 1e fa          	endbr64 
    1324:	80 3d 9d 2d 00 00 00 	cmpb   $0x0,0x2d9d(%rip)        # 40c8 <__TMC_END__>
    132b:	75 33                	jne    1360 <__do_global_dtors_aux+0x40>
    132d:	55                   	push   %rbp
    132e:	48 83 3d 82 2c 00 00 	cmpq   $0x0,0x2c82(%rip)        # 3fb8 <__cxa_finalize@GLIBC_2.2.5>
    1335:	00 
    1336:	48 89 e5             	mov    %rsp,%rbp
    1339:	74 0d                	je     1348 <__do_global_dtors_aux+0x28>
    133b:	48 8b 3d 76 2d 00 00 	mov    0x2d76(%rip),%rdi        # 40b8 <__dso_handle>
    1342:	ff 15 70 2c 00 00    	call   *0x2c70(%rip)        # 3fb8 <__cxa_finalize@GLIBC_2.2.5>
    1348:	e8 63 ff ff ff       	call   12b0 <deregister_tm_clones>
    134d:	c6 05 74 2d 00 00 01 	movb   $0x1,0x2d74(%rip)        # 40c8 <__TMC_END__>
    1354:	5d                   	pop    %rbp
    1355:	c3                   	ret    
    1356:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
    135d:	00 00 00 
    1360:	c3                   	ret    
    1361:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
    1368:	00 00 00 00 
    136c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000001370 <frame_dummy>:
    1370:	f3 0f 1e fa          	endbr64 
    1374:	e9 67 ff ff ff       	jmp    12e0 <register_tm_clones>
    1379:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000001380 <accumulate_bench(benchmark::State&)>:
    1380:	41 56                	push   %r14
    1382:	41 55                	push   %r13
    1384:	41 54                	push   %r12
    1386:	55                   	push   %rbp
    1387:	53                   	push   %rbx
    1388:	48 8b 47 20          	mov    0x20(%rdi),%rax
    138c:	48 39 47 28          	cmp    %rax,0x28(%rdi)
    1390:	0f 84 b6 01 00 00    	je     154c <accumulate_bench(benchmark::State&)+0x1cc>
    1396:	48 ba ff ff ff ff ff 	movabs $0x1fffffffffffffff,%rdx
    139d:	ff ff 1f 
    13a0:	48 8b 08             	mov    (%rax),%rcx
    13a3:	b8 01 00 00 00       	mov    $0x1,%eax
    13a8:	d3 e0                	shl    %cl,%eax
    13aa:	48 98                	cltq   
    13ac:	48 39 d0             	cmp    %rdx,%rax
    13af:	0f 87 8b 01 00 00    	ja     1540 <accumulate_bench(benchmark::State&)+0x1c0>
    13b5:	49 89 fd             	mov    %rdi,%r13
    13b8:	4c 8d 34 85 00 00 00 	lea    0x0(,%rax,4),%r14
    13bf:	00 
    13c0:	48 85 c0             	test   %rax,%rax
    13c3:	0f 84 65 01 00 00    	je     152e <accumulate_bench(benchmark::State&)+0x1ae>
    13c9:	4c 89 f7             	mov    %r14,%rdi
    13cc:	e8 ff fc ff ff       	call   10d0 <operator new(unsigned long)@plt>
    13d1:	4a 8d 1c 30          	lea    (%rax,%r14,1),%rbx
    13d5:	49 89 c4             	mov    %rax,%r12
    13d8:	48 39 d8             	cmp    %rbx,%rax
    13db:	0f 84 4f 01 00 00    	je     1530 <accumulate_bench(benchmark::State&)+0x1b0>
    13e1:	4c 89 f2             	mov    %r14,%rdx
    13e4:	31 f6                	xor    %esi,%esi
    13e6:	48 89 c7             	mov    %rax,%rdi
    13e9:	48 89 c5             	mov    %rax,%rbp
    13ec:	e8 9f fc ff ff       	call   1090 <memset@plt>
    13f1:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
    13f8:	e8 53 fc ff ff       	call   1050 <rand@plt>
    13fd:	48 83 c5 04          	add    $0x4,%rbp
    1401:	89 c2                	mov    %eax,%edx
    1403:	48 98                	cltq   
    1405:	48 69 c0 1f 85 eb 51 	imul   $0x51eb851f,%rax,%rax
    140c:	89 d1                	mov    %edx,%ecx
    140e:	c1 f9 1f             	sar    $0x1f,%ecx
    1411:	48 c1 f8 25          	sar    $0x25,%rax
    1415:	29 c8                	sub    %ecx,%eax
    1417:	6b c0 64             	imul   $0x64,%eax,%eax
    141a:	29 c2                	sub    %eax,%edx
    141c:	89 55 fc             	mov    %edx,-0x4(%rbp)
    141f:	48 39 eb             	cmp    %rbp,%rbx
    1422:	75 d4                	jne    13f8 <accumulate_bench(benchmark::State&)+0x78>
    1424:	41 80 7d 1a 00       	cmpb   $0x0,0x1a(%r13)
    1429:	0f 85 e8 00 00 00    	jne    1517 <accumulate_bench(benchmark::State&)+0x197>
    142f:	4c 89 ef             	mov    %r13,%rdi
    1432:	49 8b 6d 10          	mov    0x10(%r13),%rbp
    1436:	e8 e5 fc ff ff       	call   1120 <benchmark::State::StartKeepRunning()@plt>
    143b:	48 85 ed             	test   %rbp,%rbp
    143e:	0f 84 b0 00 00 00    	je     14f4 <accumulate_bench(benchmark::State&)+0x174>
    1444:	48 8d 73 fc          	lea    -0x4(%rbx),%rsi
    1448:	4c 29 e6             	sub    %r12,%rsi
    144b:	48 89 f7             	mov    %rsi,%rdi
    144e:	48 c1 ef 02          	shr    $0x2,%rdi
    1452:	48 83 c7 01          	add    $0x1,%rdi
    1456:	48 89 fa             	mov    %rdi,%rdx
    1459:	49 89 f8             	mov    %rdi,%r8
    145c:	48 c1 ea 02          	shr    $0x2,%rdx
    1460:	49 83 e0 fc          	and    $0xfffffffffffffffc,%r8
    1464:	48 c1 e2 04          	shl    $0x4,%rdx
    1468:	4b 8d 0c 84          	lea    (%r12,%r8,4),%rcx
    146c:	4c 01 e2             	add    %r12,%rdx
    146f:	48 83 fe 0c          	cmp    $0xc,%rsi
    1473:	49 0f 46 cc          	cmovbe %r12,%rcx
    1477:	4c 8d 49 04          	lea    0x4(%rcx),%r9
    147b:	4c 8d 51 08          	lea    0x8(%rcx),%r10
    147f:	4c 8d 59 0c          	lea    0xc(%rcx),%r11
    1483:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
    1488:	31 c0                	xor    %eax,%eax
    148a:	4c 39 e3             	cmp    %r12,%rbx
    148d:	74 5f                	je     14ee <accumulate_bench(benchmark::State&)+0x16e>
    148f:	48 83 fe 0c          	cmp    $0xc,%rsi
    1493:	0f 86 88 00 00 00    	jbe    1521 <accumulate_bench(benchmark::State&)+0x1a1>
    1499:	4c 89 e0             	mov    %r12,%rax
    149c:	66 0f ef c0          	pxor   %xmm0,%xmm0
    14a0:	f3 0f 6f 10          	movdqu (%rax),%xmm2
    14a4:	48 83 c0 10          	add    $0x10,%rax
    14a8:	66 0f fe c2          	paddd  %xmm2,%xmm0
    14ac:	48 39 d0             	cmp    %rdx,%rax
    14af:	75 ef                	jne    14a0 <accumulate_bench(benchmark::State&)+0x120>
    14b1:	66 0f 6f c8          	movdqa %xmm0,%xmm1
    14b5:	66 0f 73 d9 08       	psrldq $0x8,%xmm1
    14ba:	66 0f fe c1          	paddd  %xmm1,%xmm0
    14be:	66 0f 6f c8          	movdqa %xmm0,%xmm1
    14c2:	66 0f 73 d9 04       	psrldq $0x4,%xmm1
    14c7:	66 0f fe c1          	paddd  %xmm1,%xmm0
    14cb:	66 0f 7e c0          	movd   %xmm0,%eax
    14cf:	49 39 f8             	cmp    %rdi,%r8
    14d2:	74 1a                	je     14ee <accumulate_bench(benchmark::State&)+0x16e>
    14d4:	03 01                	add    (%rcx),%eax
    14d6:	49 39 d9             	cmp    %rbx,%r9
    14d9:	74 13                	je     14ee <accumulate_bench(benchmark::State&)+0x16e>
    14db:	03 41 04             	add    0x4(%rcx),%eax
    14de:	4c 39 d3             	cmp    %r10,%rbx
    14e1:	74 0b                	je     14ee <accumulate_bench(benchmark::State&)+0x16e>
    14e3:	03 41 08             	add    0x8(%rcx),%eax
    14e6:	4c 39 db             	cmp    %r11,%rbx
    14e9:	74 03                	je     14ee <accumulate_bench(benchmark::State&)+0x16e>
    14eb:	03 41 0c             	add    0xc(%rcx),%eax
    14ee:	48 83 ed 01          	sub    $0x1,%rbp
    14f2:	75 94                	jne    1488 <accumulate_bench(benchmark::State&)+0x108>
    14f4:	4c 89 ef             	mov    %r13,%rdi
    14f7:	e8 34 fb ff ff       	call   1030 <benchmark::State::FinishKeepRunning()@plt>
    14fc:	4d 85 e4             	test   %r12,%r12
    14ff:	74 24                	je     1525 <accumulate_bench(benchmark::State&)+0x1a5>
    1501:	48 89 de             	mov    %rbx,%rsi
    1504:	4c 89 e7             	mov    %r12,%rdi
    1507:	5b                   	pop    %rbx
    1508:	5d                   	pop    %rbp
    1509:	4c 29 e6             	sub    %r12,%rsi
    150c:	41 5c                	pop    %r12
    150e:	41 5d                	pop    %r13
    1510:	41 5e                	pop    %r14
    1512:	e9 c9 fb ff ff       	jmp    10e0 <operator delete(void*, unsigned long)@plt>
    1517:	4c 89 ef             	mov    %r13,%rdi
    151a:	e8 01 fc ff ff       	call   1120 <benchmark::State::StartKeepRunning()@plt>
    151f:	eb d3                	jmp    14f4 <accumulate_bench(benchmark::State&)+0x174>
    1521:	31 c0                	xor    %eax,%eax
    1523:	eb af                	jmp    14d4 <accumulate_bench(benchmark::State&)+0x154>
    1525:	5b                   	pop    %rbx
    1526:	5d                   	pop    %rbp
    1527:	41 5c                	pop    %r12
    1529:	41 5d                	pop    %r13
    152b:	41 5e                	pop    %r14
    152d:	c3                   	ret    
    152e:	31 db                	xor    %ebx,%ebx
    1530:	41 80 7d 1a 00       	cmpb   $0x0,0x1a(%r13)
    1535:	49 89 dc             	mov    %rbx,%r12
    1538:	0f 84 f1 fe ff ff    	je     142f <accumulate_bench(benchmark::State&)+0xaf>
    153e:	eb d7                	jmp    1517 <accumulate_bench(benchmark::State&)+0x197>
    1540:	48 8d 3d 21 0b 00 00 	lea    0xb21(%rip),%rdi        # 2068 <_IO_stdin_used+0x68>
    1547:	e8 34 fb ff ff       	call   1080 <std::__throw_length_error(char const*)@plt>
    154c:	48 8d 0d b5 0a 00 00 	lea    0xab5(%rip),%rcx        # 2008 <_IO_stdin_used+0x8>
    1553:	ba 72 02 00 00       	mov    $0x272,%edx
    1558:	48 8d 35 e1 0a 00 00 	lea    0xae1(%rip),%rsi        # 2040 <_IO_stdin_used+0x40>
    155f:	48 8d 3d 33 0b 00 00 	lea    0xb33(%rip),%rdi        # 2099 <_IO_stdin_used+0x99>
    1566:	e8 35 fb ff ff       	call   10a0 <__assert_fail@plt>
    156b:	48 89 c5             	mov    %rax,%rbp
    156e:	e9 ed fb ff ff       	jmp    1160 <accumulate_bench(benchmark::State&) [clone .cold]>
    1573:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
    157a:	00 00 00 
    157d:	0f 1f 00             	nopl   (%rax)

0000000000001580 <__libc_csu_init>:
    1580:	f3 0f 1e fa          	endbr64 
    1584:	41 57                	push   %r15
    1586:	4c 8d 3d bb 27 00 00 	lea    0x27bb(%rip),%r15        # 3d48 <__frame_dummy_init_array_entry>
    158d:	41 56                	push   %r14
    158f:	49 89 d6             	mov    %rdx,%r14
    1592:	41 55                	push   %r13
    1594:	49 89 f5             	mov    %rsi,%r13
    1597:	41 54                	push   %r12
    1599:	41 89 fc             	mov    %edi,%r12d
    159c:	55                   	push   %rbp
    159d:	48 8d 2d b4 27 00 00 	lea    0x27b4(%rip),%rbp        # 3d58 <__do_global_dtors_aux_fini_array_entry>
    15a4:	53                   	push   %rbx
    15a5:	4c 29 fd             	sub    %r15,%rbp
    15a8:	48 83 ec 08          	sub    $0x8,%rsp
    15ac:	e8 4f fa ff ff       	call   1000 <_init>
    15b1:	48 c1 fd 03          	sar    $0x3,%rbp
    15b5:	74 1f                	je     15d6 <__libc_csu_init+0x56>
    15b7:	31 db                	xor    %ebx,%ebx
    15b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
    15c0:	4c 89 f2             	mov    %r14,%rdx
    15c3:	4c 89 ee             	mov    %r13,%rsi
    15c6:	44 89 e7             	mov    %r12d,%edi
    15c9:	41 ff 14 df          	call   *(%r15,%rbx,8)
    15cd:	48 83 c3 01          	add    $0x1,%rbx
    15d1:	48 39 dd             	cmp    %rbx,%rbp
    15d4:	75 ea                	jne    15c0 <__libc_csu_init+0x40>
    15d6:	48 83 c4 08          	add    $0x8,%rsp
    15da:	5b                   	pop    %rbx
    15db:	5d                   	pop    %rbp
    15dc:	41 5c                	pop    %r12
    15de:	41 5d                	pop    %r13
    15e0:	41 5e                	pop    %r14
    15e2:	41 5f                	pop    %r15
    15e4:	c3                   	ret    
    15e5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
    15ec:	00 00 00 00 

00000000000015f0 <__libc_csu_fini>:
    15f0:	f3 0f 1e fa          	endbr64 
    15f4:	c3                   	ret    

Disassembly of section .fini:

00000000000015f8 <_fini>:
    15f8:	f3 0f 1e fa          	endbr64 
    15fc:	48 83 ec 08          	sub    $0x8,%rsp
    1600:	48 83 c4 08          	add    $0x8,%rsp
    1604:	c3                   	ret    
