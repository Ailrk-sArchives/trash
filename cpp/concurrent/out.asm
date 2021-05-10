
a.out:     file format elf64-x86-64


Disassembly of section .init:

0000000000002000 <_init>:
    2000:	f3 0f 1e fa          	endbr64 
    2004:	48 83 ec 08          	sub    $0x8,%rsp
    2008:	48 8b 05 d9 2f 00 00 	mov    0x2fd9(%rip),%rax        # 4fe8 <__gmon_start__>
    200f:	48 85 c0             	test   %rax,%rax
    2012:	74 02                	je     2016 <_init+0x16>
    2014:	ff d0                	call   *%rax
    2016:	48 83 c4 08          	add    $0x8,%rsp
    201a:	c3                   	ret    

Disassembly of section .plt:

0000000000002020 <std::chrono::_V2::system_clock::now()@plt-0x10>:
    2020:	ff 35 e2 2f 00 00    	push   0x2fe2(%rip)        # 5008 <_GLOBAL_OFFSET_TABLE_+0x8>
    2026:	ff 25 e4 2f 00 00    	jmp    *0x2fe4(%rip)        # 5010 <_GLOBAL_OFFSET_TABLE_+0x10>
    202c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000002030 <std::chrono::_V2::system_clock::now()@plt>:
    2030:	ff 25 e2 2f 00 00    	jmp    *0x2fe2(%rip)        # 5018 <std::chrono::_V2::system_clock::now()@GLIBCXX_3.4.19>
    2036:	68 00 00 00 00       	push   $0x0
    203b:	e9 e0 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002040 <std::terminate()@plt>:
    2040:	ff 25 da 2f 00 00    	jmp    *0x2fda(%rip)        # 5020 <std::terminate()@GLIBCXX_3.4>
    2046:	68 01 00 00 00       	push   $0x1
    204b:	e9 d0 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002050 <std::thread::_State::~_State()@plt>:
    2050:	ff 25 d2 2f 00 00    	jmp    *0x2fd2(%rip)        # 5028 <std::thread::_State::~_State()@GLIBCXX_3.4.22>
    2056:	68 02 00 00 00       	push   $0x2
    205b:	e9 c0 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002060 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@plt>:
    2060:	ff 25 ca 2f 00 00    	jmp    *0x2fca(%rip)        # 5030 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@GLIBCXX_3.4>
    2066:	68 03 00 00 00       	push   $0x3
    206b:	e9 b0 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002070 <std::__throw_length_error(char const*)@plt>:
    2070:	ff 25 c2 2f 00 00    	jmp    *0x2fc2(%rip)        # 5038 <std::__throw_length_error(char const*)@GLIBCXX_3.4>
    2076:	68 04 00 00 00       	push   $0x4
    207b:	e9 a0 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002080 <__cxa_atexit@plt>:
    2080:	ff 25 ba 2f 00 00    	jmp    *0x2fba(%rip)        # 5040 <__cxa_atexit@GLIBC_2.2.5>
    2086:	68 05 00 00 00       	push   $0x5
    208b:	e9 90 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>:
    2090:	ff 25 b2 2f 00 00    	jmp    *0x2fb2(%rip)        # 5048 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@GLIBCXX_3.4.22>
    2096:	68 06 00 00 00       	push   $0x6
    209b:	e9 80 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>:
    20a0:	ff 25 aa 2f 00 00    	jmp    *0x2faa(%rip)        # 5050 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@GLIBCXX_3.4>
    20a6:	68 07 00 00 00       	push   $0x7
    20ab:	e9 70 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020b0 <operator new(unsigned long)@plt>:
    20b0:	ff 25 a2 2f 00 00    	jmp    *0x2fa2(%rip)        # 5058 <operator new(unsigned long)@GLIBCXX_3.4>
    20b6:	68 08 00 00 00       	push   $0x8
    20bb:	e9 60 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020c0 <operator delete(void*, unsigned long)@plt>:
    20c0:	ff 25 9a 2f 00 00    	jmp    *0x2f9a(%rip)        # 5060 <operator delete(void*, unsigned long)@CXXABI_1.3.9>
    20c6:	68 09 00 00 00       	push   $0x9
    20cb:	e9 50 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020d0 <__stack_chk_fail@plt>:
    20d0:	ff 25 92 2f 00 00    	jmp    *0x2f92(%rip)        # 5068 <__stack_chk_fail@GLIBC_2.4>
    20d6:	68 0a 00 00 00       	push   $0xa
    20db:	e9 40 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020e0 <std::ios_base::Init::Init()@plt>:
    20e0:	ff 25 8a 2f 00 00    	jmp    *0x2f8a(%rip)        # 5070 <std::ios_base::Init::Init()@GLIBCXX_3.4>
    20e6:	68 0b 00 00 00       	push   $0xb
    20eb:	e9 30 ff ff ff       	jmp    2020 <_init+0x20>

00000000000020f0 <std::ostream& std::ostream::_M_insert<double>(double)@plt>:
    20f0:	ff 25 82 2f 00 00    	jmp    *0x2f82(%rip)        # 5078 <std::ostream& std::ostream::_M_insert<double>(double)@GLIBCXX_3.4.9>
    20f6:	68 0c 00 00 00       	push   $0xc
    20fb:	e9 20 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002100 <std::thread::join()@plt>:
    2100:	ff 25 7a 2f 00 00    	jmp    *0x2f7a(%rip)        # 5080 <std::thread::join()@GLIBCXX_3.4.11>
    2106:	68 0d 00 00 00       	push   $0xd
    210b:	e9 10 ff ff ff       	jmp    2020 <_init+0x20>

0000000000002110 <_Unwind_Resume@plt>:
    2110:	ff 25 72 2f 00 00    	jmp    *0x2f72(%rip)        # 5088 <_Unwind_Resume@GCC_3.0>
    2116:	68 0e 00 00 00       	push   $0xe
    211b:	e9 00 ff ff ff       	jmp    2020 <_init+0x20>

Disassembly of section .text:

0000000000002120 <_start>:
    2120:	f3 0f 1e fa          	endbr64 
    2124:	31 ed                	xor    %ebp,%ebp
    2126:	49 89 d1             	mov    %rdx,%r9
    2129:	5e                   	pop    %rsi
    212a:	48 89 e2             	mov    %rsp,%rdx
    212d:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
    2131:	50                   	push   %rax
    2132:	54                   	push   %rsp
    2133:	4c 8d 05 a6 0e 00 00 	lea    0xea6(%rip),%r8        # 2fe0 <__libc_csu_fini>
    213a:	48 8d 0d 2f 0e 00 00 	lea    0xe2f(%rip),%rcx        # 2f70 <__libc_csu_init>
    2141:	48 8d 3d d2 0a 00 00 	lea    0xad2(%rip),%rdi        # 2c1a <main>
    2148:	ff 15 92 2e 00 00    	call   *0x2e92(%rip)        # 4fe0 <__libc_start_main@GLIBC_2.2.5>
    214e:	f4                   	hlt    
    214f:	90                   	nop

0000000000002150 <deregister_tm_clones>:
    2150:	48 8d 3d 51 2f 00 00 	lea    0x2f51(%rip),%rdi        # 50a8 <__TMC_END__>
    2157:	48 8d 05 4a 2f 00 00 	lea    0x2f4a(%rip),%rax        # 50a8 <__TMC_END__>
    215e:	48 39 f8             	cmp    %rdi,%rax
    2161:	74 15                	je     2178 <deregister_tm_clones+0x28>
    2163:	48 8b 05 6e 2e 00 00 	mov    0x2e6e(%rip),%rax        # 4fd8 <_ITM_deregisterTMCloneTable>
    216a:	48 85 c0             	test   %rax,%rax
    216d:	74 09                	je     2178 <deregister_tm_clones+0x28>
    216f:	ff e0                	jmp    *%rax
    2171:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
    2178:	c3                   	ret    
    2179:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

0000000000002180 <register_tm_clones>:
    2180:	48 8d 3d 21 2f 00 00 	lea    0x2f21(%rip),%rdi        # 50a8 <__TMC_END__>
    2187:	48 8d 35 1a 2f 00 00 	lea    0x2f1a(%rip),%rsi        # 50a8 <__TMC_END__>
    218e:	48 29 fe             	sub    %rdi,%rsi
    2191:	48 89 f0             	mov    %rsi,%rax
    2194:	48 c1 ee 3f          	shr    $0x3f,%rsi
    2198:	48 c1 f8 03          	sar    $0x3,%rax
    219c:	48 01 c6             	add    %rax,%rsi
    219f:	48 d1 fe             	sar    %rsi
    21a2:	74 14                	je     21b8 <register_tm_clones+0x38>
    21a4:	48 8b 05 45 2e 00 00 	mov    0x2e45(%rip),%rax        # 4ff0 <_ITM_registerTMCloneTable>
    21ab:	48 85 c0             	test   %rax,%rax
    21ae:	74 08                	je     21b8 <register_tm_clones+0x38>
    21b0:	ff e0                	jmp    *%rax
    21b2:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)
    21b8:	c3                   	ret    
    21b9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)

00000000000021c0 <__do_global_dtors_aux>:
    21c0:	f3 0f 1e fa          	endbr64 
    21c4:	80 3d 05 30 00 00 00 	cmpb   $0x0,0x3005(%rip)        # 51d0 <completed.0>
    21cb:	75 33                	jne    2200 <__do_global_dtors_aux+0x40>
    21cd:	55                   	push   %rbp
    21ce:	48 83 3d f2 2d 00 00 	cmpq   $0x0,0x2df2(%rip)        # 4fc8 <__cxa_finalize@GLIBC_2.2.5>
    21d5:	00 
    21d6:	48 89 e5             	mov    %rsp,%rbp
    21d9:	74 0d                	je     21e8 <__do_global_dtors_aux+0x28>
    21db:	48 8b 3d b6 2e 00 00 	mov    0x2eb6(%rip),%rdi        # 5098 <__dso_handle>
    21e2:	ff 15 e0 2d 00 00    	call   *0x2de0(%rip)        # 4fc8 <__cxa_finalize@GLIBC_2.2.5>
    21e8:	e8 63 ff ff ff       	call   2150 <deregister_tm_clones>
    21ed:	c6 05 dc 2f 00 00 01 	movb   $0x1,0x2fdc(%rip)        # 51d0 <completed.0>
    21f4:	5d                   	pop    %rbp
    21f5:	c3                   	ret    
    21f6:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
    21fd:	00 00 00 
    2200:	c3                   	ret    
    2201:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
    2208:	00 00 00 00 
    220c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000002210 <frame_dummy>:
    2210:	f3 0f 1e fa          	endbr64 
    2214:	e9 67 ff ff ff       	jmp    2180 <register_tm_clones>
    2219:	90                   	nop

000000000000221a <std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >::~_State_impl()>:
    221a:	48 83 ec 08          	sub    $0x8,%rsp
    221e:	48 8d 05 53 29 00 00 	lea    0x2953(%rip),%rax        # 4b78 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >+0x10>
    2225:	48 89 07             	mov    %rax,(%rdi)
    2228:	e8 23 fe ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    222d:	48 83 c4 08          	add    $0x8,%rsp
    2231:	c3                   	ret    

0000000000002232 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >::~_State_impl()>:
    2232:	53                   	push   %rbx
    2233:	48 89 fb             	mov    %rdi,%rbx
    2236:	48 8d 05 3b 29 00 00 	lea    0x293b(%rip),%rax        # 4b78 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >+0x10>
    223d:	48 89 07             	mov    %rax,(%rdi)
    2240:	e8 0b fe ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2245:	be 10 00 00 00       	mov    $0x10,%esi
    224a:	48 89 df             	mov    %rbx,%rdi
    224d:	e8 6e fe ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2252:	5b                   	pop    %rbx
    2253:	c3                   	ret    

0000000000002254 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >::~_State_impl()>:
    2254:	48 83 ec 08          	sub    $0x8,%rsp
    2258:	48 8d 05 41 29 00 00 	lea    0x2941(%rip),%rax        # 4ba0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >+0x10>
    225f:	48 89 07             	mov    %rax,(%rdi)
    2262:	e8 e9 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2267:	48 83 c4 08          	add    $0x8,%rsp
    226b:	c3                   	ret    

000000000000226c <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >::~_State_impl()>:
    226c:	53                   	push   %rbx
    226d:	48 89 fb             	mov    %rdi,%rbx
    2270:	48 8d 05 29 29 00 00 	lea    0x2929(%rip),%rax        # 4ba0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >+0x10>
    2277:	48 89 07             	mov    %rax,(%rdi)
    227a:	e8 d1 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    227f:	be 10 00 00 00       	mov    $0x10,%esi
    2284:	48 89 df             	mov    %rbx,%rdi
    2287:	e8 34 fe ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    228c:	5b                   	pop    %rbx
    228d:	c3                   	ret    

000000000000228e <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >::~_State_impl()>:
    228e:	48 83 ec 08          	sub    $0x8,%rsp
    2292:	48 8d 05 2f 29 00 00 	lea    0x292f(%rip),%rax        # 4bc8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >+0x10>
    2299:	48 89 07             	mov    %rax,(%rdi)
    229c:	e8 af fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    22a1:	48 83 c4 08          	add    $0x8,%rsp
    22a5:	c3                   	ret    

00000000000022a6 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >::~_State_impl()>:
    22a6:	53                   	push   %rbx
    22a7:	48 89 fb             	mov    %rdi,%rbx
    22aa:	48 8d 05 17 29 00 00 	lea    0x2917(%rip),%rax        # 4bc8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >+0x10>
    22b1:	48 89 07             	mov    %rax,(%rdi)
    22b4:	e8 97 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    22b9:	be 10 00 00 00       	mov    $0x10,%esi
    22be:	48 89 df             	mov    %rbx,%rdi
    22c1:	e8 fa fd ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    22c6:	5b                   	pop    %rbx
    22c7:	c3                   	ret    

00000000000022c8 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >::~_State_impl()>:
    22c8:	48 83 ec 08          	sub    $0x8,%rsp
    22cc:	48 8d 05 1d 29 00 00 	lea    0x291d(%rip),%rax        # 4bf0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >+0x10>
    22d3:	48 89 07             	mov    %rax,(%rdi)
    22d6:	e8 75 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    22db:	48 83 c4 08          	add    $0x8,%rsp
    22df:	c3                   	ret    

00000000000022e0 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >::~_State_impl()>:
    22e0:	53                   	push   %rbx
    22e1:	48 89 fb             	mov    %rdi,%rbx
    22e4:	48 8d 05 05 29 00 00 	lea    0x2905(%rip),%rax        # 4bf0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >+0x10>
    22eb:	48 89 07             	mov    %rax,(%rdi)
    22ee:	e8 5d fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    22f3:	be 10 00 00 00       	mov    $0x10,%esi
    22f8:	48 89 df             	mov    %rbx,%rdi
    22fb:	e8 c0 fd ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2300:	5b                   	pop    %rbx
    2301:	c3                   	ret    

0000000000002302 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >::~_State_impl()>:
    2302:	48 83 ec 08          	sub    $0x8,%rsp
    2306:	48 8d 05 0b 29 00 00 	lea    0x290b(%rip),%rax        # 4c18 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >+0x10>
    230d:	48 89 07             	mov    %rax,(%rdi)
    2310:	e8 3b fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2315:	48 83 c4 08          	add    $0x8,%rsp
    2319:	c3                   	ret    

000000000000231a <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >::~_State_impl()>:
    231a:	53                   	push   %rbx
    231b:	48 89 fb             	mov    %rdi,%rbx
    231e:	48 8d 05 f3 28 00 00 	lea    0x28f3(%rip),%rax        # 4c18 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >+0x10>
    2325:	48 89 07             	mov    %rax,(%rdi)
    2328:	e8 23 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    232d:	be 10 00 00 00       	mov    $0x10,%esi
    2332:	48 89 df             	mov    %rbx,%rdi
    2335:	e8 86 fd ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    233a:	5b                   	pop    %rbx
    233b:	c3                   	ret    

000000000000233c <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >::~_State_impl()>:
    233c:	48 83 ec 08          	sub    $0x8,%rsp
    2340:	48 8d 05 f9 28 00 00 	lea    0x28f9(%rip),%rax        # 4c40 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >+0x10>
    2347:	48 89 07             	mov    %rax,(%rdi)
    234a:	e8 01 fd ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    234f:	48 83 c4 08          	add    $0x8,%rsp
    2353:	c3                   	ret    

0000000000002354 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >::~_State_impl()>:
    2354:	53                   	push   %rbx
    2355:	48 89 fb             	mov    %rdi,%rbx
    2358:	48 8d 05 e1 28 00 00 	lea    0x28e1(%rip),%rax        # 4c40 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >+0x10>
    235f:	48 89 07             	mov    %rax,(%rdi)
    2362:	e8 e9 fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2367:	be 10 00 00 00       	mov    $0x10,%esi
    236c:	48 89 df             	mov    %rbx,%rdi
    236f:	e8 4c fd ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2374:	5b                   	pop    %rbx
    2375:	c3                   	ret    

0000000000002376 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >::~_State_impl()>:
    2376:	48 83 ec 08          	sub    $0x8,%rsp
    237a:	48 8d 05 e7 28 00 00 	lea    0x28e7(%rip),%rax        # 4c68 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >+0x10>
    2381:	48 89 07             	mov    %rax,(%rdi)
    2384:	e8 c7 fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2389:	48 83 c4 08          	add    $0x8,%rsp
    238d:	c3                   	ret    

000000000000238e <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >::~_State_impl()>:
    238e:	53                   	push   %rbx
    238f:	48 89 fb             	mov    %rdi,%rbx
    2392:	48 8d 05 cf 28 00 00 	lea    0x28cf(%rip),%rax        # 4c68 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >+0x10>
    2399:	48 89 07             	mov    %rax,(%rdi)
    239c:	e8 af fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    23a1:	be 10 00 00 00       	mov    $0x10,%esi
    23a6:	48 89 df             	mov    %rbx,%rdi
    23a9:	e8 12 fd ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    23ae:	5b                   	pop    %rbx
    23af:	c3                   	ret    

00000000000023b0 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >::~_State_impl()>:
    23b0:	48 83 ec 08          	sub    $0x8,%rsp
    23b4:	48 8d 05 d5 28 00 00 	lea    0x28d5(%rip),%rax        # 4c90 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >+0x10>
    23bb:	48 89 07             	mov    %rax,(%rdi)
    23be:	e8 8d fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    23c3:	48 83 c4 08          	add    $0x8,%rsp
    23c7:	c3                   	ret    

00000000000023c8 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >::~_State_impl()>:
    23c8:	53                   	push   %rbx
    23c9:	48 89 fb             	mov    %rdi,%rbx
    23cc:	48 8d 05 bd 28 00 00 	lea    0x28bd(%rip),%rax        # 4c90 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >+0x10>
    23d3:	48 89 07             	mov    %rax,(%rdi)
    23d6:	e8 75 fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    23db:	be 10 00 00 00       	mov    $0x10,%esi
    23e0:	48 89 df             	mov    %rbx,%rdi
    23e3:	e8 d8 fc ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    23e8:	5b                   	pop    %rbx
    23e9:	c3                   	ret    

00000000000023ea <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >::~_State_impl()>:
    23ea:	48 83 ec 08          	sub    $0x8,%rsp
    23ee:	48 8d 05 c3 28 00 00 	lea    0x28c3(%rip),%rax        # 4cb8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x10>
    23f5:	48 89 07             	mov    %rax,(%rdi)
    23f8:	e8 53 fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    23fd:	48 83 c4 08          	add    $0x8,%rsp
    2401:	c3                   	ret    

0000000000002402 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >::~_State_impl()>:
    2402:	53                   	push   %rbx
    2403:	48 89 fb             	mov    %rdi,%rbx
    2406:	48 8d 05 ab 28 00 00 	lea    0x28ab(%rip),%rax        # 4cb8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x10>
    240d:	48 89 07             	mov    %rax,(%rdi)
    2410:	e8 3b fc ff ff       	call   2050 <std::thread::_State::~_State()@plt>
    2415:	be 10 00 00 00       	mov    $0x10,%esi
    241a:	48 89 df             	mov    %rbx,%rdi
    241d:	e8 9e fc ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2422:	5b                   	pop    %rbx
    2423:	c3                   	ret    

0000000000002424 <work(std::atomic<int>&)>:
    2424:	b8 a0 86 01 00       	mov    $0x186a0,%eax
    2429:	f0 83 07 01          	lock addl $0x1,(%rdi)
    242d:	83 e8 01             	sub    $0x1,%eax
    2430:	75 f7                	jne    2429 <work(std::atomic<int>&)+0x5>
    2432:	c3                   	ret    
    2433:	90                   	nop

0000000000002434 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >::_M_run()>:
    2434:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2438:	e8 e7 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    243d:	c3                   	ret    

000000000000243e <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >::_M_run()>:
    243e:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2442:	e8 dd ff ff ff       	call   2424 <work(std::atomic<int>&)>
    2447:	c3                   	ret    

0000000000002448 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >::_M_run()>:
    2448:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    244c:	e8 d3 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    2451:	c3                   	ret    

0000000000002452 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >::_M_run()>:
    2452:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2456:	e8 c9 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    245b:	c3                   	ret    

000000000000245c <std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >::_M_run()>:
    245c:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2460:	e8 bf ff ff ff       	call   2424 <work(std::atomic<int>&)>
    2465:	c3                   	ret    

0000000000002466 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >::_M_run()>:
    2466:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    246a:	e8 b5 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    246f:	c3                   	ret    

0000000000002470 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >::_M_run()>:
    2470:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2474:	e8 ab ff ff ff       	call   2424 <work(std::atomic<int>&)>
    2479:	c3                   	ret    

000000000000247a <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >::_M_run()>:
    247a:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    247e:	e8 a1 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    2483:	c3                   	ret    

0000000000002484 <std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >::_M_run()>:
    2484:	48 8b 7f 08          	mov    0x8(%rdi),%rdi
    2488:	e8 97 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    248d:	c3                   	ret    

000000000000248e <single_thread()>:
    248e:	53                   	push   %rbx
    248f:	48 83 ec 10          	sub    $0x10,%rsp
    2493:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
    249a:	00 00 
    249c:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
    24a1:	31 c0                	xor    %eax,%eax
    24a3:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
    24aa:	00 
    24ab:	87 44 24 04          	xchg   %eax,0x4(%rsp)
    24af:	48 8d 5c 24 04       	lea    0x4(%rsp),%rbx
    24b4:	48 89 df             	mov    %rbx,%rdi
    24b7:	e8 68 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    24bc:	48 89 df             	mov    %rbx,%rdi
    24bf:	e8 60 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    24c4:	48 89 df             	mov    %rbx,%rdi
    24c7:	e8 58 ff ff ff       	call   2424 <work(std::atomic<int>&)>
    24cc:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
    24d1:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
    24d8:	00 00 
    24da:	75 06                	jne    24e2 <single_thread()+0x54>
    24dc:	48 83 c4 10          	add    $0x10,%rsp
    24e0:	5b                   	pop    %rbx
    24e1:	c3                   	ret    
    24e2:	e8 e9 fb ff ff       	call   20d0 <__stack_chk_fail@plt>

00000000000024e7 <false_sharing()>:
    24e7:	53                   	push   %rbx
    24e8:	48 83 ec 40          	sub    $0x40,%rsp
    24ec:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
    24f3:	00 00 
    24f5:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    24fa:	31 c0                	xor    %eax,%eax
    24fc:	c7 04 24 00 00 00 00 	movl   $0x0,(%rsp)
    2503:	c7 44 24 04 00 00 00 	movl   $0x0,0x4(%rsp)
    250a:	00 
    250b:	c7 44 24 08 00 00 00 	movl   $0x0,0x8(%rsp)
    2512:	00 
    2513:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
    251a:	00 
    251b:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
    2522:	00 00 
    2524:	bf 10 00 00 00       	mov    $0x10,%edi
    2529:	e8 82 fb ff ff       	call   20b0 <operator new(unsigned long)@plt>
    252e:	48 8d 0d 6b 26 00 00 	lea    0x266b(%rip),%rcx        # 4ba0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#1}> > >+0x10>
    2535:	48 89 08             	mov    %rcx,(%rax)
    2538:	48 89 e2             	mov    %rsp,%rdx
    253b:	48 89 50 08          	mov    %rdx,0x8(%rax)
    253f:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
    2544:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
    2549:	48 8d 7c 24 10       	lea    0x10(%rsp),%rdi
    254e:	48 8b 15 7b 2a 00 00 	mov    0x2a7b(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2555:	e8 36 fb ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    255a:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
    255f:	48 85 ff             	test   %rdi,%rdi
    2562:	74 06                	je     256a <false_sharing()+0x83>
    2564:	48 8b 07             	mov    (%rdi),%rax
    2567:	ff 50 08             	call   *0x8(%rax)
    256a:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
    2571:	00 00 
    2573:	bf 10 00 00 00       	mov    $0x10,%edi
    2578:	e8 33 fb ff ff       	call   20b0 <operator new(unsigned long)@plt>
    257d:	48 8d 1d 44 26 00 00 	lea    0x2644(%rip),%rbx        # 4bc8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#2}> > >+0x10>
    2584:	48 89 18             	mov    %rbx,(%rax)
    2587:	48 8d 54 24 04       	lea    0x4(%rsp),%rdx
    258c:	48 89 50 08          	mov    %rdx,0x8(%rax)
    2590:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
    2595:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
    259a:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
    259f:	48 8b 15 2a 2a 00 00 	mov    0x2a2a(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    25a6:	e8 e5 fa ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    25ab:	eb 15                	jmp    25c2 <false_sharing()+0xdb>
    25ad:	48 89 c3             	mov    %rax,%rbx
    25b0:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    25b5:	e8 2a 08 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    25ba:	48 89 df             	mov    %rbx,%rdi
    25bd:	e8 4e fb ff ff       	call   2110 <_Unwind_Resume@plt>
    25c2:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
    25c7:	48 85 ff             	test   %rdi,%rdi
    25ca:	74 06                	je     25d2 <false_sharing()+0xeb>
    25cc:	48 8b 07             	mov    (%rdi),%rax
    25cf:	ff 50 08             	call   *0x8(%rax)
    25d2:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
    25d9:	00 00 
    25db:	bf 10 00 00 00       	mov    $0x10,%edi
    25e0:	e8 cb fa ff ff       	call   20b0 <operator new(unsigned long)@plt>
    25e5:	48 8d 0d 04 26 00 00 	lea    0x2604(%rip),%rcx        # 4bf0 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#3}> > >+0x10>
    25ec:	48 89 08             	mov    %rcx,(%rax)
    25ef:	48 8d 54 24 08       	lea    0x8(%rsp),%rdx
    25f4:	48 89 50 08          	mov    %rdx,0x8(%rax)
    25f8:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
    25fd:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
    2602:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
    2607:	48 8b 15 c2 29 00 00 	mov    0x29c2(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    260e:	e8 7d fa ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    2613:	eb 21                	jmp    2636 <false_sharing()+0x14f>
    2615:	48 89 c3             	mov    %rax,%rbx
    2618:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    261d:	e8 c2 07 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    2622:	48 89 df             	mov    %rbx,%rdi
    2625:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
    262b:	0f 84 3e 01 00 00    	je     276f <false_sharing()+0x288>
    2631:	e8 0a fa ff ff       	call   2040 <std::terminate()@plt>
    2636:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
    263b:	48 85 ff             	test   %rdi,%rdi
    263e:	74 06                	je     2646 <false_sharing()+0x15f>
    2640:	48 8b 07             	mov    (%rdi),%rax
    2643:	ff 50 08             	call   *0x8(%rax)
    2646:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
    264d:	00 00 
    264f:	bf 10 00 00 00       	mov    $0x10,%edi
    2654:	e8 57 fa ff ff       	call   20b0 <operator new(unsigned long)@plt>
    2659:	48 8d 1d b8 25 00 00 	lea    0x25b8(%rip),%rbx        # 4c18 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<false_sharing()::{lambda()#4}> > >+0x10>
    2660:	48 89 18             	mov    %rbx,(%rax)
    2663:	48 8d 54 24 0c       	lea    0xc(%rsp),%rdx
    2668:	48 89 50 08          	mov    %rdx,0x8(%rax)
    266c:	48 89 44 24 30       	mov    %rax,0x30(%rsp)
    2671:	48 8d 74 24 30       	lea    0x30(%rsp),%rsi
    2676:	48 8d 7c 24 28       	lea    0x28(%rsp),%rdi
    267b:	48 8b 15 4e 29 00 00 	mov    0x294e(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2682:	e8 09 fa ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    2687:	eb 1d                	jmp    26a6 <false_sharing()+0x1bf>
    2689:	48 89 c3             	mov    %rax,%rbx
    268c:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    2691:	e8 4e 07 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    2696:	48 89 df             	mov    %rbx,%rdi
    2699:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
    269f:	74 84                	je     2625 <false_sharing()+0x13e>
    26a1:	e8 9a f9 ff ff       	call   2040 <std::terminate()@plt>
    26a6:	48 8b 7c 24 30       	mov    0x30(%rsp),%rdi
    26ab:	48 85 ff             	test   %rdi,%rdi
    26ae:	74 06                	je     26b6 <false_sharing()+0x1cf>
    26b0:	48 8b 07             	mov    (%rdi),%rax
    26b3:	ff 50 08             	call   *0x8(%rax)
    26b6:	48 8d 7c 24 10       	lea    0x10(%rsp),%rdi
    26bb:	e8 40 fa ff ff       	call   2100 <std::thread::join()@plt>
    26c0:	eb 1d                	jmp    26df <false_sharing()+0x1f8>
    26c2:	48 89 c3             	mov    %rax,%rbx
    26c5:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    26ca:	e8 15 07 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    26cf:	48 89 df             	mov    %rbx,%rdi
    26d2:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
    26d8:	74 bf                	je     2699 <false_sharing()+0x1b2>
    26da:	e8 61 f9 ff ff       	call   2040 <std::terminate()@plt>
    26df:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
    26e4:	e8 17 fa ff ff       	call   2100 <std::thread::join()@plt>
    26e9:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
    26ee:	e8 0d fa ff ff       	call   2100 <std::thread::join()@plt>
    26f3:	48 8d 7c 24 28       	lea    0x28(%rsp),%rdi
    26f8:	e8 03 fa ff ff       	call   2100 <std::thread::join()@plt>
    26fd:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
    2703:	75 2e                	jne    2733 <false_sharing()+0x24c>
    2705:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
    270b:	75 2b                	jne    2738 <false_sharing()+0x251>
    270d:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
    2713:	75 28                	jne    273d <false_sharing()+0x256>
    2715:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
    271b:	75 25                	jne    2742 <false_sharing()+0x25b>
    271d:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
    2722:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
    2729:	00 00 
    272b:	75 47                	jne    2774 <false_sharing()+0x28d>
    272d:	48 83 c4 40          	add    $0x40,%rsp
    2731:	5b                   	pop    %rbx
    2732:	c3                   	ret    
    2733:	e8 08 f9 ff ff       	call   2040 <std::terminate()@plt>
    2738:	e8 03 f9 ff ff       	call   2040 <std::terminate()@plt>
    273d:	e8 fe f8 ff ff       	call   2040 <std::terminate()@plt>
    2742:	e8 f9 f8 ff ff       	call   2040 <std::terminate()@plt>
    2747:	48 89 c7             	mov    %rax,%rdi
    274a:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
    2750:	74 80                	je     26d2 <false_sharing()+0x1eb>
    2752:	e8 e9 f8 ff ff       	call   2040 <std::terminate()@plt>
    2757:	48 89 c7             	mov    %rax,%rdi
    275a:	e9 73 ff ff ff       	jmp    26d2 <false_sharing()+0x1eb>
    275f:	48 89 c7             	mov    %rax,%rdi
    2762:	e9 32 ff ff ff       	jmp    2699 <false_sharing()+0x1b2>
    2767:	48 89 c7             	mov    %rax,%rdi
    276a:	e9 b6 fe ff ff       	jmp    2625 <false_sharing()+0x13e>
    276f:	e8 9c f9 ff ff       	call   2110 <_Unwind_Resume@plt>
    2774:	e8 57 f9 ff ff       	call   20d0 <__stack_chk_fail@plt>

0000000000002779 <no_sharing()>:
    2779:	55                   	push   %rbp
    277a:	48 89 e5             	mov    %rsp,%rbp
    277d:	53                   	push   %rbx
    277e:	48 83 e4 c0          	and    $0xffffffffffffffc0,%rsp
    2782:	48 81 ec 80 01 00 00 	sub    $0x180,%rsp
    2789:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
    2790:	00 00 
    2792:	48 89 84 24 78 01 00 	mov    %rax,0x178(%rsp)
    2799:	00 
    279a:	31 c0                	xor    %eax,%eax
    279c:	c7 44 24 40 00 00 00 	movl   $0x0,0x40(%rsp)
    27a3:	00 
    27a4:	89 c1                	mov    %eax,%ecx
    27a6:	87 4c 24 40          	xchg   %ecx,0x40(%rsp)
    27aa:	c7 84 24 80 00 00 00 	movl   $0x0,0x80(%rsp)
    27b1:	00 00 00 00 
    27b5:	89 c3                	mov    %eax,%ebx
    27b7:	87 9c 24 80 00 00 00 	xchg   %ebx,0x80(%rsp)
    27be:	c7 84 24 c0 00 00 00 	movl   $0x0,0xc0(%rsp)
    27c5:	00 00 00 00 
    27c9:	89 c1                	mov    %eax,%ecx
    27cb:	87 8c 24 c0 00 00 00 	xchg   %ecx,0xc0(%rsp)
    27d2:	c7 84 24 00 01 00 00 	movl   $0x0,0x100(%rsp)
    27d9:	00 00 00 00 
    27dd:	87 84 24 00 01 00 00 	xchg   %eax,0x100(%rsp)
    27e4:	48 c7 44 24 18 00 00 	movq   $0x0,0x18(%rsp)
    27eb:	00 00 
    27ed:	bf 10 00 00 00       	mov    $0x10,%edi
    27f2:	e8 b9 f8 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    27f7:	48 8d 1d 42 24 00 00 	lea    0x2442(%rip),%rbx        # 4c40 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#1}> > >+0x10>
    27fe:	48 89 18             	mov    %rbx,(%rax)
    2801:	48 8d 54 24 40       	lea    0x40(%rsp),%rdx
    2806:	48 89 50 08          	mov    %rdx,0x8(%rax)
    280a:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    280f:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
    2814:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
    2819:	48 8b 15 b0 27 00 00 	mov    0x27b0(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2820:	e8 6b f8 ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    2825:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
    282a:	48 85 ff             	test   %rdi,%rdi
    282d:	74 06                	je     2835 <no_sharing()+0xbc>
    282f:	48 8b 07             	mov    (%rdi),%rax
    2832:	ff 50 08             	call   *0x8(%rax)
    2835:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
    283c:	00 00 
    283e:	bf 10 00 00 00       	mov    $0x10,%edi
    2843:	e8 68 f8 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    2848:	48 8d 0d 19 24 00 00 	lea    0x2419(%rip),%rcx        # 4c68 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#2}> > >+0x10>
    284f:	48 89 08             	mov    %rcx,(%rax)
    2852:	48 8d 94 24 80 00 00 	lea    0x80(%rsp),%rdx
    2859:	00 
    285a:	48 89 50 08          	mov    %rdx,0x8(%rax)
    285e:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    2863:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
    2868:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
    286d:	48 8b 15 5c 27 00 00 	mov    0x275c(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2874:	e8 17 f8 ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    2879:	eb 15                	jmp    2890 <no_sharing()+0x117>
    287b:	48 89 c3             	mov    %rax,%rbx
    287e:	48 8d 7c 24 38       	lea    0x38(%rsp),%rdi
    2883:	e8 5c 05 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    2888:	48 89 df             	mov    %rbx,%rdi
    288b:	e8 80 f8 ff ff       	call   2110 <_Unwind_Resume@plt>
    2890:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
    2895:	48 85 ff             	test   %rdi,%rdi
    2898:	74 06                	je     28a0 <no_sharing()+0x127>
    289a:	48 8b 07             	mov    (%rdi),%rax
    289d:	ff 50 08             	call   *0x8(%rax)
    28a0:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
    28a7:	00 00 
    28a9:	bf 10 00 00 00       	mov    $0x10,%edi
    28ae:	e8 fd f7 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    28b3:	48 8d 1d d6 23 00 00 	lea    0x23d6(%rip),%rbx        # 4c90 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#3}> > >+0x10>
    28ba:	48 89 18             	mov    %rbx,(%rax)
    28bd:	48 8d 94 24 c0 00 00 	lea    0xc0(%rsp),%rdx
    28c4:	00 
    28c5:	48 89 50 08          	mov    %rdx,0x8(%rax)
    28c9:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    28ce:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
    28d3:	48 8d 7c 24 28       	lea    0x28(%rsp),%rdi
    28d8:	48 8b 15 f1 26 00 00 	mov    0x26f1(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    28df:	e8 ac f7 ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    28e4:	eb 21                	jmp    2907 <no_sharing()+0x18e>
    28e6:	48 89 c3             	mov    %rax,%rbx
    28e9:	48 8d 7c 24 38       	lea    0x38(%rsp),%rdi
    28ee:	e8 f1 04 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    28f3:	48 89 df             	mov    %rbx,%rdi
    28f6:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
    28fc:	0f 84 48 01 00 00    	je     2a4a <no_sharing()+0x2d1>
    2902:	e8 39 f7 ff ff       	call   2040 <std::terminate()@plt>
    2907:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
    290c:	48 85 ff             	test   %rdi,%rdi
    290f:	74 06                	je     2917 <no_sharing()+0x19e>
    2911:	48 8b 07             	mov    (%rdi),%rax
    2914:	ff 50 08             	call   *0x8(%rax)
    2917:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
    291e:	00 00 
    2920:	bf 10 00 00 00       	mov    $0x10,%edi
    2925:	e8 86 f7 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    292a:	48 8d 0d 87 23 00 00 	lea    0x2387(%rip),%rcx        # 4cb8 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x10>
    2931:	48 89 08             	mov    %rcx,(%rax)
    2934:	48 8d 94 24 00 01 00 	lea    0x100(%rsp),%rdx
    293b:	00 
    293c:	48 89 50 08          	mov    %rdx,0x8(%rax)
    2940:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    2945:	48 8d 74 24 38       	lea    0x38(%rsp),%rsi
    294a:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    294f:	48 8b 15 7a 26 00 00 	mov    0x267a(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2956:	e8 35 f7 ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    295b:	eb 1d                	jmp    297a <no_sharing()+0x201>
    295d:	48 89 c3             	mov    %rax,%rbx
    2960:	48 8d 7c 24 38       	lea    0x38(%rsp),%rdi
    2965:	e8 7a 04 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    296a:	48 89 df             	mov    %rbx,%rdi
    296d:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
    2973:	74 81                	je     28f6 <no_sharing()+0x17d>
    2975:	e8 c6 f6 ff ff       	call   2040 <std::terminate()@plt>
    297a:	48 8b 7c 24 38       	mov    0x38(%rsp),%rdi
    297f:	48 85 ff             	test   %rdi,%rdi
    2982:	74 06                	je     298a <no_sharing()+0x211>
    2984:	48 8b 07             	mov    (%rdi),%rax
    2987:	ff 50 08             	call   *0x8(%rax)
    298a:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
    298f:	e8 6c f7 ff ff       	call   2100 <std::thread::join()@plt>
    2994:	eb 1d                	jmp    29b3 <no_sharing()+0x23a>
    2996:	48 89 c3             	mov    %rax,%rbx
    2999:	48 8d 7c 24 38       	lea    0x38(%rsp),%rdi
    299e:	e8 41 04 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    29a3:	48 89 df             	mov    %rbx,%rdi
    29a6:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
    29ac:	74 bf                	je     296d <no_sharing()+0x1f4>
    29ae:	e8 8d f6 ff ff       	call   2040 <std::terminate()@plt>
    29b3:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
    29b8:	e8 43 f7 ff ff       	call   2100 <std::thread::join()@plt>
    29bd:	48 8d 7c 24 28       	lea    0x28(%rsp),%rdi
    29c2:	e8 39 f7 ff ff       	call   2100 <std::thread::join()@plt>
    29c7:	48 8d 7c 24 30       	lea    0x30(%rsp),%rdi
    29cc:	e8 2f f7 ff ff       	call   2100 <std::thread::join()@plt>
    29d1:	48 83 7c 24 30 00    	cmpq   $0x0,0x30(%rsp)
    29d7:	75 31                	jne    2a0a <no_sharing()+0x291>
    29d9:	48 83 7c 24 28 00    	cmpq   $0x0,0x28(%rsp)
    29df:	75 2e                	jne    2a0f <no_sharing()+0x296>
    29e1:	48 83 7c 24 20 00    	cmpq   $0x0,0x20(%rsp)
    29e7:	75 2b                	jne    2a14 <no_sharing()+0x29b>
    29e9:	48 83 7c 24 18 00    	cmpq   $0x0,0x18(%rsp)
    29ef:	75 28                	jne    2a19 <no_sharing()+0x2a0>
    29f1:	48 8b 84 24 78 01 00 	mov    0x178(%rsp),%rax
    29f8:	00 
    29f9:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
    2a00:	00 00 
    2a02:	75 4b                	jne    2a4f <no_sharing()+0x2d6>
    2a04:	48 8b 5d f8          	mov    -0x8(%rbp),%rbx
    2a08:	c9                   	leave  
    2a09:	c3                   	ret    
    2a0a:	e8 31 f6 ff ff       	call   2040 <std::terminate()@plt>
    2a0f:	e8 2c f6 ff ff       	call   2040 <std::terminate()@plt>
    2a14:	e8 27 f6 ff ff       	call   2040 <std::terminate()@plt>
    2a19:	e8 22 f6 ff ff       	call   2040 <std::terminate()@plt>
    2a1e:	48 89 c7             	mov    %rax,%rdi
    2a21:	48 83 7c 24 30 00    	cmpq   $0x0,0x30(%rsp)
    2a27:	0f 84 79 ff ff ff    	je     29a6 <no_sharing()+0x22d>
    2a2d:	e8 0e f6 ff ff       	call   2040 <std::terminate()@plt>
    2a32:	48 89 c7             	mov    %rax,%rdi
    2a35:	e9 6c ff ff ff       	jmp    29a6 <no_sharing()+0x22d>
    2a3a:	48 89 c7             	mov    %rax,%rdi
    2a3d:	e9 2b ff ff ff       	jmp    296d <no_sharing()+0x1f4>
    2a42:	48 89 c7             	mov    %rax,%rdi
    2a45:	e9 ac fe ff ff       	jmp    28f6 <no_sharing()+0x17d>
    2a4a:	e8 c1 f6 ff ff       	call   2110 <_Unwind_Resume@plt>
    2a4f:	e8 7c f6 ff ff       	call   20d0 <__stack_chk_fail@plt>

0000000000002a54 <direct_sharing()>:
    2a54:	55                   	push   %rbp
    2a55:	53                   	push   %rbx
    2a56:	48 83 ec 48          	sub    $0x48,%rsp
    2a5a:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
    2a61:	00 00 
    2a63:	48 89 44 24 38       	mov    %rax,0x38(%rsp)
    2a68:	31 c0                	xor    %eax,%eax
    2a6a:	c7 44 24 0c 00 00 00 	movl   $0x0,0xc(%rsp)
    2a71:	00 
    2a72:	87 44 24 0c          	xchg   %eax,0xc(%rsp)
    2a76:	48 c7 44 24 20 00 00 	movq   $0x0,0x20(%rsp)
    2a7d:	00 00 
    2a7f:	48 c7 44 24 28 00 00 	movq   $0x0,0x28(%rsp)
    2a86:	00 00 
    2a88:	48 c7 44 24 30 00 00 	movq   $0x0,0x30(%rsp)
    2a8f:	00 00 
    2a91:	bb 03 00 00 00       	mov    $0x3,%ebx
    2a96:	48 8d 2d db 20 00 00 	lea    0x20db(%rip),%rbp        # 4b78 <vtable for std::thread::_State_impl<std::thread::_Invoker<std::tuple<direct_sharing()::{lambda()#1}> > >+0x10>
    2a9d:	eb 5d                	jmp    2afc <direct_sharing()+0xa8>
    2a9f:	48 89 28             	mov    %rbp,(%rax)
    2aa2:	48 8d 54 24 0c       	lea    0xc(%rsp),%rdx
    2aa7:	48 89 50 08          	mov    %rdx,0x8(%rax)
    2aab:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
    2ab0:	48 8d 74 24 18       	lea    0x18(%rsp),%rsi
    2ab5:	48 8d 7c 24 10       	lea    0x10(%rsp),%rdi
    2aba:	48 8b 15 0f 25 00 00 	mov    0x250f(%rip),%rdx        # 4fd0 <pthread_create@GLIBC_2.2.5>
    2ac1:	e8 ca f5 ff ff       	call   2090 <std::thread::_M_start_thread(std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >, void (*)())@plt>
    2ac6:	48 8b 7c 24 18       	mov    0x18(%rsp),%rdi
    2acb:	48 85 ff             	test   %rdi,%rdi
    2ace:	74 06                	je     2ad6 <direct_sharing()+0x82>
    2ad0:	48 8b 07             	mov    (%rdi),%rax
    2ad3:	ff 50 08             	call   *0x8(%rax)
    2ad6:	48 8b 74 24 28       	mov    0x28(%rsp),%rsi
    2adb:	48 3b 74 24 30       	cmp    0x30(%rsp),%rsi
    2ae0:	74 5e                	je     2b40 <direct_sharing()+0xec>
    2ae2:	48 c7 06 00 00 00 00 	movq   $0x0,(%rsi)
    2ae9:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
    2aee:	48 89 06             	mov    %rax,(%rsi)
    2af1:	48 83 44 24 28 08    	addq   $0x8,0x28(%rsp)
    2af7:	83 eb 01             	sub    $0x1,%ebx
    2afa:	74 60                	je     2b5c <direct_sharing()+0x108>
    2afc:	48 c7 44 24 10 00 00 	movq   $0x0,0x10(%rsp)
    2b03:	00 00 
    2b05:	bf 10 00 00 00       	mov    $0x10,%edi
    2b0a:	e8 a1 f5 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    2b0f:	eb 8e                	jmp    2a9f <direct_sharing()+0x4b>
    2b11:	48 89 c3             	mov    %rax,%rbx
    2b14:	48 8d 7c 24 18       	lea    0x18(%rsp),%rdi
    2b19:	e8 c6 02 00 00       	call   2de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>
    2b1e:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
    2b23:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
    2b28:	48 39 c2             	cmp    %rax,%rdx
    2b2b:	0f 84 c5 00 00 00    	je     2bf6 <direct_sharing()+0x1a2>
    2b31:	48 83 38 00          	cmpq   $0x0,(%rax)
    2b35:	0f 84 b2 00 00 00    	je     2bed <direct_sharing()+0x199>
    2b3b:	e8 00 f5 ff ff       	call   2040 <std::terminate()@plt>
    2b40:	48 8d 54 24 10       	lea    0x10(%rsp),%rdx
    2b45:	48 8d 7c 24 20       	lea    0x20(%rsp),%rdi
    2b4a:	e8 ad 02 00 00       	call   2dfc <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)>
    2b4f:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
    2b55:	74 a0                	je     2af7 <direct_sharing()+0xa3>
    2b57:	e8 e4 f4 ff ff       	call   2040 <std::terminate()@plt>
    2b5c:	48 8b 5c 24 20       	mov    0x20(%rsp),%rbx
    2b61:	48 8b 6c 24 28       	mov    0x28(%rsp),%rbp
    2b66:	48 39 dd             	cmp    %rbx,%rbp
    2b69:	75 0b                	jne    2b76 <direct_sharing()+0x122>
    2b6b:	eb 31                	jmp    2b9e <direct_sharing()+0x14a>
    2b6d:	48 83 c3 08          	add    $0x8,%rbx
    2b71:	48 39 dd             	cmp    %rbx,%rbp
    2b74:	74 0a                	je     2b80 <direct_sharing()+0x12c>
    2b76:	48 89 df             	mov    %rbx,%rdi
    2b79:	e8 82 f5 ff ff       	call   2100 <std::thread::join()@plt>
    2b7e:	eb ed                	jmp    2b6d <direct_sharing()+0x119>
    2b80:	48 8b 54 24 28       	mov    0x28(%rsp),%rdx
    2b85:	48 8b 44 24 20       	mov    0x20(%rsp),%rax
    2b8a:	48 39 c2             	cmp    %rax,%rdx
    2b8d:	74 0f                	je     2b9e <direct_sharing()+0x14a>
    2b8f:	48 83 38 00          	cmpq   $0x0,(%rax)
    2b93:	75 37                	jne    2bcc <direct_sharing()+0x178>
    2b95:	48 83 c0 08          	add    $0x8,%rax
    2b99:	48 39 c2             	cmp    %rax,%rdx
    2b9c:	75 f1                	jne    2b8f <direct_sharing()+0x13b>
    2b9e:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
    2ba3:	48 85 ff             	test   %rdi,%rdi
    2ba6:	74 0d                	je     2bb5 <direct_sharing()+0x161>
    2ba8:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
    2bad:	48 29 fe             	sub    %rdi,%rsi
    2bb0:	e8 0b f5 ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2bb5:	48 8b 44 24 38       	mov    0x38(%rsp),%rax
    2bba:	64 48 2b 04 25 28 00 	sub    %fs:0x28,%rax
    2bc1:	00 00 
    2bc3:	75 50                	jne    2c15 <direct_sharing()+0x1c1>
    2bc5:	48 83 c4 48          	add    $0x48,%rsp
    2bc9:	5b                   	pop    %rbx
    2bca:	5d                   	pop    %rbp
    2bcb:	c3                   	ret    
    2bcc:	e8 6f f4 ff ff       	call   2040 <std::terminate()@plt>
    2bd1:	48 89 c3             	mov    %rax,%rbx
    2bd4:	48 83 7c 24 10 00    	cmpq   $0x0,0x10(%rsp)
    2bda:	0f 84 3e ff ff ff    	je     2b1e <direct_sharing()+0xca>
    2be0:	e8 5b f4 ff ff       	call   2040 <std::terminate()@plt>
    2be5:	48 89 c3             	mov    %rax,%rbx
    2be8:	e9 31 ff ff ff       	jmp    2b1e <direct_sharing()+0xca>
    2bed:	48 83 c0 08          	add    $0x8,%rax
    2bf1:	e9 32 ff ff ff       	jmp    2b28 <direct_sharing()+0xd4>
    2bf6:	48 8b 7c 24 20       	mov    0x20(%rsp),%rdi
    2bfb:	48 8b 74 24 30       	mov    0x30(%rsp),%rsi
    2c00:	48 29 fe             	sub    %rdi,%rsi
    2c03:	48 85 ff             	test   %rdi,%rdi
    2c06:	74 05                	je     2c0d <direct_sharing()+0x1b9>
    2c08:	e8 b3 f4 ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2c0d:	48 89 df             	mov    %rbx,%rdi
    2c10:	e8 fb f4 ff ff       	call   2110 <_Unwind_Resume@plt>
    2c15:	e8 b6 f4 ff ff       	call   20d0 <__stack_chk_fail@plt>

0000000000002c1a <main>:
    2c1a:	53                   	push   %rbx
    2c1b:	48 83 ec 10          	sub    $0x10,%rsp
    2c1f:	e8 0c f4 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2c24:	48 89 c3             	mov    %rax,%rbx
    2c27:	e8 62 f8 ff ff       	call   248e <single_thread()>
    2c2c:	e8 ff f3 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2c31:	48 29 d8             	sub    %rbx,%rax
    2c34:	66 0f ef c0          	pxor   %xmm0,%xmm0
    2c38:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
    2c3d:	f2 0f 5e 05 6b 07 00 	divsd  0x76b(%rip),%xmm0        # 33b0 <typeinfo name for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x50>
    2c44:	00 
    2c45:	66 48 0f 7e c3       	movq   %xmm0,%rbx
    2c4a:	48 8d 35 cd 03 00 00 	lea    0x3cd(%rip),%rsi        # 301e <_IO_stdin_used+0x1e>
    2c51:	48 8d 3d 68 24 00 00 	lea    0x2468(%rip),%rdi        # 50c0 <std::cout@@GLIBCXX_3.4>
    2c58:	e8 43 f4 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2c5d:	48 89 c7             	mov    %rax,%rdi
    2c60:	66 48 0f 6e c3       	movq   %rbx,%xmm0
    2c65:	e8 86 f4 ff ff       	call   20f0 <std::ostream& std::ostream::_M_insert<double>(double)@plt>
    2c6a:	48 89 c7             	mov    %rax,%rdi
    2c6d:	48 8d 35 ba 03 00 00 	lea    0x3ba(%rip),%rsi        # 302e <_IO_stdin_used+0x2e>
    2c74:	e8 27 f4 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2c79:	48 89 c7             	mov    %rax,%rdi
    2c7c:	e8 df f3 ff ff       	call   2060 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@plt>
    2c81:	e8 aa f3 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2c86:	48 89 c3             	mov    %rax,%rbx
    2c89:	e8 c6 fd ff ff       	call   2a54 <direct_sharing()>
    2c8e:	e8 9d f3 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2c93:	48 29 d8             	sub    %rbx,%rax
    2c96:	66 0f ef c0          	pxor   %xmm0,%xmm0
    2c9a:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
    2c9f:	f2 0f 5e 05 09 07 00 	divsd  0x709(%rip),%xmm0        # 33b0 <typeinfo name for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x50>
    2ca6:	00 
    2ca7:	66 48 0f 7e c3       	movq   %xmm0,%rbx
    2cac:	48 8d 35 7e 03 00 00 	lea    0x37e(%rip),%rsi        # 3031 <_IO_stdin_used+0x31>
    2cb3:	48 8d 3d 06 24 00 00 	lea    0x2406(%rip),%rdi        # 50c0 <std::cout@@GLIBCXX_3.4>
    2cba:	e8 e1 f3 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2cbf:	48 89 c7             	mov    %rax,%rdi
    2cc2:	66 48 0f 6e c3       	movq   %rbx,%xmm0
    2cc7:	e8 24 f4 ff ff       	call   20f0 <std::ostream& std::ostream::_M_insert<double>(double)@plt>
    2ccc:	48 89 c7             	mov    %rax,%rdi
    2ccf:	48 8d 35 58 03 00 00 	lea    0x358(%rip),%rsi        # 302e <_IO_stdin_used+0x2e>
    2cd6:	e8 c5 f3 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2cdb:	48 89 c7             	mov    %rax,%rdi
    2cde:	e8 7d f3 ff ff       	call   2060 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@plt>
    2ce3:	e8 48 f3 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2ce8:	48 89 c3             	mov    %rax,%rbx
    2ceb:	e8 f7 f7 ff ff       	call   24e7 <false_sharing()>
    2cf0:	e8 3b f3 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2cf5:	48 29 d8             	sub    %rbx,%rax
    2cf8:	66 0f ef c0          	pxor   %xmm0,%xmm0
    2cfc:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
    2d01:	f2 0f 5e 05 a7 06 00 	divsd  0x6a7(%rip),%xmm0        # 33b0 <typeinfo name for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x50>
    2d08:	00 
    2d09:	66 48 0f 7e c3       	movq   %xmm0,%rbx
    2d0e:	48 8d 35 2d 03 00 00 	lea    0x32d(%rip),%rsi        # 3042 <_IO_stdin_used+0x42>
    2d15:	48 8d 3d a4 23 00 00 	lea    0x23a4(%rip),%rdi        # 50c0 <std::cout@@GLIBCXX_3.4>
    2d1c:	e8 7f f3 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2d21:	48 89 c7             	mov    %rax,%rdi
    2d24:	66 48 0f 6e c3       	movq   %rbx,%xmm0
    2d29:	e8 c2 f3 ff ff       	call   20f0 <std::ostream& std::ostream::_M_insert<double>(double)@plt>
    2d2e:	48 89 c7             	mov    %rax,%rdi
    2d31:	48 8d 35 f6 02 00 00 	lea    0x2f6(%rip),%rsi        # 302e <_IO_stdin_used+0x2e>
    2d38:	e8 63 f3 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2d3d:	48 89 c7             	mov    %rax,%rdi
    2d40:	e8 1b f3 ff ff       	call   2060 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@plt>
    2d45:	e8 e6 f2 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2d4a:	48 89 c3             	mov    %rax,%rbx
    2d4d:	e8 27 fa ff ff       	call   2779 <no_sharing()>
    2d52:	e8 d9 f2 ff ff       	call   2030 <std::chrono::_V2::system_clock::now()@plt>
    2d57:	48 29 d8             	sub    %rbx,%rax
    2d5a:	66 0f ef c0          	pxor   %xmm0,%xmm0
    2d5e:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0
    2d63:	f2 0f 5e 05 45 06 00 	divsd  0x645(%rip),%xmm0        # 33b0 <typeinfo name for std::thread::_State_impl<std::thread::_Invoker<std::tuple<no_sharing()::{lambda()#4}> > >+0x50>
    2d6a:	00 
    2d6b:	f2 0f 11 44 24 08    	movsd  %xmm0,0x8(%rsp)
    2d71:	48 8d 35 da 02 00 00 	lea    0x2da(%rip),%rsi        # 3052 <_IO_stdin_used+0x52>
    2d78:	48 8d 3d 41 23 00 00 	lea    0x2341(%rip),%rdi        # 50c0 <std::cout@@GLIBCXX_3.4>
    2d7f:	e8 1c f3 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2d84:	48 89 c7             	mov    %rax,%rdi
    2d87:	f2 0f 10 44 24 08    	movsd  0x8(%rsp),%xmm0
    2d8d:	e8 5e f3 ff ff       	call   20f0 <std::ostream& std::ostream::_M_insert<double>(double)@plt>
    2d92:	48 89 c7             	mov    %rax,%rdi
    2d95:	48 8d 35 92 02 00 00 	lea    0x292(%rip),%rsi        # 302e <_IO_stdin_used+0x2e>
    2d9c:	e8 ff f2 ff ff       	call   20a0 <std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@plt>
    2da1:	48 89 c7             	mov    %rax,%rdi
    2da4:	e8 b7 f2 ff ff       	call   2060 <std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@plt>
    2da9:	b8 00 00 00 00       	mov    $0x0,%eax
    2dae:	48 83 c4 10          	add    $0x10,%rsp
    2db2:	5b                   	pop    %rbx
    2db3:	c3                   	ret    

0000000000002db4 <_GLOBAL__sub_I__Z4workRSt6atomicIiE>:
    2db4:	48 83 ec 08          	sub    $0x8,%rsp
    2db8:	48 8d 3d 12 24 00 00 	lea    0x2412(%rip),%rdi        # 51d1 <std::__ioinit>
    2dbf:	e8 1c f3 ff ff       	call   20e0 <std::ios_base::Init::Init()@plt>
    2dc4:	48 8d 15 cd 22 00 00 	lea    0x22cd(%rip),%rdx        # 5098 <__dso_handle>
    2dcb:	48 8d 35 ff 23 00 00 	lea    0x23ff(%rip),%rsi        # 51d1 <std::__ioinit>
    2dd2:	48 8b 3d 1f 22 00 00 	mov    0x221f(%rip),%rdi        # 4ff8 <std::ios_base::Init::~Init()@GLIBCXX_3.4>
    2dd9:	e8 a2 f2 ff ff       	call   2080 <__cxa_atexit@plt>
    2dde:	48 83 c4 08          	add    $0x8,%rsp
    2de2:	c3                   	ret    
    2de3:	90                   	nop

0000000000002de4 <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()>:
    2de4:	48 8b 3f             	mov    (%rdi),%rdi
    2de7:	48 85 ff             	test   %rdi,%rdi
    2dea:	74 0f                	je     2dfb <std::unique_ptr<std::thread::_State, std::default_delete<std::thread::_State> >::~unique_ptr()+0x17>
    2dec:	48 83 ec 08          	sub    $0x8,%rsp
    2df0:	48 8b 07             	mov    (%rdi),%rax
    2df3:	ff 50 08             	call   *0x8(%rax)
    2df6:	48 83 c4 08          	add    $0x8,%rsp
    2dfa:	c3                   	ret    
    2dfb:	c3                   	ret    

0000000000002dfc <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)>:
    2dfc:	41 57                	push   %r15
    2dfe:	41 56                	push   %r14
    2e00:	41 55                	push   %r13
    2e02:	41 54                	push   %r12
    2e04:	55                   	push   %rbp
    2e05:	53                   	push   %rbx
    2e06:	48 83 ec 28          	sub    $0x28,%rsp
    2e0a:	48 89 54 24 08       	mov    %rdx,0x8(%rsp)
    2e0f:	48 8b 6f 08          	mov    0x8(%rdi),%rbp
    2e13:	4c 8b 37             	mov    (%rdi),%r14
    2e16:	48 89 e8             	mov    %rbp,%rax
    2e19:	4c 29 f0             	sub    %r14,%rax
    2e1c:	48 c1 f8 03          	sar    $0x3,%rax
    2e20:	48 89 c1             	mov    %rax,%rcx
    2e23:	48 b8 ff ff ff ff ff 	movabs $0xfffffffffffffff,%rax
    2e2a:	ff ff 0f 
    2e2d:	48 39 c1             	cmp    %rax,%rcx
    2e30:	74 48                	je     2e7a <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x7e>
    2e32:	49 89 fd             	mov    %rdi,%r13
    2e35:	49 89 f4             	mov    %rsi,%r12
    2e38:	48 89 f3             	mov    %rsi,%rbx
    2e3b:	48 85 c9             	test   %rcx,%rcx
    2e3e:	ba 01 00 00 00       	mov    $0x1,%edx
    2e43:	48 89 d0             	mov    %rdx,%rax
    2e46:	48 0f 45 c1          	cmovne %rcx,%rax
    2e4a:	48 01 c8             	add    %rcx,%rax
    2e4d:	72 3c                	jb     2e8b <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x8f>
    2e4f:	48 ba ff ff ff ff ff 	movabs $0xfffffffffffffff,%rdx
    2e56:	ff ff 0f 
    2e59:	48 39 d0             	cmp    %rdx,%rax
    2e5c:	48 0f 46 d0          	cmovbe %rax,%rdx
    2e60:	48 89 54 24 10       	mov    %rdx,0x10(%rsp)
    2e65:	4c 29 f6             	sub    %r14,%rsi
    2e68:	48 89 74 24 18       	mov    %rsi,0x18(%rsp)
    2e6d:	41 bf 00 00 00 00    	mov    $0x0,%r15d
    2e73:	48 85 c0             	test   %rax,%rax
    2e76:	74 42                	je     2eba <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0xbe>
    2e78:	eb 2b                	jmp    2ea5 <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0xa9>
    2e7a:	48 8d 3d 83 01 00 00 	lea    0x183(%rip),%rdi        # 3004 <_IO_stdin_used+0x4>
    2e81:	e8 ea f1 ff ff       	call   2070 <std::__throw_length_error(char const*)@plt>
    2e86:	4c 89 f8             	mov    %r15,%rax
    2e89:	eb 6f                	jmp    2efa <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0xfe>
    2e8b:	48 89 f0             	mov    %rsi,%rax
    2e8e:	4c 29 f0             	sub    %r14,%rax
    2e91:	48 89 44 24 18       	mov    %rax,0x18(%rsp)
    2e96:	48 b8 ff ff ff ff ff 	movabs $0xfffffffffffffff,%rax
    2e9d:	ff ff 0f 
    2ea0:	48 89 44 24 10       	mov    %rax,0x10(%rsp)
    2ea5:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
    2eaa:	48 8d 3c c5 00 00 00 	lea    0x0(,%rax,8),%rdi
    2eb1:	00 
    2eb2:	e8 f9 f1 ff ff       	call   20b0 <operator new(unsigned long)@plt>
    2eb7:	49 89 c7             	mov    %rax,%r15
    2eba:	48 8b 7c 24 08       	mov    0x8(%rsp),%rdi
    2ebf:	48 8b 07             	mov    (%rdi),%rax
    2ec2:	48 8b 74 24 18       	mov    0x18(%rsp),%rsi
    2ec7:	49 89 04 37          	mov    %rax,(%r15,%rsi,1)
    2ecb:	48 c7 07 00 00 00 00 	movq   $0x0,(%rdi)
    2ed2:	4d 39 f4             	cmp    %r14,%r12
    2ed5:	74 af                	je     2e86 <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x8a>
    2ed7:	4c 89 e6             	mov    %r12,%rsi
    2eda:	4c 29 f6             	sub    %r14,%rsi
    2edd:	4c 89 fa             	mov    %r15,%rdx
    2ee0:	4c 89 f0             	mov    %r14,%rax
    2ee3:	48 8b 08             	mov    (%rax),%rcx
    2ee6:	48 89 0a             	mov    %rcx,(%rdx)
    2ee9:	48 83 c0 08          	add    $0x8,%rax
    2eed:	48 83 c2 08          	add    $0x8,%rdx
    2ef1:	48 39 d8             	cmp    %rbx,%rax
    2ef4:	75 ed                	jne    2ee3 <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0xe7>
    2ef6:	49 8d 04 37          	lea    (%r15,%rsi,1),%rax
    2efa:	48 83 c0 08          	add    $0x8,%rax
    2efe:	48 89 44 24 08       	mov    %rax,0x8(%rsp)
    2f03:	49 39 ec             	cmp    %rbp,%r12
    2f06:	74 1e                	je     2f26 <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x12a>
    2f08:	48 89 e9             	mov    %rbp,%rcx
    2f0b:	4c 29 e1             	sub    %r12,%rcx
    2f0e:	48 8b 13             	mov    (%rbx),%rdx
    2f11:	48 89 10             	mov    %rdx,(%rax)
    2f14:	48 83 c3 08          	add    $0x8,%rbx
    2f18:	48 83 c0 08          	add    $0x8,%rax
    2f1c:	48 39 eb             	cmp    %rbp,%rbx
    2f1f:	75 ed                	jne    2f0e <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x112>
    2f21:	48 01 4c 24 08       	add    %rcx,0x8(%rsp)
    2f26:	4d 85 f6             	test   %r14,%r14
    2f29:	74 0f                	je     2f3a <void std::vector<std::thread, std::allocator<std::thread> >::_M_realloc_insert<std::thread>(__gnu_cxx::__normal_iterator<std::thread*, std::vector<std::thread, std::allocator<std::thread> > >, std::thread&&)+0x13e>
    2f2b:	49 8b 75 10          	mov    0x10(%r13),%rsi
    2f2f:	4c 29 f6             	sub    %r14,%rsi
    2f32:	4c 89 f7             	mov    %r14,%rdi
    2f35:	e8 86 f1 ff ff       	call   20c0 <operator delete(void*, unsigned long)@plt>
    2f3a:	4d 89 7d 00          	mov    %r15,0x0(%r13)
    2f3e:	48 8b 44 24 08       	mov    0x8(%rsp),%rax
    2f43:	49 89 45 08          	mov    %rax,0x8(%r13)
    2f47:	48 8b 44 24 10       	mov    0x10(%rsp),%rax
    2f4c:	49 8d 04 c7          	lea    (%r15,%rax,8),%rax
    2f50:	49 89 45 10          	mov    %rax,0x10(%r13)
    2f54:	48 83 c4 28          	add    $0x28,%rsp
    2f58:	5b                   	pop    %rbx
    2f59:	5d                   	pop    %rbp
    2f5a:	41 5c                	pop    %r12
    2f5c:	41 5d                	pop    %r13
    2f5e:	41 5e                	pop    %r14
    2f60:	41 5f                	pop    %r15
    2f62:	c3                   	ret    
    2f63:	66 2e 0f 1f 84 00 00 	cs nopw 0x0(%rax,%rax,1)
    2f6a:	00 00 00 
    2f6d:	0f 1f 00             	nopl   (%rax)

0000000000002f70 <__libc_csu_init>:
    2f70:	f3 0f 1e fa          	endbr64 
    2f74:	41 57                	push   %r15
    2f76:	4c 8d 3d d3 1b 00 00 	lea    0x1bd3(%rip),%r15        # 4b50 <__frame_dummy_init_array_entry>
    2f7d:	41 56                	push   %r14
    2f7f:	49 89 d6             	mov    %rdx,%r14
    2f82:	41 55                	push   %r13
    2f84:	49 89 f5             	mov    %rsi,%r13
    2f87:	41 54                	push   %r12
    2f89:	41 89 fc             	mov    %edi,%r12d
    2f8c:	55                   	push   %rbp
    2f8d:	48 8d 2d cc 1b 00 00 	lea    0x1bcc(%rip),%rbp        # 4b60 <__do_global_dtors_aux_fini_array_entry>
    2f94:	53                   	push   %rbx
    2f95:	4c 29 fd             	sub    %r15,%rbp
    2f98:	48 83 ec 08          	sub    $0x8,%rsp
    2f9c:	e8 5f f0 ff ff       	call   2000 <_init>
    2fa1:	48 c1 fd 03          	sar    $0x3,%rbp
    2fa5:	74 1f                	je     2fc6 <__libc_csu_init+0x56>
    2fa7:	31 db                	xor    %ebx,%ebx
    2fa9:	0f 1f 80 00 00 00 00 	nopl   0x0(%rax)
    2fb0:	4c 89 f2             	mov    %r14,%rdx
    2fb3:	4c 89 ee             	mov    %r13,%rsi
    2fb6:	44 89 e7             	mov    %r12d,%edi
    2fb9:	41 ff 14 df          	call   *(%r15,%rbx,8)
    2fbd:	48 83 c3 01          	add    $0x1,%rbx
    2fc1:	48 39 dd             	cmp    %rbx,%rbp
    2fc4:	75 ea                	jne    2fb0 <__libc_csu_init+0x40>
    2fc6:	48 83 c4 08          	add    $0x8,%rsp
    2fca:	5b                   	pop    %rbx
    2fcb:	5d                   	pop    %rbp
    2fcc:	41 5c                	pop    %r12
    2fce:	41 5d                	pop    %r13
    2fd0:	41 5e                	pop    %r14
    2fd2:	41 5f                	pop    %r15
    2fd4:	c3                   	ret    
    2fd5:	66 66 2e 0f 1f 84 00 	data16 cs nopw 0x0(%rax,%rax,1)
    2fdc:	00 00 00 00 

0000000000002fe0 <__libc_csu_fini>:
    2fe0:	f3 0f 1e fa          	endbr64 
    2fe4:	c3                   	ret    

Disassembly of section .fini:

0000000000002fe8 <_fini>:
    2fe8:	f3 0f 1e fa          	endbr64 
    2fec:	48 83 ec 08          	sub    $0x8,%rsp
    2ff0:	48 83 c4 08          	add    $0x8,%rsp
    2ff4:	c3                   	ret    
