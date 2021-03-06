// MQ2Lua.cpp : Defines the exported functions for the DLL application.
//
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <time.h>
#include <locale.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <intrin.h>
#include <limits.h>
#include <stdint.h>
#include <assert.h>
#include <setjmp.h>
#pragma warning(disable:4996)
namespace Lua5 {
extern "C" {
#define lapi_c
#define LUA_CORE
#ifndef lprefix_h
#define lprefix_h
#if !defined(LUA_USE_C89)	
#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE           600
#elif _XOPEN_SOURCE == 0
#undef _XOPEN_SOURCE  
#endif
#if !defined(LUA_32BITS) && !defined(_FILE_OFFSET_BITS)
#define _LARGEFILE_SOURCE       1
#define _FILE_OFFSET_BITS       64
#endif
#endif				
#if defined(_WIN32) 	
#if !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS  
#endif
#endif			
#endif
#include <stdarg.h>
#include <string.h>
#ifndef lua_h
#define lua_h
#include <stdarg.h>
#include <stddef.h>
#ifndef luaconf_h
#define luaconf_h
#include <limits.h>
#include <stddef.h>
#if !defined(LUA_USE_C89) && defined(_WIN32) && !defined(_WIN32_WCE)
#define LUA_USE_WINDOWS  
#endif
#if defined(LUA_USE_WINDOWS)
#define LUA_DL_DLL	
#define LUA_USE_C89	
#endif
#if defined(LUA_USE_LINUX)
#define LUA_USE_POSIX
#define LUA_USE_DLOPEN		
#define LUA_USE_READLINE	
#endif
#if defined(LUA_USE_MACOSX)
#define LUA_USE_POSIX
#define LUA_USE_DLOPEN		
#define LUA_USE_READLINE	
#endif
#if defined(LUA_USE_C89) && !defined(LUA_USE_WINDOWS)
#define LUA_C89_NUMBERS
#endif
#if ((INT_MAX >> 15) >> 15) >= 1
#define LUAI_BITSINT	32
#else
#define LUAI_BITSINT	16
#endif
#define LUA_INT_INT		1
#define LUA_INT_LONG		2
#define LUA_INT_LONGLONG	3
#define LUA_FLOAT_FLOAT		1
#define LUA_FLOAT_DOUBLE	2
#define LUA_FLOAT_LONGDOUBLE	3
#if defined(LUA_32BITS)		
#if LUAI_BITSINT >= 32  
#define LUA_INT_TYPE	LUA_INT_INT
#else  
#define LUA_INT_TYPE	LUA_INT_LONG
#endif
#define LUA_FLOAT_TYPE	LUA_FLOAT_FLOAT
#elif defined(LUA_C89_NUMBERS)	
#define LUA_INT_TYPE	LUA_INT_LONG
#define LUA_FLOAT_TYPE	LUA_FLOAT_DOUBLE
#endif				
#if !defined(LUA_INT_TYPE)
#define LUA_INT_TYPE	LUA_INT_LONGLONG
#endif
#if !defined(LUA_FLOAT_TYPE)
#define LUA_FLOAT_TYPE	LUA_FLOAT_DOUBLE
#endif
#define LUA_PATH_SEP            ";"
#define LUA_PATH_MARK           "?"
#define LUA_EXEC_DIR            "!"
#define LUA_VDIR	LUA_VERSION_MAJOR "." LUA_VERSION_MINOR
#if defined(_WIN32)	
#define LUA_LDIR	"!\\lua\\"
#define LUA_CDIR	"!\\"
#define LUA_SHRDIR	"!\\..\\share\\lua\\" LUA_VDIR "\\"
#define LUA_PATH_DEFAULT  \
LUA_LDIR"?.lua;"  LUA_LDIR"?\\init.lua;" \
LUA_CDIR"?.lua;"  LUA_CDIR"?\\init.lua;" \
LUA_SHRDIR"?.lua;" LUA_SHRDIR"?\\init.lua;" \
".\\?.lua;" ".\\?\\init.lua"
#define LUA_CPATH_DEFAULT \
LUA_CDIR"?.dll;" \
LUA_CDIR"..\\lib\\lua\\" LUA_VDIR "\\?.dll;" \
LUA_CDIR"loadall.dll;" ".\\?.dll"
#else			
#define LUA_ROOT	"/usr/local/"
#define LUA_LDIR	LUA_ROOT "share/lua/" LUA_VDIR "/"
#define LUA_CDIR	LUA_ROOT "lib/lua/" LUA_VDIR "/"
#define LUA_PATH_DEFAULT  \
LUA_LDIR"?.lua;"  LUA_LDIR"?/init.lua;" \
LUA_CDIR"?.lua;"  LUA_CDIR"?/init.lua;" \
"./?.lua;" "./?/init.lua"
#define LUA_CPATH_DEFAULT \
LUA_CDIR"?.so;" LUA_CDIR"loadall.so;" "./?.so"
#endif			
#if defined(_WIN32)
#define LUA_DIRSEP	"\\"
#else
#define LUA_DIRSEP	"/"
#endif
#if defined(LUA_BUILD_AS_DLL)	
#if defined(LUA_CORE) || defined(LUA_LIB)	
#define LUA_API __declspec(dllexport)
#else						
#define LUA_API __declspec(dllimport)
#endif						
#else				
#define LUA_API		extern
#endif				
#define LUALIB_API	LUA_API
#define LUAMOD_API	LUALIB_API
#if defined(__GNUC__) && ((__GNUC__*100 + __GNUC_MINOR__) >= 302) && \
defined(__ELF__)		
#define LUAI_FUNC	__attribute__((visibility("hidden"))) extern
#else				
#define LUAI_FUNC	extern
#endif				
#define LUAI_DDEC	LUAI_FUNC
#define LUAI_DDEF	
#if defined(LUA_COMPAT_5_2)	
#define LUA_COMPAT_MATHLIB
#define LUA_COMPAT_BITLIB
#define LUA_COMPAT_IPAIRS
#define LUA_COMPAT_APIINTCASTS
#endif				
#if defined(LUA_COMPAT_5_1)	
#define LUA_COMPAT_MATHLIB
#define LUA_COMPAT_APIINTCASTS
#define LUA_COMPAT_UNPACK
#define LUA_COMPAT_LOADERS
#define lua_cpcall(L,f,u)  \
(lua_pushcfunction(L, (f)), \
lua_pushlightuserdata(L,(u)), \
lua_pcall(L,1,0,0))
#define LUA_COMPAT_LOG10
#define LUA_COMPAT_LOADSTRING
#define LUA_COMPAT_MAXN
#define lua_strlen(L,i)		lua_rawlen(L, (i))
#define lua_objlen(L,i)		lua_rawlen(L, (i))
#define lua_equal(L,idx1,idx2)		lua_compare(L,(idx1),(idx2),LUA_OPEQ)
#define lua_lessthan(L,idx1,idx2)	lua_compare(L,(idx1),(idx2),LUA_OPLT)
#define LUA_COMPAT_MODULE
#endif				
#define l_floor(x)		(l_mathop(floor)(x))
#define lua_number2str(s,sz,n)  \
l_sprintf((s), sz, LUA_NUMBER_FMT, (LUAI_UACNUMBER)(n))
#define lua_numbertointeger(n,p) \
((n) >= (LUA_NUMBER)(LUA_MININTEGER) && \
(n) < -(LUA_NUMBER)(LUA_MININTEGER) && \
(*(p) = (LUA_INTEGER)(n), 1))
#if LUA_FLOAT_TYPE == LUA_FLOAT_FLOAT		
#define LUA_NUMBER	float
#define l_mathlim(n)		(FLT_##n)
#define LUAI_UACNUMBER	double
#define LUA_NUMBER_FRMLEN	""
#define LUA_NUMBER_FMT		"%.7g"
#define l_mathop(op)		op##f
#define lua_str2number(s,p)	strtof((s), (p))
#elif LUA_FLOAT_TYPE == LUA_FLOAT_LONGDOUBLE	
#define LUA_NUMBER	long double
#define l_mathlim(n)		(LDBL_##n)
#define LUAI_UACNUMBER	long double
#define LUA_NUMBER_FRMLEN	"L"
#define LUA_NUMBER_FMT		"%.19Lg"
#define l_mathop(op)		op##l
#define lua_str2number(s,p)	strtold((s), (p))
#elif LUA_FLOAT_TYPE == LUA_FLOAT_DOUBLE	
#define LUA_NUMBER	double
#define l_mathlim(n)		(DBL_##n)
#define LUAI_UACNUMBER	double
#define LUA_NUMBER_FRMLEN	""
#define LUA_NUMBER_FMT		"%.14g"
#define l_mathop(op)		op
#define lua_str2number(s,p)	strtod((s), (p))
#else						
#error "numeric float type not defined"
#endif					
#define LUA_INTEGER_FMT		"%" LUA_INTEGER_FRMLEN "d"
#define LUAI_UACINT		LUA_INTEGER
#define lua_integer2str(s,sz,n)  \
l_sprintf((s), sz, LUA_INTEGER_FMT, (LUAI_UACINT)(n))
#define LUA_UNSIGNED		unsigned LUAI_UACINT
#if LUA_INT_TYPE == LUA_INT_INT		
#define LUA_INTEGER		int
#define LUA_INTEGER_FRMLEN	""
#define LUA_MAXINTEGER		INT_MAX
#define LUA_MININTEGER		INT_MIN
#elif LUA_INT_TYPE == LUA_INT_LONG	
#define LUA_INTEGER		long
#define LUA_INTEGER_FRMLEN	"l"
#define LUA_MAXINTEGER		LONG_MAX
#define LUA_MININTEGER		LONG_MIN
#elif LUA_INT_TYPE == LUA_INT_LONGLONG	
#if defined(LLONG_MAX)		
#define LUA_INTEGER		long long
#define LUA_INTEGER_FRMLEN	"ll"
#define LUA_MAXINTEGER		LLONG_MAX
#define LUA_MININTEGER		LLONG_MIN
#elif defined(LUA_USE_WINDOWS) 
#define LUA_INTEGER		__int64
#define LUA_INTEGER_FRMLEN	"I64"
#define LUA_MAXINTEGER		_I64_MAX
#define LUA_MININTEGER		_I64_MIN
#else				
#error "Compiler does not support 'long long'. Use option '-DLUA_32BITS' \
or '-DLUA_C89_NUMBERS' (see file 'luaconf.h' for details)"
#endif				
#else				
#error "numeric integer type not defined"
#endif				
#if !defined(LUA_USE_C89)
#define l_sprintf(s,sz,f,i)	snprintf(s,sz,f,i)
#else
#define l_sprintf(s,sz,f,i)	((void)(sz), sprintf(s,f,i))
#endif
#if !defined(LUA_USE_C89)
#define lua_strx2number(s,p)		lua_str2number(s,p)
#endif
#define lua_pointer2str(buff,sz,p)	l_sprintf(buff,sz,"%p",p)
#if !defined(LUA_USE_C89)
#define lua_number2strx(L,b,sz,f,n)  \
((void)L, l_sprintf(b,sz,f,(LUAI_UACNUMBER)(n)))
#endif
#if defined(LUA_USE_C89) || (defined(HUGE_VAL) && !defined(HUGE_VALF))
#undef l_mathop  
#undef lua_str2number
#define l_mathop(op)		(lua_Number)op  
#define lua_str2number(s,p)	((lua_Number)strtod((s), (p)))
#endif
#define LUA_KCONTEXT	ptrdiff_t
#if !defined(LUA_USE_C89) && defined(__STDC_VERSION__) && \
__STDC_VERSION__ >= 199901L
#include <stdint.h>
#if defined(INTPTR_MAX)  
#undef LUA_KCONTEXT
#define LUA_KCONTEXT	intptr_t
#endif
#endif
#if !defined(lua_getlocaledecpoint)
#define lua_getlocaledecpoint()		(localeconv()->decimal_point[0])
#endif
#if defined(LUA_USE_APICHECK)
#include <assert.h>
#define luai_apicheck(l,e)	assert(e)
#endif
#if LUAI_BITSINT >= 32
#define LUAI_MAXSTACK		1000000
#else
#define LUAI_MAXSTACK		15000
#endif
#define LUA_EXTRASPACE		(sizeof(void *))
#define LUA_IDSIZE	60
#if LUA_FLOAT_TYPE == LUA_FLOAT_LONGDOUBLE
#define LUAL_BUFFERSIZE		8192
#else
#define LUAL_BUFFERSIZE   ((int)(0x80 * sizeof(void*) * sizeof(lua_Integer)))
#endif
#define LUA_QL(x)	"'" x "'"
#define LUA_QS		LUA_QL("%s")
#endif
#define LUA_VERSION_MAJOR	"5"
#define LUA_VERSION_MINOR	"3"
#define LUA_VERSION_NUM		503
#define LUA_VERSION_RELEASE	"5"
#define LUA_VERSION	"Lua " LUA_VERSION_MAJOR "." LUA_VERSION_MINOR
#define LUA_RELEASE	LUA_VERSION "." LUA_VERSION_RELEASE
#define LUA_COPYRIGHT	LUA_RELEASE "  Copyright (C) 1994-2018 Lua.org, PUC-Rio"
#define LUA_AUTHORS	"R. Ierusalimschy, L. H. de Figueiredo, W. Celes"
#define LUA_SIGNATURE	"\x1bLua"
#define LUA_MULTRET	(-1)
#define LUA_REGISTRYINDEX	(-LUAI_MAXSTACK - 1000)
#define lua_upvalueindex(i)	(LUA_REGISTRYINDEX - (i))
#define LUA_OK		0
#define LUA_YIELD	1
#define LUA_ERRRUN	2
#define LUA_ERRSYNTAX	3
#define LUA_ERRMEM	4
#define LUA_ERRGCMM	5
#define LUA_ERRERR	6
typedef struct lua_State lua_State;
#define LUA_TNONE		(-1)
#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8
#define LUA_NUMTAGS		9
#define LUA_MINSTACK	20
#define LUA_RIDX_MAINTHREAD	1
#define LUA_RIDX_GLOBALS	2
#define LUA_RIDX_LAST		LUA_RIDX_GLOBALS
typedef LUA_NUMBER lua_Number;
typedef LUA_INTEGER lua_Integer;
typedef LUA_UNSIGNED lua_Unsigned;
typedef LUA_KCONTEXT lua_KContext;
typedef int (*lua_CFunction) (lua_State *L);
typedef int (*lua_KFunction) (lua_State *L, int status, lua_KContext ctx);
typedef const char * (*lua_Reader) (lua_State *L, void *ud, size_t *sz);
typedef int (*lua_Writer) (lua_State *L, const void *p, size_t sz, void *ud);
typedef void * (*lua_Alloc) (void *ud, void *ptr, size_t osize, size_t nsize);
#if defined(LUA_USER_H)
#include LUA_USER_H
#endif
extern const char lua_ident[];
LUA_API lua_State *(lua_newstate) (lua_Alloc f, void *ud);
LUA_API void       (lua_close) (lua_State *L);
LUA_API lua_State *(lua_newthread) (lua_State *L);
LUA_API lua_CFunction (lua_atpanic) (lua_State *L, lua_CFunction panicf);
LUA_API const lua_Number *(lua_version) (lua_State *L);
LUA_API int   (lua_absindex) (lua_State *L, int idx);
LUA_API int   (lua_gettop) (lua_State *L);
LUA_API void  (lua_settop) (lua_State *L, int idx);
LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
LUA_API void  (lua_rotate) (lua_State *L, int idx, int n);
LUA_API void  (lua_copy) (lua_State *L, int fromidx, int toidx);
LUA_API int   (lua_checkstack) (lua_State *L, int n);
LUA_API void  (lua_xmove) (lua_State *from, lua_State *to, int n);
LUA_API int             (lua_isnumber) (lua_State *L, int idx);
LUA_API int             (lua_isstring) (lua_State *L, int idx);
LUA_API int             (lua_iscfunction) (lua_State *L, int idx);
LUA_API int             (lua_isinteger) (lua_State *L, int idx);
LUA_API int             (lua_isuserdata) (lua_State *L, int idx);
LUA_API int             (lua_type) (lua_State *L, int idx);
LUA_API const char     *(lua_typename) (lua_State *L, int tp);
LUA_API lua_Number      (lua_tonumberx) (lua_State *L, int idx, int *isnum);
LUA_API lua_Integer     (lua_tointegerx) (lua_State *L, int idx, int *isnum);
LUA_API int             (lua_toboolean) (lua_State *L, int idx);
LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
LUA_API size_t          (lua_rawlen) (lua_State *L, int idx);
LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
LUA_API const void     *(lua_topointer) (lua_State *L, int idx);
#define LUA_OPADD	0	
#define LUA_OPSUB	1
#define LUA_OPMUL	2
#define LUA_OPMOD	3
#define LUA_OPPOW	4
#define LUA_OPDIV	5
#define LUA_OPIDIV	6
#define LUA_OPBAND	7
#define LUA_OPBOR	8
#define LUA_OPBXOR	9
#define LUA_OPSHL	10
#define LUA_OPSHR	11
#define LUA_OPUNM	12
#define LUA_OPBNOT	13
LUA_API void  (lua_arith) (lua_State *L, int op);
#define LUA_OPEQ	0
#define LUA_OPLT	1
#define LUA_OPLE	2
LUA_API int   (lua_rawequal) (lua_State *L, int idx1, int idx2);
LUA_API int   (lua_compare) (lua_State *L, int idx1, int idx2, int op);
LUA_API void        (lua_pushnil) (lua_State *L);
LUA_API void        (lua_pushnumber) (lua_State *L, lua_Number n);
LUA_API void        (lua_pushinteger) (lua_State *L, lua_Integer n);
LUA_API const char *(lua_pushlstring) (lua_State *L, const char *s, size_t len);
LUA_API const char *(lua_pushstring) (lua_State *L, const char *s);
LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt,
va_list argp);
LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);
LUA_API void  (lua_pushcclosure) (lua_State *L, lua_CFunction fn, int n);
LUA_API void  (lua_pushboolean) (lua_State *L, int b);
LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *p);
LUA_API int   (lua_pushthread) (lua_State *L);
LUA_API int (lua_getglobal) (lua_State *L, const char *name);
LUA_API int (lua_gettable) (lua_State *L, int idx);
LUA_API int (lua_getfield) (lua_State *L, int idx, const char *k);
LUA_API int (lua_geti) (lua_State *L, int idx, lua_Integer n);
LUA_API int (lua_rawget) (lua_State *L, int idx);
LUA_API int (lua_rawgeti) (lua_State *L, int idx, lua_Integer n);
LUA_API int (lua_rawgetp) (lua_State *L, int idx, const void *p);
LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
LUA_API void *(lua_newuserdata) (lua_State *L, size_t sz);
LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
LUA_API int  (lua_getuservalue) (lua_State *L, int idx);
LUA_API void  (lua_setglobal) (lua_State *L, const char *name);
LUA_API void  (lua_settable) (lua_State *L, int idx);
LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
LUA_API void  (lua_seti) (lua_State *L, int idx, lua_Integer n);
LUA_API void  (lua_rawset) (lua_State *L, int idx);
LUA_API void  (lua_rawseti) (lua_State *L, int idx, lua_Integer n);
LUA_API void  (lua_rawsetp) (lua_State *L, int idx, const void *p);
LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
LUA_API void  (lua_setuservalue) (lua_State *L, int idx);
LUA_API void  (lua_callk) (lua_State *L, int nargs, int nresults,
lua_KContext ctx, lua_KFunction k);
#define lua_call(L,n,r)		lua_callk(L, (n), (r), 0, NULL)
LUA_API int   (lua_pcallk) (lua_State *L, int nargs, int nresults, int errfunc,
lua_KContext ctx, lua_KFunction k);
#define lua_pcall(L,n,r,f)	lua_pcallk(L, (n), (r), (f), 0, NULL)
LUA_API int   (lua_load) (lua_State *L, lua_Reader reader, void *dt,
const char *chunkname, const char *mode);
LUA_API int (lua_dump) (lua_State *L, lua_Writer writer, void *data, int strip);
LUA_API int  (lua_yieldk)     (lua_State *L, int nresults, lua_KContext ctx,
lua_KFunction k);
LUA_API int  (lua_resume)     (lua_State *L, lua_State *from, int narg);
LUA_API int  (lua_status)     (lua_State *L);
LUA_API int (lua_isyieldable) (lua_State *L);
#define lua_yield(L,n)		lua_yieldk(L, (n), 0, NULL)
#define LUA_GCSTOP		0
#define LUA_GCRESTART		1
#define LUA_GCCOLLECT		2
#define LUA_GCCOUNT		3
#define LUA_GCCOUNTB		4
#define LUA_GCSTEP		5
#define LUA_GCSETPAUSE		6
#define LUA_GCSETSTEPMUL	7
#define LUA_GCISRUNNING		9
LUA_API int (lua_gc) (lua_State *L, int what, int data);
LUA_API int   (lua_error) (lua_State *L);
LUA_API int   (lua_next) (lua_State *L, int idx);
LUA_API void  (lua_concat) (lua_State *L, int n);
LUA_API void  (lua_len)    (lua_State *L, int idx);
LUA_API size_t   (lua_stringtonumber) (lua_State *L, const char *s);
LUA_API lua_Alloc (lua_getallocf) (lua_State *L, void **ud);
LUA_API void      (lua_setallocf) (lua_State *L, lua_Alloc f, void *ud);
#define lua_getextraspace(L)	((void *)((char *)(L) - LUA_EXTRASPACE))
#define lua_tonumber(L,i)	lua_tonumberx(L,(i),NULL)
#define lua_tointeger(L,i)	lua_tointegerx(L,(i),NULL)
#define lua_pop(L,n)		lua_settop(L, -(n)-1)
#define lua_newtable(L)		lua_createtable(L, 0, 0)
#define lua_register(L,n,f) (lua_pushcfunction(L, (f)), lua_setglobal(L, (n)))
#define lua_pushcfunction(L,f)	lua_pushcclosure(L, (f), 0)
#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
#define lua_pushliteral(L, s)	lua_pushstring(L, "" s)
#define lua_pushglobaltable(L)  \
((void)lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS))
#define lua_tostring(L,i)	lua_tolstring(L, (i), NULL)
#define lua_insert(L,idx)	lua_rotate(L, (idx), 1)
#define lua_remove(L,idx)	(lua_rotate(L, (idx), -1), lua_pop(L, 1))
#define lua_replace(L,idx)	(lua_copy(L, -1, (idx)), lua_pop(L, 1))
#if defined(LUA_COMPAT_APIINTCASTS)
#define lua_pushunsigned(L,n)	lua_pushinteger(L, (lua_Integer)(n))
#define lua_tounsignedx(L,i,is)	((lua_Unsigned)lua_tointegerx(L,i,is))
#define lua_tounsigned(L,i)	lua_tounsignedx(L,(i),NULL)
#endif
#define LUA_HOOKCALL	0
#define LUA_HOOKRET	1
#define LUA_HOOKLINE	2
#define LUA_HOOKCOUNT	3
#define LUA_HOOKTAILCALL 4
#define LUA_MASKCALL	(1 << LUA_HOOKCALL)
#define LUA_MASKRET	(1 << LUA_HOOKRET)
#define LUA_MASKLINE	(1 << LUA_HOOKLINE)
#define LUA_MASKCOUNT	(1 << LUA_HOOKCOUNT)
typedef struct lua_Debug lua_Debug;  
typedef void (*lua_Hook) (lua_State *L, lua_Debug *ar);
LUA_API int (lua_getstack) (lua_State *L, int level, lua_Debug *ar);
LUA_API int (lua_getinfo) (lua_State *L, const char *what, lua_Debug *ar);
LUA_API const char *(lua_getlocal) (lua_State *L, const lua_Debug *ar, int n);
LUA_API const char *(lua_setlocal) (lua_State *L, const lua_Debug *ar, int n);
LUA_API const char *(lua_getupvalue) (lua_State *L, int funcindex, int n);
LUA_API const char *(lua_setupvalue) (lua_State *L, int funcindex, int n);
LUA_API void *(lua_upvalueid) (lua_State *L, int fidx, int n);
LUA_API void  (lua_upvaluejoin) (lua_State *L, int fidx1, int n1,
int fidx2, int n2);
LUA_API void (lua_sethook) (lua_State *L, lua_Hook func, int mask, int count);
LUA_API lua_Hook (lua_gethook) (lua_State *L);
LUA_API int (lua_gethookmask) (lua_State *L);
LUA_API int (lua_gethookcount) (lua_State *L);
struct lua_Debug {
int event;
const char *name;	
const char *namewhat;	
const char *what;	
const char *source;	
int currentline;	
int linedefined;	
int lastlinedefined;	
unsigned char nups;	
unsigned char nparams;
char isvararg;        
char istailcall;	
char short_src[LUA_IDSIZE]; 
struct CallInfo *i_ci;  
};
/******************************************************************************
* Copyright (C) 1994-2018 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/
#endif
#ifndef lapi_h
#define lapi_h
#ifndef llimits_h
#define llimits_h
#include <limits.h>
#include <stddef.h>
#if defined(LUAI_MEM)		
typedef LUAI_UMEM lu_mem;
typedef LUAI_MEM l_mem;
#elif LUAI_BITSINT >= 32	
typedef size_t lu_mem;
typedef ptrdiff_t l_mem;
#else  	/* }{ */
typedef unsigned long lu_mem;
typedef long l_mem;
#endif				
typedef unsigned char lu_byte;
#define MAX_SIZET	((size_t)(~(size_t)0))
#define MAX_SIZE	(sizeof(size_t) < sizeof(lua_Integer) ? MAX_SIZET \
: (size_t)(LUA_MAXINTEGER))
#define MAX_LUMEM	((lu_mem)(~(lu_mem)0))
#define MAX_LMEM	((l_mem)(MAX_LUMEM >> 1))
#define MAX_INT		INT_MAX  
#define point2uint(p)	((unsigned int)((size_t)(p) & UINT_MAX))
#if defined(LUAI_USER_ALIGNMENT_T)
typedef LUAI_USER_ALIGNMENT_T L_Umaxalign;
#else
typedef union {
lua_Number n;
double u;
void *s;
lua_Integer i;
long l;
} L_Umaxalign;
#endif
typedef LUAI_UACNUMBER l_uacNumber;
typedef LUAI_UACINT l_uacInt;
#if defined(lua_assert)
#define check_exp(c,e)		(lua_assert(c), (e))
#define lua_longassert(c)	((c) ? (void)0 : lua_assert(0))
#else
#define lua_assert(c)		((void)0)
#define check_exp(c,e)		(e)
#define lua_longassert(c)	((void)0)
#endif
#if !defined(luai_apicheck)
#define luai_apicheck(l,e)	lua_assert(e)
#endif
#define api_check(l,e,msg)	luai_apicheck(l,(e) && msg)
#if !defined(UNUSED)
#define UNUSED(x)	((void)(x))
#endif
#define cast(t, exp)	((t)(exp))
#define cast_void(i)	cast(void, (i))
#define cast_byte(i)	cast(lu_byte, (i))
#define cast_num(i)	cast(lua_Number, (i))
#define cast_int(i)	cast(int, (i))
#define cast_uchar(i)	cast(unsigned char, (i))
#if !defined(l_castS2U)
#define l_castS2U(i)	((lua_Unsigned)(i))
#endif
#if !defined(l_castU2S)
#define l_castU2S(i)	((lua_Integer)(i))
#endif
#if defined(__GNUC__)
#define l_noret		void __attribute__((noreturn))
#elif defined(_MSC_VER) && _MSC_VER >= 1200
#define l_noret		void __declspec(noreturn)
#else
#define l_noret		void
#endif
#if !defined(LUAI_MAXCCALLS)
#define LUAI_MAXCCALLS		200
#endif
#if LUAI_BITSINT >= 32
typedef unsigned int Instruction;
#else
typedef unsigned long Instruction;
#endif
#if !defined(LUAI_MAXSHORTLEN)
#define LUAI_MAXSHORTLEN	40
#endif
#if !defined(MINSTRTABSIZE)
#define MINSTRTABSIZE	128
#endif
#if !defined(STRCACHE_N)
#define STRCACHE_N		53
#define STRCACHE_M		2
#endif
#if !defined(LUA_MINBUFFER)
#define LUA_MINBUFFER	32
#endif
#if !defined(lua_lock)
#define lua_lock(L)	((void) 0)
#define lua_unlock(L)	((void) 0)
#endif
#if !defined(luai_threadyield)
#define luai_threadyield(L)	{lua_unlock(L); lua_lock(L);}
#endif
#if !defined(luai_userstateopen)
#define luai_userstateopen(L)		((void)L)
#endif
#if !defined(luai_userstateclose)
#define luai_userstateclose(L)		((void)L)
#endif
#if !defined(luai_userstatethread)
#define luai_userstatethread(L,L1)	((void)L)
#endif
#if !defined(luai_userstatefree)
#define luai_userstatefree(L,L1)	((void)L)
#endif
#if !defined(luai_userstateresume)
#define luai_userstateresume(L,n)	((void)L)
#endif
#if !defined(luai_userstateyield)
#define luai_userstateyield(L,n)	((void)L)
#endif
#if !defined(luai_numidiv)
#define luai_numidiv(L,a,b)     ((void)L, l_floor(luai_numdiv(L,a,b)))
#endif
#if !defined(luai_numdiv)
#define luai_numdiv(L,a,b)      ((a)/(b))
#endif
#if !defined(luai_nummod)
#define luai_nummod(L,a,b,m)  \
{ (m) = l_mathop(fmod)(a,b); if ((m)*(b) < 0) (m) += (b); }
#endif
#if !defined(luai_numpow)
#define luai_numpow(L,a,b)      ((void)L, l_mathop(pow)(a,b))
#endif
#if !defined(luai_numadd)
#define luai_numadd(L,a,b)      ((a)+(b))
#define luai_numsub(L,a,b)      ((a)-(b))
#define luai_nummul(L,a,b)      ((a)*(b))
#define luai_numunm(L,a)        (-(a))
#define luai_numeq(a,b)         ((a)==(b))
#define luai_numlt(a,b)         ((a)<(b))
#define luai_numle(a,b)         ((a)<=(b))
#define luai_numisnan(a)        (!luai_numeq((a), (a)))
#endif
#if !defined(HARDSTACKTESTS)
#define condmovestack(L,pre,pos)	((void)0)
#else
#define condmovestack(L,pre,pos)  \
{ int sz_ = (L)->stacksize; pre; luaD_reallocstack((L), sz_); pos; }
#endif
#if !defined(HARDMEMTESTS)
#define condchangemem(L,pre,pos)	((void)0)
#else
#define condchangemem(L,pre,pos)  \
{ if (G(L)->gcrunning) { pre; luaC_fullgc(L, 0); pos; } }
#endif
#endif
#ifndef lstate_h
#define lstate_h
#ifndef lobject_h
#define lobject_h
#include <stdarg.h>
#define LUA_TPROTO	LUA_NUMTAGS		
#define LUA_TDEADKEY	(LUA_NUMTAGS+1)		
#define LUA_TOTALTAGS	(LUA_TPROTO + 2)
#define LUA_TLCL	(LUA_TFUNCTION | (0 << 4))  
#define LUA_TLCF	(LUA_TFUNCTION | (1 << 4))  
#define LUA_TCCL	(LUA_TFUNCTION | (2 << 4))  
#define LUA_TSHRSTR	(LUA_TSTRING | (0 << 4))  
#define LUA_TLNGSTR	(LUA_TSTRING | (1 << 4))  
#define LUA_TNUMFLT	(LUA_TNUMBER | (0 << 4))  
#define LUA_TNUMINT	(LUA_TNUMBER | (1 << 4))  
#define BIT_ISCOLLECTABLE	(1 << 6)
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)
typedef struct GCObject GCObject;
#define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked
struct GCObject {
CommonHeader;
};
typedef union Value {
GCObject *gc;    
void *p;         
int b;           
lua_CFunction f; 
lua_Integer i;   
lua_Number n;    
} Value;
#define TValuefields	Value value_; int tt_
typedef struct lua_TValue {
TValuefields;
} TValue;
#define NILCONSTANT	{NULL}, LUA_TNIL
#define val_(o)		((o)->value_)
#define rttype(o)	((o)->tt_)
#define novariant(x)	((x) & 0x0F)
#define ttype(o)	(rttype(o) & 0x3F)
#define ttnov(o)	(novariant(rttype(o)))
#define checktag(o,t)		(rttype(o) == (t))
#define checktype(o,t)		(ttnov(o) == (t))
#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_TNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_TNUMINT)
#define ttisnil(o)		checktag((o), LUA_TNIL)
#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)
#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))
#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))
#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
#define ttisclosure(o)		((rttype(o) & 0x1F) == LUA_TFUNCTION)
#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))
#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
#define ttislcf(o)		checktag((o), LUA_TLCF)
#define ttisfulluserdata(o)	checktag((o), ctb(LUA_TUSERDATA))
#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))
#define ttisdeadkey(o)		checktag((o), LUA_TDEADKEY)
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)
#define nvalue(o)	check_exp(ttisnumber(o), \
(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))
#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))
#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))
#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)
#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))
#define deadvalue(o)	check_exp(ttisdeadkey(o), cast(void *, val_(o).gc))
#define l_isfalse(o)	(ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))
#define iscollectable(o)	(rttype(o) & BIT_ISCOLLECTABLE)
#define righttt(obj)		(ttype(obj) == gcvalue(obj)->tt)
#define checkliveness(L,obj) \
lua_longassert(!iscollectable(obj) || \
(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj)))))
#define settt_(o,t)	((o)->tt_=(t))
#define setfltvalue(obj,x) \
{ TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_TNUMFLT); }
#define chgfltvalue(obj,x) \
{ TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }
#define setivalue(obj,x) \
{ TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_TNUMINT); }
#define chgivalue(obj,x) \
{ TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }
#define setnilvalue(obj) settt_(obj, LUA_TNIL)
#define setfvalue(obj,x) \
{ TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }
#define setpvalue(obj,x) \
{ TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }
#define setbvalue(obj,x) \
{ TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }
#define setgcovalue(L,obj,x) \
{ TValue *io = (obj); GCObject *i_g=(x); \
val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }
#define setsvalue(L,obj,x) \
{ TValue *io = (obj); TString *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
checkliveness(L,io); }
#define setuvalue(L,obj,x) \
{ TValue *io = (obj); Udata *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TUSERDATA)); \
checkliveness(L,io); }
#define setthvalue(L,obj,x) \
{ TValue *io = (obj); lua_State *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTHREAD)); \
checkliveness(L,io); }
#define setclLvalue(L,obj,x) \
{ TValue *io = (obj); LClosure *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TLCL)); \
checkliveness(L,io); }
#define setclCvalue(L,obj,x) \
{ TValue *io = (obj); CClosure *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TCCL)); \
checkliveness(L,io); }
#define sethvalue(L,obj,x) \
{ TValue *io = (obj); Table *x_ = (x); \
val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTABLE)); \
checkliveness(L,io); }
#define setdeadvalue(obj)	settt_(obj, LUA_TDEADKEY)
#define setobj(L,obj1,obj2) \
{ TValue *io1=(obj1); *io1 = *(obj2); \
(void)L; checkliveness(L,io1); }
#define setobjs2s	setobj
#define setobj2s	setobj
#define setsvalue2s	setsvalue
#define sethvalue2s	sethvalue
#define setptvalue2s	setptvalue
#define setobjt2t	setobj
#define setobj2n	setobj
#define setsvalue2n	setsvalue
#define setobj2t(L,o1,o2)  ((void)L, *(o1)=*(o2), checkliveness(L,(o1)))
typedef TValue *StkId;  
typedef struct TString {
CommonHeader;
lu_byte extra;  
lu_byte shrlen;  
unsigned int hash;
union {
size_t lnglen;  
struct TString *hnext;  
} u;
} TString;
typedef union UTString {
L_Umaxalign dummy;  
TString tsv;
} UTString;
#define getstr(ts)  \
check_exp(sizeof((ts)->extra), cast(char *, (ts)) + sizeof(UTString))
#define svalue(o)       getstr(tsvalue(o))
#define tsslen(s)	((s)->tt == LUA_TSHRSTR ? (s)->shrlen : (s)->u.lnglen)
#define vslen(o)	tsslen(tsvalue(o))
typedef struct Udata {
CommonHeader;
lu_byte ttuv_;  
struct Table *metatable;
size_t len;  
union Value user_;  
} Udata;
typedef union UUdata {
L_Umaxalign dummy;  
Udata uv;
} UUdata;
#define getudatamem(u)  \
check_exp(sizeof((u)->ttuv_), (cast(char*, (u)) + sizeof(UUdata)))
#define setuservalue(L,u,o) \
{ const TValue *io=(o); Udata *iu = (u); \
iu->user_ = io->value_; iu->ttuv_ = rttype(io); \
checkliveness(L,io); }
#define getuservalue(L,u,o) \
{ TValue *io=(o); const Udata *iu = (u); \
io->value_ = iu->user_; settt_(io, iu->ttuv_); \
checkliveness(L,io); }
typedef struct Upvaldesc {
TString *name;  
lu_byte instack;  
lu_byte idx;  
} Upvaldesc;
typedef struct LocVar {
TString *varname;
int startpc;  
int endpc;    
} LocVar;
typedef struct Proto {
CommonHeader;
lu_byte numparams;  
lu_byte is_vararg;
lu_byte maxstacksize;  
int sizeupvalues;  
int sizek;  
int sizecode;
int sizelineinfo;
int sizep;  
int sizelocvars;
int linedefined;  
int lastlinedefined;  
TValue *k;  
Instruction *code;  
struct Proto **p;  
int *lineinfo;  
LocVar *locvars;  
Upvaldesc *upvalues;  
struct LClosure *cache;  
TString  *source;  
GCObject *gclist;
} Proto;
typedef struct UpVal UpVal;
#define ClosureHeader \
CommonHeader; lu_byte nupvalues; GCObject *gclist
typedef struct CClosure {
ClosureHeader;
lua_CFunction f;
TValue upvalue[1];  
} CClosure;
typedef struct LClosure {
ClosureHeader;
struct Proto *p;
UpVal *upvals[1];  
} LClosure;
typedef union Closure {
CClosure c;
LClosure l;
} Closure;
#define isLfunction(o)	ttisLclosure(o)
#define getproto(o)	(clLvalue(o)->p)
typedef union TKey {
struct {
TValuefields;
int next;  
} nk;
TValue tvk;
} TKey;
#define setnodekey(L,key,obj) \
{ TKey *k_=(key); const TValue *io_=(obj); \
k_->nk.value_ = io_->value_; k_->nk.tt_ = io_->tt_; \
(void)L; checkliveness(L,io_); }
typedef struct Node {
TValue i_val;
TKey i_key;
} Node;
typedef struct Table {
CommonHeader;
lu_byte flags;  
lu_byte lsizenode;  
unsigned int sizearray;  
TValue *array;  
Node *node;
Node *lastfree;  
struct Table *metatable;
GCObject *gclist;
} Table;
#define lmod(s,size) \
(check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))
#define twoto(x)	(1<<(x))
#define sizenode(t)	(twoto((t)->lsizenode))
#define luaO_nilobject		(&luaO_nilobject_)
LUAI_DDEC const TValue luaO_nilobject_;
#define UTF8BUFFSZ	8
LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
const TValue *p2, TValue *res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, StkId obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);
#endif
#ifndef ltm_h
#define ltm_h
typedef enum {
TM_INDEX,
TM_NEWINDEX,
TM_GC,
TM_MODE,
TM_LEN,
TM_EQ,  
TM_ADD,
TM_SUB,
TM_MUL,
TM_MOD,
TM_POW,
TM_DIV,
TM_IDIV,
TM_BAND,
TM_BOR,
TM_BXOR,
TM_SHL,
TM_SHR,
TM_UNM,
TM_BNOT,
TM_LT,
TM_LE,
TM_CONCAT,
TM_CALL,
TM_N		
} TMS;
#define gfasttm(g,et,e) ((et) == NULL ? NULL : \
((et)->flags & (1u<<(e))) ? NULL : luaT_gettm(et, e, (g)->tmname[e]))
#define fasttm(l,et,e)	gfasttm(G(l), et, e)
#define ttypename(x)	luaT_typenames_[(x) + 1]
LUAI_DDEC const char *const luaT_typenames_[LUA_TOTALTAGS];
LUAI_FUNC const char *luaT_objtypename (lua_State *L, const TValue *o);
LUAI_FUNC const TValue *luaT_gettm (Table *events, TMS event, TString *ename);
LUAI_FUNC const TValue *luaT_gettmbyobj (lua_State *L, const TValue *o,
TMS event);
LUAI_FUNC void luaT_init (lua_State *L);
LUAI_FUNC void luaT_callTM (lua_State *L, const TValue *f, const TValue *p1,
const TValue *p2, TValue *p3, int hasres);
LUAI_FUNC int luaT_callbinTM (lua_State *L, const TValue *p1, const TValue *p2,
StkId res, TMS event);
LUAI_FUNC void luaT_trybinTM (lua_State *L, const TValue *p1, const TValue *p2,
StkId res, TMS event);
LUAI_FUNC int luaT_callorderTM (lua_State *L, const TValue *p1,
const TValue *p2, TMS event);
#endif
#ifndef lzio_h
#define lzio_h
#ifndef lmem_h
#define lmem_h
#include <stddef.h>
#define luaM_reallocv(L,b,on,n,e) \
(((sizeof(n) >= sizeof(size_t) && cast(size_t, (n)) + 1 > MAX_SIZET/(e)) \
? luaM_toobig(L) : cast_void(0)) , \
luaM_realloc_(L, (b), (on)*(e), (n)*(e)))
#define luaM_reallocvchar(L,b,on,n)  \
cast(char *, luaM_realloc_(L, (b), (on)*sizeof(char), (n)*sizeof(char)))
#define luaM_freemem(L, b, s)	luaM_realloc_(L, (b), (s), 0)
#define luaM_free(L, b)		luaM_realloc_(L, (b), sizeof(*(b)), 0)
#define luaM_freearray(L, b, n)   luaM_realloc_(L, (b), (n)*sizeof(*(b)), 0)
#define luaM_malloc(L,s)	luaM_realloc_(L, NULL, 0, (s))
#define luaM_new(L,t)		cast(t *, luaM_malloc(L, sizeof(t)))
#define luaM_newvector(L,n,t) \
cast(t *, luaM_reallocv(L, NULL, 0, n, sizeof(t)))
#define luaM_newobject(L,tag,s)	luaM_realloc_(L, NULL, tag, (s))
#define luaM_growvector(L,v,nelems,size,t,limit,e) \
if ((nelems)+1 > (size)) \
((v)=cast(t *, luaM_growaux_(L,v,&(size),sizeof(t),limit,e)))
#define luaM_reallocvector(L, v,oldn,n,t) \
((v)=cast(t *, luaM_reallocv(L, v, oldn, n, sizeof(t))))
LUAI_FUNC l_noret luaM_toobig (lua_State *L);
LUAI_FUNC void *luaM_realloc_ (lua_State *L, void *block, size_t oldsize,
size_t size);
LUAI_FUNC void *luaM_growaux_ (lua_State *L, void *block, int *size,
size_t size_elem, int limit,
const char *what);
#endif
#define EOZ	(-1)			
typedef struct Zio ZIO;
#define zgetc(z)  (((z)->n--)>0 ?  cast_uchar(*(z)->p++) : luaZ_fill(z))
typedef struct Mbuffer {
char *buffer;
size_t n;
size_t buffsize;
} Mbuffer;
#define luaZ_initbuffer(L, buff) ((buff)->buffer = NULL, (buff)->buffsize = 0)
#define luaZ_buffer(buff)	((buff)->buffer)
#define luaZ_sizebuffer(buff)	((buff)->buffsize)
#define luaZ_bufflen(buff)	((buff)->n)
#define luaZ_buffremove(buff,i)	((buff)->n -= (i))
#define luaZ_resetbuffer(buff) ((buff)->n = 0)
#define luaZ_resizebuffer(L, buff, size) \
((buff)->buffer = luaM_reallocvchar(L, (buff)->buffer, \
(buff)->buffsize, size), \
(buff)->buffsize = size)
#define luaZ_freebuffer(L, buff)	luaZ_resizebuffer(L, buff, 0)
LUAI_FUNC void luaZ_init (lua_State *L, ZIO *z, lua_Reader reader,
void *data);
LUAI_FUNC size_t luaZ_read (ZIO* z, void *b, size_t n);	
struct Zio {
size_t n;			
const char *p;		
lua_Reader reader;		
void *data;			
lua_State *L;			
};
LUAI_FUNC int luaZ_fill (ZIO *z);
#endif
struct lua_longjmp;  
#if !defined(l_signalT)
#include <signal.h>
#define l_signalT	sig_atomic_t
#endif
#define EXTRA_STACK   5
#define BASIC_STACK_SIZE        (2*LUA_MINSTACK)
#define KGC_NORMAL	0
#define KGC_EMERGENCY	1	
typedef struct stringtable {
TString **hash;
int nuse;  
int size;
} stringtable;
typedef struct CallInfo {
StkId func;  
StkId	top;  
struct CallInfo *previous, *next;  
union {
struct {  
StkId base;  
const Instruction *savedpc;
} l;
struct {  
lua_KFunction k;  
ptrdiff_t old_errfunc;
lua_KContext ctx;  
} c;
} u;
ptrdiff_t extra;
short nresults;  
unsigned short callstatus;
} CallInfo;
#define CIST_OAH	(1<<0)	
#define CIST_LUA	(1<<1)	
#define CIST_HOOKED	(1<<2)	
#define CIST_FRESH	(1<<3)	/* call is running on a fresh invocation
of luaV_execute */
#define CIST_YPCALL	(1<<4)	
#define CIST_TAIL	(1<<5)	
#define CIST_HOOKYIELD	(1<<6)	
#define CIST_LEQ	(1<<7)  
#define CIST_FIN	(1<<8)  
#define isLua(ci)	((ci)->callstatus & CIST_LUA)
#define setoah(st,v)	((st) = ((st) & ~CIST_OAH) | (v))
#define getoah(st)	((st) & CIST_OAH)
typedef struct global_State {
lua_Alloc frealloc;  
void *ud;         
l_mem totalbytes;  
l_mem GCdebt;  
lu_mem GCmemtrav;  
lu_mem GCestimate;  
stringtable strt;  
TValue l_registry;
unsigned int seed;  
lu_byte currentwhite;
lu_byte gcstate;  
lu_byte gckind;  
lu_byte gcrunning;  
GCObject *allgc;  
GCObject **sweepgc;  
GCObject *finobj;  
GCObject *gray;  
GCObject *grayagain;  
GCObject *weak;  
GCObject *ephemeron;  
GCObject *allweak;  
GCObject *tobefnz;  
GCObject *fixedgc;  
struct lua_State *twups;  
unsigned int gcfinnum;  
int gcpause;  
int gcstepmul;  
lua_CFunction panic;  
struct lua_State *mainthread;
const lua_Number *version;  
TString *memerrmsg;  
TString *tmname[TM_N];  
struct Table *mt[LUA_NUMTAGS];  
TString *strcache[STRCACHE_N][STRCACHE_M];  
} global_State;
struct lua_State {
CommonHeader;
unsigned short nci;  
lu_byte status;
StkId top;  
global_State *l_G;
CallInfo *ci;  
const Instruction *oldpc;  
StkId stack_last;  
StkId stack;  
UpVal *openupval;  
GCObject *gclist;
struct lua_State *twups;  
struct lua_longjmp *errorJmp;  
CallInfo base_ci;  
volatile lua_Hook hook;
ptrdiff_t errfunc;  
int stacksize;
int basehookcount;
int hookcount;
unsigned short nny;  
unsigned short nCcalls;  
l_signalT hookmask;
lu_byte allowhook;
};
#define G(L)	(L->l_G)
union GCUnion {
GCObject gc;  
struct TString ts;
struct Udata u;
union Closure cl;
struct Table h;
struct Proto p;
struct lua_State th;  
};
#define cast_u(o)	cast(union GCUnion *, (o))
#define gco2ts(o)  \
check_exp(novariant((o)->tt) == LUA_TSTRING, &((cast_u(o))->ts))
#define gco2u(o)  check_exp((o)->tt == LUA_TUSERDATA, &((cast_u(o))->u))
#define gco2lcl(o)  check_exp((o)->tt == LUA_TLCL, &((cast_u(o))->cl.l))
#define gco2ccl(o)  check_exp((o)->tt == LUA_TCCL, &((cast_u(o))->cl.c))
#define gco2cl(o)  \
check_exp(novariant((o)->tt) == LUA_TFUNCTION, &((cast_u(o))->cl))
#define gco2t(o)  check_exp((o)->tt == LUA_TTABLE, &((cast_u(o))->h))
#define gco2p(o)  check_exp((o)->tt == LUA_TPROTO, &((cast_u(o))->p))
#define gco2th(o)  check_exp((o)->tt == LUA_TTHREAD, &((cast_u(o))->th))
#define obj2gco(v) \
check_exp(novariant((v)->tt) < LUA_TDEADKEY, (&(cast_u(v)->gc)))
#define gettotalbytes(g)	cast(lu_mem, (g)->totalbytes + (g)->GCdebt)
LUAI_FUNC void luaE_setdebt (global_State *g, l_mem debt);
LUAI_FUNC void luaE_freethread (lua_State *L, lua_State *L1);
LUAI_FUNC CallInfo *luaE_extendCI (lua_State *L);
LUAI_FUNC void luaE_freeCI (lua_State *L);
LUAI_FUNC void luaE_shrinkCI (lua_State *L);
#endif
#define api_incr_top(L)   {L->top++; api_check(L, L->top <= L->ci->top, \
"stack overflow");}
#define adjustresults(L,nres) \
{ if ((nres) == LUA_MULTRET && L->ci->top < L->top) L->ci->top = L->top; }
#define api_checknelems(L,n)	api_check(L, (n) < (L->top - L->ci->func), \
"not enough elements in the stack")
#endif
#ifndef ldebug_h
#define ldebug_h
#define pcRel(pc, p)	(cast(int, (pc) - (p)->code) - 1)
#define getfuncline(f,pc)	(((f)->lineinfo) ? (f)->lineinfo[pc] : -1)
#define resethookcount(L)	(L->hookcount = L->basehookcount)
LUAI_FUNC l_noret luaG_typeerror (lua_State *L, const TValue *o,
const char *opname);
LUAI_FUNC l_noret luaG_concaterror (lua_State *L, const TValue *p1,
const TValue *p2);
LUAI_FUNC l_noret luaG_opinterror (lua_State *L, const TValue *p1,
const TValue *p2,
const char *msg);
LUAI_FUNC l_noret luaG_tointerror (lua_State *L, const TValue *p1,
const TValue *p2);
LUAI_FUNC l_noret luaG_ordererror (lua_State *L, const TValue *p1,
const TValue *p2);
LUAI_FUNC l_noret luaG_runerror (lua_State *L, const char *fmt, ...);
LUAI_FUNC const char *luaG_addinfo (lua_State *L, const char *msg,
TString *src, int line);
LUAI_FUNC l_noret luaG_errormsg (lua_State *L);
LUAI_FUNC void luaG_traceexec (lua_State *L);
#endif
#ifndef ldo_h
#define ldo_h
#define luaD_checkstackaux(L,n,pre,pos)  \
if (L->stack_last - L->top <= (n)) \
{ pre; luaD_growstack(L, n); pos; } else { condmovestack(L,pre,pos); }
#define luaD_checkstack(L,n)	luaD_checkstackaux(L,n,(void)0,(void)0)
#define savestack(L,p)		((char *)(p) - (char *)L->stack)
#define restorestack(L,n)	((TValue *)((char *)L->stack + (n)))
typedef void (*Pfunc) (lua_State *L, void *ud);
LUAI_FUNC int luaD_protectedparser (lua_State *L, ZIO *z, const char *name,
const char *mode);
LUAI_FUNC void luaD_hook (lua_State *L, int event, int line);
LUAI_FUNC int luaD_precall (lua_State *L, StkId func, int nresults);
LUAI_FUNC void luaD_call (lua_State *L, StkId func, int nResults);
LUAI_FUNC void luaD_callnoyield (lua_State *L, StkId func, int nResults);
LUAI_FUNC int luaD_pcall (lua_State *L, Pfunc func, void *u,
ptrdiff_t oldtop, ptrdiff_t ef);
LUAI_FUNC int luaD_poscall (lua_State *L, CallInfo *ci, StkId firstResult,
int nres);
LUAI_FUNC void luaD_reallocstack (lua_State *L, int newsize);
LUAI_FUNC void luaD_growstack (lua_State *L, int n);
LUAI_FUNC void luaD_shrinkstack (lua_State *L);
LUAI_FUNC void luaD_inctop (lua_State *L);
LUAI_FUNC l_noret luaD_throw (lua_State *L, int errcode);
LUAI_FUNC int luaD_rawrunprotected (lua_State *L, Pfunc f, void *ud);
#endif
#ifndef lfunc_h
#define lfunc_h
#define sizeCclosure(n)	(cast(int, sizeof(CClosure)) + \
cast(int, sizeof(TValue)*((n)-1)))
#define sizeLclosure(n)	(cast(int, sizeof(LClosure)) + \
cast(int, sizeof(TValue *)*((n)-1)))
#define isintwups(L)	(L->twups != L)
#define MAXUPVAL	255
struct UpVal {
TValue *v;  
lu_mem refcount;  
union {
struct {  
UpVal *next;  
int touched;  
} open;
TValue value;  
} u;
};
#define upisopen(up)	((up)->v != &(up)->u.value)
LUAI_FUNC Proto *luaF_newproto (lua_State *L);
LUAI_FUNC CClosure *luaF_newCclosure (lua_State *L, int nelems);
LUAI_FUNC LClosure *luaF_newLclosure (lua_State *L, int nelems);
LUAI_FUNC void luaF_initupvals (lua_State *L, LClosure *cl);
LUAI_FUNC UpVal *luaF_findupval (lua_State *L, StkId level);
LUAI_FUNC void luaF_close (lua_State *L, StkId level);
LUAI_FUNC void luaF_freeproto (lua_State *L, Proto *f);
LUAI_FUNC const char *luaF_getlocalname (const Proto *func, int local_number,
int pc);
#endif
#ifndef lgc_h
#define lgc_h
#if !defined(GCSTEPSIZE)
#define GCSTEPSIZE	(cast_int(100 * sizeof(TString)))
#endif
#define GCSpropagate	0
#define GCSatomic	1
#define GCSswpallgc	2
#define GCSswpfinobj	3
#define GCSswptobefnz	4
#define GCSswpend	5
#define GCScallfin	6
#define GCSpause	7
#define issweepphase(g)  \
(GCSswpallgc <= (g)->gcstate && (g)->gcstate <= GCSswpend)
#define keepinvariant(g)	((g)->gcstate <= GCSatomic)
#define resetbits(x,m)		((x) &= cast(lu_byte, ~(m)))
#define setbits(x,m)		((x) |= (m))
#define testbits(x,m)		((x) & (m))
#define bitmask(b)		(1<<(b))
#define bit2mask(b1,b2)		(bitmask(b1) | bitmask(b2))
#define l_setbit(x,b)		setbits(x, bitmask(b))
#define resetbit(x,b)		resetbits(x, bitmask(b))
#define testbit(x,b)		testbits(x, bitmask(b))
#define WHITE0BIT	0  
#define WHITE1BIT	1  
#define BLACKBIT	2  
#define FINALIZEDBIT	3  
#define WHITEBITS	bit2mask(WHITE0BIT, WHITE1BIT)
#define iswhite(x)      testbits((x)->marked, WHITEBITS)
#define isblack(x)      testbit((x)->marked, BLACKBIT)
#define isgray(x)    \
(!testbits((x)->marked, WHITEBITS | bitmask(BLACKBIT)))
#define tofinalize(x)	testbit((x)->marked, FINALIZEDBIT)
#define otherwhite(g)	((g)->currentwhite ^ WHITEBITS)
#define isdeadm(ow,m)	(!(((m) ^ WHITEBITS) & (ow)))
#define isdead(g,v)	isdeadm(otherwhite(g), (v)->marked)
#define changewhite(x)	((x)->marked ^= WHITEBITS)
#define gray2black(x)	l_setbit((x)->marked, BLACKBIT)
#define luaC_white(g)	cast(lu_byte, (g)->currentwhite & WHITEBITS)
#define luaC_condGC(L,pre,pos) \
{ if (G(L)->GCdebt > 0) { pre; luaC_step(L); pos;}; \
condchangemem(L,pre,pos); }
#define luaC_checkGC(L)		luaC_condGC(L,(void)0,(void)0)
#define luaC_barrier(L,p,v) (  \
(iscollectable(v) && isblack(p) && iswhite(gcvalue(v))) ?  \
luaC_barrier_(L,obj2gco(p),gcvalue(v)) : cast_void(0))
#define luaC_barrierback(L,p,v) (  \
(iscollectable(v) && isblack(p) && iswhite(gcvalue(v))) ? \
luaC_barrierback_(L,p) : cast_void(0))
#define luaC_objbarrier(L,p,o) (  \
(isblack(p) && iswhite(o)) ? \
luaC_barrier_(L,obj2gco(p),obj2gco(o)) : cast_void(0))
#define luaC_upvalbarrier(L,uv) ( \
(iscollectable((uv)->v) && !upisopen(uv)) ? \
luaC_upvalbarrier_(L,uv) : cast_void(0))
LUAI_FUNC void luaC_fix (lua_State *L, GCObject *o);
LUAI_FUNC void luaC_freeallobjects (lua_State *L);
LUAI_FUNC void luaC_step (lua_State *L);
LUAI_FUNC void luaC_runtilstate (lua_State *L, int statesmask);
LUAI_FUNC void luaC_fullgc (lua_State *L, int isemergency);
LUAI_FUNC GCObject *luaC_newobj (lua_State *L, int tt, size_t sz);
LUAI_FUNC void luaC_barrier_ (lua_State *L, GCObject *o, GCObject *v);
LUAI_FUNC void luaC_barrierback_ (lua_State *L, Table *o);
LUAI_FUNC void luaC_upvalbarrier_ (lua_State *L, UpVal *uv);
LUAI_FUNC void luaC_checkfinalizer (lua_State *L, GCObject *o, Table *mt);
LUAI_FUNC void luaC_upvdeccount (lua_State *L, UpVal *uv);
#endif
#ifndef lstring_h
#define lstring_h
#define sizelstring(l)  (sizeof(union UTString) + ((l) + 1) * sizeof(char))
#define sizeludata(l)	(sizeof(union UUdata) + (l))
#define sizeudata(u)	sizeludata((u)->len)
#define luaS_newliteral(L, s)	(luaS_newlstr(L, "" s, \
(sizeof(s)/sizeof(char))-1))
#define isreserved(s)	((s)->tt == LUA_TSHRSTR && (s)->extra > 0)
#define eqshrstr(a,b)	check_exp((a)->tt == LUA_TSHRSTR, (a) == (b))
LUAI_FUNC unsigned int luaS_hash (const char *str, size_t l, unsigned int seed);
LUAI_FUNC unsigned int luaS_hashlongstr (TString *ts);
LUAI_FUNC int luaS_eqlngstr (TString *a, TString *b);
LUAI_FUNC void luaS_resize (lua_State *L, int newsize);
LUAI_FUNC void luaS_clearcache (global_State *g);
LUAI_FUNC void luaS_init (lua_State *L);
LUAI_FUNC void luaS_remove (lua_State *L, TString *ts);
LUAI_FUNC Udata *luaS_newudata (lua_State *L, size_t s);
LUAI_FUNC TString *luaS_newlstr (lua_State *L, const char *str, size_t l);
LUAI_FUNC TString *luaS_new (lua_State *L, const char *str);
LUAI_FUNC TString *luaS_createlngstrobj (lua_State *L, size_t l);
#endif
#ifndef ltable_h
#define ltable_h
#define gnode(t,i)	(&(t)->node[i])
#define gval(n)		(&(n)->i_val)
#define gnext(n)	((n)->i_key.nk.next)
#define gkey(n)		cast(const TValue*, (&(n)->i_key.tvk))
#define wgkey(n)		(&(n)->i_key.nk)
#define invalidateTMcache(t)	((t)->flags = 0)
#define isdummy(t)		((t)->lastfree == NULL)
#define allocsizenode(t)	(isdummy(t) ? 0 : sizenode(t))
#define keyfromval(v) \
(gkey(cast(Node *, cast(char *, (v)) - offsetof(Node, i_val))))
LUAI_FUNC const TValue *luaH_getint (Table *t, lua_Integer key);
LUAI_FUNC void luaH_setint (lua_State *L, Table *t, lua_Integer key,
TValue *value);
LUAI_FUNC const TValue *luaH_getshortstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_getstr (Table *t, TString *key);
LUAI_FUNC const TValue *luaH_get (Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_newkey (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC TValue *luaH_set (lua_State *L, Table *t, const TValue *key);
LUAI_FUNC Table *luaH_new (lua_State *L);
LUAI_FUNC void luaH_resize (lua_State *L, Table *t, unsigned int nasize,
unsigned int nhsize);
LUAI_FUNC void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize);
LUAI_FUNC void luaH_free (lua_State *L, Table *t);
LUAI_FUNC int luaH_next (lua_State *L, Table *t, StkId key);
LUAI_FUNC lua_Unsigned luaH_getn (Table *t);
#if defined(LUA_DEBUG)
LUAI_FUNC Node *luaH_mainposition (const Table *t, const TValue *key);
LUAI_FUNC int luaH_isdummy (const Table *t);
#endif
#endif
#ifndef lundump_h
#define lundump_h
#define LUAC_DATA	"\x19\x93\r\n\x1a\n"
#define LUAC_INT	0x5678
#define LUAC_NUM	cast_num(370.5)
#define MYINT(s)	(s[0]-'0')
#define LUAC_VERSION	(MYINT(LUA_VERSION_MAJOR)*16+MYINT(LUA_VERSION_MINOR))
#define LUAC_FORMAT	0	
LUAI_FUNC LClosure* luaU_undump (lua_State* L, ZIO* Z, const char* name);
LUAI_FUNC int luaU_dump (lua_State* L, const Proto* f, lua_Writer w,
void* data, int strip);
#endif
#ifndef lvm_h
#define lvm_h
#if !defined(LUA_NOCVTN2S)
#define cvt2str(o)	ttisnumber(o)
#else
#define cvt2str(o)	0	
#endif
#if !defined(LUA_NOCVTS2N)
#define cvt2num(o)	ttisstring(o)
#else
#define cvt2num(o)	0	
#endif
#if !defined(LUA_FLOORN2I)
#define LUA_FLOORN2I		0
#endif
#define tonumber(o,n) \
(ttisfloat(o) ? (*(n) = fltvalue(o), 1) : luaV_tonumber_(o,n))
#define tointeger(o,i) \
(ttisinteger(o) ? (*(i) = ivalue(o), 1) : luaV_tointeger(o,i,LUA_FLOORN2I))
#define intop(op,v1,v2) l_castU2S(l_castS2U(v1) op l_castS2U(v2))
#define luaV_rawequalobj(t1,t2)		luaV_equalobj(NULL,t1,t2)
#define luaV_fastget(L,t,k,slot,f) \
(!ttistable(t)  \
? (slot = NULL, 0)    \
: (slot = f(hvalue(t), k),    \
!ttisnil(slot)))  
#define luaV_gettable(L,t,k,v) { const TValue *slot; \
if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
else luaV_finishget(L,t,k,v,slot); }
#define luaV_fastset(L,t,k,slot,f,v) \
(!ttistable(t) \
? (slot = NULL, 0) \
: (slot = f(hvalue(t), k), \
ttisnil(slot) ? 0 \
: (luaC_barrierback(L, hvalue(t), v), \
setobj2t(L, cast(TValue *,slot), v), \
1)))
#define luaV_settable(L,t,k,v) { const TValue *slot; \
if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
luaV_finishset(L,t,k,v,slot); }
LUAI_FUNC int luaV_equalobj (lua_State *L, const TValue *t1, const TValue *t2);
LUAI_FUNC int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r);
LUAI_FUNC int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r);
LUAI_FUNC int luaV_tonumber_ (const TValue *obj, lua_Number *n);
LUAI_FUNC int luaV_tointeger (const TValue *obj, lua_Integer *p, int mode);
LUAI_FUNC void luaV_finishget (lua_State *L, const TValue *t, TValue *key,
StkId val, const TValue *slot);
LUAI_FUNC void luaV_finishset (lua_State *L, const TValue *t, TValue *key,
StkId val, const TValue *slot);
LUAI_FUNC void luaV_finishOp (lua_State *L);
LUAI_FUNC void luaV_execute (lua_State *L);
LUAI_FUNC void luaV_concat (lua_State *L, int total);
LUAI_FUNC lua_Integer luaV_div (lua_State *L, lua_Integer x, lua_Integer y);
LUAI_FUNC lua_Integer luaV_mod (lua_State *L, lua_Integer x, lua_Integer y);
LUAI_FUNC lua_Integer luaV_shiftl (lua_Integer x, lua_Integer y);
LUAI_FUNC void luaV_objlen (lua_State *L, StkId ra, const TValue *rb);
#endif
const char lua_ident[] =
"$LuaVersion: " LUA_COPYRIGHT " $"
"$LuaAuthors: " LUA_AUTHORS " $";
#define NONVALIDVALUE		cast(TValue *, luaO_nilobject)
#define isvalid(o)	((o) != luaO_nilobject)
#define ispseudo(i)		((i) <= LUA_REGISTRYINDEX)
#define isupvalue(i)		((i) < LUA_REGISTRYINDEX)
#define isstackindex(i, o)	(isvalid(o) && !ispseudo(i))
#define api_checkvalidindex(l,o)  api_check(l, isvalid(o), "invalid index")
#define api_checkstackindex(l, i, o)  \
api_check(l, isstackindex(i, o), "index not in the stack")
static TValue *index2addr (lua_State *L, int idx) {
CallInfo *ci = L->ci;
if (idx > 0) {
TValue *o = ci->func + idx;
api_check(L, idx <= ci->top - (ci->func + 1), "unacceptable index");
if (o >= L->top) return NONVALIDVALUE;
else return o;
}
else if (!ispseudo(idx)) {  
api_check(L, idx != 0 && -idx <= L->top - (ci->func + 1), "invalid index");
return L->top + idx;
}
else if (idx == LUA_REGISTRYINDEX)
return &G(L)->l_registry;
else {  
idx = LUA_REGISTRYINDEX - idx;
api_check(L, idx <= MAXUPVAL + 1, "upvalue index too large");
if (ttislcf(ci->func))  
return NONVALIDVALUE;  
else {
CClosure *func = clCvalue(ci->func);
return (idx <= func->nupvalues) ? &func->upvalue[idx-1] : NONVALIDVALUE;
}
}
}
static void growstack (lua_State *L, void *ud) {
int size = *(int *)ud;
luaD_growstack(L, size);
}
LUA_API int lua_checkstack (lua_State *L, int n) {
int res;
CallInfo *ci = L->ci;
lua_lock(L);
api_check(L, n >= 0, "negative 'n'");
if (L->stack_last - L->top > n)  
res = 1;  
else {  
int inuse = cast_int(L->top - L->stack) + EXTRA_STACK;
if (inuse > LUAI_MAXSTACK - n)  
res = 0;  
else  
res = (luaD_rawrunprotected(L, &growstack, &n) == LUA_OK);
}
if (res && ci->top < L->top + n)
ci->top = L->top + n;  
lua_unlock(L);
return res;
}
LUA_API void lua_xmove (lua_State *from, lua_State *to, int n) {
int i;
if (from == to) return;
lua_lock(to);
api_checknelems(from, n);
api_check(from, G(from) == G(to), "moving among independent states");
api_check(from, to->ci->top - to->top >= n, "stack overflow");
from->top -= n;
for (i = 0; i < n; i++) {
setobj2s(to, to->top, from->top + i);
to->top++;  
}
lua_unlock(to);
}
LUA_API lua_CFunction lua_atpanic (lua_State *L, lua_CFunction panicf) {
lua_CFunction old;
lua_lock(L);
old = G(L)->panic;
G(L)->panic = panicf;
lua_unlock(L);
return old;
}
LUA_API const lua_Number *lua_version (lua_State *L) {
static const lua_Number version = LUA_VERSION_NUM;
if (L == NULL) return &version;
else return G(L)->version;
}
LUA_API int lua_absindex (lua_State *L, int idx) {
return (idx > 0 || ispseudo(idx))
? idx
: cast_int(L->top - L->ci->func) + idx;
}
LUA_API int lua_gettop (lua_State *L) {
return cast_int(L->top - (L->ci->func + 1));
}
LUA_API void lua_settop (lua_State *L, int idx) {
StkId func = L->ci->func;
lua_lock(L);
if (idx >= 0) {
api_check(L, idx <= L->stack_last - (func + 1), "new top too large");
while (L->top < (func + 1) + idx)
setnilvalue(L->top++);
L->top = (func + 1) + idx;
}
else {
api_check(L, -(idx+1) <= (L->top - (func + 1)), "invalid new top");
L->top += idx+1;  
}
lua_unlock(L);
}
static void reverse (lua_State *L, StkId from, StkId to) {
for (; from < to; from++, to--) {
TValue temp;
setobj(L, &temp, from);
setobjs2s(L, from, to);
setobj2s(L, to, &temp);
}
}
LUA_API void lua_rotate (lua_State *L, int idx, int n) {
StkId p, t, m;
lua_lock(L);
t = L->top - 1;  
p = index2addr(L, idx);  
api_checkstackindex(L, idx, p);
api_check(L, (n >= 0 ? n : -n) <= (t - p + 1), "invalid 'n'");
m = (n >= 0 ? t - n : p - n - 1);  
reverse(L, p, m);  
reverse(L, m + 1, t);  
reverse(L, p, t);  
lua_unlock(L);
}
LUA_API void lua_copy (lua_State *L, int fromidx, int toidx) {
TValue *fr, *to;
lua_lock(L);
fr = index2addr(L, fromidx);
to = index2addr(L, toidx);
api_checkvalidindex(L, to);
setobj(L, to, fr);
if (isupvalue(toidx))  
luaC_barrier(L, clCvalue(L->ci->func), fr);
/* LUA_REGISTRYINDEX does not need gc barrier
(collector revisits it before finishing collection) */
lua_unlock(L);
}
LUA_API void lua_pushvalue (lua_State *L, int idx) {
lua_lock(L);
setobj2s(L, L->top, index2addr(L, idx));
api_incr_top(L);
lua_unlock(L);
}
LUA_API int lua_type (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
return (isvalid(o) ? ttnov(o) : LUA_TNONE);
}
LUA_API const char *lua_typename (lua_State *L, int t) {
UNUSED(L);
api_check(L, LUA_TNONE <= t && t < LUA_NUMTAGS, "invalid tag");
return ttypename(t);
}
LUA_API int lua_iscfunction (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
return (ttislcf(o) || (ttisCclosure(o)));
}
LUA_API int lua_isinteger (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
return ttisinteger(o);
}
LUA_API int lua_isnumber (lua_State *L, int idx) {
lua_Number n;
const TValue *o = index2addr(L, idx);
return tonumber(o, &n);
}
LUA_API int lua_isstring (lua_State *L, int idx) {
const TValue *o = index2addr(L, idx);
return (ttisstring(o) || cvt2str(o));
}
LUA_API int lua_isuserdata (lua_State *L, int idx) {
const TValue *o = index2addr(L, idx);
return (ttisfulluserdata(o) || ttislightuserdata(o));
}
LUA_API int lua_rawequal (lua_State *L, int index1, int index2) {
StkId o1 = index2addr(L, index1);
StkId o2 = index2addr(L, index2);
return (isvalid(o1) && isvalid(o2)) ? luaV_rawequalobj(o1, o2) : 0;
}
LUA_API void lua_arith (lua_State *L, int op) {
lua_lock(L);
if (op != LUA_OPUNM && op != LUA_OPBNOT)
api_checknelems(L, 2);  
else {  
api_checknelems(L, 1);
setobjs2s(L, L->top, L->top - 1);
api_incr_top(L);
}
luaO_arith(L, op, L->top - 2, L->top - 1, L->top - 2);
L->top--;  
lua_unlock(L);
}
LUA_API int lua_compare (lua_State *L, int index1, int index2, int op) {
StkId o1, o2;
int i = 0;
lua_lock(L);  
o1 = index2addr(L, index1);
o2 = index2addr(L, index2);
if (isvalid(o1) && isvalid(o2)) {
switch (op) {
case LUA_OPEQ: i = luaV_equalobj(L, o1, o2); break;
case LUA_OPLT: i = luaV_lessthan(L, o1, o2); break;
case LUA_OPLE: i = luaV_lessequal(L, o1, o2); break;
default: api_check(L, 0, "invalid option");
}
}
lua_unlock(L);
return i;
}
LUA_API size_t lua_stringtonumber (lua_State *L, const char *s) {
size_t sz = luaO_str2num(s, L->top);
if (sz != 0)
api_incr_top(L);
return sz;
}
LUA_API lua_Number lua_tonumberx (lua_State *L, int idx, int *pisnum) {
lua_Number n;
const TValue *o = index2addr(L, idx);
int isnum = tonumber(o, &n);
if (!isnum)
n = 0;  
if (pisnum) *pisnum = isnum;
return n;
}
LUA_API lua_Integer lua_tointegerx (lua_State *L, int idx, int *pisnum) {
lua_Integer res;
const TValue *o = index2addr(L, idx);
int isnum = tointeger(o, &res);
if (!isnum)
res = 0;  
if (pisnum) *pisnum = isnum;
return res;
}
LUA_API int lua_toboolean (lua_State *L, int idx) {
const TValue *o = index2addr(L, idx);
return !l_isfalse(o);
}
LUA_API const char *lua_tolstring (lua_State *L, int idx, size_t *len) {
StkId o = index2addr(L, idx);
if (!ttisstring(o)) {
if (!cvt2str(o)) {  
if (len != NULL) *len = 0;
return NULL;
}
lua_lock(L);  
luaO_tostring(L, o);
luaC_checkGC(L);
o = index2addr(L, idx);  
lua_unlock(L);
}
if (len != NULL)
*len = vslen(o);
return svalue(o);
}
LUA_API size_t lua_rawlen (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
switch (ttype(o)) {
case LUA_TSHRSTR: return tsvalue(o)->shrlen;
case LUA_TLNGSTR: return tsvalue(o)->u.lnglen;
case LUA_TUSERDATA: return uvalue(o)->len;
case LUA_TTABLE: return luaH_getn(hvalue(o));
default: return 0;
}
}
LUA_API lua_CFunction lua_tocfunction (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
if (ttislcf(o)) return fvalue(o);
else if (ttisCclosure(o))
return clCvalue(o)->f;
else return NULL;  
}
LUA_API void *lua_touserdata (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
switch (ttnov(o)) {
case LUA_TUSERDATA: return getudatamem(uvalue(o));
case LUA_TLIGHTUSERDATA: return pvalue(o);
default: return NULL;
}
}
LUA_API lua_State *lua_tothread (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
return (!ttisthread(o)) ? NULL : thvalue(o);
}
LUA_API const void *lua_topointer (lua_State *L, int idx) {
StkId o = index2addr(L, idx);
switch (ttype(o)) {
case LUA_TTABLE: return hvalue(o);
case LUA_TLCL: return clLvalue(o);
case LUA_TCCL: return clCvalue(o);
case LUA_TLCF: return cast(void *, cast(size_t, fvalue(o)));
case LUA_TTHREAD: return thvalue(o);
case LUA_TUSERDATA: return getudatamem(uvalue(o));
case LUA_TLIGHTUSERDATA: return pvalue(o);
default: return NULL;
}
}
LUA_API void lua_pushnil (lua_State *L) {
lua_lock(L);
setnilvalue(L->top);
api_incr_top(L);
lua_unlock(L);
}
LUA_API void lua_pushnumber (lua_State *L, lua_Number n) {
lua_lock(L);
setfltvalue(L->top, n);
api_incr_top(L);
lua_unlock(L);
}
LUA_API void lua_pushinteger (lua_State *L, lua_Integer n) {
lua_lock(L);
setivalue(L->top, n);
api_incr_top(L);
lua_unlock(L);
}
LUA_API const char *lua_pushlstring (lua_State *L, const char *s, size_t len) {
TString *ts;
lua_lock(L);
ts = (len == 0) ? luaS_new(L, "") : luaS_newlstr(L, s, len);
setsvalue2s(L, L->top, ts);
api_incr_top(L);
luaC_checkGC(L);
lua_unlock(L);
return getstr(ts);
}
LUA_API const char *lua_pushstring (lua_State *L, const char *s) {
lua_lock(L);
if (s == NULL)
setnilvalue(L->top);
else {
TString *ts;
ts = luaS_new(L, s);
setsvalue2s(L, L->top, ts);
s = getstr(ts);  
}
api_incr_top(L);
luaC_checkGC(L);
lua_unlock(L);
return s;
}
LUA_API const char *lua_pushvfstring (lua_State *L, const char *fmt,
va_list argp) {
const char *ret;
lua_lock(L);
ret = luaO_pushvfstring(L, fmt, argp);
luaC_checkGC(L);
lua_unlock(L);
return ret;
}
LUA_API const char *lua_pushfstring (lua_State *L, const char *fmt, ...) {
const char *ret;
va_list argp;
lua_lock(L);
va_start(argp, fmt);
ret = luaO_pushvfstring(L, fmt, argp);
va_end(argp);
luaC_checkGC(L);
lua_unlock(L);
return ret;
}
LUA_API void lua_pushcclosure (lua_State *L, lua_CFunction fn, int n) {
lua_lock(L);
if (n == 0) {
setfvalue(L->top, fn);
api_incr_top(L);
}
else {
CClosure *cl;
api_checknelems(L, n);
api_check(L, n <= MAXUPVAL, "upvalue index too large");
cl = luaF_newCclosure(L, n);
cl->f = fn;
L->top -= n;
while (n--) {
setobj2n(L, &cl->upvalue[n], L->top + n);
}
setclCvalue(L, L->top, cl);
api_incr_top(L);
luaC_checkGC(L);
}
lua_unlock(L);
}
LUA_API void lua_pushboolean (lua_State *L, int b) {
lua_lock(L);
setbvalue(L->top, (b != 0));  
api_incr_top(L);
lua_unlock(L);
}
LUA_API void lua_pushlightuserdata (lua_State *L, void *p) {
lua_lock(L);
setpvalue(L->top, p);
api_incr_top(L);
lua_unlock(L);
}
LUA_API int lua_pushthread (lua_State *L) {
lua_lock(L);
setthvalue(L, L->top, L);
api_incr_top(L);
lua_unlock(L);
return (G(L)->mainthread == L);
}
static int auxgetstr (lua_State *L, const TValue *t, const char *k) {
const TValue *slot;
TString *str = luaS_new(L, k);
if (luaV_fastget(L, t, str, slot, luaH_getstr)) {
setobj2s(L, L->top, slot);
api_incr_top(L);
}
else {
setsvalue2s(L, L->top, str);
api_incr_top(L);
luaV_finishget(L, t, L->top - 1, L->top - 1, slot);
}
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API int lua_getglobal (lua_State *L, const char *name) {
Table *reg = hvalue(&G(L)->l_registry);
lua_lock(L);
return auxgetstr(L, luaH_getint(reg, LUA_RIDX_GLOBALS), name);
}
LUA_API int lua_gettable (lua_State *L, int idx) {
StkId t;
lua_lock(L);
t = index2addr(L, idx);
luaV_gettable(L, t, L->top - 1, L->top - 1);
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API int lua_getfield (lua_State *L, int idx, const char *k) {
lua_lock(L);
return auxgetstr(L, index2addr(L, idx), k);
}
LUA_API int lua_geti (lua_State *L, int idx, lua_Integer n) {
StkId t;
const TValue *slot;
lua_lock(L);
t = index2addr(L, idx);
if (luaV_fastget(L, t, n, slot, luaH_getint)) {
setobj2s(L, L->top, slot);
api_incr_top(L);
}
else {
setivalue(L->top, n);
api_incr_top(L);
luaV_finishget(L, t, L->top - 1, L->top - 1, slot);
}
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API int lua_rawget (lua_State *L, int idx) {
StkId t;
lua_lock(L);
t = index2addr(L, idx);
api_check(L, ttistable(t), "table expected");
setobj2s(L, L->top - 1, luaH_get(hvalue(t), L->top - 1));
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API int lua_rawgeti (lua_State *L, int idx, lua_Integer n) {
StkId t;
lua_lock(L);
t = index2addr(L, idx);
api_check(L, ttistable(t), "table expected");
setobj2s(L, L->top, luaH_getint(hvalue(t), n));
api_incr_top(L);
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API int lua_rawgetp (lua_State *L, int idx, const void *p) {
StkId t;
TValue k;
lua_lock(L);
t = index2addr(L, idx);
api_check(L, ttistable(t), "table expected");
setpvalue(&k, cast(void *, p));
setobj2s(L, L->top, luaH_get(hvalue(t), &k));
api_incr_top(L);
lua_unlock(L);
return ttnov(L->top - 1);
}
LUA_API void lua_createtable (lua_State *L, int narray, int nrec) {
Table *t;
lua_lock(L);
t = luaH_new(L);
sethvalue(L, L->top, t);
api_incr_top(L);
if (narray > 0 || nrec > 0)
luaH_resize(L, t, narray, nrec);
luaC_checkGC(L);
lua_unlock(L);
}
LUA_API int lua_getmetatable (lua_State *L, int objindex) {
const TValue *obj;
Table *mt;
int res = 0;
lua_lock(L);
obj = index2addr(L, objindex);
switch (ttnov(obj)) {
case LUA_TTABLE:
mt = hvalue(obj)->metatable;
break;
case LUA_TUSERDATA:
mt = uvalue(obj)->metatable;
break;
default:
mt = G(L)->mt[ttnov(obj)];
break;
}
if (mt != NULL) {
sethvalue(L, L->top, mt);
api_incr_top(L);
res = 1;
}
lua_unlock(L);
return res;
}
LUA_API int lua_getuservalue (lua_State *L, int idx) {
StkId o;
lua_lock(L);
o = index2addr(L, idx);
api_check(L, ttisfulluserdata(o), "full userdata expected");
getuservalue(L, uvalue(o), L->top);
api_incr_top(L);
lua_unlock(L);
return ttnov(L->top - 1);
}
static void auxsetstr (lua_State *L, const TValue *t, const char *k) {
const TValue *slot;
TString *str = luaS_new(L, k);
api_checknelems(L, 1);
if (luaV_fastset(L, t, str, slot, luaH_getstr, L->top - 1))
L->top--;  
else {
setsvalue2s(L, L->top, str);  
api_incr_top(L);
luaV_finishset(L, t, L->top - 1, L->top - 2, slot);
L->top -= 2;  
}
lua_unlock(L);  
}
LUA_API void lua_setglobal (lua_State *L, const char *name) {
Table *reg = hvalue(&G(L)->l_registry);
lua_lock(L);  
auxsetstr(L, luaH_getint(reg, LUA_RIDX_GLOBALS), name);
}
LUA_API void lua_settable (lua_State *L, int idx) {
StkId t;
lua_lock(L);
api_checknelems(L, 2);
t = index2addr(L, idx);
luaV_settable(L, t, L->top - 2, L->top - 1);
L->top -= 2;  
lua_unlock(L);
}
LUA_API void lua_setfield (lua_State *L, int idx, const char *k) {
lua_lock(L);  
auxsetstr(L, index2addr(L, idx), k);
}
LUA_API void lua_seti (lua_State *L, int idx, lua_Integer n) {
StkId t;
const TValue *slot;
lua_lock(L);
api_checknelems(L, 1);
t = index2addr(L, idx);
if (luaV_fastset(L, t, n, slot, luaH_getint, L->top - 1))
L->top--;  
else {
setivalue(L->top, n);
api_incr_top(L);
luaV_finishset(L, t, L->top - 1, L->top - 2, slot);
L->top -= 2;  
}
lua_unlock(L);
}
LUA_API void lua_rawset (lua_State *L, int idx) {
StkId o;
TValue *slot;
lua_lock(L);
api_checknelems(L, 2);
o = index2addr(L, idx);
api_check(L, ttistable(o), "table expected");
slot = luaH_set(L, hvalue(o), L->top - 2);
setobj2t(L, slot, L->top - 1);
invalidateTMcache(hvalue(o));
luaC_barrierback(L, hvalue(o), L->top-1);
L->top -= 2;
lua_unlock(L);
}
LUA_API void lua_rawseti (lua_State *L, int idx, lua_Integer n) {
StkId o;
lua_lock(L);
api_checknelems(L, 1);
o = index2addr(L, idx);
api_check(L, ttistable(o), "table expected");
luaH_setint(L, hvalue(o), n, L->top - 1);
luaC_barrierback(L, hvalue(o), L->top-1);
L->top--;
lua_unlock(L);
}
LUA_API void lua_rawsetp (lua_State *L, int idx, const void *p) {
StkId o;
TValue k, *slot;
lua_lock(L);
api_checknelems(L, 1);
o = index2addr(L, idx);
api_check(L, ttistable(o), "table expected");
setpvalue(&k, cast(void *, p));
slot = luaH_set(L, hvalue(o), &k);
setobj2t(L, slot, L->top - 1);
luaC_barrierback(L, hvalue(o), L->top - 1);
L->top--;
lua_unlock(L);
}
LUA_API int lua_setmetatable (lua_State *L, int objindex) {
TValue *obj;
Table *mt;
lua_lock(L);
api_checknelems(L, 1);
obj = index2addr(L, objindex);
if (ttisnil(L->top - 1))
mt = NULL;
else {
api_check(L, ttistable(L->top - 1), "table expected");
mt = hvalue(L->top - 1);
}
switch (ttnov(obj)) {
case LUA_TTABLE: {
hvalue(obj)->metatable = mt;
if (mt) {
luaC_objbarrier(L, gcvalue(obj), mt);
luaC_checkfinalizer(L, gcvalue(obj), mt);
}
break;
}
case LUA_TUSERDATA: {
uvalue(obj)->metatable = mt;
if (mt) {
luaC_objbarrier(L, uvalue(obj), mt);
luaC_checkfinalizer(L, gcvalue(obj), mt);
}
break;
}
default: {
G(L)->mt[ttnov(obj)] = mt;
break;
}
}
L->top--;
lua_unlock(L);
return 1;
}
LUA_API void lua_setuservalue (lua_State *L, int idx) {
StkId o;
lua_lock(L);
api_checknelems(L, 1);
o = index2addr(L, idx);
api_check(L, ttisfulluserdata(o), "full userdata expected");
setuservalue(L, uvalue(o), L->top - 1);
luaC_barrier(L, gcvalue(o), L->top - 1);
L->top--;
lua_unlock(L);
}
#define checkresults(L,na,nr) \
api_check(L, (nr) == LUA_MULTRET || (L->ci->top - L->top >= (nr) - (na)), \
"results from function overflow current stack size")
LUA_API void lua_callk (lua_State *L, int nargs, int nresults,
lua_KContext ctx, lua_KFunction k) {
StkId func;
lua_lock(L);
api_check(L, k == NULL || !isLua(L->ci),
"cannot use continuations inside hooks");
api_checknelems(L, nargs+1);
api_check(L, L->status == LUA_OK, "cannot do calls on non-normal thread");
checkresults(L, nargs, nresults);
func = L->top - (nargs+1);
if (k != NULL && L->nny == 0) {  
L->ci->u.c.k = k;  
L->ci->u.c.ctx = ctx;  
luaD_call(L, func, nresults);  
}
else  
luaD_callnoyield(L, func, nresults);  
adjustresults(L, nresults);
lua_unlock(L);
}
struct CallS {  
StkId func;
int nresults;
};
static void f_call (lua_State *L, void *ud) {
struct CallS *c = cast(struct CallS *, ud);
luaD_callnoyield(L, c->func, c->nresults);
}
LUA_API int lua_pcallk (lua_State *L, int nargs, int nresults, int errfunc,
lua_KContext ctx, lua_KFunction k) {
struct CallS c;
int status;
ptrdiff_t func;
lua_lock(L);
api_check(L, k == NULL || !isLua(L->ci),
"cannot use continuations inside hooks");
api_checknelems(L, nargs+1);
api_check(L, L->status == LUA_OK, "cannot do calls on non-normal thread");
checkresults(L, nargs, nresults);
if (errfunc == 0)
func = 0;
else {
StkId o = index2addr(L, errfunc);
api_checkstackindex(L, errfunc, o);
func = savestack(L, o);
}
c.func = L->top - (nargs+1);  
if (k == NULL || L->nny > 0) {  
c.nresults = nresults;  
status = luaD_pcall(L, f_call, &c, savestack(L, c.func), func);
}
else {  
CallInfo *ci = L->ci;
ci->u.c.k = k;  
ci->u.c.ctx = ctx;  
ci->extra = savestack(L, c.func);
ci->u.c.old_errfunc = L->errfunc;
L->errfunc = func;
setoah(ci->callstatus, L->allowhook);  
ci->callstatus |= CIST_YPCALL;  
luaD_call(L, c.func, nresults);  
ci->callstatus &= ~CIST_YPCALL;
L->errfunc = ci->u.c.old_errfunc;
status = LUA_OK;  
}
adjustresults(L, nresults);
lua_unlock(L);
return status;
}
LUA_API int lua_load (lua_State *L, lua_Reader reader, void *data,
const char *chunkname, const char *mode) {
ZIO z;
int status;
lua_lock(L);
if (!chunkname) chunkname = "?";
luaZ_init(L, &z, reader, data);
status = luaD_protectedparser(L, &z, chunkname, mode);
if (status == LUA_OK) {  
LClosure *f = clLvalue(L->top - 1);  
if (f->nupvalues >= 1) {  
Table *reg = hvalue(&G(L)->l_registry);
const TValue *gt = luaH_getint(reg, LUA_RIDX_GLOBALS);
setobj(L, f->upvals[0]->v, gt);
luaC_upvalbarrier(L, f->upvals[0]);
}
}
lua_unlock(L);
return status;
}
LUA_API int lua_dump (lua_State *L, lua_Writer writer, void *data, int strip) {
int status;
TValue *o;
lua_lock(L);
api_checknelems(L, 1);
o = L->top - 1;
if (isLfunction(o))
status = luaU_dump(L, getproto(o), writer, data, strip);
else
status = 1;
lua_unlock(L);
return status;
}
LUA_API int lua_status (lua_State *L) {
return L->status;
}
LUA_API int lua_gc (lua_State *L, int what, int data) {
int res = 0;
global_State *g;
lua_lock(L);
g = G(L);
switch (what) {
case LUA_GCSTOP: {
g->gcrunning = 0;
break;
}
case LUA_GCRESTART: {
luaE_setdebt(g, 0);
g->gcrunning = 1;
break;
}
case LUA_GCCOLLECT: {
luaC_fullgc(L, 0);
break;
}
case LUA_GCCOUNT: {
res = cast_int(gettotalbytes(g) >> 10);
break;
}
case LUA_GCCOUNTB: {
res = cast_int(gettotalbytes(g) & 0x3ff);
break;
}
case LUA_GCSTEP: {
l_mem debt = 1;  
lu_byte oldrunning = g->gcrunning;
g->gcrunning = 1;  
if (data == 0) {
luaE_setdebt(g, -GCSTEPSIZE);  
luaC_step(L);
}
else {  
debt = cast(l_mem, data) * 1024 + g->GCdebt;
luaE_setdebt(g, debt);
luaC_checkGC(L);
}
g->gcrunning = oldrunning;  
if (debt > 0 && g->gcstate == GCSpause)  
res = 1;  
break;
}
case LUA_GCSETPAUSE: {
res = g->gcpause;
g->gcpause = data;
break;
}
case LUA_GCSETSTEPMUL: {
res = g->gcstepmul;
if (data < 40) data = 40;  
g->gcstepmul = data;
break;
}
case LUA_GCISRUNNING: {
res = g->gcrunning;
break;
}
default: res = -1;  
}
lua_unlock(L);
return res;
}
LUA_API int lua_error (lua_State *L) {
lua_lock(L);
api_checknelems(L, 1);
luaG_errormsg(L);
return 0;  
}
LUA_API int lua_next (lua_State *L, int idx) {
StkId t;
int more;
lua_lock(L);
t = index2addr(L, idx);
api_check(L, ttistable(t), "table expected");
more = luaH_next(L, hvalue(t), L->top - 1);
if (more) {
api_incr_top(L);
}
else  
L->top -= 1;  
lua_unlock(L);
return more;
}
LUA_API void lua_concat (lua_State *L, int n) {
lua_lock(L);
api_checknelems(L, n);
if (n >= 2) {
luaV_concat(L, n);
}
else if (n == 0) {  
setsvalue2s(L, L->top, luaS_newlstr(L, "", 0));
api_incr_top(L);
}
luaC_checkGC(L);
lua_unlock(L);
}
LUA_API void lua_len (lua_State *L, int idx) {
StkId t;
lua_lock(L);
t = index2addr(L, idx);
luaV_objlen(L, L->top, t);
api_incr_top(L);
lua_unlock(L);
}
LUA_API lua_Alloc lua_getallocf (lua_State *L, void **ud) {
lua_Alloc f;
lua_lock(L);
if (ud) *ud = G(L)->ud;
f = G(L)->frealloc;
lua_unlock(L);
return f;
}
LUA_API void lua_setallocf (lua_State *L, lua_Alloc f, void *ud) {
lua_lock(L);
G(L)->ud = ud;
G(L)->frealloc = f;
lua_unlock(L);
}
LUA_API void *lua_newuserdata (lua_State *L, size_t size) {
Udata *u;
lua_lock(L);
u = luaS_newudata(L, size);
setuvalue(L, L->top, u);
api_incr_top(L);
luaC_checkGC(L);
lua_unlock(L);
return getudatamem(u);
}
static const char *aux_upvalue (StkId fi, int n, TValue **val,
CClosure **owner, UpVal **uv) {
switch (ttype(fi)) {
case LUA_TCCL: {  
CClosure *f = clCvalue(fi);
if (!(1 <= n && n <= f->nupvalues)) return NULL;
*val = &f->upvalue[n-1];
if (owner) *owner = f;
return "";
}
case LUA_TLCL: {  
LClosure *f = clLvalue(fi);
TString *name;
Proto *p = f->p;
if (!(1 <= n && n <= p->sizeupvalues)) return NULL;
*val = f->upvals[n-1]->v;
if (uv) *uv = f->upvals[n - 1];
name = p->upvalues[n-1].name;
return (name == NULL) ? "(*no name)" : getstr(name);
}
default: return NULL;  
}
}
LUA_API const char *lua_getupvalue (lua_State *L, int funcindex, int n) {
const char *name;
TValue *val = NULL;  
lua_lock(L);
name = aux_upvalue(index2addr(L, funcindex), n, &val, NULL, NULL);
if (name) {
setobj2s(L, L->top, val);
api_incr_top(L);
}
lua_unlock(L);
return name;
}
LUA_API const char *lua_setupvalue (lua_State *L, int funcindex, int n) {
const char *name;
TValue *val = NULL;  
CClosure *owner = NULL;
UpVal *uv = NULL;
StkId fi;
lua_lock(L);
fi = index2addr(L, funcindex);
api_checknelems(L, 1);
name = aux_upvalue(fi, n, &val, &owner, &uv);
if (name) {
L->top--;
setobj(L, val, L->top);
if (owner) { luaC_barrier(L, owner, L->top); }
else if (uv) { luaC_upvalbarrier(L, uv); }
}
lua_unlock(L);
return name;
}
static UpVal **getupvalref (lua_State *L, int fidx, int n, LClosure **pf) {
LClosure *f;
StkId fi = index2addr(L, fidx);
api_check(L, ttisLclosure(fi), "Lua function expected");
f = clLvalue(fi);
api_check(L, (1 <= n && n <= f->p->sizeupvalues), "invalid upvalue index");
if (pf) *pf = f;
return &f->upvals[n - 1];  
}
LUA_API void *lua_upvalueid (lua_State *L, int fidx, int n) {
StkId fi = index2addr(L, fidx);
switch (ttype(fi)) {
case LUA_TLCL: {  
return *getupvalref(L, fidx, n, NULL);
}
case LUA_TCCL: {  
CClosure *f = clCvalue(fi);
api_check(L, 1 <= n && n <= f->nupvalues, "invalid upvalue index");
return &f->upvalue[n - 1];
}
default: {
api_check(L, 0, "closure expected");
return NULL;
}
}
}
LUA_API void lua_upvaluejoin (lua_State *L, int fidx1, int n1,
int fidx2, int n2) {
LClosure *f1;
UpVal **up1 = getupvalref(L, fidx1, n1, &f1);
UpVal **up2 = getupvalref(L, fidx2, n2, NULL);
luaC_upvdeccount(L, *up1);
*up1 = *up2;
(*up1)->refcount++;
if (upisopen(*up1)) (*up1)->u.open.touched = 1;
luaC_upvalbarrier(L, *up1);
}
#define lauxlib_c
#define LUA_LIB
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef lauxlib_h
#define lauxlib_h
#include <stddef.h>
#include <stdio.h>
#define LUA_ERRFILE     (LUA_ERRERR+1)
#define LUA_LOADED_TABLE	"_LOADED"
#define LUA_PRELOAD_TABLE	"_PRELOAD"
typedef struct luaL_Reg {
const char *name;
lua_CFunction func;
} luaL_Reg;
#define LUAL_NUMSIZES	(sizeof(lua_Integer)*16 + sizeof(lua_Number))
LUALIB_API void (luaL_checkversion_) (lua_State *L, lua_Number ver, size_t sz);
#define luaL_checkversion(L)  \
luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES)
LUALIB_API int (luaL_getmetafield) (lua_State *L, int obj, const char *e);
LUALIB_API int (luaL_callmeta) (lua_State *L, int obj, const char *e);
LUALIB_API const char *(luaL_tolstring) (lua_State *L, int idx, size_t *len);
LUALIB_API int (luaL_argerror) (lua_State *L, int arg, const char *extramsg);
LUALIB_API const char *(luaL_checklstring) (lua_State *L, int arg,
size_t *l);
LUALIB_API const char *(luaL_optlstring) (lua_State *L, int arg,
const char *def, size_t *l);
LUALIB_API lua_Number (luaL_checknumber) (lua_State *L, int arg);
LUALIB_API lua_Number (luaL_optnumber) (lua_State *L, int arg, lua_Number def);
LUALIB_API lua_Integer (luaL_checkinteger) (lua_State *L, int arg);
LUALIB_API lua_Integer (luaL_optinteger) (lua_State *L, int arg,
lua_Integer def);
LUALIB_API void (luaL_checkstack) (lua_State *L, int sz, const char *msg);
LUALIB_API void (luaL_checktype) (lua_State *L, int arg, int t);
LUALIB_API void (luaL_checkany) (lua_State *L, int arg);
LUALIB_API int   (luaL_newmetatable) (lua_State *L, const char *tname);
LUALIB_API void  (luaL_setmetatable) (lua_State *L, const char *tname);
LUALIB_API void *(luaL_testudata) (lua_State *L, int ud, const char *tname);
LUALIB_API void *(luaL_checkudata) (lua_State *L, int ud, const char *tname);
LUALIB_API void (luaL_where) (lua_State *L, int lvl);
LUALIB_API int (luaL_error) (lua_State *L, const char *fmt, ...);
LUALIB_API int (luaL_checkoption) (lua_State *L, int arg, const char *def,
const char *const lst[]);
LUALIB_API int (luaL_fileresult) (lua_State *L, int stat, const char *fname);
LUALIB_API int (luaL_execresult) (lua_State *L, int stat);
#define LUA_NOREF       (-2)
#define LUA_REFNIL      (-1)
LUALIB_API int (luaL_ref) (lua_State *L, int t);
LUALIB_API void (luaL_unref) (lua_State *L, int t, int ref);
LUALIB_API int (luaL_loadfilex) (lua_State *L, const char *filename,
const char *mode);
#define luaL_loadfile(L,f)	luaL_loadfilex(L,f,NULL)
LUALIB_API int (luaL_loadbufferx) (lua_State *L, const char *buff, size_t sz,
const char *name, const char *mode);
LUALIB_API int (luaL_loadstring) (lua_State *L, const char *s);
LUALIB_API lua_State *(luaL_newstate) (void);
LUALIB_API lua_Integer (luaL_len) (lua_State *L, int idx);
LUALIB_API const char *(luaL_gsub) (lua_State *L, const char *s, const char *p,
const char *r);
LUALIB_API void (luaL_setfuncs) (lua_State *L, const luaL_Reg *l, int nup);
LUALIB_API int (luaL_getsubtable) (lua_State *L, int idx, const char *fname);
LUALIB_API void (luaL_traceback) (lua_State *L, lua_State *L1,
const char *msg, int level);
LUALIB_API void (luaL_requiref) (lua_State *L, const char *modname,
lua_CFunction openf, int glb);
#define luaL_newlibtable(L,l)	\
lua_createtable(L, 0, sizeof(l)/sizeof((l)[0]) - 1)
#define luaL_newlib(L,l)  \
(luaL_checkversion(L), luaL_newlibtable(L,l), luaL_setfuncs(L,l,0))
#define luaL_argcheck(L, cond,arg,extramsg)	\
((void)((cond) || luaL_argerror(L, (arg), (extramsg))))
#define luaL_checkstring(L,n)	(luaL_checklstring(L, (n), NULL))
#define luaL_optstring(L,n,d)	(luaL_optlstring(L, (n), (d), NULL))
#define luaL_typename(L,i)	lua_typename(L, lua_type(L,(i)))
#define luaL_dofile(L, fn) \
(luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))
#define luaL_dostring(L, s) \
(luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))
#define luaL_getmetatable(L,n)	(lua_getfield(L, LUA_REGISTRYINDEX, (n)))
#define luaL_opt(L,f,n,d)	(lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
#define luaL_loadbuffer(L,s,sz,n)	luaL_loadbufferx(L,s,sz,n,NULL)
typedef struct luaL_Buffer {
char *b;  
size_t size;  
size_t n;  
lua_State *L;
char initb[LUAL_BUFFERSIZE];  
} luaL_Buffer;
#define luaL_addchar(B,c) \
((void)((B)->n < (B)->size || luaL_prepbuffsize((B), 1)), \
((B)->b[(B)->n++] = (c)))
#define luaL_addsize(B,s)	((B)->n += (s))
LUALIB_API void (luaL_buffinit) (lua_State *L, luaL_Buffer *B);
LUALIB_API char *(luaL_prepbuffsize) (luaL_Buffer *B, size_t sz);
LUALIB_API void (luaL_addlstring) (luaL_Buffer *B, const char *s, size_t l);
LUALIB_API void (luaL_addstring) (luaL_Buffer *B, const char *s);
LUALIB_API void (luaL_addvalue) (luaL_Buffer *B);
LUALIB_API void (luaL_pushresult) (luaL_Buffer *B);
LUALIB_API void (luaL_pushresultsize) (luaL_Buffer *B, size_t sz);
LUALIB_API char *(luaL_buffinitsize) (lua_State *L, luaL_Buffer *B, size_t sz);
#define luaL_prepbuffer(B)	luaL_prepbuffsize(B, LUAL_BUFFERSIZE)
#define LUA_FILEHANDLE          "FILE*"
typedef struct luaL_Stream {
FILE *f;  
lua_CFunction closef;  
} luaL_Stream;
#if defined(LUA_COMPAT_MODULE)
LUALIB_API void (luaL_pushmodule) (lua_State *L, const char *modname,
int sizehint);
LUALIB_API void (luaL_openlib) (lua_State *L, const char *libname,
const luaL_Reg *l, int nup);
#define luaL_register(L,n,l)	(luaL_openlib(L,(n),(l),0))
#endif
#if !defined(lua_writestring)
#define lua_writestring(s,l)   fwrite((s), sizeof(char), (l), stdout)
#endif
#if !defined(lua_writeline)
#define lua_writeline()        (lua_writestring("\n", 1), fflush(stdout))
#endif
#if !defined(lua_writestringerror)
#define lua_writestringerror(s,p) \
(fprintf(stderr, (s), (p)), fflush(stderr))
#endif
#if defined(LUA_COMPAT_APIINTCASTS)
#define luaL_checkunsigned(L,a)	((lua_Unsigned)luaL_checkinteger(L,a))
#define luaL_optunsigned(L,a,d)	\
((lua_Unsigned)luaL_optinteger(L,a,(lua_Integer)(d)))
#define luaL_checkint(L,n)	((int)luaL_checkinteger(L, (n)))
#define luaL_optint(L,n,d)	((int)luaL_optinteger(L, (n), (d)))
#define luaL_checklong(L,n)	((long)luaL_checkinteger(L, (n)))
#define luaL_optlong(L,n,d)	((long)luaL_optinteger(L, (n), (d)))
#endif
#endif
#define LEVELS1	10	
#define LEVELS2	11	
static int findfield (lua_State *L, int objidx, int level) {
if (level == 0 || !lua_istable(L, -1))
return 0;  
lua_pushnil(L);  
while (lua_next(L, -2)) {  
if (lua_type(L, -2) == LUA_TSTRING) {  
if (lua_rawequal(L, objidx, -1)) {  
lua_pop(L, 1);  
return 1;
}
else if (findfield(L, objidx, level - 1)) {  
lua_remove(L, -2);  
lua_pushliteral(L, ".");
lua_insert(L, -2);  
lua_concat(L, 3);
return 1;
}
}
lua_pop(L, 1);  
}
return 0;  
}
static int pushglobalfuncname (lua_State *L, lua_Debug *ar) {
int top = lua_gettop(L);
lua_getinfo(L, "f", ar);  
lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
if (findfield(L, top + 1, 2)) {
const char *name = lua_tostring(L, -1);
if (strncmp(name, "_G.", 3) == 0) {  
lua_pushstring(L, name + 3);  
lua_remove(L, -2);  
}
lua_copy(L, -1, top + 1);  
lua_pop(L, 2);  
return 1;
}
else {
lua_settop(L, top);  
return 0;
}
}
static void pushfuncname (lua_State *L, lua_Debug *ar) {
if (pushglobalfuncname(L, ar)) {  
lua_pushfstring(L, "function '%s'", lua_tostring(L, -1));
lua_remove(L, -2);  
}
else if (*ar->namewhat != '\0')  
lua_pushfstring(L, "%s '%s'", ar->namewhat, ar->name);  
else if (*ar->what == 'm')  
lua_pushliteral(L, "main chunk");
else if (*ar->what != 'C')  
lua_pushfstring(L, "function <%s:%d>", ar->short_src, ar->linedefined);
else  
lua_pushliteral(L, "?");
}
static int lastlevel (lua_State *L) {
lua_Debug ar;
int li = 1, le = 1;
while (lua_getstack(L, le, &ar)) { li = le; le *= 2; }
while (li < le) {
int m = (li + le)/2;
if (lua_getstack(L, m, &ar)) li = m + 1;
else le = m;
}
return le - 1;
}
LUALIB_API void luaL_traceback (lua_State *L, lua_State *L1,
const char *msg, int level) {
lua_Debug ar;
int top = lua_gettop(L);
int last = lastlevel(L1);
int n1 = (last - level > LEVELS1 + LEVELS2) ? LEVELS1 : -1;
if (msg)
lua_pushfstring(L, "%s\n", msg);
luaL_checkstack(L, 10, NULL);
lua_pushliteral(L, "stack traceback:");
while (lua_getstack(L1, level++, &ar)) {
if (n1-- == 0) {  
lua_pushliteral(L, "\n\t...");  
level = last - LEVELS2 + 1;  
}
else {
lua_getinfo(L1, "Slnt", &ar);
lua_pushfstring(L, "\n\t%s:", ar.short_src);
if (ar.currentline > 0)
lua_pushfstring(L, "%d:", ar.currentline);
lua_pushliteral(L, " in ");
pushfuncname(L, &ar);
if (ar.istailcall)
lua_pushliteral(L, "\n\t(...tail calls...)");
lua_concat(L, lua_gettop(L) - top);
}
}
lua_concat(L, lua_gettop(L) - top);
}
LUALIB_API int luaL_argerror (lua_State *L, int arg, const char *extramsg) {
lua_Debug ar;
if (!lua_getstack(L, 0, &ar))  
return luaL_error(L, "bad argument #%d (%s)", arg, extramsg);
lua_getinfo(L, "n", &ar);
if (strcmp(ar.namewhat, "method") == 0) {
arg--;  
if (arg == 0)  
return luaL_error(L, "calling '%s' on bad self (%s)",
ar.name, extramsg);
}
if (ar.name == NULL)
ar.name = (pushglobalfuncname(L, &ar)) ? lua_tostring(L, -1) : "?";
return luaL_error(L, "bad argument #%d to '%s' (%s)",
arg, ar.name, extramsg);
}
static int typeerror (lua_State *L, int arg, const char *tname) {
const char *msg;
const char *typearg;  
if (luaL_getmetafield(L, arg, "__name") == LUA_TSTRING)
typearg = lua_tostring(L, -1);  
else if (lua_type(L, arg) == LUA_TLIGHTUSERDATA)
typearg = "light userdata";  
else
typearg = luaL_typename(L, arg);  
msg = lua_pushfstring(L, "%s expected, got %s", tname, typearg);
return luaL_argerror(L, arg, msg);
}
static void tag_error (lua_State *L, int arg, int tag) {
typeerror(L, arg, lua_typename(L, tag));
}
LUALIB_API void luaL_where (lua_State *L, int level) {
lua_Debug ar;
if (lua_getstack(L, level, &ar)) {  
lua_getinfo(L, "Sl", &ar);  
if (ar.currentline > 0) {  
lua_pushfstring(L, "%s:%d: ", ar.short_src, ar.currentline);
return;
}
}
lua_pushfstring(L, "");  
}
LUALIB_API int luaL_error (lua_State *L, const char *fmt, ...) {
va_list argp;
va_start(argp, fmt);
luaL_where(L, 1);
lua_pushvfstring(L, fmt, argp);
va_end(argp);
lua_concat(L, 2);
return lua_error(L);
}
LUALIB_API int luaL_fileresult (lua_State *L, int stat, const char *fname) {
int en = errno;  
if (stat) {
lua_pushboolean(L, 1);
return 1;
}
else {
lua_pushnil(L);
if (fname)
lua_pushfstring(L, "%s: %s", fname, strerror(en));
else
lua_pushstring(L, strerror(en));
lua_pushinteger(L, en);
return 3;
}
}
#if !defined(l_inspectstat)	
#if defined(LUA_USE_POSIX)
#include <sys/wait.h>
#define l_inspectstat(stat,what)  \
if (WIFEXITED(stat)) { stat = WEXITSTATUS(stat); } \
else if (WIFSIGNALED(stat)) { stat = WTERMSIG(stat); what = "signal"; }
#else
#define l_inspectstat(stat,what)  
#endif
#endif				
LUALIB_API int luaL_execresult (lua_State *L, int stat) {
const char *what = "exit";  
if (stat == -1)  
return luaL_fileresult(L, 0, NULL);
else {
l_inspectstat(stat, what);  
if (*what == 'e' && stat == 0)  
lua_pushboolean(L, 1);
else
lua_pushnil(L);
lua_pushstring(L, what);
lua_pushinteger(L, stat);
return 3;  
}
}
LUALIB_API int luaL_newmetatable (lua_State *L, const char *tname) {
if (luaL_getmetatable(L, tname) != LUA_TNIL)  
return 0;  
lua_pop(L, 1);
lua_createtable(L, 0, 2);  
lua_pushstring(L, tname);
lua_setfield(L, -2, "__name");  
lua_pushvalue(L, -1);
lua_setfield(L, LUA_REGISTRYINDEX, tname);  
return 1;
}
LUALIB_API void luaL_setmetatable (lua_State *L, const char *tname) {
luaL_getmetatable(L, tname);
lua_setmetatable(L, -2);
}
LUALIB_API void *luaL_testudata (lua_State *L, int ud, const char *tname) {
void *p = lua_touserdata(L, ud);
if (p != NULL) {  
if (lua_getmetatable(L, ud)) {  
luaL_getmetatable(L, tname);  
if (!lua_rawequal(L, -1, -2))  
p = NULL;  
lua_pop(L, 2);  
return p;
}
}
return NULL;  
}
LUALIB_API void *luaL_checkudata (lua_State *L, int ud, const char *tname) {
void *p = luaL_testudata(L, ud, tname);
if (p == NULL) typeerror(L, ud, tname);
return p;
}
LUALIB_API int luaL_checkoption (lua_State *L, int arg, const char *def,
const char *const lst[]) {
const char *name = (def) ? luaL_optstring(L, arg, def) :
luaL_checkstring(L, arg);
int i;
for (i=0; lst[i]; i++)
if (strcmp(lst[i], name) == 0)
return i;
return luaL_argerror(L, arg,
lua_pushfstring(L, "invalid option '%s'", name));
}
LUALIB_API void luaL_checkstack (lua_State *L, int space, const char *msg) {
if (!lua_checkstack(L, space)) {
if (msg)
luaL_error(L, "stack overflow (%s)", msg);
else
luaL_error(L, "stack overflow");
}
}
LUALIB_API void luaL_checktype (lua_State *L, int arg, int t) {
if (lua_type(L, arg) != t)
tag_error(L, arg, t);
}
LUALIB_API void luaL_checkany (lua_State *L, int arg) {
if (lua_type(L, arg) == LUA_TNONE)
luaL_argerror(L, arg, "value expected");
}
LUALIB_API const char *luaL_checklstring (lua_State *L, int arg, size_t *len) {
const char *s = lua_tolstring(L, arg, len);
if (!s) tag_error(L, arg, LUA_TSTRING);
return s;
}
LUALIB_API const char *luaL_optlstring (lua_State *L, int arg,
const char *def, size_t *len) {
if (lua_isnoneornil(L, arg)) {
if (len)
*len = (def ? strlen(def) : 0);
return def;
}
else return luaL_checklstring(L, arg, len);
}
LUALIB_API lua_Number luaL_checknumber (lua_State *L, int arg) {
int isnum;
lua_Number d = lua_tonumberx(L, arg, &isnum);
if (!isnum)
tag_error(L, arg, LUA_TNUMBER);
return d;
}
LUALIB_API lua_Number luaL_optnumber (lua_State *L, int arg, lua_Number def) {
return luaL_opt(L, luaL_checknumber, arg, def);
}
static void interror (lua_State *L, int arg) {
if (lua_isnumber(L, arg))
luaL_argerror(L, arg, "number has no integer representation");
else
tag_error(L, arg, LUA_TNUMBER);
}
LUALIB_API lua_Integer luaL_checkinteger (lua_State *L, int arg) {
int isnum;
lua_Integer d = lua_tointegerx(L, arg, &isnum);
if (!isnum) {
interror(L, arg);
}
return d;
}
LUALIB_API lua_Integer luaL_optinteger (lua_State *L, int arg,
lua_Integer def) {
return luaL_opt(L, luaL_checkinteger, arg, def);
}
typedef struct UBox {
void *box;
size_t bsize;
} UBox;
static void *resizebox (lua_State *L, int idx, size_t newsize) {
void *ud;
lua_Alloc allocf = lua_getallocf(L, &ud);
UBox *box = (UBox *)lua_touserdata(L, idx);
void *temp = allocf(ud, box->box, box->bsize, newsize);
if (temp == NULL && newsize > 0) {  
resizebox(L, idx, 0);  
luaL_error(L, "not enough memory for buffer allocation");
}
box->box = temp;
box->bsize = newsize;
return temp;
}
static int boxgc (lua_State *L) {
resizebox(L, 1, 0);
return 0;
}
static void *newbox (lua_State *L, size_t newsize) {
UBox *box = (UBox *)lua_newuserdata(L, sizeof(UBox));
box->box = NULL;
box->bsize = 0;
if (luaL_newmetatable(L, "LUABOX")) {  
lua_pushcfunction(L, boxgc);
lua_setfield(L, -2, "__gc");  
}
lua_setmetatable(L, -2);
return resizebox(L, -1, newsize);
}
#define buffonstack(B)	((B)->b != (B)->initb)
LUALIB_API char *luaL_prepbuffsize (luaL_Buffer *B, size_t sz) {
lua_State *L = B->L;
if (B->size - B->n < sz) {  
char *newbuff;
size_t newsize = B->size * 2;  
if (newsize - B->n < sz)  
newsize = B->n + sz;
if (newsize < B->n || newsize - B->n < sz)
luaL_error(L, "buffer too large");
if (buffonstack(B))
newbuff = (char *)resizebox(L, -1, newsize);
else {  
newbuff = (char *)newbox(L, newsize);
memcpy(newbuff, B->b, B->n * sizeof(char));  
}
B->b = newbuff;
B->size = newsize;
}
return &B->b[B->n];
}
LUALIB_API void luaL_addlstring (luaL_Buffer *B, const char *s, size_t l) {
if (l > 0) {  
char *b = luaL_prepbuffsize(B, l);
memcpy(b, s, l * sizeof(char));
luaL_addsize(B, l);
}
}
LUALIB_API void luaL_addstring (luaL_Buffer *B, const char *s) {
luaL_addlstring(B, s, strlen(s));
}
LUALIB_API void luaL_pushresult (luaL_Buffer *B) {
lua_State *L = B->L;
lua_pushlstring(L, B->b, B->n);
if (buffonstack(B)) {
resizebox(L, -2, 0);  
lua_remove(L, -2);  
}
}
LUALIB_API void luaL_pushresultsize (luaL_Buffer *B, size_t sz) {
luaL_addsize(B, sz);
luaL_pushresult(B);
}
LUALIB_API void luaL_addvalue (luaL_Buffer *B) {
lua_State *L = B->L;
size_t l;
const char *s = lua_tolstring(L, -1, &l);
if (buffonstack(B))
lua_insert(L, -2);  
luaL_addlstring(B, s, l);
lua_remove(L, (buffonstack(B)) ? -2 : -1);  
}
LUALIB_API void luaL_buffinit (lua_State *L, luaL_Buffer *B) {
B->L = L;
B->b = B->initb;
B->n = 0;
B->size = LUAL_BUFFERSIZE;
}
LUALIB_API char *luaL_buffinitsize (lua_State *L, luaL_Buffer *B, size_t sz) {
luaL_buffinit(L, B);
return luaL_prepbuffsize(B, sz);
}
#define freelist	0
LUALIB_API int luaL_ref (lua_State *L, int t) {
int ref;
if (lua_isnil(L, -1)) {
lua_pop(L, 1);  
return LUA_REFNIL;  
}
t = lua_absindex(L, t);
lua_rawgeti(L, t, freelist);  
ref = (int)lua_tointeger(L, -1);  
lua_pop(L, 1);  
if (ref != 0) {  
lua_rawgeti(L, t, ref);  
lua_rawseti(L, t, freelist);  
}
else  
ref = (int)lua_rawlen(L, t) + 1;  
lua_rawseti(L, t, ref);
return ref;
}
LUALIB_API void luaL_unref (lua_State *L, int t, int ref) {
if (ref >= 0) {
t = lua_absindex(L, t);
lua_rawgeti(L, t, freelist);
lua_rawseti(L, t, ref);  
lua_pushinteger(L, ref);
lua_rawseti(L, t, freelist);  
}
}
typedef struct LoadF {
int n;  
FILE *f;  
char buff[BUFSIZ];  
} LoadF;
static const char *getF (lua_State *L, void *ud, size_t *size) {
LoadF *lf = (LoadF *)ud;
(void)L;  
if (lf->n > 0) {  
*size = lf->n;  
lf->n = 0;  
}
else {  
/* 'fread' can return > 0 *and* set the EOF flag. If next call to
'getF' called 'fread', it might still wait for user input.
The next check avoids this problem. */
if (feof(lf->f)) return NULL;
*size = fread(lf->buff, 1, sizeof(lf->buff), lf->f);  
}
return lf->buff;
}
static int errfile (lua_State *L, const char *what, int fnameindex) {
const char *serr = strerror(errno);
const char *filename = lua_tostring(L, fnameindex) + 1;
lua_pushfstring(L, "cannot %s %s: %s", what, filename, serr);
lua_remove(L, fnameindex);
return LUA_ERRFILE;
}
static int skipBOM (LoadF *lf) {
const char *p = "\xEF\xBB\xBF";  
int c;
lf->n = 0;
do {
c = getc(lf->f);
if (c == EOF || c != *(const unsigned char *)p++) return c;
lf->buff[lf->n++] = c;  
} while (*p != '\0');
lf->n = 0;  
return getc(lf->f);  
}
static int skipcomment (LoadF *lf, int *cp) {
int c = *cp = skipBOM(lf);
if (c == '#') {  
do {  
c = getc(lf->f);
} while (c != EOF && c != '\n');
*cp = getc(lf->f);  
return 1;  
}
else return 0;  
}
LUALIB_API int luaL_loadfilex (lua_State *L, const char *filename,
const char *mode) {
LoadF lf;
int status, readstatus;
int c;
int fnameindex = lua_gettop(L) + 1;  
if (filename == NULL) {
lua_pushliteral(L, "=stdin");
lf.f = stdin;
}
else {
lua_pushfstring(L, "@%s", filename);
lf.f = fopen(filename, "r");
if (lf.f == NULL) return errfile(L, "open", fnameindex);
}
if (skipcomment(&lf, &c))  
lf.buff[lf.n++] = '\n';  
if (c == LUA_SIGNATURE[0] && filename) {  
lf.f = freopen(filename, "rb", lf.f);  
if (lf.f == NULL) return errfile(L, "reopen", fnameindex);
skipcomment(&lf, &c);  
}
if (c != EOF)
lf.buff[lf.n++] = c;  
status = lua_load(L, getF, &lf, lua_tostring(L, -1), mode);
readstatus = ferror(lf.f);
if (filename) fclose(lf.f);  
if (readstatus) {
lua_settop(L, fnameindex);  
return errfile(L, "read", fnameindex);
}
lua_remove(L, fnameindex);
return status;
}
typedef struct LoadS {
const char *s;
size_t size;
} LoadS;
static const char *getS (lua_State *L, void *ud, size_t *size) {
LoadS *ls = (LoadS *)ud;
(void)L;  
if (ls->size == 0) return NULL;
*size = ls->size;
ls->size = 0;
return ls->s;
}
LUALIB_API int luaL_loadbufferx (lua_State *L, const char *buff, size_t size,
const char *name, const char *mode) {
LoadS ls;
ls.s = buff;
ls.size = size;
return lua_load(L, getS, &ls, name, mode);
}
LUALIB_API int luaL_loadstring (lua_State *L, const char *s) {
return luaL_loadbuffer(L, s, strlen(s), s);
}
LUALIB_API int luaL_getmetafield (lua_State *L, int obj, const char *event) {
if (!lua_getmetatable(L, obj))  
return LUA_TNIL;
else {
int tt;
lua_pushstring(L, event);
tt = lua_rawget(L, -2);
if (tt == LUA_TNIL)  
lua_pop(L, 2);  
else
lua_remove(L, -2);  
return tt;  
}
}
LUALIB_API int luaL_callmeta (lua_State *L, int obj, const char *event) {
obj = lua_absindex(L, obj);
if (luaL_getmetafield(L, obj, event) == LUA_TNIL)  
return 0;
lua_pushvalue(L, obj);
lua_call(L, 1, 1);
return 1;
}
LUALIB_API lua_Integer luaL_len (lua_State *L, int idx) {
lua_Integer l;
int isnum;
lua_len(L, idx);
l = lua_tointegerx(L, -1, &isnum);
if (!isnum)
luaL_error(L, "object length is not an integer");
lua_pop(L, 1);  
return l;
}
LUALIB_API const char *luaL_tolstring (lua_State *L, int idx, size_t *len) {
if (luaL_callmeta(L, idx, "__tostring")) {  
if (!lua_isstring(L, -1))
luaL_error(L, "'__tostring' must return a string");
}
else {
switch (lua_type(L, idx)) {
case LUA_TNUMBER: {
if (lua_isinteger(L, idx))
lua_pushfstring(L, "%I", (LUAI_UACINT)lua_tointeger(L, idx));
else
lua_pushfstring(L, "%f", (LUAI_UACNUMBER)lua_tonumber(L, idx));
break;
}
case LUA_TSTRING:
lua_pushvalue(L, idx);
break;
case LUA_TBOOLEAN:
lua_pushstring(L, (lua_toboolean(L, idx) ? "true" : "false"));
break;
case LUA_TNIL:
lua_pushliteral(L, "nil");
break;
default: {
int tt = luaL_getmetafield(L, idx, "__name");  
const char *kind = (tt == LUA_TSTRING) ? lua_tostring(L, -1) :
luaL_typename(L, idx);
lua_pushfstring(L, "%s: %p", kind, lua_topointer(L, idx));
if (tt != LUA_TNIL)
lua_remove(L, -2);  
break;
}
}
}
return lua_tolstring(L, -1, len);
}
#if defined(LUA_COMPAT_MODULE)
static const char *luaL_findtable (lua_State *L, int idx,
const char *fname, int szhint) {
const char *e;
if (idx) lua_pushvalue(L, idx);
do {
e = strchr(fname, '.');
if (e == NULL) e = fname + strlen(fname);
lua_pushlstring(L, fname, e - fname);
if (lua_rawget(L, -2) == LUA_TNIL) {  
lua_pop(L, 1);  
lua_createtable(L, 0, (*e == '.' ? 1 : szhint)); 
lua_pushlstring(L, fname, e - fname);
lua_pushvalue(L, -2);
lua_settable(L, -4);  
}
else if (!lua_istable(L, -1)) {  
lua_pop(L, 2);  
return fname;  
}
lua_remove(L, -2);  
fname = e + 1;
} while (*e == '.');
return NULL;
}
static int libsize (const luaL_Reg *l) {
int size = 0;
for (; l && l->name; l++) size++;
return size;
}
LUALIB_API void luaL_pushmodule (lua_State *L, const char *modname,
int sizehint) {
luaL_findtable(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE, 1);
if (lua_getfield(L, -1, modname) != LUA_TTABLE) {  
lua_pop(L, 1);  
lua_pushglobaltable(L);
if (luaL_findtable(L, 0, modname, sizehint) != NULL)
luaL_error(L, "name conflict for module '%s'", modname);
lua_pushvalue(L, -1);
lua_setfield(L, -3, modname);  
}
lua_remove(L, -2);  
}
LUALIB_API void luaL_openlib (lua_State *L, const char *libname,
const luaL_Reg *l, int nup) {
luaL_checkversion(L);
if (libname) {
luaL_pushmodule(L, libname, libsize(l));  
lua_insert(L, -(nup + 1));  
}
if (l)
luaL_setfuncs(L, l, nup);
else
lua_pop(L, nup);  
}
#endif
LUALIB_API void luaL_setfuncs (lua_State *L, const luaL_Reg *l, int nup) {
luaL_checkstack(L, nup, "too many upvalues");
for (; l->name != NULL; l++) {  
int i;
for (i = 0; i < nup; i++)  
lua_pushvalue(L, -nup);
lua_pushcclosure(L, l->func, nup);  
lua_setfield(L, -(nup + 2), l->name);
}
lua_pop(L, nup);  
}
LUALIB_API int luaL_getsubtable (lua_State *L, int idx, const char *fname) {
if (lua_getfield(L, idx, fname) == LUA_TTABLE)
return 1;  
else {
lua_pop(L, 1);  
idx = lua_absindex(L, idx);
lua_newtable(L);
lua_pushvalue(L, -1);  
lua_setfield(L, idx, fname);  
return 0;  
}
}
LUALIB_API void luaL_requiref (lua_State *L, const char *modname,
lua_CFunction openf, int glb) {
luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
lua_getfield(L, -1, modname);  
if (!lua_toboolean(L, -1)) {  
lua_pop(L, 1);  
lua_pushcfunction(L, openf);
lua_pushstring(L, modname);  
lua_call(L, 1, 1);  
lua_pushvalue(L, -1);  
lua_setfield(L, -3, modname);  
}
lua_remove(L, -2);  
if (glb) {
lua_pushvalue(L, -1);  
lua_setglobal(L, modname);  
}
}
LUALIB_API const char *luaL_gsub (lua_State *L, const char *s, const char *p,
const char *r) {
const char *wild;
size_t l = strlen(p);
luaL_Buffer b;
luaL_buffinit(L, &b);
while ((wild = strstr(s, p)) != NULL) {
luaL_addlstring(&b, s, wild - s);  
luaL_addstring(&b, r);  
s = wild + l;  
}
luaL_addstring(&b, s);  
luaL_pushresult(&b);
return lua_tostring(L, -1);
}
static void *l_alloc (void *ud, void *ptr, size_t osize, size_t nsize) {
(void)ud; (void)osize;  
if (nsize == 0) {
free(ptr);
return NULL;
}
else
return realloc(ptr, nsize);
}
static int panic (lua_State *L) {
lua_writestringerror("PANIC: unprotected error in call to Lua API (%s)\n",
lua_tostring(L, -1));
return 0;  
}
LUALIB_API lua_State *luaL_newstate (void) {
lua_State *L = lua_newstate(l_alloc, NULL);
if (L) lua_atpanic(L, &panic);
return L;
}
LUALIB_API void luaL_checkversion_ (lua_State *L, lua_Number ver, size_t sz) {
const lua_Number *v = lua_version(L);
if (sz != LUAL_NUMSIZES)  
luaL_error(L, "core and library have incompatible numeric types");
if (v != lua_version(NULL))
luaL_error(L, "multiple Lua VMs detected");
else if (*v != ver)
luaL_error(L, "version mismatch: app. needs %f, Lua core provides %f",
(LUAI_UACNUMBER)ver, (LUAI_UACNUMBER)*v);
}
#define lbaselib_c
#define LUA_LIB
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef lualib_h
#define lualib_h
#define LUA_VERSUFFIX          "_" LUA_VERSION_MAJOR "_" LUA_VERSION_MINOR
LUAMOD_API int (luaopen_base) (lua_State *L);
#define LUA_COLIBNAME	"coroutine"
LUAMOD_API int (luaopen_coroutine) (lua_State *L);
#define LUA_TABLIBNAME	"table"
LUAMOD_API int (luaopen_table) (lua_State *L);
#define LUA_IOLIBNAME	"io"
LUAMOD_API int (luaopen_io) (lua_State *L);
#define LUA_OSLIBNAME	"os"
LUAMOD_API int (luaopen_os) (lua_State *L);
#define LUA_STRLIBNAME	"string"
LUAMOD_API int (luaopen_string) (lua_State *L);
#define LUA_UTF8LIBNAME	"utf8"
LUAMOD_API int (luaopen_utf8) (lua_State *L);
#define LUA_BITLIBNAME	"bit32"
LUAMOD_API int (luaopen_bit32) (lua_State *L);
#define LUA_MATHLIBNAME	"math"
LUAMOD_API int (luaopen_math) (lua_State *L);
#define LUA_DBLIBNAME	"debug"
LUAMOD_API int (luaopen_debug) (lua_State *L);
#define LUA_LOADLIBNAME	"package"
LUAMOD_API int (luaopen_package) (lua_State *L);
LUALIB_API void (luaL_openlibs) (lua_State *L);
#if !defined(lua_assert)
#define lua_assert(x)	((void)0)
#endif
#endif
static int luaB_print (lua_State *L) {
int n = lua_gettop(L);  
int i;
lua_getglobal(L, "tostring");
for (i=1; i<=n; i++) {
const char *s;
size_t l;
lua_pushvalue(L, -1);  
lua_pushvalue(L, i);   
lua_call(L, 1, 1);
s = lua_tolstring(L, -1, &l);  
if (s == NULL)
return luaL_error(L, "'tostring' must return a string to 'print'");
if (i>1) lua_writestring("\t", 1);
lua_writestring(s, l);
lua_pop(L, 1);  
}
lua_writeline();
return 0;
}
#define SPACECHARS	" \f\n\r\t\v"
static const char *b_str2int (const char *s, int base, lua_Integer *pn) {
lua_Unsigned n = 0;
int neg = 0;
s += strspn(s, SPACECHARS);  
if (*s == '-') { s++; neg = 1; }  
else if (*s == '+') s++;
if (!isalnum((unsigned char)*s))  
return NULL;
do {
int digit = (isdigit((unsigned char)*s)) ? *s - '0'
: (toupper((unsigned char)*s) - 'A') + 10;
if (digit >= base) return NULL;  
n = n * base + digit;
s++;
} while (isalnum((unsigned char)*s));
s += strspn(s, SPACECHARS);  
*pn = (lua_Integer)((neg) ? (0u - n) : n);
return s;
}
static int luaB_tonumber (lua_State *L) {
if (lua_isnoneornil(L, 2)) {  
luaL_checkany(L, 1);
if (lua_type(L, 1) == LUA_TNUMBER) {  
lua_settop(L, 1);  
return 1;
}
else {
size_t l;
const char *s = lua_tolstring(L, 1, &l);
if (s != NULL && lua_stringtonumber(L, s) == l + 1)
return 1;  
}
}
else {
size_t l;
const char *s;
lua_Integer n = 0;  
lua_Integer base = luaL_checkinteger(L, 2);
luaL_checktype(L, 1, LUA_TSTRING);  
s = lua_tolstring(L, 1, &l);
luaL_argcheck(L, 2 <= base && base <= 36, 2, "base out of range");
if (b_str2int(s, (int)base, &n) == s + l) {
lua_pushinteger(L, n);
return 1;
}  
}  
lua_pushnil(L);  
return 1;
}
static int luaB_error (lua_State *L) {
int level = (int)luaL_optinteger(L, 2, 1);
lua_settop(L, 1);
if (lua_type(L, 1) == LUA_TSTRING && level > 0) {
luaL_where(L, level);   
lua_pushvalue(L, 1);
lua_concat(L, 2);
}
return lua_error(L);
}
static int luaB_getmetatable (lua_State *L) {
luaL_checkany(L, 1);
if (!lua_getmetatable(L, 1)) {
lua_pushnil(L);
return 1;  
}
luaL_getmetafield(L, 1, "__metatable");
return 1;  
}
static int luaB_setmetatable (lua_State *L) {
int t = lua_type(L, 2);
luaL_checktype(L, 1, LUA_TTABLE);
luaL_argcheck(L, t == LUA_TNIL || t == LUA_TTABLE, 2,
"nil or table expected");
if (luaL_getmetafield(L, 1, "__metatable") != LUA_TNIL)
return luaL_error(L, "cannot change a protected metatable");
lua_settop(L, 2);
lua_setmetatable(L, 1);
return 1;
}
static int luaB_rawequal (lua_State *L) {
luaL_checkany(L, 1);
luaL_checkany(L, 2);
lua_pushboolean(L, lua_rawequal(L, 1, 2));
return 1;
}
static int luaB_rawlen (lua_State *L) {
int t = lua_type(L, 1);
luaL_argcheck(L, t == LUA_TTABLE || t == LUA_TSTRING, 1,
"table or string expected");
lua_pushinteger(L, lua_rawlen(L, 1));
return 1;
}
static int luaB_rawget (lua_State *L) {
luaL_checktype(L, 1, LUA_TTABLE);
luaL_checkany(L, 2);
lua_settop(L, 2);
lua_rawget(L, 1);
return 1;
}
static int luaB_rawset (lua_State *L) {
luaL_checktype(L, 1, LUA_TTABLE);
luaL_checkany(L, 2);
luaL_checkany(L, 3);
lua_settop(L, 3);
lua_rawset(L, 1);
return 1;
}
static int luaB_collectgarbage (lua_State *L) {
static const char *const opts[] = {"stop", "restart", "collect",
"count", "step", "setpause", "setstepmul",
"isrunning", NULL};
static const int optsnum[] = {LUA_GCSTOP, LUA_GCRESTART, LUA_GCCOLLECT,
LUA_GCCOUNT, LUA_GCSTEP, LUA_GCSETPAUSE, LUA_GCSETSTEPMUL,
LUA_GCISRUNNING};
int o = optsnum[luaL_checkoption(L, 1, "collect", opts)];
int ex = (int)luaL_optinteger(L, 2, 0);
int res = lua_gc(L, o, ex);
switch (o) {
case LUA_GCCOUNT: {
int b = lua_gc(L, LUA_GCCOUNTB, 0);
lua_pushnumber(L, (lua_Number)res + ((lua_Number)b/1024));
return 1;
}
case LUA_GCSTEP: case LUA_GCISRUNNING: {
lua_pushboolean(L, res);
return 1;
}
default: {
lua_pushinteger(L, res);
return 1;
}
}
}
static int luaB_type (lua_State *L) {
int t = lua_type(L, 1);
luaL_argcheck(L, t != LUA_TNONE, 1, "value expected");
lua_pushstring(L, lua_typename(L, t));
return 1;
}
static int pairsmeta (lua_State *L, const char *method, int iszero,
lua_CFunction iter) {
luaL_checkany(L, 1);
if (luaL_getmetafield(L, 1, method) == LUA_TNIL) {  
lua_pushcfunction(L, iter);  
lua_pushvalue(L, 1);  
if (iszero) lua_pushinteger(L, 0);  
else lua_pushnil(L);
}
else {
lua_pushvalue(L, 1);  
lua_call(L, 1, 3);  
}
return 3;
}
static int luaB_next (lua_State *L) {
luaL_checktype(L, 1, LUA_TTABLE);
lua_settop(L, 2);  
if (lua_next(L, 1))
return 2;
else {
lua_pushnil(L);
return 1;
}
}
static int luaB_pairs (lua_State *L) {
return pairsmeta(L, "__pairs", 0, luaB_next);
}
static int ipairsaux (lua_State *L) {
lua_Integer i = luaL_checkinteger(L, 2) + 1;
lua_pushinteger(L, i);
return (lua_geti(L, 1, i) == LUA_TNIL) ? 1 : 2;
}
static int luaB_ipairs (lua_State *L) {
#if defined(LUA_COMPAT_IPAIRS)
return pairsmeta(L, "__ipairs", 1, ipairsaux);
#else
luaL_checkany(L, 1);
lua_pushcfunction(L, ipairsaux);  
lua_pushvalue(L, 1);  
lua_pushinteger(L, 0);  
return 3;
#endif
}
static int load_aux (lua_State *L, int status, int envidx) {
if (status == LUA_OK) {
if (envidx != 0) {  
lua_pushvalue(L, envidx);  
if (!lua_setupvalue(L, -2, 1))  
lua_pop(L, 1);  
}
return 1;
}
else {  
lua_pushnil(L);
lua_insert(L, -2);  
return 2;  
}
}
static int luaB_loadfile (lua_State *L) {
const char *fname = luaL_optstring(L, 1, NULL);
const char *mode = luaL_optstring(L, 2, NULL);
int env = (!lua_isnone(L, 3) ? 3 : 0);  
int status = luaL_loadfilex(L, fname, mode);
return load_aux(L, status, env);
}
#define RESERVEDSLOT	5
static const char *generic_reader (lua_State *L, void *ud, size_t *size) {
(void)(ud);  
luaL_checkstack(L, 2, "too many nested functions");
lua_pushvalue(L, 1);  
lua_call(L, 0, 1);  
if (lua_isnil(L, -1)) {
lua_pop(L, 1);  
*size = 0;
return NULL;
}
else if (!lua_isstring(L, -1))
luaL_error(L, "reader function must return a string");
lua_replace(L, RESERVEDSLOT);  
return lua_tolstring(L, RESERVEDSLOT, size);
}
static int luaB_load (lua_State *L) {
int status;
size_t l;
const char *s = lua_tolstring(L, 1, &l);
const char *mode = luaL_optstring(L, 3, "bt");
int env = (!lua_isnone(L, 4) ? 4 : 0);  
if (s != NULL) {  
const char *chunkname = luaL_optstring(L, 2, s);
status = luaL_loadbufferx(L, s, l, chunkname, mode);
}
else {  
const char *chunkname = luaL_optstring(L, 2, "=(load)");
luaL_checktype(L, 1, LUA_TFUNCTION);
lua_settop(L, RESERVEDSLOT);  
status = lua_load(L, generic_reader, NULL, chunkname, mode);
}
return load_aux(L, status, env);
}
static int dofilecont (lua_State *L, int d1, lua_KContext d2) {
(void)d1;  (void)d2;  
return lua_gettop(L) - 1;
}
static int luaB_dofile (lua_State *L) {
const char *fname = luaL_optstring(L, 1, NULL);
lua_settop(L, 1);
if (luaL_loadfile(L, fname) != LUA_OK)
return lua_error(L);
lua_callk(L, 0, LUA_MULTRET, 0, dofilecont);
return dofilecont(L, 0, 0);
}
static int luaB_assert (lua_State *L) {
if (lua_toboolean(L, 1))  
return lua_gettop(L);  
else {  
luaL_checkany(L, 1);  
lua_remove(L, 1);  
lua_pushliteral(L, "assertion failed!");  
lua_settop(L, 1);  
return luaB_error(L);  
}
}
static int luaB_select (lua_State *L) {
int n = lua_gettop(L);
if (lua_type(L, 1) == LUA_TSTRING && *lua_tostring(L, 1) == '#') {
lua_pushinteger(L, n-1);
return 1;
}
else {
lua_Integer i = luaL_checkinteger(L, 1);
if (i < 0) i = n + i;
else if (i > n) i = n;
luaL_argcheck(L, 1 <= i, 1, "index out of range");
return n - (int)i;
}
}
static int finishpcall (lua_State *L, int status, lua_KContext extra) {
if (status != LUA_OK && status != LUA_YIELD) {  
lua_pushboolean(L, 0);  
lua_pushvalue(L, -2);  
return 2;  
}
else
return lua_gettop(L) - (int)extra;  
}
static int luaB_pcall (lua_State *L) {
int status;
luaL_checkany(L, 1);
lua_pushboolean(L, 1);  
lua_insert(L, 1);  
status = lua_pcallk(L, lua_gettop(L) - 2, LUA_MULTRET, 0, 0, finishpcall);
return finishpcall(L, status, 0);
}
static int luaB_xpcall (lua_State *L) {
int status;
int n = lua_gettop(L);
luaL_checktype(L, 2, LUA_TFUNCTION);  
lua_pushboolean(L, 1);  
lua_pushvalue(L, 1);  
lua_rotate(L, 3, 2);  
status = lua_pcallk(L, n - 2, LUA_MULTRET, 2, 2, finishpcall);
return finishpcall(L, status, 2);
}
static int luaB_tostring (lua_State *L) {
luaL_checkany(L, 1);
luaL_tolstring(L, 1, NULL);
return 1;
}
static const luaL_Reg base_funcs[] = {
{"assert", luaB_assert},
{"collectgarbage", luaB_collectgarbage},
{"dofile", luaB_dofile},
{"error", luaB_error},
{"getmetatable", luaB_getmetatable},
{"ipairs", luaB_ipairs},
{"loadfile", luaB_loadfile},
{"load", luaB_load},
#if defined(LUA_COMPAT_LOADSTRING)
{"loadstring", luaB_load},
#endif
{"next", luaB_next},
{"pairs", luaB_pairs},
{"pcall", luaB_pcall},
{"print", luaB_print},
{"rawequal", luaB_rawequal},
{"rawlen", luaB_rawlen},
{"rawget", luaB_rawget},
{"rawset", luaB_rawset},
{"select", luaB_select},
{"setmetatable", luaB_setmetatable},
{"tonumber", luaB_tonumber},
{"tostring", luaB_tostring},
{"type", luaB_type},
{"xpcall", luaB_xpcall},
{"_G", NULL},
{"_VERSION", NULL},
{NULL, NULL}
};
LUAMOD_API int luaopen_base (lua_State *L) {
lua_pushglobaltable(L);
luaL_setfuncs(L, base_funcs, 0);
lua_pushvalue(L, -1);
lua_setfield(L, -2, "_G");
lua_pushliteral(L, LUA_VERSION);
lua_setfield(L, -2, "_VERSION");
return 1;
}
#define lbitlib_c
#define LUA_LIB
#if defined(LUA_COMPAT_BITLIB)		
#define pushunsigned(L,n)	lua_pushinteger(L, (lua_Integer)(n))
#define checkunsigned(L,i)	((lua_Unsigned)luaL_checkinteger(L,i))
#if !defined(LUA_NBITS)
#define LUA_NBITS	32
#endif
#define ALLONES		(~(((~(lua_Unsigned)0) << (LUA_NBITS - 1)) << 1))
#define trim(x)		((x) & ALLONES)
#define mask(n)		(~((ALLONES << 1) << ((n) - 1)))
static lua_Unsigned andaux (lua_State *L) {
int i, n = lua_gettop(L);
lua_Unsigned r = ~(lua_Unsigned)0;
for (i = 1; i <= n; i++)
r &= checkunsigned(L, i);
return trim(r);
}
static int b_and (lua_State *L) {
lua_Unsigned r = andaux(L);
pushunsigned(L, r);
return 1;
}
static int b_test (lua_State *L) {
lua_Unsigned r = andaux(L);
lua_pushboolean(L, r != 0);
return 1;
}
static int b_or (lua_State *L) {
int i, n = lua_gettop(L);
lua_Unsigned r = 0;
for (i = 1; i <= n; i++)
r |= checkunsigned(L, i);
pushunsigned(L, trim(r));
return 1;
}
static int b_xor (lua_State *L) {
int i, n = lua_gettop(L);
lua_Unsigned r = 0;
for (i = 1; i <= n; i++)
r ^= checkunsigned(L, i);
pushunsigned(L, trim(r));
return 1;
}
static int b_not (lua_State *L) {
lua_Unsigned r = ~checkunsigned(L, 1);
pushunsigned(L, trim(r));
return 1;
}
static int b_shift (lua_State *L, lua_Unsigned r, lua_Integer i) {
if (i < 0) {  
i = -i;
r = trim(r);
if (i >= LUA_NBITS) r = 0;
else r >>= i;
}
else {  
if (i >= LUA_NBITS) r = 0;
else r <<= i;
r = trim(r);
}
pushunsigned(L, r);
return 1;
}
static int b_lshift (lua_State *L) {
return b_shift(L, checkunsigned(L, 1), luaL_checkinteger(L, 2));
}
static int b_rshift (lua_State *L) {
return b_shift(L, checkunsigned(L, 1), -luaL_checkinteger(L, 2));
}
static int b_arshift (lua_State *L) {
lua_Unsigned r = checkunsigned(L, 1);
lua_Integer i = luaL_checkinteger(L, 2);
if (i < 0 || !(r & ((lua_Unsigned)1 << (LUA_NBITS - 1))))
return b_shift(L, r, -i);
else {  
if (i >= LUA_NBITS) r = ALLONES;
else
r = trim((r >> i) | ~(trim(~(lua_Unsigned)0) >> i));  
pushunsigned(L, r);
return 1;
}
}
static int b_rot (lua_State *L, lua_Integer d) {
lua_Unsigned r = checkunsigned(L, 1);
int i = d & (LUA_NBITS - 1);  
r = trim(r);
if (i != 0)  
r = (r << i) | (r >> (LUA_NBITS - i));
pushunsigned(L, trim(r));
return 1;
}
static int b_lrot (lua_State *L) {
return b_rot(L, luaL_checkinteger(L, 2));
}
static int b_rrot (lua_State *L) {
return b_rot(L, -luaL_checkinteger(L, 2));
}
static int fieldargs (lua_State *L, int farg, int *width) {
lua_Integer f = luaL_checkinteger(L, farg);
lua_Integer w = luaL_optinteger(L, farg + 1, 1);
luaL_argcheck(L, 0 <= f, farg, "field cannot be negative");
luaL_argcheck(L, 0 < w, farg + 1, "width must be positive");
if (f + w > LUA_NBITS)
luaL_error(L, "trying to access non-existent bits");
*width = (int)w;
return (int)f;
}
static int b_extract (lua_State *L) {
int w;
lua_Unsigned r = trim(checkunsigned(L, 1));
int f = fieldargs(L, 2, &w);
r = (r >> f) & mask(w);
pushunsigned(L, r);
return 1;
}
static int b_replace (lua_State *L) {
int w;
lua_Unsigned r = trim(checkunsigned(L, 1));
lua_Unsigned v = trim(checkunsigned(L, 2));
int f = fieldargs(L, 3, &w);
lua_Unsigned m = mask(w);
r = (r & ~(m << f)) | ((v & m) << f);
pushunsigned(L, r);
return 1;
}
static const luaL_Reg bitlib[] = {
{"arshift", b_arshift},
{"band", b_and},
{"bnot", b_not},
{"bor", b_or},
{"bxor", b_xor},
{"btest", b_test},
{"extract", b_extract},
{"lrotate", b_lrot},
{"lshift", b_lshift},
{"replace", b_replace},
{"rrotate", b_rrot},
{"rshift", b_rshift},
{NULL, NULL}
};
LUAMOD_API int luaopen_bit32 (lua_State *L) {
luaL_newlib(L, bitlib);
return 1;
}
#else					
LUAMOD_API int luaopen_bit32 (lua_State *L) {
return luaL_error(L, "library 'bit32' has been deprecated");
}
#endif					
#define lcode_c
#define LUA_CORE
#include <math.h>
#include <stdlib.h>
#ifndef lcode_h
#define lcode_h
#ifndef llex_h
#define llex_h
#define FIRST_RESERVED	257
#if !defined(LUA_ENV)
#define LUA_ENV		"_ENV"
#endif
enum RESERVED {
TK_AND = FIRST_RESERVED, TK_BREAK,
TK_DO, TK_ELSE, TK_ELSEIF, TK_END, TK_FALSE, TK_FOR, TK_FUNCTION,
TK_GOTO, TK_IF, TK_IN, TK_LOCAL, TK_NIL, TK_NOT, TK_OR, TK_REPEAT,
TK_RETURN, TK_THEN, TK_TRUE, TK_UNTIL, TK_WHILE,
TK_IDIV, TK_CONCAT, TK_DOTS, TK_EQ, TK_GE, TK_LE, TK_NE,
TK_SHL, TK_SHR,
TK_DBCOLON, TK_EOS,
TK_FLT, TK_INT, TK_NAME, TK_STRING
};
#define NUM_RESERVED	(cast(int, TK_WHILE-FIRST_RESERVED+1))
typedef union {
lua_Number r;
lua_Integer i;
TString *ts;
} SemInfo;  
typedef struct Token {
int token;
SemInfo seminfo;
} Token;
/* state of the lexer plus state of the parser when shared by all
functions */
typedef struct LexState {
int current;  
int linenumber;  
int lastline;  
Token t;  
Token lookahead;  
struct FuncState *fs;  
struct lua_State *L;
ZIO *z;  
Mbuffer *buff;  
Table *h;  
struct Dyndata *dyd;  
TString *source;  
TString *envn;  
} LexState;
LUAI_FUNC void luaX_init (lua_State *L);
LUAI_FUNC void luaX_setinput (lua_State *L, LexState *ls, ZIO *z,
TString *source, int firstchar);
LUAI_FUNC TString *luaX_newstring (LexState *ls, const char *str, size_t l);
LUAI_FUNC void luaX_next (LexState *ls);
LUAI_FUNC int luaX_lookahead (LexState *ls);
LUAI_FUNC l_noret luaX_syntaxerror (LexState *ls, const char *s);
LUAI_FUNC const char *luaX_token2str (LexState *ls, int token);
#endif
#ifndef lopcodes_h
#define lopcodes_h
/*===========================================================================
We assume that instructions are unsigned numbers.
All instructions have an opcode in the first 6 bits.
Instructions can have the following fields:
'A' : 8 bits
'B' : 9 bits
'C' : 9 bits
'Ax' : 26 bits ('A', 'B', and 'C' together)
'Bx' : 18 bits ('B' and 'C' together)
'sBx' : signed Bx
A signed argument is represented in excess K; that is, the number
value is the unsigned value minus K. K is exactly the maximum value
for that argument (so that -max is represented by 0, and +max is
represented by 2*max), which is half the maximum for the corresponding
unsigned argument.
===========================================================================*/
enum OpMode {iABC, iABx, iAsBx, iAx};  
#define SIZE_C		9
#define SIZE_B		9
#define SIZE_Bx		(SIZE_C + SIZE_B)
#define SIZE_A		8
#define SIZE_Ax		(SIZE_C + SIZE_B + SIZE_A)
#define SIZE_OP		6
#define POS_OP		0
#define POS_A		(POS_OP + SIZE_OP)
#define POS_C		(POS_A + SIZE_A)
#define POS_B		(POS_C + SIZE_C)
#define POS_Bx		POS_C
#define POS_Ax		POS_A
#if SIZE_Bx < LUAI_BITSINT-1
#define MAXARG_Bx        ((1<<SIZE_Bx)-1)
#define MAXARG_sBx        (MAXARG_Bx>>1)         
#else
#define MAXARG_Bx        MAX_INT
#define MAXARG_sBx        MAX_INT
#endif
#if SIZE_Ax < LUAI_BITSINT-1
#define MAXARG_Ax	((1<<SIZE_Ax)-1)
#else
#define MAXARG_Ax	MAX_INT
#endif
#define MAXARG_A        ((1<<SIZE_A)-1)
#define MAXARG_B        ((1<<SIZE_B)-1)
#define MAXARG_C        ((1<<SIZE_C)-1)
#define MASK1(n,p)	((~((~(Instruction)0)<<(n)))<<(p))
#define MASK0(n,p)	(~MASK1(n,p))
#define GET_OPCODE(i)	(cast(OpCode, ((i)>>POS_OP) & MASK1(SIZE_OP,0)))
#define SET_OPCODE(i,o)	((i) = (((i)&MASK0(SIZE_OP,POS_OP)) | \
((cast(Instruction, o)<<POS_OP)&MASK1(SIZE_OP,POS_OP))))
#define getarg(i,pos,size)	(cast(int, ((i)>>pos) & MASK1(size,0)))
#define setarg(i,v,pos,size)	((i) = (((i)&MASK0(size,pos)) | \
((cast(Instruction, v)<<pos)&MASK1(size,pos))))
#define GETARG_A(i)	getarg(i, POS_A, SIZE_A)
#define SETARG_A(i,v)	setarg(i, v, POS_A, SIZE_A)
#define GETARG_B(i)	getarg(i, POS_B, SIZE_B)
#define SETARG_B(i,v)	setarg(i, v, POS_B, SIZE_B)
#define GETARG_C(i)	getarg(i, POS_C, SIZE_C)
#define SETARG_C(i,v)	setarg(i, v, POS_C, SIZE_C)
#define GETARG_Bx(i)	getarg(i, POS_Bx, SIZE_Bx)
#define SETARG_Bx(i,v)	setarg(i, v, POS_Bx, SIZE_Bx)
#define GETARG_Ax(i)	getarg(i, POS_Ax, SIZE_Ax)
#define SETARG_Ax(i,v)	setarg(i, v, POS_Ax, SIZE_Ax)
#define GETARG_sBx(i)	(GETARG_Bx(i)-MAXARG_sBx)
#define SETARG_sBx(i,b)	SETARG_Bx((i),cast(unsigned int, (b)+MAXARG_sBx))
#define CREATE_ABC(o,a,b,c)	((cast(Instruction, o)<<POS_OP) \
| (cast(Instruction, a)<<POS_A) \
| (cast(Instruction, b)<<POS_B) \
| (cast(Instruction, c)<<POS_C))
#define CREATE_ABx(o,a,bc)	((cast(Instruction, o)<<POS_OP) \
| (cast(Instruction, a)<<POS_A) \
| (cast(Instruction, bc)<<POS_Bx))
#define CREATE_Ax(o,a)		((cast(Instruction, o)<<POS_OP) \
| (cast(Instruction, a)<<POS_Ax))
#define BITRK		(1 << (SIZE_B - 1))
#define ISK(x)		((x) & BITRK)
#define INDEXK(r)	((int)(r) & ~BITRK)
#if !defined(MAXINDEXRK)  
#define MAXINDEXRK	(BITRK - 1)
#endif
#define RKASK(x)	((x) | BITRK)
#define NO_REG		MAXARG_A
typedef enum {
/*----------------------------------------------------------------------
name		args	description
------------------------------------------------------------------------*/
OP_MOVE,
OP_LOADK,
OP_LOADKX,
OP_LOADBOOL,
OP_LOADNIL,
OP_GETUPVAL,
OP_GETTABUP,
OP_GETTABLE,
OP_SETTABUP,
OP_SETUPVAL,
OP_SETTABLE,
OP_NEWTABLE,
OP_SELF,
OP_ADD,
OP_SUB,
OP_MUL,
OP_MOD,
OP_POW,
OP_DIV,
OP_IDIV,
OP_BAND,
OP_BOR,
OP_BXOR,
OP_SHL,
OP_SHR,
OP_UNM,
OP_BNOT,
OP_NOT,
OP_LEN,
OP_CONCAT,
OP_JMP,
OP_EQ,
OP_LT,
OP_LE,
OP_TEST,
OP_TESTSET,
OP_CALL,
OP_TAILCALL,
OP_RETURN,
OP_FORLOOP,/*	A sBx	R(A)+=R(A+2);
if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }*/
OP_FORPREP,
OP_TFORCALL,
OP_TFORLOOP,
OP_SETLIST,
OP_CLOSURE,
OP_VARARG,
OP_EXTRAARG
} OpCode;
#define NUM_OPCODES	(cast(int, OP_EXTRAARG) + 1)
/*===========================================================================
Notes:
(*) In OP_CALL, if (B == 0) then B = top. If (C == 0), then 'top' is
set to last_result+1, so next open instruction (OP_CALL, OP_RETURN,
OP_SETLIST) may use 'top'.
(*) In OP_VARARG, if (B == 0) then use actual number of varargs and
set top (like in OP_CALL with C == 0).
(*) In OP_RETURN, if (B == 0) then return up to 'top'.
(*) In OP_SETLIST, if (B == 0) then B = 'top'; if (C == 0) then next
'instruction' is EXTRAARG(real C).
(*) In OP_LOADKX, the next 'instruction' is always EXTRAARG.
(*) For comparisons, A specifies what condition the test should accept
(true or false).
(*) All 'skips' (pc++) assume that next instruction is a jump.
===========================================================================*/
enum OpArgMask {
OpArgN,  
OpArgU,  
OpArgR,  
OpArgK   
};
LUAI_DDEC const lu_byte luaP_opmodes[NUM_OPCODES];
#define getOpMode(m)	(cast(enum OpMode, luaP_opmodes[m] & 3))
#define getBMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 4) & 3))
#define getCMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 2) & 3))
#define testAMode(m)	(luaP_opmodes[m] & (1 << 6))
#define testTMode(m)	(luaP_opmodes[m] & (1 << 7))
LUAI_DDEC const char *const luaP_opnames[NUM_OPCODES+1];  
#define LFIELDS_PER_FLUSH	50
#endif
#ifndef lparser_h
#define lparser_h
typedef enum {
VVOID,  /* when 'expdesc' describes the last expression a list,
this kind means an empty list (so, no expression) */
VNIL,  
VTRUE,  
VFALSE,  
VK,  
VKFLT,  
VKINT,  
VNONRELOC,  /* expression has its value in a fixed register;
info = result register */
VLOCAL,  
VUPVAL,  
VINDEXED,  /* indexed variable;
ind.vt = whether 't' is register or upvalue;
ind.t = table register or upvalue;
ind.idx = key's R/K index */
VJMP,  /* expression is a test/comparison;
info = pc of corresponding jump instruction */
VRELOCABLE,  /* expression can put result in any register;
info = instruction pc */
VCALL,  
VVARARG  
} expkind;
#define vkisvar(k)	(VLOCAL <= (k) && (k) <= VINDEXED)
#define vkisinreg(k)	((k) == VNONRELOC || (k) == VLOCAL)
typedef struct expdesc {
expkind k;
union {
lua_Integer ival;    
lua_Number nval;  
int info;  
struct {  
short idx;  
lu_byte t;  
lu_byte vt;  
} ind;
} u;
int t;  
int f;  
} expdesc;
typedef struct Vardesc {
short idx;  
} Vardesc;
typedef struct Labeldesc {
TString *name;  
int pc;  
int line;  
lu_byte nactvar;  
} Labeldesc;
typedef struct Labellist {
Labeldesc *arr;  
int n;  
int size;  
} Labellist;
typedef struct Dyndata {
struct {  
Vardesc *arr;
int n;
int size;
} actvar;
Labellist gt;  
Labellist label;   
} Dyndata;
struct BlockCnt;  
typedef struct FuncState {
Proto *f;  
struct FuncState *prev;  
struct LexState *ls;  
struct BlockCnt *bl;  
int pc;  
int lasttarget;   
int jpc;  
int nk;  
int np;  
int firstlocal;  
short nlocvars;  
lu_byte nactvar;  
lu_byte nups;  
lu_byte freereg;  
} FuncState;
LUAI_FUNC LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
Dyndata *dyd, const char *name, int firstchar);
#endif
#define NO_JUMP (-1)
typedef enum BinOpr {
OPR_ADD, OPR_SUB, OPR_MUL, OPR_MOD, OPR_POW,
OPR_DIV,
OPR_IDIV,
OPR_BAND, OPR_BOR, OPR_BXOR,
OPR_SHL, OPR_SHR,
OPR_CONCAT,
OPR_EQ, OPR_LT, OPR_LE,
OPR_NE, OPR_GT, OPR_GE,
OPR_AND, OPR_OR,
OPR_NOBINOPR
} BinOpr;
typedef enum UnOpr { OPR_MINUS, OPR_BNOT, OPR_NOT, OPR_LEN, OPR_NOUNOPR } UnOpr;
#define getinstruction(fs,e)	((fs)->f->code[(e)->u.info])
#define luaK_codeAsBx(fs,o,A,sBx)	luaK_codeABx(fs,o,A,(sBx)+MAXARG_sBx)
#define luaK_setmultret(fs,e)	luaK_setreturns(fs, e, LUA_MULTRET)
#define luaK_jumpto(fs,t)	luaK_patchlist(fs, luaK_jump(fs), t)
LUAI_FUNC int luaK_codeABx (FuncState *fs, OpCode o, int A, unsigned int Bx);
LUAI_FUNC int luaK_codeABC (FuncState *fs, OpCode o, int A, int B, int C);
LUAI_FUNC int luaK_codek (FuncState *fs, int reg, int k);
LUAI_FUNC void luaK_fixline (FuncState *fs, int line);
LUAI_FUNC void luaK_nil (FuncState *fs, int from, int n);
LUAI_FUNC void luaK_reserveregs (FuncState *fs, int n);
LUAI_FUNC void luaK_checkstack (FuncState *fs, int n);
LUAI_FUNC int luaK_stringK (FuncState *fs, TString *s);
LUAI_FUNC int luaK_intK (FuncState *fs, lua_Integer n);
LUAI_FUNC void luaK_dischargevars (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_exp2anyreg (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_exp2anyregup (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_exp2nextreg (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_exp2val (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_exp2RK (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_self (FuncState *fs, expdesc *e, expdesc *key);
LUAI_FUNC void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k);
LUAI_FUNC void luaK_goiftrue (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_goiffalse (FuncState *fs, expdesc *e);
LUAI_FUNC void luaK_storevar (FuncState *fs, expdesc *var, expdesc *e);
LUAI_FUNC void luaK_setreturns (FuncState *fs, expdesc *e, int nresults);
LUAI_FUNC void luaK_setoneret (FuncState *fs, expdesc *e);
LUAI_FUNC int luaK_jump (FuncState *fs);
LUAI_FUNC void luaK_ret (FuncState *fs, int first, int nret);
LUAI_FUNC void luaK_patchlist (FuncState *fs, int list, int target);
LUAI_FUNC void luaK_patchtohere (FuncState *fs, int list);
LUAI_FUNC void luaK_patchclose (FuncState *fs, int list, int level);
LUAI_FUNC void luaK_concat (FuncState *fs, int *l1, int l2);
LUAI_FUNC int luaK_getlabel (FuncState *fs);
LUAI_FUNC void luaK_prefix (FuncState *fs, UnOpr op, expdesc *v, int line);
LUAI_FUNC void luaK_infix (FuncState *fs, BinOpr op, expdesc *v);
LUAI_FUNC void luaK_posfix (FuncState *fs, BinOpr op, expdesc *v1,
expdesc *v2, int line);
LUAI_FUNC void luaK_setlist (FuncState *fs, int base, int nelems, int tostore);
#endif
#define MAXREGS		255
#define hasjumps(e)	((e)->t != (e)->f)
static int tonumeral(const expdesc *e, TValue *v) {
if (hasjumps(e))
return 0;  
switch (e->k) {
case VKINT:
if (v) setivalue(v, e->u.ival);
return 1;
case VKFLT:
if (v) setfltvalue(v, e->u.nval);
return 1;
default: return 0;
}
}
void luaK_nil (FuncState *fs, int from, int n) {
Instruction *previous;
int l = from + n - 1;  
if (fs->pc > fs->lasttarget) {  
previous = &fs->f->code[fs->pc-1];
if (GET_OPCODE(*previous) == OP_LOADNIL) {  
int pfrom = GETARG_A(*previous);  
int pl = pfrom + GETARG_B(*previous);
if ((pfrom <= from && from <= pl + 1) ||
(from <= pfrom && pfrom <= l + 1)) {  
if (pfrom < from) from = pfrom;  
if (pl > l) l = pl;  
SETARG_A(*previous, from);
SETARG_B(*previous, l - from);
return;
}
}  
}
luaK_codeABC(fs, OP_LOADNIL, from, n - 1, 0);  
}
static int getjump (FuncState *fs, int pc) {
int offset = GETARG_sBx(fs->f->code[pc]);
if (offset == NO_JUMP)  
return NO_JUMP;  
else
return (pc+1)+offset;  
}
static void fixjump (FuncState *fs, int pc, int dest) {
Instruction *jmp = &fs->f->code[pc];
int offset = dest - (pc + 1);
lua_assert(dest != NO_JUMP);
if (abs(offset) > MAXARG_sBx)
luaX_syntaxerror(fs->ls, "control structure too long");
SETARG_sBx(*jmp, offset);
}
void luaK_concat (FuncState *fs, int *l1, int l2) {
if (l2 == NO_JUMP) return;  
else if (*l1 == NO_JUMP)  
*l1 = l2;  
else {
int list = *l1;
int next;
while ((next = getjump(fs, list)) != NO_JUMP)  
list = next;
fixjump(fs, list, l2);  
}
}
int luaK_jump (FuncState *fs) {
int jpc = fs->jpc;  
int j;
fs->jpc = NO_JUMP;  
j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP);
luaK_concat(fs, &j, jpc);  
return j;
}
void luaK_ret (FuncState *fs, int first, int nret) {
luaK_codeABC(fs, OP_RETURN, first, nret+1, 0);
}
static int condjump (FuncState *fs, OpCode op, int A, int B, int C) {
luaK_codeABC(fs, op, A, B, C);
return luaK_jump(fs);
}
int luaK_getlabel (FuncState *fs) {
fs->lasttarget = fs->pc;
return fs->pc;
}
static Instruction *getjumpcontrol (FuncState *fs, int pc) {
Instruction *pi = &fs->f->code[pc];
if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
return pi-1;
else
return pi;
}
static int patchtestreg (FuncState *fs, int node, int reg) {
Instruction *i = getjumpcontrol(fs, node);
if (GET_OPCODE(*i) != OP_TESTSET)
return 0;  
if (reg != NO_REG && reg != GETARG_B(*i))
SETARG_A(*i, reg);
else {
/* no register to put value or register already has the value;
change instruction to simple test */
*i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));
}
return 1;
}
static void removevalues (FuncState *fs, int list) {
for (; list != NO_JUMP; list = getjump(fs, list))
patchtestreg(fs, list, NO_REG);
}
static void patchlistaux (FuncState *fs, int list, int vtarget, int reg,
int dtarget) {
while (list != NO_JUMP) {
int next = getjump(fs, list);
if (patchtestreg(fs, list, reg))
fixjump(fs, list, vtarget);
else
fixjump(fs, list, dtarget);  
list = next;
}
}
static void dischargejpc (FuncState *fs) {
patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
fs->jpc = NO_JUMP;
}
void luaK_patchtohere (FuncState *fs, int list) {
luaK_getlabel(fs);  
luaK_concat(fs, &fs->jpc, list);
}
void luaK_patchlist (FuncState *fs, int list, int target) {
if (target == fs->pc)  
luaK_patchtohere(fs, list);  
else {
lua_assert(target < fs->pc);
patchlistaux(fs, list, target, NO_REG, target);
}
}
void luaK_patchclose (FuncState *fs, int list, int level) {
level++;  
for (; list != NO_JUMP; list = getjump(fs, list)) {
lua_assert(GET_OPCODE(fs->f->code[list]) == OP_JMP &&
(GETARG_A(fs->f->code[list]) == 0 ||
GETARG_A(fs->f->code[list]) >= level));
SETARG_A(fs->f->code[list], level);
}
}
static int luaK_code (FuncState *fs, Instruction i) {
Proto *f = fs->f;
dischargejpc(fs);  
luaM_growvector(fs->ls->L, f->code, fs->pc, f->sizecode, Instruction,
MAX_INT, "opcodes");
f->code[fs->pc] = i;
luaM_growvector(fs->ls->L, f->lineinfo, fs->pc, f->sizelineinfo, int,
MAX_INT, "opcodes");
f->lineinfo[fs->pc] = fs->ls->lastline;
return fs->pc++;
}
int luaK_codeABC (FuncState *fs, OpCode o, int a, int b, int c) {
lua_assert(getOpMode(o) == iABC);
lua_assert(getBMode(o) != OpArgN || b == 0);
lua_assert(getCMode(o) != OpArgN || c == 0);
lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
return luaK_code(fs, CREATE_ABC(o, a, b, c));
}
int luaK_codeABx (FuncState *fs, OpCode o, int a, unsigned int bc) {
lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
lua_assert(getCMode(o) == OpArgN);
lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
return luaK_code(fs, CREATE_ABx(o, a, bc));
}
static int codeextraarg (FuncState *fs, int a) {
lua_assert(a <= MAXARG_Ax);
return luaK_code(fs, CREATE_Ax(OP_EXTRAARG, a));
}
int luaK_codek (FuncState *fs, int reg, int k) {
if (k <= MAXARG_Bx)
return luaK_codeABx(fs, OP_LOADK, reg, k);
else {
int p = luaK_codeABx(fs, OP_LOADKX, reg, 0);
codeextraarg(fs, k);
return p;
}
}
void luaK_checkstack (FuncState *fs, int n) {
int newstack = fs->freereg + n;
if (newstack > fs->f->maxstacksize) {
if (newstack >= MAXREGS)
luaX_syntaxerror(fs->ls,
"function or expression needs too many registers");
fs->f->maxstacksize = cast_byte(newstack);
}
}
void luaK_reserveregs (FuncState *fs, int n) {
luaK_checkstack(fs, n);
fs->freereg += n;
}
static void freereg (FuncState *fs, int reg) {
if (!ISK(reg) && reg >= fs->nactvar) {
fs->freereg--;
lua_assert(reg == fs->freereg);
}
}
static void freeexp (FuncState *fs, expdesc *e) {
if (e->k == VNONRELOC)
freereg(fs, e->u.info);
}
static void freeexps (FuncState *fs, expdesc *e1, expdesc *e2) {
int r1 = (e1->k == VNONRELOC) ? e1->u.info : -1;
int r2 = (e2->k == VNONRELOC) ? e2->u.info : -1;
if (r1 > r2) {
freereg(fs, r1);
freereg(fs, r2);
}
else {
freereg(fs, r2);
freereg(fs, r1);
}
}
static int addk (FuncState *fs, TValue *key, TValue *v) {
lua_State *L = fs->ls->L;
Proto *f = fs->f;
TValue *idx = luaH_set(L, fs->ls->h, key);  
int k, oldsize;
if (ttisinteger(idx)) {  
k = cast_int(ivalue(idx));
if (k < fs->nk && ttype(&f->k[k]) == ttype(v) &&
luaV_rawequalobj(&f->k[k], v))
return k;  
}
oldsize = f->sizek;
k = fs->nk;
/* numerical value does not need GC barrier;
table has no metatable, so it does not need to invalidate cache */
setivalue(idx, k);
luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
setobj(L, &f->k[k], v);
fs->nk++;
luaC_barrier(L, f, v);
return k;
}
int luaK_stringK (FuncState *fs, TString *s) {
TValue o;
setsvalue(fs->ls->L, &o, s);
return addk(fs, &o, &o);  
}
int luaK_intK (FuncState *fs, lua_Integer n) {
TValue k, o;
setpvalue(&k, cast(void*, cast(size_t, n)));
setivalue(&o, n);
return addk(fs, &k, &o);
}
static int luaK_numberK (FuncState *fs, lua_Number r) {
TValue o;
setfltvalue(&o, r);
return addk(fs, &o, &o);  
}
static int boolK (FuncState *fs, int b) {
TValue o;
setbvalue(&o, b);
return addk(fs, &o, &o);  
}
static int nilK (FuncState *fs) {
TValue k, v;
setnilvalue(&v);
sethvalue(fs->ls->L, &k, fs->ls->h);
return addk(fs, &k, &v);
}
void luaK_setreturns (FuncState *fs, expdesc *e, int nresults) {
if (e->k == VCALL) {  
SETARG_C(getinstruction(fs, e), nresults + 1);
}
else if (e->k == VVARARG) {
Instruction *pc = &getinstruction(fs, e);
SETARG_B(*pc, nresults + 1);
SETARG_A(*pc, fs->freereg);
luaK_reserveregs(fs, 1);
}
else lua_assert(nresults == LUA_MULTRET);
}
void luaK_setoneret (FuncState *fs, expdesc *e) {
if (e->k == VCALL) {  
lua_assert(GETARG_C(getinstruction(fs, e)) == 2);
e->k = VNONRELOC;  
e->u.info = GETARG_A(getinstruction(fs, e));
}
else if (e->k == VVARARG) {
SETARG_B(getinstruction(fs, e), 2);
e->k = VRELOCABLE;  
}
}
void luaK_dischargevars (FuncState *fs, expdesc *e) {
switch (e->k) {
case VLOCAL: {  
e->k = VNONRELOC;  
break;
}
case VUPVAL: {  
e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 0, e->u.info, 0);
e->k = VRELOCABLE;
break;
}
case VINDEXED: {
OpCode op;
freereg(fs, e->u.ind.idx);
if (e->u.ind.vt == VLOCAL) {  
freereg(fs, e->u.ind.t);
op = OP_GETTABLE;
}
else {
lua_assert(e->u.ind.vt == VUPVAL);
op = OP_GETTABUP;  
}
e->u.info = luaK_codeABC(fs, op, 0, e->u.ind.t, e->u.ind.idx);
e->k = VRELOCABLE;
break;
}
case VVARARG: case VCALL: {
luaK_setoneret(fs, e);
break;
}
default: break;  
}
}
static void discharge2reg (FuncState *fs, expdesc *e, int reg) {
luaK_dischargevars(fs, e);
switch (e->k) {
case VNIL: {
luaK_nil(fs, reg, 1);
break;
}
case VFALSE: case VTRUE: {
luaK_codeABC(fs, OP_LOADBOOL, reg, e->k == VTRUE, 0);
break;
}
case VK: {
luaK_codek(fs, reg, e->u.info);
break;
}
case VKFLT: {
luaK_codek(fs, reg, luaK_numberK(fs, e->u.nval));
break;
}
case VKINT: {
luaK_codek(fs, reg, luaK_intK(fs, e->u.ival));
break;
}
case VRELOCABLE: {
Instruction *pc = &getinstruction(fs, e);
SETARG_A(*pc, reg);  
break;
}
case VNONRELOC: {
if (reg != e->u.info)
luaK_codeABC(fs, OP_MOVE, reg, e->u.info, 0);
break;
}
default: {
lua_assert(e->k == VJMP);
return;  
}
}
e->u.info = reg;
e->k = VNONRELOC;
}
static void discharge2anyreg (FuncState *fs, expdesc *e) {
if (e->k != VNONRELOC) {  
luaK_reserveregs(fs, 1);  
discharge2reg(fs, e, fs->freereg-1);  
}
}
static int code_loadbool (FuncState *fs, int A, int b, int jump) {
luaK_getlabel(fs);  
return luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}
static int need_value (FuncState *fs, int list) {
for (; list != NO_JUMP; list = getjump(fs, list)) {
Instruction i = *getjumpcontrol(fs, list);
if (GET_OPCODE(i) != OP_TESTSET) return 1;
}
return 0;  
}
static void exp2reg (FuncState *fs, expdesc *e, int reg) {
discharge2reg(fs, e, reg);
if (e->k == VJMP)  
luaK_concat(fs, &e->t, e->u.info);  
if (hasjumps(e)) {
int final;  
int p_f = NO_JUMP;  
int p_t = NO_JUMP;  
if (need_value(fs, e->t) || need_value(fs, e->f)) {
int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);
p_f = code_loadbool(fs, reg, 0, 1);
p_t = code_loadbool(fs, reg, 1, 0);
luaK_patchtohere(fs, fj);
}
final = luaK_getlabel(fs);
patchlistaux(fs, e->f, final, reg, p_f);
patchlistaux(fs, e->t, final, reg, p_t);
}
e->f = e->t = NO_JUMP;
e->u.info = reg;
e->k = VNONRELOC;
}
void luaK_exp2nextreg (FuncState *fs, expdesc *e) {
luaK_dischargevars(fs, e);
freeexp(fs, e);
luaK_reserveregs(fs, 1);
exp2reg(fs, e, fs->freereg - 1);
}
int luaK_exp2anyreg (FuncState *fs, expdesc *e) {
luaK_dischargevars(fs, e);
if (e->k == VNONRELOC) {  
if (!hasjumps(e))  
return e->u.info;  
if (e->u.info >= fs->nactvar) {  
exp2reg(fs, e, e->u.info);  
return e->u.info;
}
}
luaK_exp2nextreg(fs, e);  
return e->u.info;
}
void luaK_exp2anyregup (FuncState *fs, expdesc *e) {
if (e->k != VUPVAL || hasjumps(e))
luaK_exp2anyreg(fs, e);
}
void luaK_exp2val (FuncState *fs, expdesc *e) {
if (hasjumps(e))
luaK_exp2anyreg(fs, e);
else
luaK_dischargevars(fs, e);
}
int luaK_exp2RK (FuncState *fs, expdesc *e) {
luaK_exp2val(fs, e);
switch (e->k) {  
case VTRUE: e->u.info = boolK(fs, 1); goto vk;
case VFALSE: e->u.info = boolK(fs, 0); goto vk;
case VNIL: e->u.info = nilK(fs); goto vk;
case VKINT: e->u.info = luaK_intK(fs, e->u.ival); goto vk;
case VKFLT: e->u.info = luaK_numberK(fs, e->u.nval); goto vk;
case VK:
vk:
e->k = VK;
if (e->u.info <= MAXINDEXRK)  
return RKASK(e->u.info);
else break;
default: break;
}
return luaK_exp2anyreg(fs, e);
}
void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex) {
switch (var->k) {
case VLOCAL: {
freeexp(fs, ex);
exp2reg(fs, ex, var->u.info);  
return;
}
case VUPVAL: {
int e = luaK_exp2anyreg(fs, ex);
luaK_codeABC(fs, OP_SETUPVAL, e, var->u.info, 0);
break;
}
case VINDEXED: {
OpCode op = (var->u.ind.vt == VLOCAL) ? OP_SETTABLE : OP_SETTABUP;
int e = luaK_exp2RK(fs, ex);
luaK_codeABC(fs, op, var->u.ind.t, var->u.ind.idx, e);
break;
}
default: lua_assert(0);  
}
freeexp(fs, ex);
}
void luaK_self (FuncState *fs, expdesc *e, expdesc *key) {
int ereg;
luaK_exp2anyreg(fs, e);
ereg = e->u.info;  
freeexp(fs, e);
e->u.info = fs->freereg;  
e->k = VNONRELOC;  
luaK_reserveregs(fs, 2);  
luaK_codeABC(fs, OP_SELF, e->u.info, ereg, luaK_exp2RK(fs, key));
freeexp(fs, key);
}
static void negatecondition (FuncState *fs, expdesc *e) {
Instruction *pc = getjumpcontrol(fs, e->u.info);
lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
GET_OPCODE(*pc) != OP_TEST);
SETARG_A(*pc, !(GETARG_A(*pc)));
}
static int jumponcond (FuncState *fs, expdesc *e, int cond) {
if (e->k == VRELOCABLE) {
Instruction ie = getinstruction(fs, e);
if (GET_OPCODE(ie) == OP_NOT) {
fs->pc--;  
return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
}
}
discharge2anyreg(fs, e);
freeexp(fs, e);
return condjump(fs, OP_TESTSET, NO_REG, e->u.info, cond);
}
void luaK_goiftrue (FuncState *fs, expdesc *e) {
int pc;  
luaK_dischargevars(fs, e);
switch (e->k) {
case VJMP: {  
negatecondition(fs, e);  
pc = e->u.info;  
break;
}
case VK: case VKFLT: case VKINT: case VTRUE: {
pc = NO_JUMP;  
break;
}
default: {
pc = jumponcond(fs, e, 0);  
break;
}
}
luaK_concat(fs, &e->f, pc);  
luaK_patchtohere(fs, e->t);  
e->t = NO_JUMP;
}
void luaK_goiffalse (FuncState *fs, expdesc *e) {
int pc;  
luaK_dischargevars(fs, e);
switch (e->k) {
case VJMP: {
pc = e->u.info;  
break;
}
case VNIL: case VFALSE: {
pc = NO_JUMP;  
break;
}
default: {
pc = jumponcond(fs, e, 1);  
break;
}
}
luaK_concat(fs, &e->t, pc);  
luaK_patchtohere(fs, e->f);  
e->f = NO_JUMP;
}
static void codenot (FuncState *fs, expdesc *e) {
luaK_dischargevars(fs, e);
switch (e->k) {
case VNIL: case VFALSE: {
e->k = VTRUE;  
break;
}
case VK: case VKFLT: case VKINT: case VTRUE: {
e->k = VFALSE;  
break;
}
case VJMP: {
negatecondition(fs, e);
break;
}
case VRELOCABLE:
case VNONRELOC: {
discharge2anyreg(fs, e);
freeexp(fs, e);
e->u.info = luaK_codeABC(fs, OP_NOT, 0, e->u.info, 0);
e->k = VRELOCABLE;
break;
}
default: lua_assert(0);  
}
{ int temp = e->f; e->f = e->t; e->t = temp; }
removevalues(fs, e->f);  
removevalues(fs, e->t);
}
void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k) {
lua_assert(!hasjumps(t) && (vkisinreg(t->k) || t->k == VUPVAL));
t->u.ind.t = t->u.info;  
t->u.ind.idx = luaK_exp2RK(fs, k);  
t->u.ind.vt = (t->k == VUPVAL) ? VUPVAL : VLOCAL;
t->k = VINDEXED;
}
static int validop (int op, TValue *v1, TValue *v2) {
switch (op) {
case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
case LUA_OPSHL: case LUA_OPSHR: case LUA_OPBNOT: {  
lua_Integer i;
return (tointeger(v1, &i) && tointeger(v2, &i));
}
case LUA_OPDIV: case LUA_OPIDIV: case LUA_OPMOD:  
return (nvalue(v2) != 0);
default: return 1;  
}
}
static int constfolding (FuncState *fs, int op, expdesc *e1,
const expdesc *e2) {
TValue v1, v2, res;
if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || !validop(op, &v1, &v2))
return 0;  
luaO_arith(fs->ls->L, op, &v1, &v2, &res);  
if (ttisinteger(&res)) {
e1->k = VKINT;
e1->u.ival = ivalue(&res);
}
else {  
lua_Number n = fltvalue(&res);
if (luai_numisnan(n) || n == 0)
return 0;
e1->k = VKFLT;
e1->u.nval = n;
}
return 1;
}
static void codeunexpval (FuncState *fs, OpCode op, expdesc *e, int line) {
int r = luaK_exp2anyreg(fs, e);  
freeexp(fs, e);
e->u.info = luaK_codeABC(fs, op, 0, r, 0);  
e->k = VRELOCABLE;  
luaK_fixline(fs, line);
}
static void codebinexpval (FuncState *fs, OpCode op,
expdesc *e1, expdesc *e2, int line) {
int rk2 = luaK_exp2RK(fs, e2);  
int rk1 = luaK_exp2RK(fs, e1);
freeexps(fs, e1, e2);
e1->u.info = luaK_codeABC(fs, op, 0, rk1, rk2);  
e1->k = VRELOCABLE;  
luaK_fixline(fs, line);
}
static void codecomp (FuncState *fs, BinOpr opr, expdesc *e1, expdesc *e2) {
int rk1 = (e1->k == VK) ? RKASK(e1->u.info)
: check_exp(e1->k == VNONRELOC, e1->u.info);
int rk2 = luaK_exp2RK(fs, e2);
freeexps(fs, e1, e2);
switch (opr) {
case OPR_NE: {  
e1->u.info = condjump(fs, OP_EQ, 0, rk1, rk2);
break;
}
case OPR_GT: case OPR_GE: {
OpCode op = cast(OpCode, (opr - OPR_NE) + OP_EQ);
e1->u.info = condjump(fs, op, 1, rk2, rk1);  
break;
}
default: {  
OpCode op = cast(OpCode, (opr - OPR_EQ) + OP_EQ);
e1->u.info = condjump(fs, op, 1, rk1, rk2);
break;
}
}
e1->k = VJMP;
}
void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e, int line) {
static const expdesc ef = {VKINT, {0}, NO_JUMP, NO_JUMP};
switch (op) {
case OPR_MINUS: case OPR_BNOT:  
if (constfolding(fs, op + LUA_OPUNM, e, &ef))
break;
case OPR_LEN:
codeunexpval(fs, cast(OpCode, op + OP_UNM), e, line);
break;
case OPR_NOT: codenot(fs, e); break;
default: lua_assert(0);
}
}
void luaK_infix (FuncState *fs, BinOpr op, expdesc *v) {
switch (op) {
case OPR_AND: {
luaK_goiftrue(fs, v);  
break;
}
case OPR_OR: {
luaK_goiffalse(fs, v);  
break;
}
case OPR_CONCAT: {
luaK_exp2nextreg(fs, v);  
break;
}
case OPR_ADD: case OPR_SUB:
case OPR_MUL: case OPR_DIV: case OPR_IDIV:
case OPR_MOD: case OPR_POW:
case OPR_BAND: case OPR_BOR: case OPR_BXOR:
case OPR_SHL: case OPR_SHR: {
if (!tonumeral(v, NULL))
luaK_exp2RK(fs, v);
break;
}
default: {
luaK_exp2RK(fs, v);
break;
}
}
}
void luaK_posfix (FuncState *fs, BinOpr op,
expdesc *e1, expdesc *e2, int line) {
switch (op) {
case OPR_AND: {
lua_assert(e1->t == NO_JUMP);  
luaK_dischargevars(fs, e2);
luaK_concat(fs, &e2->f, e1->f);
*e1 = *e2;
break;
}
case OPR_OR: {
lua_assert(e1->f == NO_JUMP);  
luaK_dischargevars(fs, e2);
luaK_concat(fs, &e2->t, e1->t);
*e1 = *e2;
break;
}
case OPR_CONCAT: {
luaK_exp2val(fs, e2);
if (e2->k == VRELOCABLE &&
GET_OPCODE(getinstruction(fs, e2)) == OP_CONCAT) {
lua_assert(e1->u.info == GETARG_B(getinstruction(fs, e2))-1);
freeexp(fs, e1);
SETARG_B(getinstruction(fs, e2), e1->u.info);
e1->k = VRELOCABLE; e1->u.info = e2->u.info;
}
else {
luaK_exp2nextreg(fs, e2);  
codebinexpval(fs, OP_CONCAT, e1, e2, line);
}
break;
}
case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
case OPR_IDIV: case OPR_MOD: case OPR_POW:
case OPR_BAND: case OPR_BOR: case OPR_BXOR:
case OPR_SHL: case OPR_SHR: {
if (!constfolding(fs, op + LUA_OPADD, e1, e2))
codebinexpval(fs, cast(OpCode, op + OP_ADD), e1, e2, line);
break;
}
case OPR_EQ: case OPR_LT: case OPR_LE:
case OPR_NE: case OPR_GT: case OPR_GE: {
codecomp(fs, op, e1, e2);
break;
}
default: lua_assert(0);
}
}
void luaK_fixline (FuncState *fs, int line) {
fs->f->lineinfo[fs->pc - 1] = line;
}
void luaK_setlist (FuncState *fs, int base, int nelems, int tostore) {
int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;
int b = (tostore == LUA_MULTRET) ? 0 : tostore;
lua_assert(tostore != 0 && tostore <= LFIELDS_PER_FLUSH);
if (c <= MAXARG_C)
luaK_codeABC(fs, OP_SETLIST, base, b, c);
else if (c <= MAXARG_Ax) {
luaK_codeABC(fs, OP_SETLIST, base, b, 0);
codeextraarg(fs, c);
}
else
luaX_syntaxerror(fs->ls, "constructor too long");
fs->freereg = base + 1;  
}
#define lcorolib_c
#define LUA_LIB
#include <stdlib.h>
static lua_State *getco (lua_State *L) {
lua_State *co = lua_tothread(L, 1);
luaL_argcheck(L, co, 1, "thread expected");
return co;
}
static int auxresume (lua_State *L, lua_State *co, int narg) {
int status;
if (!lua_checkstack(co, narg)) {
lua_pushliteral(L, "too many arguments to resume");
return -1;  
}
if (lua_status(co) == LUA_OK && lua_gettop(co) == 0) {
lua_pushliteral(L, "cannot resume dead coroutine");
return -1;  
}
lua_xmove(L, co, narg);
status = lua_resume(co, L, narg);
if (status == LUA_OK || status == LUA_YIELD) {
int nres = lua_gettop(co);
if (!lua_checkstack(L, nres + 1)) {
lua_pop(co, nres);  
lua_pushliteral(L, "too many results to resume");
return -1;  
}
lua_xmove(co, L, nres);  
return nres;
}
else {
lua_xmove(co, L, 1);  
return -1;  
}
}
static int luaB_coresume (lua_State *L) {
lua_State *co = getco(L);
int r;
r = auxresume(L, co, lua_gettop(L) - 1);
if (r < 0) {
lua_pushboolean(L, 0);
lua_insert(L, -2);
return 2;  
}
else {
lua_pushboolean(L, 1);
lua_insert(L, -(r + 1));
return r + 1;  
}
}
static int luaB_auxwrap (lua_State *L) {
lua_State *co = lua_tothread(L, lua_upvalueindex(1));
int r = auxresume(L, co, lua_gettop(L));
if (r < 0) {
if (lua_type(L, -1) == LUA_TSTRING) {  
luaL_where(L, 1);  
lua_insert(L, -2);
lua_concat(L, 2);
}
return lua_error(L);  
}
return r;
}
static int luaB_cocreate (lua_State *L) {
lua_State *NL;
luaL_checktype(L, 1, LUA_TFUNCTION);
NL = lua_newthread(L);
lua_pushvalue(L, 1);  
lua_xmove(L, NL, 1);  
return 1;
}
static int luaB_cowrap (lua_State *L) {
luaB_cocreate(L);
lua_pushcclosure(L, luaB_auxwrap, 1);
return 1;
}
static int luaB_yield (lua_State *L) {
return lua_yield(L, lua_gettop(L));
}
static int luaB_costatus (lua_State *L) {
lua_State *co = getco(L);
if (L == co) lua_pushliteral(L, "running");
else {
switch (lua_status(co)) {
case LUA_YIELD:
lua_pushliteral(L, "suspended");
break;
case LUA_OK: {
lua_Debug ar;
if (lua_getstack(co, 0, &ar) > 0)  
lua_pushliteral(L, "normal");  
else if (lua_gettop(co) == 0)
lua_pushliteral(L, "dead");
else
lua_pushliteral(L, "suspended");  
break;
}
default:  
lua_pushliteral(L, "dead");
break;
}
}
return 1;
}
static int luaB_yieldable (lua_State *L) {
lua_pushboolean(L, lua_isyieldable(L));
return 1;
}
static int luaB_corunning (lua_State *L) {
int ismain = lua_pushthread(L);
lua_pushboolean(L, ismain);
return 2;
}
static const luaL_Reg co_funcs[] = {
{"create", luaB_cocreate},
{"resume", luaB_coresume},
{"running", luaB_corunning},
{"status", luaB_costatus},
{"wrap", luaB_cowrap},
{"yield", luaB_yield},
{"isyieldable", luaB_yieldable},
{NULL, NULL}
};
LUAMOD_API int luaopen_coroutine (lua_State *L) {
luaL_newlib(L, co_funcs);
return 1;
}
#define lctype_c
#define LUA_CORE
#ifndef lctype_h
#define lctype_h
#if !defined(LUA_USE_CTYPE)
#if 'A' == 65 && '0' == 48
#define LUA_USE_CTYPE	0
#else
#define LUA_USE_CTYPE	1
#endif
#endif
#if !LUA_USE_CTYPE	
#include <limits.h>
#define ALPHABIT	0
#define DIGITBIT	1
#define PRINTBIT	2
#define SPACEBIT	3
#define XDIGITBIT	4
#define MASK(B)		(1 << (B))
#define testprop(c,p)	(luai_ctype_[(c)+1] & (p))
#define lislalpha(c)	testprop(c, MASK(ALPHABIT))
#define lislalnum(c)	testprop(c, (MASK(ALPHABIT) | MASK(DIGITBIT)))
#define lisdigit(c)	testprop(c, MASK(DIGITBIT))
#define lisspace(c)	testprop(c, MASK(SPACEBIT))
#define lisprint(c)	testprop(c, MASK(PRINTBIT))
#define lisxdigit(c)	testprop(c, MASK(XDIGITBIT))
#define ltolower(c)	((c) | ('A' ^ 'a'))
LUAI_DDEC const lu_byte luai_ctype_[UCHAR_MAX + 2];
#else			
#include <ctype.h>
#define lislalpha(c)	(isalpha(c) || (c) == '_')
#define lislalnum(c)	(isalnum(c) || (c) == '_')
#define lisdigit(c)	(isdigit(c))
#define lisspace(c)	(isspace(c))
#define lisprint(c)	(isprint(c))
#define lisxdigit(c)	(isxdigit(c))
#define ltolower(c)	(tolower(c))
#endif			
#endif
#if !LUA_USE_CTYPE	
#include <limits.h>
LUAI_DDEF const lu_byte luai_ctype_[UCHAR_MAX + 2] = {
0x00,  
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x08,  0x08,  0x08,  0x08,  0x08,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x0c,  0x04,  0x04,  0x04,  0x04,  0x04,  0x04,  0x04,	
0x04,  0x04,  0x04,  0x04,  0x04,  0x04,  0x04,  0x04,
0x16,  0x16,  0x16,  0x16,  0x16,  0x16,  0x16,  0x16,	
0x16,  0x16,  0x04,  0x04,  0x04,  0x04,  0x04,  0x04,
0x04,  0x15,  0x15,  0x15,  0x15,  0x15,  0x15,  0x05,	
0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,
0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,	
0x05,  0x05,  0x05,  0x04,  0x04,  0x04,  0x04,  0x05,
0x04,  0x15,  0x15,  0x15,  0x15,  0x15,  0x15,  0x05,	
0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,
0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,  0x05,	
0x05,  0x05,  0x05,  0x04,  0x04,  0x04,  0x04,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,	
0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,  0x00,
};
#endif			
#define ldblib_c
#define LUA_LIB
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static const int HOOKKEY = 0;
static void checkstack (lua_State *L, lua_State *L1, int n) {
if (L != L1 && !lua_checkstack(L1, n))
luaL_error(L, "stack overflow");
}
static int db_getregistry (lua_State *L) {
lua_pushvalue(L, LUA_REGISTRYINDEX);
return 1;
}
static int db_getmetatable (lua_State *L) {
luaL_checkany(L, 1);
if (!lua_getmetatable(L, 1)) {
lua_pushnil(L);  
}
return 1;
}
static int db_setmetatable (lua_State *L) {
int t = lua_type(L, 2);
luaL_argcheck(L, t == LUA_TNIL || t == LUA_TTABLE, 2,
"nil or table expected");
lua_settop(L, 2);
lua_setmetatable(L, 1);
return 1;  
}
static int db_getuservalue (lua_State *L) {
if (lua_type(L, 1) != LUA_TUSERDATA)
lua_pushnil(L);
else
lua_getuservalue(L, 1);
return 1;
}
static int db_setuservalue (lua_State *L) {
luaL_checktype(L, 1, LUA_TUSERDATA);
luaL_checkany(L, 2);
lua_settop(L, 2);
lua_setuservalue(L, 1);
return 1;
}
static lua_State *getthread (lua_State *L, int *arg) {
if (lua_isthread(L, 1)) {
*arg = 1;
return lua_tothread(L, 1);
}
else {
*arg = 0;
return L;  
}
}
static void settabss (lua_State *L, const char *k, const char *v) {
lua_pushstring(L, v);
lua_setfield(L, -2, k);
}
static void settabsi (lua_State *L, const char *k, int v) {
lua_pushinteger(L, v);
lua_setfield(L, -2, k);
}
static void settabsb (lua_State *L, const char *k, int v) {
lua_pushboolean(L, v);
lua_setfield(L, -2, k);
}
static void treatstackoption (lua_State *L, lua_State *L1, const char *fname) {
if (L == L1)
lua_rotate(L, -2, 1);  
else
lua_xmove(L1, L, 1);  
lua_setfield(L, -2, fname);  
}
static int db_getinfo (lua_State *L) {
lua_Debug ar;
int arg;
lua_State *L1 = getthread(L, &arg);
const char *options = luaL_optstring(L, arg+2, "flnStu");
checkstack(L, L1, 3);
if (lua_isfunction(L, arg + 1)) {  
options = lua_pushfstring(L, ">%s", options);  
lua_pushvalue(L, arg + 1);  
lua_xmove(L, L1, 1);
}
else {  
if (!lua_getstack(L1, (int)luaL_checkinteger(L, arg + 1), &ar)) {
lua_pushnil(L);  
return 1;
}
}
if (!lua_getinfo(L1, options, &ar))
return luaL_argerror(L, arg+2, "invalid option");
lua_newtable(L);  
if (strchr(options, 'S')) {
settabss(L, "source", ar.source);
settabss(L, "short_src", ar.short_src);
settabsi(L, "linedefined", ar.linedefined);
settabsi(L, "lastlinedefined", ar.lastlinedefined);
settabss(L, "what", ar.what);
}
if (strchr(options, 'l'))
settabsi(L, "currentline", ar.currentline);
if (strchr(options, 'u')) {
settabsi(L, "nups", ar.nups);
settabsi(L, "nparams", ar.nparams);
settabsb(L, "isvararg", ar.isvararg);
}
if (strchr(options, 'n')) {
settabss(L, "name", ar.name);
settabss(L, "namewhat", ar.namewhat);
}
if (strchr(options, 't'))
settabsb(L, "istailcall", ar.istailcall);
if (strchr(options, 'L'))
treatstackoption(L, L1, "activelines");
if (strchr(options, 'f'))
treatstackoption(L, L1, "func");
return 1;  
}
static int db_getlocal (lua_State *L) {
int arg;
lua_State *L1 = getthread(L, &arg);
lua_Debug ar;
const char *name;
int nvar = (int)luaL_checkinteger(L, arg + 2);  
if (lua_isfunction(L, arg + 1)) {  
lua_pushvalue(L, arg + 1);  
lua_pushstring(L, lua_getlocal(L, NULL, nvar));  
return 1;  
}
else {  
int level = (int)luaL_checkinteger(L, arg + 1);
if (!lua_getstack(L1, level, &ar))  
return luaL_argerror(L, arg+1, "level out of range");
checkstack(L, L1, 1);
name = lua_getlocal(L1, &ar, nvar);
if (name) {
lua_xmove(L1, L, 1);  
lua_pushstring(L, name);  
lua_rotate(L, -2, 1);  
return 2;
}
else {
lua_pushnil(L);  
return 1;
}
}
}
static int db_setlocal (lua_State *L) {
int arg;
const char *name;
lua_State *L1 = getthread(L, &arg);
lua_Debug ar;
int level = (int)luaL_checkinteger(L, arg + 1);
int nvar = (int)luaL_checkinteger(L, arg + 2);
if (!lua_getstack(L1, level, &ar))  
return luaL_argerror(L, arg+1, "level out of range");
luaL_checkany(L, arg+3);
lua_settop(L, arg+3);
checkstack(L, L1, 1);
lua_xmove(L, L1, 1);
name = lua_setlocal(L1, &ar, nvar);
if (name == NULL)
lua_pop(L1, 1);  
lua_pushstring(L, name);
return 1;
}
static int auxupvalue (lua_State *L, int get) {
const char *name;
int n = (int)luaL_checkinteger(L, 2);  
luaL_checktype(L, 1, LUA_TFUNCTION);  
name = get ? lua_getupvalue(L, 1, n) : lua_setupvalue(L, 1, n);
if (name == NULL) return 0;
lua_pushstring(L, name);
lua_insert(L, -(get+1));  
return get + 1;
}
static int db_getupvalue (lua_State *L) {
return auxupvalue(L, 1);
}
static int db_setupvalue (lua_State *L) {
luaL_checkany(L, 3);
return auxupvalue(L, 0);
}
static int checkupval (lua_State *L, int argf, int argnup) {
int nup = (int)luaL_checkinteger(L, argnup);  
luaL_checktype(L, argf, LUA_TFUNCTION);  
luaL_argcheck(L, (lua_getupvalue(L, argf, nup) != NULL), argnup,
"invalid upvalue index");
return nup;
}
static int db_upvalueid (lua_State *L) {
int n = checkupval(L, 1, 2);
lua_pushlightuserdata(L, lua_upvalueid(L, 1, n));
return 1;
}
static int db_upvaluejoin (lua_State *L) {
int n1 = checkupval(L, 1, 2);
int n2 = checkupval(L, 3, 4);
luaL_argcheck(L, !lua_iscfunction(L, 1), 1, "Lua function expected");
luaL_argcheck(L, !lua_iscfunction(L, 3), 3, "Lua function expected");
lua_upvaluejoin(L, 1, n1, 3, n2);
return 0;
}
static void hookf (lua_State *L, lua_Debug *ar) {
static const char *const hooknames[] =
{"call", "return", "line", "count", "tail call"};
lua_rawgetp(L, LUA_REGISTRYINDEX, &HOOKKEY);
lua_pushthread(L);
if (lua_rawget(L, -2) == LUA_TFUNCTION) {  
lua_pushstring(L, hooknames[(int)ar->event]);  
if (ar->currentline >= 0)
lua_pushinteger(L, ar->currentline);  
else lua_pushnil(L);
lua_assert(lua_getinfo(L, "lS", ar));
lua_call(L, 2, 0);  
}
}
static int makemask (const char *smask, int count) {
int mask = 0;
if (strchr(smask, 'c')) mask |= LUA_MASKCALL;
if (strchr(smask, 'r')) mask |= LUA_MASKRET;
if (strchr(smask, 'l')) mask |= LUA_MASKLINE;
if (count > 0) mask |= LUA_MASKCOUNT;
return mask;
}
static char *unmakemask (int mask, char *smask) {
int i = 0;
if (mask & LUA_MASKCALL) smask[i++] = 'c';
if (mask & LUA_MASKRET) smask[i++] = 'r';
if (mask & LUA_MASKLINE) smask[i++] = 'l';
smask[i] = '\0';
return smask;
}
static int db_sethook (lua_State *L) {
int arg, mask, count;
lua_Hook func;
lua_State *L1 = getthread(L, &arg);
if (lua_isnoneornil(L, arg+1)) {  
lua_settop(L, arg+1);
func = NULL; mask = 0; count = 0;  
}
else {
const char *smask = luaL_checkstring(L, arg+2);
luaL_checktype(L, arg+1, LUA_TFUNCTION);
count = (int)luaL_optinteger(L, arg + 3, 0);
func = hookf; mask = makemask(smask, count);
}
if (lua_rawgetp(L, LUA_REGISTRYINDEX, &HOOKKEY) == LUA_TNIL) {
lua_createtable(L, 0, 2);  
lua_pushvalue(L, -1);
lua_rawsetp(L, LUA_REGISTRYINDEX, &HOOKKEY);  
lua_pushstring(L, "k");
lua_setfield(L, -2, "__mode");  
lua_pushvalue(L, -1);
lua_setmetatable(L, -2);  
}
checkstack(L, L1, 1);
lua_pushthread(L1); lua_xmove(L1, L, 1);  
lua_pushvalue(L, arg + 1);  
lua_rawset(L, -3);  
lua_sethook(L1, func, mask, count);
return 0;
}
static int db_gethook (lua_State *L) {
int arg;
lua_State *L1 = getthread(L, &arg);
char buff[5];
int mask = lua_gethookmask(L1);
lua_Hook hook = lua_gethook(L1);
if (hook == NULL)  
lua_pushnil(L);
else if (hook != hookf)  
lua_pushliteral(L, "external hook");
else {  
lua_rawgetp(L, LUA_REGISTRYINDEX, &HOOKKEY);
checkstack(L, L1, 1);
lua_pushthread(L1); lua_xmove(L1, L, 1);
lua_rawget(L, -2);   
lua_remove(L, -2);  
}
lua_pushstring(L, unmakemask(mask, buff));  
lua_pushinteger(L, lua_gethookcount(L1));  
return 3;
}
static int db_debug (lua_State *L) {
for (;;) {
char buffer[250];
lua_writestringerror("%s", "lua_debug> ");
if (fgets(buffer, sizeof(buffer), stdin) == 0 ||
strcmp(buffer, "cont\n") == 0)
return 0;
if (luaL_loadbuffer(L, buffer, strlen(buffer), "=(debug command)") ||
lua_pcall(L, 0, 0, 0))
lua_writestringerror("%s\n", lua_tostring(L, -1));
lua_settop(L, 0);  
}
}
static int db_traceback (lua_State *L) {
int arg;
lua_State *L1 = getthread(L, &arg);
const char *msg = lua_tostring(L, arg + 1);
if (msg == NULL && !lua_isnoneornil(L, arg + 1))  
lua_pushvalue(L, arg + 1);  
else {
int level = (int)luaL_optinteger(L, arg + 2, (L == L1) ? 1 : 0);
luaL_traceback(L, L1, msg, level);
}
return 1;
}
static const luaL_Reg dblib[] = {
{"debug", db_debug},
{"getuservalue", db_getuservalue},
{"gethook", db_gethook},
{"getinfo", db_getinfo},
{"getlocal", db_getlocal},
{"getregistry", db_getregistry},
{"getmetatable", db_getmetatable},
{"getupvalue", db_getupvalue},
{"upvaluejoin", db_upvaluejoin},
{"upvalueid", db_upvalueid},
{"setuservalue", db_setuservalue},
{"sethook", db_sethook},
{"setlocal", db_setlocal},
{"setmetatable", db_setmetatable},
{"setupvalue", db_setupvalue},
{"traceback", db_traceback},
{NULL, NULL}
};
LUAMOD_API int luaopen_debug (lua_State *L) {
luaL_newlib(L, dblib);
return 1;
}
#define ldebug_c
#define LUA_CORE
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#define noLuaClosure(f)		((f) == NULL || (f)->c.tt == LUA_TCCL)
#define ci_func(ci)		(clLvalue((ci)->func))
static const char *funcnamefromcode (lua_State *L, CallInfo *ci,
const char **name);
static int currentpc (CallInfo *ci) {
lua_assert(isLua(ci));
return pcRel(ci->u.l.savedpc, ci_func(ci)->p);
}
static int currentline (CallInfo *ci) {
return getfuncline(ci_func(ci)->p, currentpc(ci));
}
static void swapextra (lua_State *L) {
if (L->status == LUA_YIELD) {
CallInfo *ci = L->ci;  
StkId temp = ci->func;  
ci->func = restorestack(L, ci->extra);
ci->extra = savestack(L, temp);
}
}
LUA_API void lua_sethook (lua_State *L, lua_Hook func, int mask, int count) {
if (func == NULL || mask == 0) {  
mask = 0;
func = NULL;
}
if (isLua(L->ci))
L->oldpc = L->ci->u.l.savedpc;
L->hook = func;
L->basehookcount = count;
resethookcount(L);
L->hookmask = cast_byte(mask);
}
LUA_API lua_Hook lua_gethook (lua_State *L) {
return L->hook;
}
LUA_API int lua_gethookmask (lua_State *L) {
return L->hookmask;
}
LUA_API int lua_gethookcount (lua_State *L) {
return L->basehookcount;
}
LUA_API int lua_getstack (lua_State *L, int level, lua_Debug *ar) {
int status;
CallInfo *ci;
if (level < 0) return 0;  
lua_lock(L);
for (ci = L->ci; level > 0 && ci != &L->base_ci; ci = ci->previous)
level--;
if (level == 0 && ci != &L->base_ci) {  
status = 1;
ar->i_ci = ci;
}
else status = 0;  
lua_unlock(L);
return status;
}
static const char *upvalname (Proto *p, int uv) {
TString *s = check_exp(uv < p->sizeupvalues, p->upvalues[uv].name);
if (s == NULL) return "?";
else return getstr(s);
}
static const char *findvararg (CallInfo *ci, int n, StkId *pos) {
int nparams = clLvalue(ci->func)->p->numparams;
if (n >= cast_int(ci->u.l.base - ci->func) - nparams)
return NULL;  
else {
*pos = ci->func + nparams + n;
return "(*vararg)";  
}
}
static const char *findlocal (lua_State *L, CallInfo *ci, int n,
StkId *pos) {
const char *name = NULL;
StkId base;
if (isLua(ci)) {
if (n < 0)  
return findvararg(ci, -n, pos);
else {
base = ci->u.l.base;
name = luaF_getlocalname(ci_func(ci)->p, n, currentpc(ci));
}
}
else
base = ci->func + 1;
if (name == NULL) {  
StkId limit = (ci == L->ci) ? L->top : ci->next->func;
if (limit - base >= n && n > 0)  
name = "(*temporary)";  
else
return NULL;  
}
*pos = base + (n - 1);
return name;
}
LUA_API const char *lua_getlocal (lua_State *L, const lua_Debug *ar, int n) {
const char *name;
lua_lock(L);
swapextra(L);
if (ar == NULL) {  
if (!isLfunction(L->top - 1))  
name = NULL;
else  
name = luaF_getlocalname(clLvalue(L->top - 1)->p, n, 0);
}
else {  
StkId pos = NULL;  
name = findlocal(L, ar->i_ci, n, &pos);
if (name) {
setobj2s(L, L->top, pos);
api_incr_top(L);
}
}
swapextra(L);
lua_unlock(L);
return name;
}
LUA_API const char *lua_setlocal (lua_State *L, const lua_Debug *ar, int n) {
StkId pos = NULL;  
const char *name;
lua_lock(L);
swapextra(L);
name = findlocal(L, ar->i_ci, n, &pos);
if (name) {
setobjs2s(L, pos, L->top - 1);
L->top--;  
}
swapextra(L);
lua_unlock(L);
return name;
}
static void funcinfo (lua_Debug *ar, Closure *cl) {
if (noLuaClosure(cl)) {
ar->source = "=[C]";
ar->linedefined = -1;
ar->lastlinedefined = -1;
ar->what = "C";
}
else {
Proto *p = cl->l.p;
ar->source = p->source ? getstr(p->source) : "=?";
ar->linedefined = p->linedefined;
ar->lastlinedefined = p->lastlinedefined;
ar->what = (ar->linedefined == 0) ? "main" : "Lua";
}
luaO_chunkid(ar->short_src, ar->source, LUA_IDSIZE);
}
static void collectvalidlines (lua_State *L, Closure *f) {
if (noLuaClosure(f)) {
setnilvalue(L->top);
api_incr_top(L);
}
else {
int i;
TValue v;
int *lineinfo = f->l.p->lineinfo;
Table *t = luaH_new(L);  
sethvalue(L, L->top, t);  
api_incr_top(L);
setbvalue(&v, 1);  
for (i = 0; i < f->l.p->sizelineinfo; i++)  
luaH_setint(L, t, lineinfo[i], &v);  
}
}
static const char *getfuncname (lua_State *L, CallInfo *ci, const char **name) {
if (ci == NULL)  
return NULL;  
else if (ci->callstatus & CIST_FIN) {  
*name = "__gc";
return "metamethod";  
}
else if (!(ci->callstatus & CIST_TAIL) && isLua(ci->previous))
return funcnamefromcode(L, ci->previous, name);
else return NULL;  
}
static int auxgetinfo (lua_State *L, const char *what, lua_Debug *ar,
Closure *f, CallInfo *ci) {
int status = 1;
for (; *what; what++) {
switch (*what) {
case 'S': {
funcinfo(ar, f);
break;
}
case 'l': {
ar->currentline = (ci && isLua(ci)) ? currentline(ci) : -1;
break;
}
case 'u': {
ar->nups = (f == NULL) ? 0 : f->c.nupvalues;
if (noLuaClosure(f)) {
ar->isvararg = 1;
ar->nparams = 0;
}
else {
ar->isvararg = f->l.p->is_vararg;
ar->nparams = f->l.p->numparams;
}
break;
}
case 't': {
ar->istailcall = (ci) ? ci->callstatus & CIST_TAIL : 0;
break;
}
case 'n': {
ar->namewhat = getfuncname(L, ci, &ar->name);
if (ar->namewhat == NULL) {
ar->namewhat = "";  
ar->name = NULL;
}
break;
}
case 'L':
case 'f':  
break;
default: status = 0;  
}
}
return status;
}
LUA_API int lua_getinfo (lua_State *L, const char *what, lua_Debug *ar) {
int status;
Closure *cl;
CallInfo *ci;
StkId func;
lua_lock(L);
swapextra(L);
if (*what == '>') {
ci = NULL;
func = L->top - 1;
api_check(L, ttisfunction(func), "function expected");
what++;  
L->top--;  
}
else {
ci = ar->i_ci;
func = ci->func;
lua_assert(ttisfunction(ci->func));
}
cl = ttisclosure(func) ? clvalue(func) : NULL;
status = auxgetinfo(L, what, ar, cl, ci);
if (strchr(what, 'f')) {
setobjs2s(L, L->top, func);
api_incr_top(L);
}
swapextra(L);  
if (strchr(what, 'L'))
collectvalidlines(L, cl);
lua_unlock(L);
return status;
}
static const char *getobjname (Proto *p, int lastpc, int reg,
const char **name);
static void kname (Proto *p, int pc, int c, const char **name) {
if (ISK(c)) {  
TValue *kvalue = &p->k[INDEXK(c)];
if (ttisstring(kvalue)) {  
*name = svalue(kvalue);  
return;
}
}
else {  
const char *what = getobjname(p, pc, c, name); 
if (what && *what == 'c') {  
return;  
}
}
*name = "?";  
}
static int filterpc (int pc, int jmptarget) {
if (pc < jmptarget)  
return -1;  
else return pc;  
}
static int findsetreg (Proto *p, int lastpc, int reg) {
int pc;
int setreg = -1;  
int jmptarget = 0;  
for (pc = 0; pc < lastpc; pc++) {
Instruction i = p->code[pc];
OpCode op = GET_OPCODE(i);
int a = GETARG_A(i);
switch (op) {
case OP_LOADNIL: {
int b = GETARG_B(i);
if (a <= reg && reg <= a + b)  
setreg = filterpc(pc, jmptarget);
break;
}
case OP_TFORCALL: {
if (reg >= a + 2)  
setreg = filterpc(pc, jmptarget);
break;
}
case OP_CALL:
case OP_TAILCALL: {
if (reg >= a)  
setreg = filterpc(pc, jmptarget);
break;
}
case OP_JMP: {
int b = GETARG_sBx(i);
int dest = pc + 1 + b;
if (pc < dest && dest <= lastpc) {
if (dest > jmptarget)
jmptarget = dest;  
}
break;
}
default:
if (testAMode(op) && reg == a)  
setreg = filterpc(pc, jmptarget);
break;
}
}
return setreg;
}
static const char *getobjname (Proto *p, int lastpc, int reg,
const char **name) {
int pc;
*name = luaF_getlocalname(p, reg + 1, lastpc);
if (*name)  
return "local";
pc = findsetreg(p, lastpc, reg);
if (pc != -1) {  
Instruction i = p->code[pc];
OpCode op = GET_OPCODE(i);
switch (op) {
case OP_MOVE: {
int b = GETARG_B(i);  
if (b < GETARG_A(i))
return getobjname(p, pc, b, name);  
break;
}
case OP_GETTABUP:
case OP_GETTABLE: {
int k = GETARG_C(i);  
int t = GETARG_B(i);  
const char *vn = (op == OP_GETTABLE)  
? luaF_getlocalname(p, t + 1, pc)
: upvalname(p, t);
kname(p, pc, k, name);
return (vn && strcmp(vn, LUA_ENV) == 0) ? "global" : "field";
}
case OP_GETUPVAL: {
*name = upvalname(p, GETARG_B(i));
return "upvalue";
}
case OP_LOADK:
case OP_LOADKX: {
int b = (op == OP_LOADK) ? GETARG_Bx(i)
: GETARG_Ax(p->code[pc + 1]);
if (ttisstring(&p->k[b])) {
*name = svalue(&p->k[b]);
return "constant";
}
break;
}
case OP_SELF: {
int k = GETARG_C(i);  
kname(p, pc, k, name);
return "method";
}
default: break;  
}
}
return NULL;  
}
static const char *funcnamefromcode (lua_State *L, CallInfo *ci,
const char **name) {
TMS tm = (TMS)0;  
Proto *p = ci_func(ci)->p;  
int pc = currentpc(ci);  
Instruction i = p->code[pc];  
if (ci->callstatus & CIST_HOOKED) {  
*name = "?";
return "hook";
}
switch (GET_OPCODE(i)) {
case OP_CALL:
case OP_TAILCALL:
return getobjname(p, pc, GETARG_A(i), name);  
case OP_TFORCALL: {  
*name = "for iterator";
return "for iterator";
}
case OP_SELF: case OP_GETTABUP: case OP_GETTABLE:
tm = TM_INDEX;
break;
case OP_SETTABUP: case OP_SETTABLE:
tm = TM_NEWINDEX;
break;
case OP_ADD: case OP_SUB: case OP_MUL: case OP_MOD:
case OP_POW: case OP_DIV: case OP_IDIV: case OP_BAND:
case OP_BOR: case OP_BXOR: case OP_SHL: case OP_SHR: {
int offset = cast_int(GET_OPCODE(i)) - cast_int(OP_ADD);  
tm = cast(TMS, offset + cast_int(TM_ADD));  
break;
}
case OP_UNM: tm = TM_UNM; break;
case OP_BNOT: tm = TM_BNOT; break;
case OP_LEN: tm = TM_LEN; break;
case OP_CONCAT: tm = TM_CONCAT; break;
case OP_EQ: tm = TM_EQ; break;
case OP_LT: tm = TM_LT; break;
case OP_LE: tm = TM_LE; break;
default:
return NULL;  
}
*name = getstr(G(L)->tmname[tm]);
return "metamethod";
}
static int isinstack (CallInfo *ci, const TValue *o) {
ptrdiff_t i = o - ci->u.l.base;
return (0 <= i && i < (ci->top - ci->u.l.base) && ci->u.l.base + i == o);
}
static const char *getupvalname (CallInfo *ci, const TValue *o,
const char **name) {
LClosure *c = ci_func(ci);
int i;
for (i = 0; i < c->nupvalues; i++) {
if (c->upvals[i]->v == o) {
*name = upvalname(c->p, i);
return "upvalue";
}
}
return NULL;
}
static const char *varinfo (lua_State *L, const TValue *o) {
const char *name = NULL;  
CallInfo *ci = L->ci;
const char *kind = NULL;
if (isLua(ci)) {
kind = getupvalname(ci, o, &name);  
if (!kind && isinstack(ci, o))  
kind = getobjname(ci_func(ci)->p, currentpc(ci),
cast_int(o - ci->u.l.base), &name);
}
return (kind) ? luaO_pushfstring(L, " (%s '%s')", kind, name) : "";
}
l_noret luaG_typeerror (lua_State *L, const TValue *o, const char *op) {
const char *t = luaT_objtypename(L, o);
luaG_runerror(L, "attempt to %s a %s value%s", op, t, varinfo(L, o));
}
l_noret luaG_concaterror (lua_State *L, const TValue *p1, const TValue *p2) {
if (ttisstring(p1) || cvt2str(p1)) p1 = p2;
luaG_typeerror(L, p1, "concatenate");
}
l_noret luaG_opinterror (lua_State *L, const TValue *p1,
const TValue *p2, const char *msg) {
lua_Number temp;
if (!tonumber(p1, &temp))  
p2 = p1;  
luaG_typeerror(L, p2, msg);
}
l_noret luaG_tointerror (lua_State *L, const TValue *p1, const TValue *p2) {
lua_Integer temp;
if (!tointeger(p1, &temp))
p2 = p1;
luaG_runerror(L, "number%s has no integer representation", varinfo(L, p2));
}
l_noret luaG_ordererror (lua_State *L, const TValue *p1, const TValue *p2) {
const char *t1 = luaT_objtypename(L, p1);
const char *t2 = luaT_objtypename(L, p2);
if (strcmp(t1, t2) == 0)
luaG_runerror(L, "attempt to compare two %s values", t1);
else
luaG_runerror(L, "attempt to compare %s with %s", t1, t2);
}
const char *luaG_addinfo (lua_State *L, const char *msg, TString *src,
int line) {
char buff[LUA_IDSIZE];
if (src)
luaO_chunkid(buff, getstr(src), LUA_IDSIZE);
else {  
buff[0] = '?'; buff[1] = '\0';
}
return luaO_pushfstring(L, "%s:%d: %s", buff, line, msg);
}
l_noret luaG_errormsg (lua_State *L) {
if (L->errfunc != 0) {  
StkId errfunc = restorestack(L, L->errfunc);
setobjs2s(L, L->top, L->top - 1);  
setobjs2s(L, L->top - 1, errfunc);  
L->top++;  
luaD_callnoyield(L, L->top - 2, 1);  
}
luaD_throw(L, LUA_ERRRUN);
}
l_noret luaG_runerror (lua_State *L, const char *fmt, ...) {
CallInfo *ci = L->ci;
const char *msg;
va_list argp;
luaC_checkGC(L);  
va_start(argp, fmt);
msg = luaO_pushvfstring(L, fmt, argp);  
va_end(argp);
if (isLua(ci))  
luaG_addinfo(L, msg, ci_func(ci)->p->source, currentline(ci));
luaG_errormsg(L);
}
void luaG_traceexec (lua_State *L) {
CallInfo *ci = L->ci;
lu_byte mask = L->hookmask;
int counthook = (--L->hookcount == 0 && (mask & LUA_MASKCOUNT));
if (counthook)
resethookcount(L);  
else if (!(mask & LUA_MASKLINE))
return;  
if (ci->callstatus & CIST_HOOKYIELD) {  
ci->callstatus &= ~CIST_HOOKYIELD;  
return;  
}
if (counthook)
luaD_hook(L, LUA_HOOKCOUNT, -1);  
if (mask & LUA_MASKLINE) {
Proto *p = ci_func(ci)->p;
int npc = pcRel(ci->u.l.savedpc, p);
int newline = getfuncline(p, npc);
if (npc == 0 ||  
ci->u.l.savedpc <= L->oldpc ||  
newline != getfuncline(p, pcRel(L->oldpc, p)))  
luaD_hook(L, LUA_HOOKLINE, newline);  
}
L->oldpc = ci->u.l.savedpc;
if (L->status == LUA_YIELD) {  
if (counthook)
L->hookcount = 1;  
ci->u.l.savedpc--;  
ci->callstatus |= CIST_HOOKYIELD;  
ci->func = L->top - 1;  
luaD_throw(L, LUA_YIELD);
}
}
#define ldo_c
#define LUA_CORE
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#define errorstatus(s)	((s) > LUA_YIELD)
#if !defined(LUAI_THROW)				
#if defined(__cplusplus) && !defined(LUA_USE_LONGJMP)	
#define LUAI_THROW(L,c)		throw(c)
#define LUAI_TRY(L,c,a) \
try { a } catch(...) { if ((c)->status == 0) (c)->status = -1; }
#define luai_jmpbuf		int  
#elif defined(LUA_USE_POSIX)				
#define LUAI_THROW(L,c)		_longjmp((c)->b, 1)
#define LUAI_TRY(L,c,a)		if (_setjmp((c)->b) == 0) { a }
#define luai_jmpbuf		jmp_buf
#else							
#define LUAI_THROW(L,c)		longjmp((c)->b, 1)
#define LUAI_TRY(L,c,a)		if (setjmp((c)->b) == 0) { a }
#define luai_jmpbuf		jmp_buf
#endif							
#endif							
struct lua_longjmp {
struct lua_longjmp *previous;
luai_jmpbuf b;
volatile int status;  
};
static void seterrorobj (lua_State *L, int errcode, StkId oldtop) {
switch (errcode) {
case LUA_ERRMEM: {  
setsvalue2s(L, oldtop, G(L)->memerrmsg); 
break;
}
case LUA_ERRERR: {
setsvalue2s(L, oldtop, luaS_newliteral(L, "error in error handling"));
break;
}
default: {
setobjs2s(L, oldtop, L->top - 1);  
break;
}
}
L->top = oldtop + 1;
}
l_noret luaD_throw (lua_State *L, int errcode) {
if (L->errorJmp) {  
L->errorJmp->status = errcode;  
LUAI_THROW(L, L->errorJmp);  
}
else {  
global_State *g = G(L);
L->status = cast_byte(errcode);  
if (g->mainthread->errorJmp) {  
setobjs2s(L, g->mainthread->top++, L->top - 1);  
luaD_throw(g->mainthread, errcode);  
}
else {  
if (g->panic) {  
seterrorobj(L, errcode, L->top);  
if (L->ci->top < L->top)
L->ci->top = L->top;  
lua_unlock(L);
g->panic(L);  
}
abort();
}
}
}
int luaD_rawrunprotected (lua_State *L, Pfunc f, void *ud) {
unsigned short oldnCcalls = L->nCcalls;
struct lua_longjmp lj;
lj.status = LUA_OK;
lj.previous = L->errorJmp;  
L->errorJmp = &lj;
LUAI_TRY(L, &lj,
(*f)(L, ud);
);
L->errorJmp = lj.previous;  
L->nCcalls = oldnCcalls;
return lj.status;
}
static void correctstack (lua_State *L, TValue *oldstack) {
CallInfo *ci;
UpVal *up;
L->top = (L->top - oldstack) + L->stack;
for (up = L->openupval; up != NULL; up = up->u.open.next)
up->v = (up->v - oldstack) + L->stack;
for (ci = L->ci; ci != NULL; ci = ci->previous) {
ci->top = (ci->top - oldstack) + L->stack;
ci->func = (ci->func - oldstack) + L->stack;
if (isLua(ci))
ci->u.l.base = (ci->u.l.base - oldstack) + L->stack;
}
}
#define ERRORSTACKSIZE	(LUAI_MAXSTACK + 200)
void luaD_reallocstack (lua_State *L, int newsize) {
TValue *oldstack = L->stack;
int lim = L->stacksize;
lua_assert(newsize <= LUAI_MAXSTACK || newsize == ERRORSTACKSIZE);
lua_assert(L->stack_last - L->stack == L->stacksize - EXTRA_STACK);
luaM_reallocvector(L, L->stack, L->stacksize, newsize, TValue);
for (; lim < newsize; lim++)
setnilvalue(L->stack + lim); 
L->stacksize = newsize;
L->stack_last = L->stack + newsize - EXTRA_STACK;
correctstack(L, oldstack);
}
void luaD_growstack (lua_State *L, int n) {
int size = L->stacksize;
if (size > LUAI_MAXSTACK)  
luaD_throw(L, LUA_ERRERR);
else {
int needed = cast_int(L->top - L->stack) + n + EXTRA_STACK;
int newsize = 2 * size;
if (newsize > LUAI_MAXSTACK) newsize = LUAI_MAXSTACK;
if (newsize < needed) newsize = needed;
if (newsize > LUAI_MAXSTACK) {  
luaD_reallocstack(L, ERRORSTACKSIZE);
luaG_runerror(L, "stack overflow");
}
else
luaD_reallocstack(L, newsize);
}
}
static int stackinuse (lua_State *L) {
CallInfo *ci;
StkId lim = L->top;
for (ci = L->ci; ci != NULL; ci = ci->previous) {
if (lim < ci->top) lim = ci->top;
}
lua_assert(lim <= L->stack_last);
return cast_int(lim - L->stack) + 1;  
}
void luaD_shrinkstack (lua_State *L) {
int inuse = stackinuse(L);
int goodsize = inuse + (inuse / 8) + 2*EXTRA_STACK;
if (goodsize > LUAI_MAXSTACK)
goodsize = LUAI_MAXSTACK;  
if (L->stacksize > LUAI_MAXSTACK)  
luaE_freeCI(L);  
else
luaE_shrinkCI(L);  
/* if thread is currently not handling a stack overflow and its
good size is smaller than current size, shrink its stack */
if (inuse <= (LUAI_MAXSTACK - EXTRA_STACK) &&
goodsize < L->stacksize)
luaD_reallocstack(L, goodsize);
else  
condmovestack(L,{},{});  
}
void luaD_inctop (lua_State *L) {
luaD_checkstack(L, 1);
L->top++;
}
void luaD_hook (lua_State *L, int event, int line) {
lua_Hook hook = L->hook;
if (hook && L->allowhook) {  
CallInfo *ci = L->ci;
ptrdiff_t top = savestack(L, L->top);
ptrdiff_t ci_top = savestack(L, ci->top);
lua_Debug ar;
ar.event = event;
ar.currentline = line;
ar.i_ci = ci;
luaD_checkstack(L, LUA_MINSTACK);  
ci->top = L->top + LUA_MINSTACK;
lua_assert(ci->top <= L->stack_last);
L->allowhook = 0;  
ci->callstatus |= CIST_HOOKED;
lua_unlock(L);
(*hook)(L, &ar);
lua_lock(L);
lua_assert(!L->allowhook);
L->allowhook = 1;
ci->top = restorestack(L, ci_top);
L->top = restorestack(L, top);
ci->callstatus &= ~CIST_HOOKED;
}
}
static void callhook (lua_State *L, CallInfo *ci) {
int hook = LUA_HOOKCALL;
ci->u.l.savedpc++;  
if (isLua(ci->previous) &&
GET_OPCODE(*(ci->previous->u.l.savedpc - 1)) == OP_TAILCALL) {
ci->callstatus |= CIST_TAIL;
hook = LUA_HOOKTAILCALL;
}
luaD_hook(L, hook, -1);
ci->u.l.savedpc--;  
}
static StkId adjust_varargs (lua_State *L, Proto *p, int actual) {
int i;
int nfixargs = p->numparams;
StkId base, fixed;
fixed = L->top - actual;  
base = L->top;  
for (i = 0; i < nfixargs && i < actual; i++) {
setobjs2s(L, L->top++, fixed + i);
setnilvalue(fixed + i);  
}
for (; i < nfixargs; i++)
setnilvalue(L->top++);  
return base;
}
static void tryfuncTM (lua_State *L, StkId func) {
const TValue *tm = luaT_gettmbyobj(L, func, TM_CALL);
StkId p;
if (!ttisfunction(tm))
luaG_typeerror(L, func, "call");
for (p = L->top; p > func; p--)
setobjs2s(L, p, p-1);
L->top++;  
setobj2s(L, func, tm);  
}
static int moveresults (lua_State *L, const TValue *firstResult, StkId res,
int nres, int wanted) {
switch (wanted) {  
case 0: break;  
case 1: {  
if (nres == 0)   
firstResult = luaO_nilobject;  
setobjs2s(L, res, firstResult);  
break;
}
case LUA_MULTRET: {
int i;
for (i = 0; i < nres; i++)  
setobjs2s(L, res + i, firstResult + i);
L->top = res + nres;
return 0;  
}
default: {
int i;
if (wanted <= nres) {  
for (i = 0; i < wanted; i++)  
setobjs2s(L, res + i, firstResult + i);
}
else {  
for (i = 0; i < nres; i++)  
setobjs2s(L, res + i, firstResult + i);
for (; i < wanted; i++)  
setnilvalue(res + i);
}
break;
}
}
L->top = res + wanted;  
return 1;
}
int luaD_poscall (lua_State *L, CallInfo *ci, StkId firstResult, int nres) {
StkId res;
int wanted = ci->nresults;
if (L->hookmask & (LUA_MASKRET | LUA_MASKLINE)) {
if (L->hookmask & LUA_MASKRET) {
ptrdiff_t fr = savestack(L, firstResult);  
luaD_hook(L, LUA_HOOKRET, -1);
firstResult = restorestack(L, fr);
}
L->oldpc = ci->previous->u.l.savedpc;  
}
res = ci->func;  
L->ci = ci->previous;  
return moveresults(L, firstResult, res, nres, wanted);
}
#define next_ci(L) (L->ci = (L->ci->next ? L->ci->next : luaE_extendCI(L)))
#define checkstackp(L,n,p)  \
luaD_checkstackaux(L, n, \
ptrdiff_t t__ = savestack(L, p);   \
luaC_checkGC(L),   \
p = restorestack(L, t__))  
int luaD_precall (lua_State *L, StkId func, int nresults) {
lua_CFunction f;
CallInfo *ci;
switch (ttype(func)) {
case LUA_TCCL:  
f = clCvalue(func)->f;
goto Cfunc;
case LUA_TLCF:  
f = fvalue(func);
Cfunc: {
int n;  
checkstackp(L, LUA_MINSTACK, func);  
ci = next_ci(L);  
ci->nresults = nresults;
ci->func = func;
ci->top = L->top + LUA_MINSTACK;
lua_assert(ci->top <= L->stack_last);
ci->callstatus = 0;
if (L->hookmask & LUA_MASKCALL)
luaD_hook(L, LUA_HOOKCALL, -1);
lua_unlock(L);
n = (*f)(L);  
lua_lock(L);
api_checknelems(L, n);
luaD_poscall(L, ci, L->top - n, n);
return 1;
}
case LUA_TLCL: {  
StkId base;
Proto *p = clLvalue(func)->p;
int n = cast_int(L->top - func) - 1;  
int fsize = p->maxstacksize;  
checkstackp(L, fsize, func);
if (p->is_vararg)
base = adjust_varargs(L, p, n);
else {  
for (; n < p->numparams; n++)
setnilvalue(L->top++);  
base = func + 1;
}
ci = next_ci(L);  
ci->nresults = nresults;
ci->func = func;
ci->u.l.base = base;
L->top = ci->top = base + fsize;
lua_assert(ci->top <= L->stack_last);
ci->u.l.savedpc = p->code;  
ci->callstatus = CIST_LUA;
if (L->hookmask & LUA_MASKCALL)
callhook(L, ci);
return 0;
}
default: {  
checkstackp(L, 1, func);  
tryfuncTM(L, func);  
return luaD_precall(L, func, nresults);  
}
}
}
static void stackerror (lua_State *L) {
if (L->nCcalls == LUAI_MAXCCALLS)
luaG_runerror(L, "C stack overflow");
else if (L->nCcalls >= (LUAI_MAXCCALLS + (LUAI_MAXCCALLS>>3)))
luaD_throw(L, LUA_ERRERR);  
}
void luaD_call (lua_State *L, StkId func, int nResults) {
if (++L->nCcalls >= LUAI_MAXCCALLS)
stackerror(L);
if (!luaD_precall(L, func, nResults))  
luaV_execute(L);  
L->nCcalls--;
}
void luaD_callnoyield (lua_State *L, StkId func, int nResults) {
L->nny++;
luaD_call(L, func, nResults);
L->nny--;
}
static void finishCcall (lua_State *L, int status) {
CallInfo *ci = L->ci;
int n;
lua_assert(ci->u.c.k != NULL && L->nny == 0);
lua_assert((ci->callstatus & CIST_YPCALL) || status == LUA_YIELD);
if (ci->callstatus & CIST_YPCALL) {  
ci->callstatus &= ~CIST_YPCALL;  
L->errfunc = ci->u.c.old_errfunc;  
}
/* finish 'lua_callk'/'lua_pcall'; CIST_YPCALL and 'errfunc' already
handled */
adjustresults(L, ci->nresults);
lua_unlock(L);
n = (*ci->u.c.k)(L, status, ci->u.c.ctx);  
lua_lock(L);
api_checknelems(L, n);
luaD_poscall(L, ci, L->top - n, n);  
}
static void unroll (lua_State *L, void *ud) {
if (ud != NULL)  
finishCcall(L, *(int *)ud);  
while (L->ci != &L->base_ci) {  
if (!isLua(L->ci))  
finishCcall(L, LUA_YIELD);  
else {  
luaV_finishOp(L);  
luaV_execute(L);  
}
}
}
static CallInfo *findpcall (lua_State *L) {
CallInfo *ci;
for (ci = L->ci; ci != NULL; ci = ci->previous) {  
if (ci->callstatus & CIST_YPCALL)
return ci;
}
return NULL;  
}
static int recover (lua_State *L, int status) {
StkId oldtop;
CallInfo *ci = findpcall(L);
if (ci == NULL) return 0;  
oldtop = restorestack(L, ci->extra);
luaF_close(L, oldtop);
seterrorobj(L, status, oldtop);
L->ci = ci;
L->allowhook = getoah(ci->callstatus);  
L->nny = 0;  
luaD_shrinkstack(L);
L->errfunc = ci->u.c.old_errfunc;
return 1;  
}
static int resume_error (lua_State *L, const char *msg, int narg) {
L->top -= narg;  
setsvalue2s(L, L->top, luaS_new(L, msg));  
api_incr_top(L);
lua_unlock(L);
return LUA_ERRRUN;
}
static void resume (lua_State *L, void *ud) {
int n = *(cast(int*, ud));  
StkId firstArg = L->top - n;  
CallInfo *ci = L->ci;
if (L->status == LUA_OK) {  
if (!luaD_precall(L, firstArg - 1, LUA_MULTRET))  
luaV_execute(L);  
}
else {  
lua_assert(L->status == LUA_YIELD);
L->status = LUA_OK;  
ci->func = restorestack(L, ci->extra);
if (isLua(ci))  
luaV_execute(L);  
else {  
if (ci->u.c.k != NULL) {  
lua_unlock(L);
n = (*ci->u.c.k)(L, LUA_YIELD, ci->u.c.ctx); 
lua_lock(L);
api_checknelems(L, n);
firstArg = L->top - n;  
}
luaD_poscall(L, ci, firstArg, n);  
}
unroll(L, NULL);  
}
}
LUA_API int lua_resume (lua_State *L, lua_State *from, int nargs) {
int status;
unsigned short oldnny = L->nny;  
lua_lock(L);
if (L->status == LUA_OK) {  
if (L->ci != &L->base_ci)  
return resume_error(L, "cannot resume non-suspended coroutine", nargs);
}
else if (L->status != LUA_YIELD)
return resume_error(L, "cannot resume dead coroutine", nargs);
L->nCcalls = (from) ? from->nCcalls + 1 : 1;
if (L->nCcalls >= LUAI_MAXCCALLS)
return resume_error(L, "C stack overflow", nargs);
luai_userstateresume(L, nargs);
L->nny = 0;  
api_checknelems(L, (L->status == LUA_OK) ? nargs + 1 : nargs);
status = luaD_rawrunprotected(L, resume, &nargs);
if (status == -1)  
status = LUA_ERRRUN;
else {  
while (errorstatus(status) && recover(L, status)) {
status = luaD_rawrunprotected(L, unroll, &status);
}
if (errorstatus(status)) {  
L->status = cast_byte(status);  
seterrorobj(L, status, L->top);  
L->ci->top = L->top;
}
else lua_assert(status == L->status);  
}
L->nny = oldnny;  
L->nCcalls--;
lua_assert(L->nCcalls == ((from) ? from->nCcalls : 0));
lua_unlock(L);
return status;
}
LUA_API int lua_isyieldable (lua_State *L) {
return (L->nny == 0);
}
LUA_API int lua_yieldk (lua_State *L, int nresults, lua_KContext ctx,
lua_KFunction k) {
CallInfo *ci = L->ci;
luai_userstateyield(L, nresults);
lua_lock(L);
api_checknelems(L, nresults);
if (L->nny > 0) {
if (L != G(L)->mainthread)
luaG_runerror(L, "attempt to yield across a C-call boundary");
else
luaG_runerror(L, "attempt to yield from outside a coroutine");
}
L->status = LUA_YIELD;
ci->extra = savestack(L, ci->func);  
if (isLua(ci)) {  
api_check(L, k == NULL, "hooks cannot continue after yielding");
}
else {
if ((ci->u.c.k = k) != NULL)  
ci->u.c.ctx = ctx;  
ci->func = L->top - nresults - 1;  
luaD_throw(L, LUA_YIELD);
}
lua_assert(ci->callstatus & CIST_HOOKED);  
lua_unlock(L);
return 0;  
}
int luaD_pcall (lua_State *L, Pfunc func, void *u,
ptrdiff_t old_top, ptrdiff_t ef) {
int status;
CallInfo *old_ci = L->ci;
lu_byte old_allowhooks = L->allowhook;
unsigned short old_nny = L->nny;
ptrdiff_t old_errfunc = L->errfunc;
L->errfunc = ef;
status = luaD_rawrunprotected(L, func, u);
if (status != LUA_OK) {  
StkId oldtop = restorestack(L, old_top);
luaF_close(L, oldtop);  
seterrorobj(L, status, oldtop);
L->ci = old_ci;
L->allowhook = old_allowhooks;
L->nny = old_nny;
luaD_shrinkstack(L);
}
L->errfunc = old_errfunc;
return status;
}
struct SParser {  
ZIO *z;
Mbuffer buff;  
Dyndata dyd;  
const char *mode;
const char *name;
};
static void checkmode (lua_State *L, const char *mode, const char *x) {
if (mode && strchr(mode, x[0]) == NULL) {
luaO_pushfstring(L,
"attempt to load a %s chunk (mode is '%s')", x, mode);
luaD_throw(L, LUA_ERRSYNTAX);
}
}
static void f_parser (lua_State *L, void *ud) {
LClosure *cl;
struct SParser *p = cast(struct SParser *, ud);
int c = zgetc(p->z);  
if (c == LUA_SIGNATURE[0]) {
checkmode(L, p->mode, "binary");
cl = luaU_undump(L, p->z, p->name);
}
else {
checkmode(L, p->mode, "text");
cl = luaY_parser(L, p->z, &p->buff, &p->dyd, p->name, c);
}
lua_assert(cl->nupvalues == cl->p->sizeupvalues);
luaF_initupvals(L, cl);
}
int luaD_protectedparser (lua_State *L, ZIO *z, const char *name,
const char *mode) {
struct SParser p;
int status;
L->nny++;  
p.z = z; p.name = name; p.mode = mode;
p.dyd.actvar.arr = NULL; p.dyd.actvar.size = 0;
p.dyd.gt.arr = NULL; p.dyd.gt.size = 0;
p.dyd.label.arr = NULL; p.dyd.label.size = 0;
luaZ_initbuffer(L, &p.buff);
status = luaD_pcall(L, f_parser, &p, savestack(L, L->top), L->errfunc);
luaZ_freebuffer(L, &p.buff);
luaM_freearray(L, p.dyd.actvar.arr, p.dyd.actvar.size);
luaM_freearray(L, p.dyd.gt.arr, p.dyd.gt.size);
luaM_freearray(L, p.dyd.label.arr, p.dyd.label.size);
L->nny--;
return status;
}
#define ldump_c
#define LUA_CORE
#include <stddef.h>
typedef struct {
lua_State *L;
lua_Writer writer;
void *data;
int strip;
int status;
} DumpState;
#define DumpVector(v,n,D)	DumpBlock(v,(n)*sizeof((v)[0]),D)
#define DumpLiteral(s,D)	DumpBlock(s, sizeof(s) - sizeof(char), D)
static void DumpBlock (const void *b, size_t size, DumpState *D) {
if (D->status == 0 && size > 0) {
lua_unlock(D->L);
D->status = (*D->writer)(D->L, b, size, D->data);
lua_lock(D->L);
}
}
#define DumpVar(x,D)		DumpVector(&x,1,D)
static void DumpByte (int y, DumpState *D) {
lu_byte x = (lu_byte)y;
DumpVar(x, D);
}
static void DumpInt (int x, DumpState *D) {
DumpVar(x, D);
}
static void DumpNumber (lua_Number x, DumpState *D) {
DumpVar(x, D);
}
static void DumpInteger (lua_Integer x, DumpState *D) {
DumpVar(x, D);
}
static void DumpString (const TString *s, DumpState *D) {
if (s == NULL)
DumpByte(0, D);
else {
size_t size = tsslen(s) + 1;  
const char *str = getstr(s);
if (size < 0xFF)
DumpByte(cast_int(size), D);
else {
DumpByte(0xFF, D);
DumpVar(size, D);
}
DumpVector(str, size - 1, D);  
}
}
static void DumpCode (const Proto *f, DumpState *D) {
DumpInt(f->sizecode, D);
DumpVector(f->code, f->sizecode, D);
}
static void DumpFunction(const Proto *f, TString *psource, DumpState *D);
static void DumpConstants (const Proto *f, DumpState *D) {
int i;
int n = f->sizek;
DumpInt(n, D);
for (i = 0; i < n; i++) {
const TValue *o = &f->k[i];
DumpByte(ttype(o), D);
switch (ttype(o)) {
case LUA_TNIL:
break;
case LUA_TBOOLEAN:
DumpByte(bvalue(o), D);
break;
case LUA_TNUMFLT:
DumpNumber(fltvalue(o), D);
break;
case LUA_TNUMINT:
DumpInteger(ivalue(o), D);
break;
case LUA_TSHRSTR:
case LUA_TLNGSTR:
DumpString(tsvalue(o), D);
break;
default:
lua_assert(0);
}
}
}
static void DumpProtos (const Proto *f, DumpState *D) {
int i;
int n = f->sizep;
DumpInt(n, D);
for (i = 0; i < n; i++)
DumpFunction(f->p[i], f->source, D);
}
static void DumpUpvalues (const Proto *f, DumpState *D) {
int i, n = f->sizeupvalues;
DumpInt(n, D);
for (i = 0; i < n; i++) {
DumpByte(f->upvalues[i].instack, D);
DumpByte(f->upvalues[i].idx, D);
}
}
static void DumpDebug (const Proto *f, DumpState *D) {
int i, n;
n = (D->strip) ? 0 : f->sizelineinfo;
DumpInt(n, D);
DumpVector(f->lineinfo, n, D);
n = (D->strip) ? 0 : f->sizelocvars;
DumpInt(n, D);
for (i = 0; i < n; i++) {
DumpString(f->locvars[i].varname, D);
DumpInt(f->locvars[i].startpc, D);
DumpInt(f->locvars[i].endpc, D);
}
n = (D->strip) ? 0 : f->sizeupvalues;
DumpInt(n, D);
for (i = 0; i < n; i++)
DumpString(f->upvalues[i].name, D);
}
static void DumpFunction (const Proto *f, TString *psource, DumpState *D) {
if (D->strip || f->source == psource)
DumpString(NULL, D);  
else
DumpString(f->source, D);
DumpInt(f->linedefined, D);
DumpInt(f->lastlinedefined, D);
DumpByte(f->numparams, D);
DumpByte(f->is_vararg, D);
DumpByte(f->maxstacksize, D);
DumpCode(f, D);
DumpConstants(f, D);
DumpUpvalues(f, D);
DumpProtos(f, D);
DumpDebug(f, D);
}
static void DumpHeader (DumpState *D) {
DumpLiteral(LUA_SIGNATURE, D);
DumpByte(LUAC_VERSION, D);
DumpByte(LUAC_FORMAT, D);
DumpLiteral(LUAC_DATA, D);
DumpByte(sizeof(int), D);
DumpByte(sizeof(size_t), D);
DumpByte(sizeof(Instruction), D);
DumpByte(sizeof(lua_Integer), D);
DumpByte(sizeof(lua_Number), D);
DumpInteger(LUAC_INT, D);
DumpNumber(LUAC_NUM, D);
}
int luaU_dump(lua_State *L, const Proto *f, lua_Writer w, void *data,
int strip) {
DumpState D;
D.L = L;
D.writer = w;
D.data = data;
D.strip = strip;
D.status = 0;
DumpHeader(&D);
DumpByte(f->sizeupvalues, &D);
DumpFunction(f, NULL, &D);
return D.status;
}
#define lfunc_c
#define LUA_CORE
#include <stddef.h>
CClosure *luaF_newCclosure (lua_State *L, int n) {
GCObject *o = luaC_newobj(L, LUA_TCCL, sizeCclosure(n));
CClosure *c = gco2ccl(o);
c->nupvalues = cast_byte(n);
return c;
}
LClosure *luaF_newLclosure (lua_State *L, int n) {
GCObject *o = luaC_newobj(L, LUA_TLCL, sizeLclosure(n));
LClosure *c = gco2lcl(o);
c->p = NULL;
c->nupvalues = cast_byte(n);
while (n--) c->upvals[n] = NULL;
return c;
}
void luaF_initupvals (lua_State *L, LClosure *cl) {
int i;
for (i = 0; i < cl->nupvalues; i++) {
UpVal *uv = luaM_new(L, UpVal);
uv->refcount = 1;
uv->v = &uv->u.value;  
setnilvalue(uv->v);
cl->upvals[i] = uv;
}
}
UpVal *luaF_findupval (lua_State *L, StkId level) {
UpVal **pp = &L->openupval;
UpVal *p;
UpVal *uv;
lua_assert(isintwups(L) || L->openupval == NULL);
while (*pp != NULL && (p = *pp)->v >= level) {
lua_assert(upisopen(p));
if (p->v == level)  
return p;  
pp = &p->u.open.next;
}
uv = luaM_new(L, UpVal);
uv->refcount = 0;
uv->u.open.next = *pp;  
uv->u.open.touched = 1;
*pp = uv;
uv->v = level;  
if (!isintwups(L)) {  
L->twups = G(L)->twups;  
G(L)->twups = L;
}
return uv;
}
void luaF_close (lua_State *L, StkId level) {
UpVal *uv;
while (L->openupval != NULL && (uv = L->openupval)->v >= level) {
lua_assert(upisopen(uv));
L->openupval = uv->u.open.next;  
if (uv->refcount == 0)  
luaM_free(L, uv);  
else {
setobj(L, &uv->u.value, uv->v);  
uv->v = &uv->u.value;  
luaC_upvalbarrier(L, uv);
}
}
}
Proto *luaF_newproto (lua_State *L) {
GCObject *o = luaC_newobj(L, LUA_TPROTO, sizeof(Proto));
Proto *f = gco2p(o);
f->k = NULL;
f->sizek = 0;
f->p = NULL;
f->sizep = 0;
f->code = NULL;
f->cache = NULL;
f->sizecode = 0;
f->lineinfo = NULL;
f->sizelineinfo = 0;
f->upvalues = NULL;
f->sizeupvalues = 0;
f->numparams = 0;
f->is_vararg = 0;
f->maxstacksize = 0;
f->locvars = NULL;
f->sizelocvars = 0;
f->linedefined = 0;
f->lastlinedefined = 0;
f->source = NULL;
return f;
}
void luaF_freeproto (lua_State *L, Proto *f) {
luaM_freearray(L, f->code, f->sizecode);
luaM_freearray(L, f->p, f->sizep);
luaM_freearray(L, f->k, f->sizek);
luaM_freearray(L, f->lineinfo, f->sizelineinfo);
luaM_freearray(L, f->locvars, f->sizelocvars);
luaM_freearray(L, f->upvalues, f->sizeupvalues);
luaM_free(L, f);
}
const char *luaF_getlocalname (const Proto *f, int local_number, int pc) {
int i;
for (i = 0; i<f->sizelocvars && f->locvars[i].startpc <= pc; i++) {
if (pc < f->locvars[i].endpc) {  
local_number--;
if (local_number == 0)
return getstr(f->locvars[i].varname);
}
}
return NULL;  
}
#define lgc_c
#define LUA_CORE
#include <string.h>
#define GCSinsideatomic		(GCSpause + 1)
#define GCSWEEPCOST	((sizeof(TString) + 4) / 4)
#define GCSWEEPMAX	(cast_int((GCSTEPSIZE / GCSWEEPCOST) / 4))
#define GCFINALIZECOST	GCSWEEPCOST
#define STEPMULADJ		200
#define PAUSEADJ		100
#define maskcolors	(~(bitmask(BLACKBIT) | WHITEBITS))
#define makewhite(g,x)	\
(x->marked = cast_byte((x->marked & maskcolors) | luaC_white(g)))
#define white2gray(x)	resetbits(x->marked, WHITEBITS)
#define black2gray(x)	resetbit(x->marked, BLACKBIT)
#define valiswhite(x)   (iscollectable(x) && iswhite(gcvalue(x)))
#define checkdeadkey(n)	lua_assert(!ttisdeadkey(gkey(n)) || ttisnil(gval(n)))
#define checkconsistency(obj)  \
lua_longassert(!iscollectable(obj) || righttt(obj))
#define markvalue(g,o) { checkconsistency(o); \
if (valiswhite(o)) reallymarkobject(g,gcvalue(o)); }
#define markobject(g,t)	{ if (iswhite(t)) reallymarkobject(g, obj2gco(t)); }
#define markobjectN(g,t)	{ if (t) markobject(g,t); }
static void reallymarkobject (global_State *g, GCObject *o);
#define gnodelast(h)	gnode(h, cast(size_t, sizenode(h)))
#define linkgclist(o,p)	((o)->gclist = (p), (p) = obj2gco(o))
static void removeentry (Node *n) {
lua_assert(ttisnil(gval(n)));
if (valiswhite(gkey(n)))
setdeadvalue(wgkey(n));  
}
static int iscleared (global_State *g, const TValue *o) {
if (!iscollectable(o)) return 0;
else if (ttisstring(o)) {
markobject(g, tsvalue(o));  
return 0;
}
else return iswhite(gcvalue(o));
}
void luaC_barrier_ (lua_State *L, GCObject *o, GCObject *v) {
global_State *g = G(L);
lua_assert(isblack(o) && iswhite(v) && !isdead(g, v) && !isdead(g, o));
if (keepinvariant(g))  
reallymarkobject(g, v);  
else {  
lua_assert(issweepphase(g));
makewhite(g, o);  
}
}
void luaC_barrierback_ (lua_State *L, Table *t) {
global_State *g = G(L);
lua_assert(isblack(t) && !isdead(g, t));
black2gray(t);  
linkgclist(t, g->grayagain);
}
void luaC_upvalbarrier_ (lua_State *L, UpVal *uv) {
global_State *g = G(L);
GCObject *o = gcvalue(uv->v);
lua_assert(!upisopen(uv));  
if (keepinvariant(g))
markobject(g, o);
}
void luaC_fix (lua_State *L, GCObject *o) {
global_State *g = G(L);
lua_assert(g->allgc == o);  
white2gray(o);  
g->allgc = o->next;  
o->next = g->fixedgc;  
g->fixedgc = o;
}
GCObject *luaC_newobj (lua_State *L, int tt, size_t sz) {
global_State *g = G(L);
GCObject *o = cast(GCObject *, luaM_newobject(L, novariant(tt), sz));
o->marked = luaC_white(g);
o->tt = tt;
o->next = g->allgc;
g->allgc = o;
return o;
}
static void reallymarkobject (global_State *g, GCObject *o) {
reentry:
white2gray(o);
switch (o->tt) {
case LUA_TSHRSTR: {
gray2black(o);
g->GCmemtrav += sizelstring(gco2ts(o)->shrlen);
break;
}
case LUA_TLNGSTR: {
gray2black(o);
g->GCmemtrav += sizelstring(gco2ts(o)->u.lnglen);
break;
}
case LUA_TUSERDATA: {
TValue uvalue;
markobjectN(g, gco2u(o)->metatable);  
gray2black(o);
g->GCmemtrav += sizeudata(gco2u(o));
getuservalue(g->mainthread, gco2u(o), &uvalue);
if (valiswhite(&uvalue)) {  
o = gcvalue(&uvalue);
goto reentry;
}
break;
}
case LUA_TLCL: {
linkgclist(gco2lcl(o), g->gray);
break;
}
case LUA_TCCL: {
linkgclist(gco2ccl(o), g->gray);
break;
}
case LUA_TTABLE: {
linkgclist(gco2t(o), g->gray);
break;
}
case LUA_TTHREAD: {
linkgclist(gco2th(o), g->gray);
break;
}
case LUA_TPROTO: {
linkgclist(gco2p(o), g->gray);
break;
}
default: lua_assert(0); break;
}
}
static void markmt (global_State *g) {
int i;
for (i=0; i < LUA_NUMTAGS; i++)
markobjectN(g, g->mt[i]);
}
static void markbeingfnz (global_State *g) {
GCObject *o;
for (o = g->tobefnz; o != NULL; o = o->next)
markobject(g, o);
}
static void remarkupvals (global_State *g) {
lua_State *thread;
lua_State **p = &g->twups;
while ((thread = *p) != NULL) {
lua_assert(!isblack(thread));  
if (isgray(thread) && thread->openupval != NULL)
p = &thread->twups;  
else {  
UpVal *uv;
*p = thread->twups;  
thread->twups = thread;  
for (uv = thread->openupval; uv != NULL; uv = uv->u.open.next) {
if (uv->u.open.touched) {
markvalue(g, uv->v);  
uv->u.open.touched = 0;
}
}
}
}
}
static void restartcollection (global_State *g) {
g->gray = g->grayagain = NULL;
g->weak = g->allweak = g->ephemeron = NULL;
markobject(g, g->mainthread);
markvalue(g, &g->l_registry);
markmt(g);
markbeingfnz(g);  
}
static void traverseweakvalue (global_State *g, Table *h) {
Node *n, *limit = gnodelast(h);
/* if there is array part, assume it may have white values (it is not
worth traversing it now just to check) */
int hasclears = (h->sizearray > 0);
for (n = gnode(h, 0); n < limit; n++) {  
checkdeadkey(n);
if (ttisnil(gval(n)))  
removeentry(n);  
else {
lua_assert(!ttisnil(gkey(n)));
markvalue(g, gkey(n));  
if (!hasclears && iscleared(g, gval(n)))  
hasclears = 1;  
}
}
if (g->gcstate == GCSpropagate)
linkgclist(h, g->grayagain);  
else if (hasclears)
linkgclist(h, g->weak);  
}
static int traverseephemeron (global_State *g, Table *h) {
int marked = 0;  
int hasclears = 0;  
int hasww = 0;  
Node *n, *limit = gnodelast(h);
unsigned int i;
for (i = 0; i < h->sizearray; i++) {
if (valiswhite(&h->array[i])) {
marked = 1;
reallymarkobject(g, gcvalue(&h->array[i]));
}
}
for (n = gnode(h, 0); n < limit; n++) {
checkdeadkey(n);
if (ttisnil(gval(n)))  
removeentry(n);  
else if (iscleared(g, gkey(n))) {  
hasclears = 1;  
if (valiswhite(gval(n)))  
hasww = 1;  
}
else if (valiswhite(gval(n))) {  
marked = 1;
reallymarkobject(g, gcvalue(gval(n)));  
}
}
if (g->gcstate == GCSpropagate)
linkgclist(h, g->grayagain);  
else if (hasww)  
linkgclist(h, g->ephemeron);  
else if (hasclears)  
linkgclist(h, g->allweak);  
return marked;
}
static void traversestrongtable (global_State *g, Table *h) {
Node *n, *limit = gnodelast(h);
unsigned int i;
for (i = 0; i < h->sizearray; i++)  
markvalue(g, &h->array[i]);
for (n = gnode(h, 0); n < limit; n++) {  
checkdeadkey(n);
if (ttisnil(gval(n)))  
removeentry(n);  
else {
lua_assert(!ttisnil(gkey(n)));
markvalue(g, gkey(n));  
markvalue(g, gval(n));  
}
}
}
static lu_mem traversetable (global_State *g, Table *h) {
const char *weakkey, *weakvalue;
const TValue *mode = gfasttm(g, h->metatable, TM_MODE);
markobjectN(g, h->metatable);
if (mode && ttisstring(mode) &&  
((weakkey = strchr(svalue(mode), 'k')),
(weakvalue = strchr(svalue(mode), 'v')),
(weakkey || weakvalue))) {  
black2gray(h);  
if (!weakkey)  
traverseweakvalue(g, h);
else if (!weakvalue)  
traverseephemeron(g, h);
else  
linkgclist(h, g->allweak);  
}
else  
traversestrongtable(g, h);
return sizeof(Table) + sizeof(TValue) * h->sizearray +
sizeof(Node) * cast(size_t, allocsizenode(h));
}
static int traverseproto (global_State *g, Proto *f) {
int i;
if (f->cache && iswhite(f->cache))
f->cache = NULL;  
markobjectN(g, f->source);
for (i = 0; i < f->sizek; i++)  
markvalue(g, &f->k[i]);
for (i = 0; i < f->sizeupvalues; i++)  
markobjectN(g, f->upvalues[i].name);
for (i = 0; i < f->sizep; i++)  
markobjectN(g, f->p[i]);
for (i = 0; i < f->sizelocvars; i++)  
markobjectN(g, f->locvars[i].varname);
return sizeof(Proto) + sizeof(Instruction) * f->sizecode +
sizeof(Proto *) * f->sizep +
sizeof(TValue) * f->sizek +
sizeof(int) * f->sizelineinfo +
sizeof(LocVar) * f->sizelocvars +
sizeof(Upvaldesc) * f->sizeupvalues;
}
static lu_mem traverseCclosure (global_State *g, CClosure *cl) {
int i;
for (i = 0; i < cl->nupvalues; i++)  
markvalue(g, &cl->upvalue[i]);
return sizeCclosure(cl->nupvalues);
}
static lu_mem traverseLclosure (global_State *g, LClosure *cl) {
int i;
markobjectN(g, cl->p);  
for (i = 0; i < cl->nupvalues; i++) {  
UpVal *uv = cl->upvals[i];
if (uv != NULL) {
if (upisopen(uv) && g->gcstate != GCSinsideatomic)
uv->u.open.touched = 1;  
else
markvalue(g, uv->v);
}
}
return sizeLclosure(cl->nupvalues);
}
static lu_mem traversethread (global_State *g, lua_State *th) {
StkId o = th->stack;
if (o == NULL)
return 1;  
lua_assert(g->gcstate == GCSinsideatomic ||
th->openupval == NULL || isintwups(th));
for (; o < th->top; o++)  
markvalue(g, o);
if (g->gcstate == GCSinsideatomic) {  
StkId lim = th->stack + th->stacksize;  
for (; o < lim; o++)  
setnilvalue(o);
if (!isintwups(th) && th->openupval != NULL) {
th->twups = g->twups;  
g->twups = th;
}
}
else if (g->gckind != KGC_EMERGENCY)
luaD_shrinkstack(th); 
return (sizeof(lua_State) + sizeof(TValue) * th->stacksize +
sizeof(CallInfo) * th->nci);
}
static void propagatemark (global_State *g) {
lu_mem size;
GCObject *o = g->gray;
lua_assert(isgray(o));
gray2black(o);
switch (o->tt) {
case LUA_TTABLE: {
Table *h = gco2t(o);
g->gray = h->gclist;  
size = traversetable(g, h);
break;
}
case LUA_TLCL: {
LClosure *cl = gco2lcl(o);
g->gray = cl->gclist;  
size = traverseLclosure(g, cl);
break;
}
case LUA_TCCL: {
CClosure *cl = gco2ccl(o);
g->gray = cl->gclist;  
size = traverseCclosure(g, cl);
break;
}
case LUA_TTHREAD: {
lua_State *th = gco2th(o);
g->gray = th->gclist;  
linkgclist(th, g->grayagain);  
black2gray(o);
size = traversethread(g, th);
break;
}
case LUA_TPROTO: {
Proto *p = gco2p(o);
g->gray = p->gclist;  
size = traverseproto(g, p);
break;
}
default: lua_assert(0); return;
}
g->GCmemtrav += size;
}
static void propagateall (global_State *g) {
while (g->gray) propagatemark(g);
}
static void convergeephemerons (global_State *g) {
int changed;
do {
GCObject *w;
GCObject *next = g->ephemeron;  
g->ephemeron = NULL;  
changed = 0;
while ((w = next) != NULL) {
next = gco2t(w)->gclist;
if (traverseephemeron(g, gco2t(w))) {  
propagateall(g);  
changed = 1;  
}
}
} while (changed);
}
static void clearkeys (global_State *g, GCObject *l, GCObject *f) {
for (; l != f; l = gco2t(l)->gclist) {
Table *h = gco2t(l);
Node *n, *limit = gnodelast(h);
for (n = gnode(h, 0); n < limit; n++) {
if (!ttisnil(gval(n)) && (iscleared(g, gkey(n)))) {
setnilvalue(gval(n));  
}
if (ttisnil(gval(n)))  
removeentry(n);  
}
}
}
static void clearvalues (global_State *g, GCObject *l, GCObject *f) {
for (; l != f; l = gco2t(l)->gclist) {
Table *h = gco2t(l);
Node *n, *limit = gnodelast(h);
unsigned int i;
for (i = 0; i < h->sizearray; i++) {
TValue *o = &h->array[i];
if (iscleared(g, o))  
setnilvalue(o);  
}
for (n = gnode(h, 0); n < limit; n++) {
if (!ttisnil(gval(n)) && iscleared(g, gval(n))) {
setnilvalue(gval(n));  
removeentry(n);  
}
}
}
}
void luaC_upvdeccount (lua_State *L, UpVal *uv) {
lua_assert(uv->refcount > 0);
uv->refcount--;
if (uv->refcount == 0 && !upisopen(uv))
luaM_free(L, uv);
}
static void freeLclosure (lua_State *L, LClosure *cl) {
int i;
for (i = 0; i < cl->nupvalues; i++) {
UpVal *uv = cl->upvals[i];
if (uv)
luaC_upvdeccount(L, uv);
}
luaM_freemem(L, cl, sizeLclosure(cl->nupvalues));
}
static void freeobj (lua_State *L, GCObject *o) {
switch (o->tt) {
case LUA_TPROTO: luaF_freeproto(L, gco2p(o)); break;
case LUA_TLCL: {
freeLclosure(L, gco2lcl(o));
break;
}
case LUA_TCCL: {
luaM_freemem(L, o, sizeCclosure(gco2ccl(o)->nupvalues));
break;
}
case LUA_TTABLE: luaH_free(L, gco2t(o)); break;
case LUA_TTHREAD: luaE_freethread(L, gco2th(o)); break;
case LUA_TUSERDATA: luaM_freemem(L, o, sizeudata(gco2u(o))); break;
case LUA_TSHRSTR:
luaS_remove(L, gco2ts(o));  
luaM_freemem(L, o, sizelstring(gco2ts(o)->shrlen));
break;
case LUA_TLNGSTR: {
luaM_freemem(L, o, sizelstring(gco2ts(o)->u.lnglen));
break;
}
default: lua_assert(0);
}
}
#define sweepwholelist(L,p)	sweeplist(L,p,MAX_LUMEM)
static GCObject **sweeplist (lua_State *L, GCObject **p, lu_mem count);
static GCObject **sweeplist (lua_State *L, GCObject **p, lu_mem count) {
global_State *g = G(L);
int ow = otherwhite(g);
int white = luaC_white(g);  
while (*p != NULL && count-- > 0) {
GCObject *curr = *p;
int marked = curr->marked;
if (isdeadm(ow, marked)) {  
*p = curr->next;  
freeobj(L, curr);  
}
else {  
curr->marked = cast_byte((marked & maskcolors) | white);
p = &curr->next;  
}
}
return (*p == NULL) ? NULL : p;
}
static GCObject **sweeptolive (lua_State *L, GCObject **p) {
GCObject **old = p;
do {
p = sweeplist(L, p, 1);
} while (p == old);
return p;
}
static void checkSizes (lua_State *L, global_State *g) {
if (g->gckind != KGC_EMERGENCY) {
l_mem olddebt = g->GCdebt;
if (g->strt.nuse < g->strt.size / 4)  
luaS_resize(L, g->strt.size / 2);  
g->GCestimate += g->GCdebt - olddebt;  
}
}
static GCObject *udata2finalize (global_State *g) {
GCObject *o = g->tobefnz;  
lua_assert(tofinalize(o));
g->tobefnz = o->next;  
o->next = g->allgc;  
g->allgc = o;
resetbit(o->marked, FINALIZEDBIT);  
if (issweepphase(g))
makewhite(g, o);  
return o;
}
static void dothecall (lua_State *L, void *ud) {
UNUSED(ud);
luaD_callnoyield(L, L->top - 2, 0);
}
static void GCTM (lua_State *L, int propagateerrors) {
global_State *g = G(L);
const TValue *tm;
TValue v;
setgcovalue(L, &v, udata2finalize(g));
tm = luaT_gettmbyobj(L, &v, TM_GC);
if (tm != NULL && ttisfunction(tm)) {  
int status;
lu_byte oldah = L->allowhook;
int running  = g->gcrunning;
L->allowhook = 0;  
g->gcrunning = 0;  
setobj2s(L, L->top, tm);  
setobj2s(L, L->top + 1, &v);  
L->top += 2;  
L->ci->callstatus |= CIST_FIN;  
status = luaD_pcall(L, dothecall, NULL, savestack(L, L->top - 2), 0);
L->ci->callstatus &= ~CIST_FIN;  
L->allowhook = oldah;  
g->gcrunning = running;  
if (status != LUA_OK && propagateerrors) {  
if (status == LUA_ERRRUN) {  
const char *msg = (ttisstring(L->top - 1))
? svalue(L->top - 1)
: "no message";
luaO_pushfstring(L, "error in __gc metamethod (%s)", msg);
status = LUA_ERRGCMM;  
}
luaD_throw(L, status);  
}
}
}
static int runafewfinalizers (lua_State *L) {
global_State *g = G(L);
unsigned int i;
lua_assert(!g->tobefnz || g->gcfinnum > 0);
for (i = 0; g->tobefnz && i < g->gcfinnum; i++)
GCTM(L, 1);  
g->gcfinnum = (!g->tobefnz) ? 0  
: g->gcfinnum * 2;  
return i;
}
static void callallpendingfinalizers (lua_State *L) {
global_State *g = G(L);
while (g->tobefnz)
GCTM(L, 0);
}
static GCObject **findlast (GCObject **p) {
while (*p != NULL)
p = &(*p)->next;
return p;
}
static void separatetobefnz (global_State *g, int all) {
GCObject *curr;
GCObject **p = &g->finobj;
GCObject **lastnext = findlast(&g->tobefnz);
while ((curr = *p) != NULL) {  
lua_assert(tofinalize(curr));
if (!(iswhite(curr) || all))  
p = &curr->next;  
else {
*p = curr->next;  
curr->next = *lastnext;  
*lastnext = curr;
lastnext = &curr->next;
}
}
}
void luaC_checkfinalizer (lua_State *L, GCObject *o, Table *mt) {
global_State *g = G(L);
if (tofinalize(o) ||                 
gfasttm(g, mt, TM_GC) == NULL)   
return;  
else {  
GCObject **p;
if (issweepphase(g)) {
makewhite(g, o);  
if (g->sweepgc == &o->next)  
g->sweepgc = sweeptolive(L, g->sweepgc);  
}
for (p = &g->allgc; *p != o; p = &(*p)->next) {  }
*p = o->next;  
o->next = g->finobj;  
g->finobj = o;
l_setbit(o->marked, FINALIZEDBIT);  
}
}
static void setpause (global_State *g) {
l_mem threshold, debt;
l_mem estimate = g->GCestimate / PAUSEADJ;  
lua_assert(estimate > 0);
threshold = (g->gcpause < MAX_LMEM / estimate)  
? estimate * g->gcpause  
: MAX_LMEM;  
debt = gettotalbytes(g) - threshold;
luaE_setdebt(g, debt);
}
static void entersweep (lua_State *L) {
global_State *g = G(L);
g->gcstate = GCSswpallgc;
lua_assert(g->sweepgc == NULL);
g->sweepgc = sweeplist(L, &g->allgc, 1);
}
void luaC_freeallobjects (lua_State *L) {
global_State *g = G(L);
separatetobefnz(g, 1);  
lua_assert(g->finobj == NULL);
callallpendingfinalizers(L);
lua_assert(g->tobefnz == NULL);
g->currentwhite = WHITEBITS; 
g->gckind = KGC_NORMAL;
sweepwholelist(L, &g->finobj);
sweepwholelist(L, &g->allgc);
sweepwholelist(L, &g->fixedgc);  
lua_assert(g->strt.nuse == 0);
}
static l_mem atomic (lua_State *L) {
global_State *g = G(L);
l_mem work;
GCObject *origweak, *origall;
GCObject *grayagain = g->grayagain;  
lua_assert(g->ephemeron == NULL && g->weak == NULL);
lua_assert(!iswhite(g->mainthread));
g->gcstate = GCSinsideatomic;
g->GCmemtrav = 0;  
markobject(g, L);  
markvalue(g, &g->l_registry);
markmt(g);  
remarkupvals(g);
propagateall(g);  
work = g->GCmemtrav;  
g->gray = grayagain;
propagateall(g);  
g->GCmemtrav = 0;  
convergeephemerons(g);
clearvalues(g, g->weak, NULL);
clearvalues(g, g->allweak, NULL);
origweak = g->weak; origall = g->allweak;
work += g->GCmemtrav;  
separatetobefnz(g, 0);  
g->gcfinnum = 1;  
markbeingfnz(g);  
propagateall(g);  
g->GCmemtrav = 0;  
convergeephemerons(g);
clearkeys(g, g->ephemeron, NULL);  
clearkeys(g, g->allweak, NULL);  
clearvalues(g, g->weak, origweak);
clearvalues(g, g->allweak, origall);
luaS_clearcache(g);
g->currentwhite = cast_byte(otherwhite(g));  
work += g->GCmemtrav;  
return work;  
}
static lu_mem sweepstep (lua_State *L, global_State *g,
int nextstate, GCObject **nextlist) {
if (g->sweepgc) {
l_mem olddebt = g->GCdebt;
g->sweepgc = sweeplist(L, g->sweepgc, GCSWEEPMAX);
g->GCestimate += g->GCdebt - olddebt;  
if (g->sweepgc)  
return (GCSWEEPMAX * GCSWEEPCOST);
}
g->gcstate = nextstate;
g->sweepgc = nextlist;
return 0;
}
static lu_mem singlestep (lua_State *L) {
global_State *g = G(L);
switch (g->gcstate) {
case GCSpause: {
g->GCmemtrav = g->strt.size * sizeof(GCObject*);
restartcollection(g);
g->gcstate = GCSpropagate;
return g->GCmemtrav;
}
case GCSpropagate: {
g->GCmemtrav = 0;
lua_assert(g->gray);
propagatemark(g);
if (g->gray == NULL)  
g->gcstate = GCSatomic;  
return g->GCmemtrav;  
}
case GCSatomic: {
lu_mem work;
propagateall(g);  
work = atomic(L);  
entersweep(L);
g->GCestimate = gettotalbytes(g);  ;
return work;
}
case GCSswpallgc: {  
return sweepstep(L, g, GCSswpfinobj, &g->finobj);
}
case GCSswpfinobj: {  
return sweepstep(L, g, GCSswptobefnz, &g->tobefnz);
}
case GCSswptobefnz: {  
return sweepstep(L, g, GCSswpend, NULL);
}
case GCSswpend: {  
makewhite(g, g->mainthread);  
checkSizes(L, g);
g->gcstate = GCScallfin;
return 0;
}
case GCScallfin: {  
if (g->tobefnz && g->gckind != KGC_EMERGENCY) {
int n = runafewfinalizers(L);
return (n * GCFINALIZECOST);
}
else {  
g->gcstate = GCSpause;  
return 0;
}
}
default: lua_assert(0); return 0;
}
}
void luaC_runtilstate (lua_State *L, int statesmask) {
global_State *g = G(L);
while (!testbit(statesmask, g->gcstate))
singlestep(L);
}
static l_mem getdebt (global_State *g) {
l_mem debt = g->GCdebt;
int stepmul = g->gcstepmul;
if (debt <= 0) return 0;  
else {
debt = (debt / STEPMULADJ) + 1;
debt = (debt < MAX_LMEM / stepmul) ? debt * stepmul : MAX_LMEM;
return debt;
}
}
void luaC_step (lua_State *L) {
global_State *g = G(L);
l_mem debt = getdebt(g);  
if (!g->gcrunning) {  
luaE_setdebt(g, -GCSTEPSIZE * 10);  
return;
}
do {  
lu_mem work = singlestep(L);  
debt -= work;
} while (debt > -GCSTEPSIZE && g->gcstate != GCSpause);
if (g->gcstate == GCSpause)
setpause(g);  
else {
debt = (debt / g->gcstepmul) * STEPMULADJ;  
luaE_setdebt(g, debt);
runafewfinalizers(L);
}
}
void luaC_fullgc (lua_State *L, int isemergency) {
global_State *g = G(L);
lua_assert(g->gckind == KGC_NORMAL);
if (isemergency) g->gckind = KGC_EMERGENCY;  
if (keepinvariant(g)) {  
entersweep(L); 
}
luaC_runtilstate(L, bitmask(GCSpause));
luaC_runtilstate(L, ~bitmask(GCSpause));  
luaC_runtilstate(L, bitmask(GCScallfin));  
lua_assert(g->GCestimate == gettotalbytes(g));
luaC_runtilstate(L, bitmask(GCSpause));  
g->gckind = KGC_NORMAL;
setpause(g);
}
#define linit_c
#define LUA_LIB
#include <stddef.h>
static const luaL_Reg loadedlibs[] = {
{"_G", luaopen_base},
{LUA_LOADLIBNAME, luaopen_package},
{LUA_COLIBNAME, luaopen_coroutine},
{LUA_TABLIBNAME, luaopen_table},
{LUA_IOLIBNAME, luaopen_io},
{LUA_OSLIBNAME, luaopen_os},
{LUA_STRLIBNAME, luaopen_string},
{LUA_MATHLIBNAME, luaopen_math},
{LUA_UTF8LIBNAME, luaopen_utf8},
{LUA_DBLIBNAME, luaopen_debug},
#if defined(LUA_COMPAT_BITLIB)
{LUA_BITLIBNAME, luaopen_bit32},
#endif
{NULL, NULL}
};
LUALIB_API void luaL_openlibs (lua_State *L) {
const luaL_Reg *lib;
for (lib = loadedlibs; lib->func; lib++) {
luaL_requiref(L, lib->name, lib->func, 1);
lua_pop(L, 1);  
}
}
#define liolib_c
#define LUA_LIB
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(l_checkmode)
#if !defined(L_MODEEXT)
#define L_MODEEXT	"b"
#endif
static int l_checkmode (const char *mode) {
return (*mode != '\0' && strchr("rwa", *(mode++)) != NULL &&
(*mode != '+' || (++mode, 1)) &&  
(strspn(mode, L_MODEEXT) == strlen(mode)));  
}
#endif
#if !defined(l_popen)		
#if defined(LUA_USE_POSIX)	
#define l_popen(L,c,m)		(fflush(NULL), popen(c,m))
#define l_pclose(L,file)	(pclose(file))
#elif defined(LUA_USE_WINDOWS)	
#define l_popen(L,c,m)		(_popen(c,m))
#define l_pclose(L,file)	(_pclose(file))
#else				
#define l_popen(L,c,m)  \
((void)((void)c, m), \
luaL_error(L, "'popen' not supported"), \
(FILE*)0)
#define l_pclose(L,file)		((void)L, (void)file, -1)
#endif				
#endif				
#if !defined(l_getc)		
#if defined(LUA_USE_POSIX)
#define l_getc(f)		getc_unlocked(f)
#define l_lockfile(f)		flockfile(f)
#define l_unlockfile(f)		funlockfile(f)
#else
#define l_getc(f)		getc(f)
#define l_lockfile(f)		((void)0)
#define l_unlockfile(f)		((void)0)
#endif
#endif				
#if !defined(l_fseek)		
#if defined(LUA_USE_POSIX)	
#include <sys/types.h>
#define l_fseek(f,o,w)		fseeko(f,o,w)
#define l_ftell(f)		ftello(f)
#define l_seeknum		off_t
#elif defined(LUA_USE_WINDOWS) && !defined(_CRTIMP_TYPEINFO) \
&& defined(_MSC_VER) && (_MSC_VER >= 1400)	
#define l_fseek(f,o,w)		_fseeki64(f,o,w)
#define l_ftell(f)		_ftelli64(f)
#define l_seeknum		__int64
#else				
#define l_fseek(f,o,w)		fseek(f,o,w)
#define l_ftell(f)		ftell(f)
#define l_seeknum		long
#endif				
#endif				
#define IO_PREFIX	"_IO_"
#define IOPREF_LEN	(sizeof(IO_PREFIX)/sizeof(char) - 1)
#define IO_INPUT	(IO_PREFIX "input")
#define IO_OUTPUT	(IO_PREFIX "output")
typedef luaL_Stream LStream;
#define tolstream(L)	((LStream *)luaL_checkudata(L, 1, LUA_FILEHANDLE))
#define isclosed(p)	((p)->closef == NULL)
static int io_type (lua_State *L) {
LStream *p;
luaL_checkany(L, 1);
p = (LStream *)luaL_testudata(L, 1, LUA_FILEHANDLE);
if (p == NULL)
lua_pushnil(L);  
else if (isclosed(p))
lua_pushliteral(L, "closed file");
else
lua_pushliteral(L, "file");
return 1;
}
static int f_tostring (lua_State *L) {
LStream *p = tolstream(L);
if (isclosed(p))
lua_pushliteral(L, "file (closed)");
else
lua_pushfstring(L, "file (%p)", p->f);
return 1;
}
static FILE *tofile (lua_State *L) {
LStream *p = tolstream(L);
if (isclosed(p))
luaL_error(L, "attempt to use a closed file");
lua_assert(p->f);
return p->f;
}
static LStream *newprefile (lua_State *L) {
LStream *p = (LStream *)lua_newuserdata(L, sizeof(LStream));
p->closef = NULL;  
luaL_setmetatable(L, LUA_FILEHANDLE);
return p;
}
static int aux_close (lua_State *L) {
LStream *p = tolstream(L);
volatile lua_CFunction cf = p->closef;
p->closef = NULL;  
return (*cf)(L);  
}
static int f_close (lua_State *L) {
tofile(L);  
return aux_close(L);
}
static int io_close (lua_State *L) {
if (lua_isnone(L, 1))  
lua_getfield(L, LUA_REGISTRYINDEX, IO_OUTPUT);  
return f_close(L);
}
static int f_gc (lua_State *L) {
LStream *p = tolstream(L);
if (!isclosed(p) && p->f != NULL)
aux_close(L);  
return 0;
}
static int io_fclose (lua_State *L) {
LStream *p = tolstream(L);
int res = fclose(p->f);
return luaL_fileresult(L, (res == 0), NULL);
}
static LStream *newfile (lua_State *L) {
LStream *p = newprefile(L);
p->f = NULL;
p->closef = &io_fclose;
return p;
}
static void opencheck (lua_State *L, const char *fname, const char *mode) {
LStream *p = newfile(L);
p->f = fopen(fname, mode);
if (p->f == NULL)
luaL_error(L, "cannot open file '%s' (%s)", fname, strerror(errno));
}
static int io_open (lua_State *L) {
const char *filename = luaL_checkstring(L, 1);
const char *mode = luaL_optstring(L, 2, "r");
LStream *p = newfile(L);
const char *md = mode;  
luaL_argcheck(L, l_checkmode(md), 2, "invalid mode");
p->f = fopen(filename, mode);
return (p->f == NULL) ? luaL_fileresult(L, 0, filename) : 1;
}
static int io_pclose (lua_State *L) {
LStream *p = tolstream(L);
return luaL_execresult(L, l_pclose(L, p->f));
}
static int io_popen (lua_State *L) {
const char *filename = luaL_checkstring(L, 1);
const char *mode = luaL_optstring(L, 2, "r");
LStream *p = newprefile(L);
p->f = l_popen(L, filename, mode);
p->closef = &io_pclose;
return (p->f == NULL) ? luaL_fileresult(L, 0, filename) : 1;
}
static int io_tmpfile (lua_State *L) {
LStream *p = newfile(L);
p->f = tmpfile();
return (p->f == NULL) ? luaL_fileresult(L, 0, NULL) : 1;
}
static FILE *getiofile (lua_State *L, const char *findex) {
LStream *p;
lua_getfield(L, LUA_REGISTRYINDEX, findex);
p = (LStream *)lua_touserdata(L, -1);
if (isclosed(p))
luaL_error(L, "standard %s file is closed", findex + IOPREF_LEN);
return p->f;
}
static int g_iofile (lua_State *L, const char *f, const char *mode) {
if (!lua_isnoneornil(L, 1)) {
const char *filename = lua_tostring(L, 1);
if (filename)
opencheck(L, filename, mode);
else {
tofile(L);  
lua_pushvalue(L, 1);
}
lua_setfield(L, LUA_REGISTRYINDEX, f);
}
lua_getfield(L, LUA_REGISTRYINDEX, f);
return 1;
}
static int io_input (lua_State *L) {
return g_iofile(L, IO_INPUT, "r");
}
static int io_output (lua_State *L) {
return g_iofile(L, IO_OUTPUT, "w");
}
static int io_readline (lua_State *L);
#define MAXARGLINE	250
static void aux_lines (lua_State *L, int toclose) {
int n = lua_gettop(L) - 1;  
luaL_argcheck(L, n <= MAXARGLINE, MAXARGLINE + 2, "too many arguments");
lua_pushinteger(L, n);  
lua_pushboolean(L, toclose);  
lua_rotate(L, 2, 2);  
lua_pushcclosure(L, io_readline, 3 + n);
}
static int f_lines (lua_State *L) {
tofile(L);  
aux_lines(L, 0);
return 1;
}
static int io_lines (lua_State *L) {
int toclose;
if (lua_isnone(L, 1)) lua_pushnil(L);  
if (lua_isnil(L, 1)) {  
lua_getfield(L, LUA_REGISTRYINDEX, IO_INPUT);  
lua_replace(L, 1);  
tofile(L);  
toclose = 0;  
}
else {  
const char *filename = luaL_checkstring(L, 1);
opencheck(L, filename, "r");
lua_replace(L, 1);  
toclose = 1;  
}
aux_lines(L, toclose);
return 1;
}
#if !defined (L_MAXLENNUM)
#define L_MAXLENNUM     200
#endif
typedef struct {
FILE *f;  
int c;  
int n;  
char buff[L_MAXLENNUM + 1];  
} RN;
static int nextc (RN *rn) {
if (rn->n >= L_MAXLENNUM) {  
rn->buff[0] = '\0';  
return 0;  
}
else {
rn->buff[rn->n++] = rn->c;  
rn->c = l_getc(rn->f);  
return 1;
}
}
static int test2 (RN *rn, const char *set) {
if (rn->c == set[0] || rn->c == set[1])
return nextc(rn);
else return 0;
}
static int readdigits (RN *rn, int hex) {
int count = 0;
while ((hex ? isxdigit(rn->c) : isdigit(rn->c)) && nextc(rn))
count++;
return count;
}
static int read_number (lua_State *L, FILE *f) {
RN rn;
int count = 0;
int hex = 0;
char decp[2];
rn.f = f; rn.n = 0;
decp[0] = lua_getlocaledecpoint();  
decp[1] = '.';  
l_lockfile(rn.f);
do { rn.c = l_getc(rn.f); } while (isspace(rn.c));  
test2(&rn, "-+");  
if (test2(&rn, "00")) {
if (test2(&rn, "xX")) hex = 1;  
else count = 1;  
}
count += readdigits(&rn, hex);  
if (test2(&rn, decp))  
count += readdigits(&rn, hex);  
if (count > 0 && test2(&rn, (hex ? "pP" : "eE"))) {  
test2(&rn, "-+");  
readdigits(&rn, 0);  
}
ungetc(rn.c, rn.f);  
l_unlockfile(rn.f);
rn.buff[rn.n] = '\0';  
if (lua_stringtonumber(L, rn.buff))  
return 1;  
else {  
lua_pushnil(L);  
return 0;  
}
}
static int test_eof (lua_State *L, FILE *f) {
int c = getc(f);
ungetc(c, f);  
lua_pushliteral(L, "");
return (c != EOF);
}
static int read_line (lua_State *L, FILE *f, int chop) {
luaL_Buffer b;
int c = '\0';
luaL_buffinit(L, &b);
while (c != EOF && c != '\n') {  
char *buff = luaL_prepbuffer(&b);  
int i = 0;
l_lockfile(f);  
while (i < LUAL_BUFFERSIZE && (c = l_getc(f)) != EOF && c != '\n')
buff[i++] = c;
l_unlockfile(f);
luaL_addsize(&b, i);
}
if (!chop && c == '\n')  
luaL_addchar(&b, c);  
luaL_pushresult(&b);  
return (c == '\n' || lua_rawlen(L, -1) > 0);
}
static void read_all (lua_State *L, FILE *f) {
size_t nr;
luaL_Buffer b;
luaL_buffinit(L, &b);
do {  
char *p = luaL_prepbuffer(&b);
nr = fread(p, sizeof(char), LUAL_BUFFERSIZE, f);
luaL_addsize(&b, nr);
} while (nr == LUAL_BUFFERSIZE);
luaL_pushresult(&b);  
}
static int read_chars (lua_State *L, FILE *f, size_t n) {
size_t nr;  
char *p;
luaL_Buffer b;
luaL_buffinit(L, &b);
p = luaL_prepbuffsize(&b, n);  
nr = fread(p, sizeof(char), n, f);  
luaL_addsize(&b, nr);
luaL_pushresult(&b);  
return (nr > 0);  
}
static int g_read (lua_State *L, FILE *f, int first) {
int nargs = lua_gettop(L) - 1;
int success;
int n;
clearerr(f);
if (nargs == 0) {  
success = read_line(L, f, 1);
n = first+1;  
}
else {  
luaL_checkstack(L, nargs+LUA_MINSTACK, "too many arguments");
success = 1;
for (n = first; nargs-- && success; n++) {
if (lua_type(L, n) == LUA_TNUMBER) {
size_t l = (size_t)luaL_checkinteger(L, n);
success = (l == 0) ? test_eof(L, f) : read_chars(L, f, l);
}
else {
const char *p = luaL_checkstring(L, n);
if (*p == '*') p++;  
switch (*p) {
case 'n':  
success = read_number(L, f);
break;
case 'l':  
success = read_line(L, f, 1);
break;
case 'L':  
success = read_line(L, f, 0);
break;
case 'a':  
read_all(L, f);  
success = 1; 
break;
default:
return luaL_argerror(L, n, "invalid format");
}
}
}
}
if (ferror(f))
return luaL_fileresult(L, 0, NULL);
if (!success) {
lua_pop(L, 1);  
lua_pushnil(L);  
}
return n - first;
}
static int io_read (lua_State *L) {
return g_read(L, getiofile(L, IO_INPUT), 1);
}
static int f_read (lua_State *L) {
return g_read(L, tofile(L), 2);
}
static int io_readline (lua_State *L) {
LStream *p = (LStream *)lua_touserdata(L, lua_upvalueindex(1));
int i;
int n = (int)lua_tointeger(L, lua_upvalueindex(2));
if (isclosed(p))  
return luaL_error(L, "file is already closed");
lua_settop(L , 1);
luaL_checkstack(L, n, "too many arguments");
for (i = 1; i <= n; i++)  
lua_pushvalue(L, lua_upvalueindex(3 + i));
n = g_read(L, p->f, 2);  
lua_assert(n > 0);  
if (lua_toboolean(L, -n))  
return n;  
else {  
if (n > 1) {  
return luaL_error(L, "%s", lua_tostring(L, -n + 1));
}
if (lua_toboolean(L, lua_upvalueindex(3))) {  
lua_settop(L, 0);
lua_pushvalue(L, lua_upvalueindex(1));
aux_close(L);  
}
return 0;
}
}
static int g_write (lua_State *L, FILE *f, int arg) {
int nargs = lua_gettop(L) - arg;
int status = 1;
for (; nargs--; arg++) {
if (lua_type(L, arg) == LUA_TNUMBER) {
int len = lua_isinteger(L, arg)
? fprintf(f, LUA_INTEGER_FMT,
(LUAI_UACINT)lua_tointeger(L, arg))
: fprintf(f, LUA_NUMBER_FMT,
(LUAI_UACNUMBER)lua_tonumber(L, arg));
status = status && (len > 0);
}
else {
size_t l;
const char *s = luaL_checklstring(L, arg, &l);
status = status && (fwrite(s, sizeof(char), l, f) == l);
}
}
if (status) return 1;  
else return luaL_fileresult(L, status, NULL);
}
static int io_write (lua_State *L) {
return g_write(L, getiofile(L, IO_OUTPUT), 1);
}
static int f_write (lua_State *L) {
FILE *f = tofile(L);
lua_pushvalue(L, 1);  
return g_write(L, f, 2);
}
static int f_seek (lua_State *L) {
static const int mode[] = {SEEK_SET, SEEK_CUR, SEEK_END};
static const char *const modenames[] = {"set", "cur", "end", NULL};
FILE *f = tofile(L);
int op = luaL_checkoption(L, 2, "cur", modenames);
lua_Integer p3 = luaL_optinteger(L, 3, 0);
l_seeknum offset = (l_seeknum)p3;
luaL_argcheck(L, (lua_Integer)offset == p3, 3,
"not an integer in proper range");
op = l_fseek(f, offset, mode[op]);
if (op)
return luaL_fileresult(L, 0, NULL);  
else {
lua_pushinteger(L, (lua_Integer)l_ftell(f));
return 1;
}
}
static int f_setvbuf (lua_State *L) {
static const int mode[] = {_IONBF, _IOFBF, _IOLBF};
static const char *const modenames[] = {"no", "full", "line", NULL};
FILE *f = tofile(L);
int op = luaL_checkoption(L, 2, NULL, modenames);
lua_Integer sz = luaL_optinteger(L, 3, LUAL_BUFFERSIZE);
int res = setvbuf(f, NULL, mode[op], (size_t)sz);
return luaL_fileresult(L, res == 0, NULL);
}
static int io_flush (lua_State *L) {
return luaL_fileresult(L, fflush(getiofile(L, IO_OUTPUT)) == 0, NULL);
}
static int f_flush (lua_State *L) {
return luaL_fileresult(L, fflush(tofile(L)) == 0, NULL);
}
static const luaL_Reg iolib[] = {
{"close", io_close},
{"flush", io_flush},
{"input", io_input},
{"lines", io_lines},
{"open", io_open},
{"output", io_output},
{"popen", io_popen},
{"read", io_read},
{"tmpfile", io_tmpfile},
{"type", io_type},
{"write", io_write},
{NULL, NULL}
};
static const luaL_Reg flib[] = {
{"close", f_close},
{"flush", f_flush},
{"lines", f_lines},
{"read", f_read},
{"seek", f_seek},
{"setvbuf", f_setvbuf},
{"write", f_write},
{"__gc", f_gc},
{"__tostring", f_tostring},
{NULL, NULL}
};
static void createmeta (lua_State *L) {
luaL_newmetatable(L, LUA_FILEHANDLE);  
lua_pushvalue(L, -1);  
lua_setfield(L, -2, "__index");  
luaL_setfuncs(L, flib, 0);  
lua_pop(L, 1);  
}
static int io_noclose (lua_State *L) {
LStream *p = tolstream(L);
p->closef = &io_noclose;  
lua_pushnil(L);
lua_pushliteral(L, "cannot close standard file");
return 2;
}
static void createstdfile (lua_State *L, FILE *f, const char *k,
const char *fname) {
LStream *p = newprefile(L);
p->f = f;
p->closef = &io_noclose;
if (k != NULL) {
lua_pushvalue(L, -1);
lua_setfield(L, LUA_REGISTRYINDEX, k);  
}
lua_setfield(L, -2, fname);  
}
LUAMOD_API int luaopen_io (lua_State *L) {
luaL_newlib(L, iolib);  
createmeta(L);
createstdfile(L, stdin, IO_INPUT, "stdin");
createstdfile(L, stdout, IO_OUTPUT, "stdout");
createstdfile(L, stderr, NULL, "stderr");
return 1;
}
#define llex_c
#define LUA_CORE
#include <locale.h>
#include <string.h>
#define next(ls) (ls->current = zgetc(ls->z))
#define currIsNewline(ls)	(ls->current == '\n' || ls->current == '\r')
static const char *const luaX_tokens [] = {
"and", "break", "do", "else", "elseif",
"end", "false", "for", "function", "goto", "if",
"in", "local", "nil", "not", "or", "repeat",
"return", "then", "true", "until", "while",
"//", "..", "...", "==", ">=", "<=", "~=",
"<<", ">>", "::", "<eof>",
"<number>", "<integer>", "<name>", "<string>"
};
#define save_and_next(ls) (save(ls, ls->current), next(ls))
static l_noret lexerror (LexState *ls, const char *msg, int token);
static void save (LexState *ls, int c) {
Mbuffer *b = ls->buff;
if (luaZ_bufflen(b) + 1 > luaZ_sizebuffer(b)) {
size_t newsize;
if (luaZ_sizebuffer(b) >= MAX_SIZE/2)
lexerror(ls, "lexical element too long", 0);
newsize = luaZ_sizebuffer(b) * 2;
luaZ_resizebuffer(ls->L, b, newsize);
}
b->buffer[luaZ_bufflen(b)++] = cast(char, c);
}
void luaX_init (lua_State *L) {
int i;
TString *e = luaS_newliteral(L, LUA_ENV);  
luaC_fix(L, obj2gco(e));  
for (i=0; i<NUM_RESERVED; i++) {
TString *ts = luaS_new(L, luaX_tokens[i]);
luaC_fix(L, obj2gco(ts));  
ts->extra = cast_byte(i+1);  
}
}
const char *luaX_token2str (LexState *ls, int token) {
if (token < FIRST_RESERVED) {  
lua_assert(token == cast_uchar(token));
return luaO_pushfstring(ls->L, "'%c'", token);
}
else {
const char *s = luaX_tokens[token - FIRST_RESERVED];
if (token < TK_EOS)  
return luaO_pushfstring(ls->L, "'%s'", s);
else  
return s;
}
}
static const char *txtToken (LexState *ls, int token) {
switch (token) {
case TK_NAME: case TK_STRING:
case TK_FLT: case TK_INT:
save(ls, '\0');
return luaO_pushfstring(ls->L, "'%s'", luaZ_buffer(ls->buff));
default:
return luaX_token2str(ls, token);
}
}
static l_noret lexerror (LexState *ls, const char *msg, int token) {
msg = luaG_addinfo(ls->L, msg, ls->source, ls->linenumber);
if (token)
luaO_pushfstring(ls->L, "%s near %s", msg, txtToken(ls, token));
luaD_throw(ls->L, LUA_ERRSYNTAX);
}
l_noret luaX_syntaxerror (LexState *ls, const char *msg) {
lexerror(ls, msg, ls->t.token);
}
TString *luaX_newstring (LexState *ls, const char *str, size_t l) {
lua_State *L = ls->L;
TValue *o;  
TString *ts = luaS_newlstr(L, str, l);  
setsvalue2s(L, L->top++, ts);  
o = luaH_set(L, ls->h, L->top - 1);
if (ttisnil(o)) {  
/* boolean value does not need GC barrier;
table has no metatable, so it does not need to invalidate cache */
setbvalue(o, 1);  
luaC_checkGC(L);
}
else {  
ts = tsvalue(keyfromval(o));  
}
L->top--;  
return ts;
}
static void inclinenumber (LexState *ls) {
int old = ls->current;
lua_assert(currIsNewline(ls));
next(ls);  
if (currIsNewline(ls) && ls->current != old)
next(ls);  
if (++ls->linenumber >= MAX_INT)
lexerror(ls, "chunk has too many lines", 0);
}
void luaX_setinput (lua_State *L, LexState *ls, ZIO *z, TString *source,
int firstchar) {
ls->t.token = 0;
ls->L = L;
ls->current = firstchar;
ls->lookahead.token = TK_EOS;  
ls->z = z;
ls->fs = NULL;
ls->linenumber = 1;
ls->lastline = 1;
ls->source = source;
ls->envn = luaS_newliteral(L, LUA_ENV);  
luaZ_resizebuffer(ls->L, ls->buff, LUA_MINBUFFER);  
}
static int check_next1 (LexState *ls, int c) {
if (ls->current == c) {
next(ls);
return 1;
}
else return 0;
}
static int check_next2 (LexState *ls, const char *set) {
lua_assert(set[2] == '\0');
if (ls->current == set[0] || ls->current == set[1]) {
save_and_next(ls);
return 1;
}
else return 0;
}
static int read_numeral (LexState *ls, SemInfo *seminfo) {
TValue obj;
const char *expo = "Ee";
int first = ls->current;
lua_assert(lisdigit(ls->current));
save_and_next(ls);
if (first == '0' && check_next2(ls, "xX"))  
expo = "Pp";
for (;;) {
if (check_next2(ls, expo))  
check_next2(ls, "-+");  
if (lisxdigit(ls->current))
save_and_next(ls);
else if (ls->current == '.')
save_and_next(ls);
else break;
}
save(ls, '\0');
if (luaO_str2num(luaZ_buffer(ls->buff), &obj) == 0)  
lexerror(ls, "malformed number", TK_FLT);
if (ttisinteger(&obj)) {
seminfo->i = ivalue(&obj);
return TK_INT;
}
else {
lua_assert(ttisfloat(&obj));
seminfo->r = fltvalue(&obj);
return TK_FLT;
}
}
static int skip_sep (LexState *ls) {
int count = 0;
int s = ls->current;
lua_assert(s == '[' || s == ']');
save_and_next(ls);
while (ls->current == '=') {
save_and_next(ls);
count++;
}
return (ls->current == s) ? count : (-count) - 1;
}
static void read_long_string (LexState *ls, SemInfo *seminfo, int sep) {
int line = ls->linenumber;  
save_and_next(ls);  
if (currIsNewline(ls))  
inclinenumber(ls);  
for (;;) {
switch (ls->current) {
case EOZ: {  
const char *what = (seminfo ? "string" : "comment");
const char *msg = luaO_pushfstring(ls->L,
"unfinished long %s (starting at line %d)", what, line);
lexerror(ls, msg, TK_EOS);
break;  
}
case ']': {
if (skip_sep(ls) == sep) {
save_and_next(ls);  
goto endloop;
}
break;
}
case '\n': case '\r': {
save(ls, '\n');
inclinenumber(ls);
if (!seminfo) luaZ_resetbuffer(ls->buff);  
break;
}
default: {
if (seminfo) save_and_next(ls);
else next(ls);
}
}
} endloop:
if (seminfo)
seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + (2 + sep),
luaZ_bufflen(ls->buff) - 2*(2 + sep));
}
static void esccheck (LexState *ls, int c, const char *msg) {
if (!c) {
if (ls->current != EOZ)
save_and_next(ls);  
lexerror(ls, msg, TK_STRING);
}
}
static int gethexa (LexState *ls) {
save_and_next(ls);
esccheck (ls, lisxdigit(ls->current), "hexadecimal digit expected");
return luaO_hexavalue(ls->current);
}
static int readhexaesc (LexState *ls) {
int r = gethexa(ls);
r = (r << 4) + gethexa(ls);
luaZ_buffremove(ls->buff, 2);  
return r;
}
static unsigned long readutf8esc (LexState *ls) {
unsigned long r;
int i = 4;  
save_and_next(ls);  
esccheck(ls, ls->current == '{', "missing '{'");
r = gethexa(ls);  
while ((save_and_next(ls), lisxdigit(ls->current))) {
i++;
r = (r << 4) + luaO_hexavalue(ls->current);
esccheck(ls, r <= 0x10FFFF, "UTF-8 value too large");
}
esccheck(ls, ls->current == '}', "missing '}'");
next(ls);  
luaZ_buffremove(ls->buff, i);  
return r;
}
static void utf8esc (LexState *ls) {
char buff[UTF8BUFFSZ];
int n = luaO_utf8esc(buff, readutf8esc(ls));
for (; n > 0; n--)  
save(ls, buff[UTF8BUFFSZ - n]);
}
static int readdecesc (LexState *ls) {
int i;
int r = 0;  
for (i = 0; i < 3 && lisdigit(ls->current); i++) {  
r = 10*r + ls->current - '0';
save_and_next(ls);
}
esccheck(ls, r <= UCHAR_MAX, "decimal escape too large");
luaZ_buffremove(ls->buff, i);  
return r;
}
static void read_string (LexState *ls, int del, SemInfo *seminfo) {
save_and_next(ls);  
while (ls->current != del) {
switch (ls->current) {
case EOZ:
lexerror(ls, "unfinished string", TK_EOS);
break;  
case '\n':
case '\r':
lexerror(ls, "unfinished string", TK_STRING);
break;  
case '\\': {  
int c;  
save_and_next(ls);  
switch (ls->current) {
case 'a': c = '\a'; goto read_save;
case 'b': c = '\b'; goto read_save;
case 'f': c = '\f'; goto read_save;
case 'n': c = '\n'; goto read_save;
case 'r': c = '\r'; goto read_save;
case 't': c = '\t'; goto read_save;
case 'v': c = '\v'; goto read_save;
case 'x': c = readhexaesc(ls); goto read_save;
case 'u': utf8esc(ls);  goto no_save;
case '\n': case '\r':
inclinenumber(ls); c = '\n'; goto only_save;
case '\\': case '\"': case '\'':
c = ls->current; goto read_save;
case EOZ: goto no_save;  
case 'z': {  
luaZ_buffremove(ls->buff, 1);  
next(ls);  
while (lisspace(ls->current)) {
if (currIsNewline(ls)) inclinenumber(ls);
else next(ls);
}
goto no_save;
}
default: {
esccheck(ls, lisdigit(ls->current), "invalid escape sequence");
c = readdecesc(ls);  
goto only_save;
}
}
read_save:
next(ls);
only_save:
luaZ_buffremove(ls->buff, 1);  
save(ls, c);
no_save: break;
}
default:
save_and_next(ls);
}
}
save_and_next(ls);  
seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + 1,
luaZ_bufflen(ls->buff) - 2);
}
static int llex (LexState *ls, SemInfo *seminfo) {
luaZ_resetbuffer(ls->buff);
for (;;) {
switch (ls->current) {
case '\n': case '\r': {  
inclinenumber(ls);
break;
}
case ' ': case '\f': case '\t': case '\v': {  
next(ls);
break;
}
case '-': {  
next(ls);
if (ls->current != '-') return '-';
next(ls);
if (ls->current == '[') {  
int sep = skip_sep(ls);
luaZ_resetbuffer(ls->buff);  
if (sep >= 0) {
read_long_string(ls, NULL, sep);  
luaZ_resetbuffer(ls->buff);  
break;
}
}
while (!currIsNewline(ls) && ls->current != EOZ)
next(ls);  
break;
}
case '[': {  
int sep = skip_sep(ls);
if (sep >= 0) {
read_long_string(ls, seminfo, sep);
return TK_STRING;
}
else if (sep != -1)  
lexerror(ls, "invalid long string delimiter", TK_STRING);
return '[';
}
case '=': {
next(ls);
if (check_next1(ls, '=')) return TK_EQ;
else return '=';
}
case '<': {
next(ls);
if (check_next1(ls, '=')) return TK_LE;
else if (check_next1(ls, '<')) return TK_SHL;
else return '<';
}
case '>': {
next(ls);
if (check_next1(ls, '=')) return TK_GE;
else if (check_next1(ls, '>')) return TK_SHR;
else return '>';
}
case '/': {
next(ls);
if (check_next1(ls, '/')) return TK_IDIV;
else return '/';
}
case '~': {
next(ls);
if (check_next1(ls, '=')) return TK_NE;
else return '~';
}
case ':': {
next(ls);
if (check_next1(ls, ':')) return TK_DBCOLON;
else return ':';
}
case '"': case '\'': {  
read_string(ls, ls->current, seminfo);
return TK_STRING;
}
case '.': {  
save_and_next(ls);
if (check_next1(ls, '.')) {
if (check_next1(ls, '.'))
return TK_DOTS;   
else return TK_CONCAT;   
}
else if (!lisdigit(ls->current)) return '.';
else return read_numeral(ls, seminfo);
}
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9': {
return read_numeral(ls, seminfo);
}
case EOZ: {
return TK_EOS;
}
default: {
if (lislalpha(ls->current)) {  
TString *ts;
do {
save_and_next(ls);
} while (lislalnum(ls->current));
ts = luaX_newstring(ls, luaZ_buffer(ls->buff),
luaZ_bufflen(ls->buff));
seminfo->ts = ts;
if (isreserved(ts))  
return ts->extra - 1 + FIRST_RESERVED;
else {
return TK_NAME;
}
}
else {  
int c = ls->current;
next(ls);
return c;
}
}
}
}
}
void luaX_next (LexState *ls) {
ls->lastline = ls->linenumber;
if (ls->lookahead.token != TK_EOS) {  
ls->t = ls->lookahead;  
ls->lookahead.token = TK_EOS;  
}
else
ls->t.token = llex(ls, &ls->t.seminfo);  
}
int luaX_lookahead (LexState *ls) {
lua_assert(ls->lookahead.token == TK_EOS);
ls->lookahead.token = llex(ls, &ls->lookahead.seminfo);
return ls->lookahead.token;
}
#define lmathlib_c
#define LUA_LIB
#include <stdlib.h>
#include <math.h>
#undef PI
#define PI	(l_mathop(3.141592653589793238462643383279502884))
#if !defined(l_rand)		
#if defined(LUA_USE_POSIX)
#define l_rand()	random()
#define l_srand(x)	srandom(x)
#define L_RANDMAX	2147483647	
#else
#define l_rand()	rand()
#define l_srand(x)	srand(x)
#define L_RANDMAX	RAND_MAX
#endif
#endif				
static int math_abs (lua_State *L) {
if (lua_isinteger(L, 1)) {
lua_Integer n = lua_tointeger(L, 1);
if (n < 0) n = (lua_Integer)(0u - (lua_Unsigned)n);
lua_pushinteger(L, n);
}
else
lua_pushnumber(L, l_mathop(fabs)(luaL_checknumber(L, 1)));
return 1;
}
static int math_sin (lua_State *L) {
lua_pushnumber(L, l_mathop(sin)(luaL_checknumber(L, 1)));
return 1;
}
static int math_cos (lua_State *L) {
lua_pushnumber(L, l_mathop(cos)(luaL_checknumber(L, 1)));
return 1;
}
static int math_tan (lua_State *L) {
lua_pushnumber(L, l_mathop(tan)(luaL_checknumber(L, 1)));
return 1;
}
static int math_asin (lua_State *L) {
lua_pushnumber(L, l_mathop(asin)(luaL_checknumber(L, 1)));
return 1;
}
static int math_acos (lua_State *L) {
lua_pushnumber(L, l_mathop(acos)(luaL_checknumber(L, 1)));
return 1;
}
static int math_atan (lua_State *L) {
lua_Number y = luaL_checknumber(L, 1);
lua_Number x = luaL_optnumber(L, 2, 1);
lua_pushnumber(L, l_mathop(atan2)(y, x));
return 1;
}
static int math_toint (lua_State *L) {
int valid;
lua_Integer n = lua_tointegerx(L, 1, &valid);
if (valid)
lua_pushinteger(L, n);
else {
luaL_checkany(L, 1);
lua_pushnil(L);  
}
return 1;
}
static void pushnumint (lua_State *L, lua_Number d) {
lua_Integer n;
if (lua_numbertointeger(d, &n))  
lua_pushinteger(L, n);  
else
lua_pushnumber(L, d);  
}
static int math_floor (lua_State *L) {
if (lua_isinteger(L, 1))
lua_settop(L, 1);  
else {
lua_Number d = l_mathop(floor)(luaL_checknumber(L, 1));
pushnumint(L, d);
}
return 1;
}
static int math_ceil (lua_State *L) {
if (lua_isinteger(L, 1))
lua_settop(L, 1);  
else {
lua_Number d = l_mathop(ceil)(luaL_checknumber(L, 1));
pushnumint(L, d);
}
return 1;
}
static int math_fmod (lua_State *L) {
if (lua_isinteger(L, 1) && lua_isinteger(L, 2)) {
lua_Integer d = lua_tointeger(L, 2);
if ((lua_Unsigned)d + 1u <= 1u) {  
luaL_argcheck(L, d != 0, 2, "zero");
lua_pushinteger(L, 0);  
}
else
lua_pushinteger(L, lua_tointeger(L, 1) % d);
}
else
lua_pushnumber(L, l_mathop(fmod)(luaL_checknumber(L, 1),
luaL_checknumber(L, 2)));
return 1;
}
static int math_modf (lua_State *L) {
if (lua_isinteger(L ,1)) {
lua_settop(L, 1);  
lua_pushnumber(L, 0);  
}
else {
lua_Number n = luaL_checknumber(L, 1);
lua_Number ip = (n < 0) ? l_mathop(ceil)(n) : l_mathop(floor)(n);
pushnumint(L, ip);
lua_pushnumber(L, (n == ip) ? l_mathop(0.0) : (n - ip));
}
return 2;
}
static int math_sqrt (lua_State *L) {
lua_pushnumber(L, l_mathop(sqrt)(luaL_checknumber(L, 1)));
return 1;
}
static int math_ult (lua_State *L) {
lua_Integer a = luaL_checkinteger(L, 1);
lua_Integer b = luaL_checkinteger(L, 2);
lua_pushboolean(L, (lua_Unsigned)a < (lua_Unsigned)b);
return 1;
}
static int math_log (lua_State *L) {
lua_Number x = luaL_checknumber(L, 1);
lua_Number res;
if (lua_isnoneornil(L, 2))
res = l_mathop(log)(x);
else {
lua_Number base = luaL_checknumber(L, 2);
#if !defined(LUA_USE_C89)
if (base == l_mathop(2.0))
res = l_mathop(log2)(x); else
#endif
if (base == l_mathop(10.0))
res = l_mathop(log10)(x);
else
res = l_mathop(log)(x)/l_mathop(log)(base);
}
lua_pushnumber(L, res);
return 1;
}
static int math_exp (lua_State *L) {
lua_pushnumber(L, l_mathop(exp)(luaL_checknumber(L, 1)));
return 1;
}
static int math_deg (lua_State *L) {
lua_pushnumber(L, luaL_checknumber(L, 1) * (l_mathop(180.0) / PI));
return 1;
}
static int math_rad (lua_State *L) {
lua_pushnumber(L, luaL_checknumber(L, 1) * (PI / l_mathop(180.0)));
return 1;
}
static int math_min (lua_State *L) {
int n = lua_gettop(L);  
int imin = 1;  
int i;
luaL_argcheck(L, n >= 1, 1, "value expected");
for (i = 2; i <= n; i++) {
if (lua_compare(L, i, imin, LUA_OPLT))
imin = i;
}
lua_pushvalue(L, imin);
return 1;
}
static int math_max (lua_State *L) {
int n = lua_gettop(L);  
int imax = 1;  
int i;
luaL_argcheck(L, n >= 1, 1, "value expected");
for (i = 2; i <= n; i++) {
if (lua_compare(L, imax, i, LUA_OPLT))
imax = i;
}
lua_pushvalue(L, imax);
return 1;
}
static int math_random (lua_State *L) {
lua_Integer low, up;
double r = (double)l_rand() * (1.0 / ((double)L_RANDMAX + 1.0));
switch (lua_gettop(L)) {  
case 0: {  
lua_pushnumber(L, (lua_Number)r);  
return 1;
}
case 1: {  
low = 1;
up = luaL_checkinteger(L, 1);
break;
}
case 2: {  
low = luaL_checkinteger(L, 1);
up = luaL_checkinteger(L, 2);
break;
}
default: return luaL_error(L, "wrong number of arguments");
}
luaL_argcheck(L, low <= up, 1, "interval is empty");
luaL_argcheck(L, low >= 0 || up <= LUA_MAXINTEGER + low, 1,
"interval too large");
r *= (double)(up - low) + 1.0;
lua_pushinteger(L, (lua_Integer)r + low);
return 1;
}
static int math_randomseed (lua_State *L) {
l_srand((unsigned int)(lua_Integer)luaL_checknumber(L, 1));
(void)l_rand(); 
return 0;
}
static int math_type (lua_State *L) {
if (lua_type(L, 1) == LUA_TNUMBER) {
if (lua_isinteger(L, 1))
lua_pushliteral(L, "integer");
else
lua_pushliteral(L, "float");
}
else {
luaL_checkany(L, 1);
lua_pushnil(L);
}
return 1;
}
#if defined(LUA_COMPAT_MATHLIB)
static int math_cosh (lua_State *L) {
lua_pushnumber(L, l_mathop(cosh)(luaL_checknumber(L, 1)));
return 1;
}
static int math_sinh (lua_State *L) {
lua_pushnumber(L, l_mathop(sinh)(luaL_checknumber(L, 1)));
return 1;
}
static int math_tanh (lua_State *L) {
lua_pushnumber(L, l_mathop(tanh)(luaL_checknumber(L, 1)));
return 1;
}
static int math_pow (lua_State *L) {
lua_Number x = luaL_checknumber(L, 1);
lua_Number y = luaL_checknumber(L, 2);
lua_pushnumber(L, l_mathop(pow)(x, y));
return 1;
}
static int math_frexp (lua_State *L) {
int e;
lua_pushnumber(L, l_mathop(frexp)(luaL_checknumber(L, 1), &e));
lua_pushinteger(L, e);
return 2;
}
static int math_ldexp (lua_State *L) {
lua_Number x = luaL_checknumber(L, 1);
int ep = (int)luaL_checkinteger(L, 2);
lua_pushnumber(L, l_mathop(ldexp)(x, ep));
return 1;
}
static int math_log10 (lua_State *L) {
lua_pushnumber(L, l_mathop(log10)(luaL_checknumber(L, 1)));
return 1;
}
#endif
static const luaL_Reg mathlib[] = {
{"abs",   math_abs},
{"acos",  math_acos},
{"asin",  math_asin},
{"atan",  math_atan},
{"ceil",  math_ceil},
{"cos",   math_cos},
{"deg",   math_deg},
{"exp",   math_exp},
{"tointeger", math_toint},
{"floor", math_floor},
{"fmod",   math_fmod},
{"ult",   math_ult},
{"log",   math_log},
{"max",   math_max},
{"min",   math_min},
{"modf",   math_modf},
{"rad",   math_rad},
{"random",     math_random},
{"randomseed", math_randomseed},
{"sin",   math_sin},
{"sqrt",  math_sqrt},
{"tan",   math_tan},
{"type", math_type},
#if defined(LUA_COMPAT_MATHLIB)
{"atan2", math_atan},
{"cosh",   math_cosh},
{"sinh",   math_sinh},
{"tanh",   math_tanh},
{"pow",   math_pow},
{"frexp", math_frexp},
{"ldexp", math_ldexp},
{"log10", math_log10},
#endif
{"pi", NULL},
{"huge", NULL},
{"maxinteger", NULL},
{"mininteger", NULL},
{NULL, NULL}
};
LUAMOD_API int luaopen_math (lua_State *L) {
luaL_newlib(L, mathlib);
lua_pushnumber(L, PI);
lua_setfield(L, -2, "pi");
lua_pushnumber(L, (lua_Number)HUGE_VAL);
lua_setfield(L, -2, "huge");
lua_pushinteger(L, LUA_MAXINTEGER);
lua_setfield(L, -2, "maxinteger");
lua_pushinteger(L, LUA_MININTEGER);
lua_setfield(L, -2, "mininteger");
return 1;
}
#define lmem_c
#define LUA_CORE
#include <stddef.h>
#define MINSIZEARRAY	4
void *luaM_growaux_ (lua_State *L, void *block, int *size, size_t size_elems,
int limit, const char *what) {
void *newblock;
int newsize;
if (*size >= limit/2) {  
if (*size >= limit)  
luaG_runerror(L, "too many %s (limit is %d)", what, limit);
newsize = limit;  
}
else {
newsize = (*size)*2;
if (newsize < MINSIZEARRAY)
newsize = MINSIZEARRAY;  
}
newblock = luaM_reallocv(L, block, *size, newsize, size_elems);
*size = newsize;  
return newblock;
}
l_noret luaM_toobig (lua_State *L) {
luaG_runerror(L, "memory allocation error: block too big");
}
void *luaM_realloc_ (lua_State *L, void *block, size_t osize, size_t nsize) {
void *newblock;
global_State *g = G(L);
size_t realosize = (block) ? osize : 0;
lua_assert((realosize == 0) == (block == NULL));
#if defined(HARDMEMTESTS)
if (nsize > realosize && g->gcrunning)
luaC_fullgc(L, 1);  
#endif
newblock = (*g->frealloc)(g->ud, block, osize, nsize);
if (newblock == NULL && nsize > 0) {
lua_assert(nsize > realosize);  
if (g->version) {  
luaC_fullgc(L, 1);  
newblock = (*g->frealloc)(g->ud, block, osize, nsize);  
}
if (newblock == NULL)
luaD_throw(L, LUA_ERRMEM);
}
lua_assert((nsize == 0) == (newblock == NULL));
g->GCdebt = (g->GCdebt + nsize) - realosize;
return newblock;
}
#define lobject_c
#define LUA_CORE
#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
LUAI_DDEF const TValue luaO_nilobject_ = {NILCONSTANT};
int luaO_int2fb (unsigned int x) {
int e = 0;  
if (x < 8) return x;
while (x >= (8 << 4)) {  
x = (x + 0xf) >> 4;  
e += 4;
}
while (x >= (8 << 1)) {  
x = (x + 1) >> 1;  
e++;
}
return ((e+1) << 3) | (cast_int(x) - 8);
}
int luaO_fb2int (int x) {
return (x < 8) ? x : ((x & 7) + 8) << ((x >> 3) - 1);
}
int luaO_ceillog2 (unsigned int x) {
static const lu_byte log_2[256] = {  
0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
};
int l = 0;
x--;
while (x >= 256) { l += 8; x >>= 8; }
return l + log_2[x];
}
static lua_Integer intarith (lua_State *L, int op, lua_Integer v1,
lua_Integer v2) {
switch (op) {
case LUA_OPADD: return intop(+, v1, v2);
case LUA_OPSUB:return intop(-, v1, v2);
case LUA_OPMUL:return intop(*, v1, v2);
case LUA_OPMOD: return luaV_mod(L, v1, v2);
case LUA_OPIDIV: return luaV_div(L, v1, v2);
case LUA_OPBAND: return intop(&, v1, v2);
case LUA_OPBOR: return intop(|, v1, v2);
case LUA_OPBXOR: return intop(^, v1, v2);
case LUA_OPSHL: return luaV_shiftl(v1, v2);
case LUA_OPSHR: return luaV_shiftl(v1, -v2);
case LUA_OPUNM: return intop(-, 0, v1);
case LUA_OPBNOT: return intop(^, ~l_castS2U(0), v1);
default: lua_assert(0); return 0;
}
}
static lua_Number numarith (lua_State *L, int op, lua_Number v1,
lua_Number v2) {
switch (op) {
case LUA_OPADD: return luai_numadd(L, v1, v2);
case LUA_OPSUB: return luai_numsub(L, v1, v2);
case LUA_OPMUL: return luai_nummul(L, v1, v2);
case LUA_OPDIV: return luai_numdiv(L, v1, v2);
case LUA_OPPOW: return luai_numpow(L, v1, v2);
case LUA_OPIDIV: return luai_numidiv(L, v1, v2);
case LUA_OPUNM: return luai_numunm(L, v1);
case LUA_OPMOD: {
lua_Number m;
luai_nummod(L, v1, v2, m);
return m;
}
default: lua_assert(0); return 0;
}
}
void luaO_arith (lua_State *L, int op, const TValue *p1, const TValue *p2,
TValue *res) {
switch (op) {
case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
case LUA_OPSHL: case LUA_OPSHR:
case LUA_OPBNOT: {  
lua_Integer i1; lua_Integer i2;
if (tointeger(p1, &i1) && tointeger(p2, &i2)) {
setivalue(res, intarith(L, op, i1, i2));
return;
}
else break;  
}
case LUA_OPDIV: case LUA_OPPOW: {  
lua_Number n1; lua_Number n2;
if (tonumber(p1, &n1) && tonumber(p2, &n2)) {
setfltvalue(res, numarith(L, op, n1, n2));
return;
}
else break;  
}
default: {  
lua_Number n1; lua_Number n2;
if (ttisinteger(p1) && ttisinteger(p2)) {
setivalue(res, intarith(L, op, ivalue(p1), ivalue(p2)));
return;
}
else if (tonumber(p1, &n1) && tonumber(p2, &n2)) {
setfltvalue(res, numarith(L, op, n1, n2));
return;
}
else break;  
}
}
lua_assert(L != NULL);  
luaT_trybinTM(L, p1, p2, res, cast(TMS, (op - LUA_OPADD) + TM_ADD));
}
int luaO_hexavalue (int c) {
if (lisdigit(c)) return c - '0';
else return (ltolower(c) - 'a') + 10;
}
static int isneg (const char **s) {
if (**s == '-') { (*s)++; return 1; }
else if (**s == '+') (*s)++;
return 0;
}
#if !defined(lua_strx2number)
/* maximum number of significant digits to read (to avoid overflows
even with single floats) */
#define MAXSIGDIG	30
static lua_Number lua_strx2number (const char *s, char **endptr) {
int dot = lua_getlocaledecpoint();
lua_Number r = 0.0;  
int sigdig = 0;  
int nosigdig = 0;  
int e = 0;  
int neg;  
int hasdot = 0;  
*endptr = cast(char *, s);  
while (lisspace(cast_uchar(*s))) s++;  
neg = isneg(&s);  
if (!(*s == '0' && (*(s + 1) == 'x' || *(s + 1) == 'X')))  
return 0.0;  
for (s += 2; ; s++) {  
if (*s == dot) {
if (hasdot) break;  
else hasdot = 1;
}
else if (lisxdigit(cast_uchar(*s))) {
if (sigdig == 0 && *s == '0')  
nosigdig++;
else if (++sigdig <= MAXSIGDIG)  
r = (r * cast_num(16.0)) + luaO_hexavalue(*s);
else e++; 
if (hasdot) e--;  
}
else break;  
}
if (nosigdig + sigdig == 0)  
return 0.0;  
*endptr = cast(char *, s);  
e *= 4;  
if (*s == 'p' || *s == 'P') {  
int exp1 = 0;  
int neg1;  
s++;  
neg1 = isneg(&s);  
if (!lisdigit(cast_uchar(*s)))
return 0.0;  
while (lisdigit(cast_uchar(*s)))  
exp1 = exp1 * 10 + *(s++) - '0';
if (neg1) exp1 = -exp1;
e += exp1;
*endptr = cast(char *, s);  
}
if (neg) r = -r;
return l_mathop(ldexp)(r, e);
}
#endif
#if !defined (L_MAXLENNUM)
#define L_MAXLENNUM	200
#endif
static const char *l_str2dloc (const char *s, lua_Number *result, int mode) {
char *endptr;
*result = (mode == 'x') ? lua_strx2number(s, &endptr)  
: lua_str2number(s, &endptr);
if (endptr == s) return NULL;  
while (lisspace(cast_uchar(*endptr))) endptr++;  
return (*endptr == '\0') ? endptr : NULL;  
}
static const char *l_str2d (const char *s, lua_Number *result) {
const char *endptr;
const char *pmode = strpbrk(s, ".xXnN");
int mode = pmode ? ltolower(cast_uchar(*pmode)) : 0;
if (mode == 'n')  
return NULL;
endptr = l_str2dloc(s, result, mode);  
if (endptr == NULL) {  
char buff[L_MAXLENNUM + 1];
const char *pdot = strchr(s, '.');
if (strlen(s) > L_MAXLENNUM || pdot == NULL)
return NULL;  
strcpy(buff, s);  
buff[pdot - s] = lua_getlocaledecpoint();  
endptr = l_str2dloc(buff, result, mode);  
if (endptr != NULL)
endptr = s + (endptr - buff);  
}
return endptr;
}
#define MAXBY10		cast(lua_Unsigned, LUA_MAXINTEGER / 10)
#define MAXLASTD	cast_int(LUA_MAXINTEGER % 10)
static const char *l_str2int (const char *s, lua_Integer *result) {
lua_Unsigned a = 0;
int empty = 1;
int neg;
while (lisspace(cast_uchar(*s))) s++;  
neg = isneg(&s);
if (s[0] == '0' &&
(s[1] == 'x' || s[1] == 'X')) {  
s += 2;  
for (; lisxdigit(cast_uchar(*s)); s++) {
a = a * 16 + luaO_hexavalue(*s);
empty = 0;
}
}
else {  
for (; lisdigit(cast_uchar(*s)); s++) {
int d = *s - '0';
if (a >= MAXBY10 && (a > MAXBY10 || d > MAXLASTD + neg))  
return NULL;  
a = a * 10 + d;
empty = 0;
}
}
while (lisspace(cast_uchar(*s))) s++;  
if (empty || *s != '\0') return NULL;  
else {
*result = l_castU2S((neg) ? 0u - a : a);
return s;
}
}
size_t luaO_str2num (const char *s, TValue *o) {
lua_Integer i; lua_Number n;
const char *e;
if ((e = l_str2int(s, &i)) != NULL) {  
setivalue(o, i);
}
else if ((e = l_str2d(s, &n)) != NULL) {  
setfltvalue(o, n);
}
else
return 0;  
return (e - s) + 1;  
}
int luaO_utf8esc (char *buff, unsigned long x) {
int n = 1;  
lua_assert(x <= 0x10FFFF);
if (x < 0x80)  
buff[UTF8BUFFSZ - 1] = cast(char, x);
else {  
unsigned int mfb = 0x3f;  
do {  
buff[UTF8BUFFSZ - (n++)] = cast(char, 0x80 | (x & 0x3f));
x >>= 6;  
mfb >>= 1;  
} while (x > mfb);  
buff[UTF8BUFFSZ - n] = cast(char, (~mfb << 1) | x);  
}
return n;
}
#define MAXNUMBER2STR	50
void luaO_tostring (lua_State *L, StkId obj) {
char buff[MAXNUMBER2STR];
size_t len;
lua_assert(ttisnumber(obj));
if (ttisinteger(obj))
len = lua_integer2str(buff, sizeof(buff), ivalue(obj));
else {
len = lua_number2str(buff, sizeof(buff), fltvalue(obj));
#if !defined(LUA_COMPAT_FLOATSTRING)
if (buff[strspn(buff, "-0123456789")] == '\0') {  
buff[len++] = lua_getlocaledecpoint();
buff[len++] = '0';  
}
#endif
}
setsvalue2s(L, obj, luaS_newlstr(L, buff, len));
}
static void pushstr (lua_State *L, const char *str, size_t l) {
setsvalue2s(L, L->top, luaS_newlstr(L, str, l));
luaD_inctop(L);
}
const char *luaO_pushvfstring (lua_State *L, const char *fmt, va_list argp) {
int n = 0;
for (;;) {
const char *e = strchr(fmt, '%');
if (e == NULL) break;
pushstr(L, fmt, e - fmt);
switch (*(e+1)) {
case 's': {  
const char *s = va_arg(argp, char *);
if (s == NULL) s = "(null)";
pushstr(L, s, strlen(s));
break;
}
case 'c': {  
char buff = cast(char, va_arg(argp, int));
if (lisprint(cast_uchar(buff)))
pushstr(L, &buff, 1);
else  
luaO_pushfstring(L, "<\\%d>", cast_uchar(buff));
break;
}
case 'd': {  
setivalue(L->top, va_arg(argp, int));
goto top2str;
}
case 'I': {  
setivalue(L->top, cast(lua_Integer, va_arg(argp, l_uacInt)));
goto top2str;
}
case 'f': {  
setfltvalue(L->top, cast_num(va_arg(argp, l_uacNumber)));
top2str:  
luaD_inctop(L);
luaO_tostring(L, L->top - 1);
break;
}
case 'p': {  
char buff[4*sizeof(void *) + 8]; 
void *p = va_arg(argp, void *);
int l = lua_pointer2str(buff, sizeof(buff), p);
pushstr(L, buff, l);
break;
}
case 'U': {  
char buff[UTF8BUFFSZ];
int l = luaO_utf8esc(buff, cast(long, va_arg(argp, long)));
pushstr(L, buff + UTF8BUFFSZ - l, l);
break;
}
case '%': {
pushstr(L, "%", 1);
break;
}
default: {
luaG_runerror(L, "invalid option '%%%c' to 'lua_pushfstring'",
*(e + 1));
}
}
n += 2;
fmt = e+2;
}
luaD_checkstack(L, 1);
pushstr(L, fmt, strlen(fmt));
if (n > 0) luaV_concat(L, n + 1);
return svalue(L->top - 1);
}
const char *luaO_pushfstring (lua_State *L, const char *fmt, ...) {
const char *msg;
va_list argp;
va_start(argp, fmt);
msg = luaO_pushvfstring(L, fmt, argp);
va_end(argp);
return msg;
}
#define LL(x)	(sizeof(x)/sizeof(char) - 1)
#define RETS	"..."
#define PRE	"[string \""
#define POS	"\"]"
#define addstr(a,b,l)	( memcpy(a,b,(l) * sizeof(char)), a += (l) )
void luaO_chunkid (char *out, const char *source, size_t bufflen) {
size_t l = strlen(source);
if (*source == '=') {  
if (l <= bufflen)  
memcpy(out, source + 1, l * sizeof(char));
else {  
addstr(out, source + 1, bufflen - 1);
*out = '\0';
}
}
else if (*source == '@') {  
if (l <= bufflen)  
memcpy(out, source + 1, l * sizeof(char));
else {  
addstr(out, RETS, LL(RETS));
bufflen -= LL(RETS);
memcpy(out, source + 1 + l - bufflen, bufflen * sizeof(char));
}
}
else {  
const char *nl = strchr(source, '\n');  
addstr(out, PRE, LL(PRE));  
bufflen -= LL(PRE RETS POS) + 1;  
if (l < bufflen && nl == NULL) {  
addstr(out, source, l);  
}
else {
if (nl != NULL) l = nl - source;  
if (l > bufflen) l = bufflen;
addstr(out, source, l);
addstr(out, RETS, LL(RETS));
}
memcpy(out, POS, (LL(POS) + 1) * sizeof(char));
}
}
#define lopcodes_c
#define LUA_CORE
#include <stddef.h>
LUAI_DDEF const char *const luaP_opnames[NUM_OPCODES+1] = {
"MOVE",
"LOADK",
"LOADKX",
"LOADBOOL",
"LOADNIL",
"GETUPVAL",
"GETTABUP",
"GETTABLE",
"SETTABUP",
"SETUPVAL",
"SETTABLE",
"NEWTABLE",
"SELF",
"ADD",
"SUB",
"MUL",
"MOD",
"POW",
"DIV",
"IDIV",
"BAND",
"BOR",
"BXOR",
"SHL",
"SHR",
"UNM",
"BNOT",
"NOT",
"LEN",
"CONCAT",
"JMP",
"EQ",
"LT",
"LE",
"TEST",
"TESTSET",
"CALL",
"TAILCALL",
"RETURN",
"FORLOOP",
"FORPREP",
"TFORCALL",
"TFORLOOP",
"SETLIST",
"CLOSURE",
"VARARG",
"EXTRAARG",
NULL
};
#define opmode(t,a,b,c,m) (((t)<<7) | ((a)<<6) | ((b)<<4) | ((c)<<2) | (m))
LUAI_DDEF const lu_byte luaP_opmodes[NUM_OPCODES] = {
opmode(0, 1, OpArgR, OpArgN, iABC)		
,opmode(0, 1, OpArgK, OpArgN, iABx)		
,opmode(0, 1, OpArgN, OpArgN, iABx)		
,opmode(0, 1, OpArgU, OpArgU, iABC)		
,opmode(0, 1, OpArgU, OpArgN, iABC)		
,opmode(0, 1, OpArgU, OpArgN, iABC)		
,opmode(0, 1, OpArgU, OpArgK, iABC)		
,opmode(0, 1, OpArgR, OpArgK, iABC)		
,opmode(0, 0, OpArgK, OpArgK, iABC)		
,opmode(0, 0, OpArgU, OpArgN, iABC)		
,opmode(0, 0, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgU, OpArgU, iABC)		
,opmode(0, 1, OpArgR, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgK, OpArgK, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iABC)		
,opmode(0, 1, OpArgR, OpArgR, iABC)		
,opmode(0, 0, OpArgR, OpArgN, iAsBx)		
,opmode(1, 0, OpArgK, OpArgK, iABC)		
,opmode(1, 0, OpArgK, OpArgK, iABC)		
,opmode(1, 0, OpArgK, OpArgK, iABC)		
,opmode(1, 0, OpArgN, OpArgU, iABC)		
,opmode(1, 1, OpArgR, OpArgU, iABC)		
,opmode(0, 1, OpArgU, OpArgU, iABC)		
,opmode(0, 1, OpArgU, OpArgU, iABC)		
,opmode(0, 0, OpArgU, OpArgN, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iAsBx)		
,opmode(0, 1, OpArgR, OpArgN, iAsBx)		
,opmode(0, 0, OpArgN, OpArgU, iABC)		
,opmode(0, 1, OpArgR, OpArgN, iAsBx)		
,opmode(0, 0, OpArgU, OpArgU, iABC)		
,opmode(0, 1, OpArgU, OpArgN, iABx)		
,opmode(0, 1, OpArgU, OpArgN, iABC)		
,opmode(0, 0, OpArgU, OpArgU, iAx)		
};
#define loslib_c
#define LUA_LIB
#include <errno.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#if !defined(LUA_STRFTIMEOPTIONS)	
#define L_STRFTIMEC89		"aAbBcdHIjmMpSUwWxXyYZ%"
#define L_STRFTIMEC99 "aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%" \
"||" "EcECExEXEyEY" "OdOeOHOIOmOMOSOuOUOVOwOWOy"  
#define L_STRFTIMEWIN "aAbBcdHIjmMpSUwWxXyYzZ%" \
"||" "#c#x#d#H#I#j#m#M#S#U#w#W#y#Y"  
#if defined(LUA_USE_WINDOWS)
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEWIN
#elif defined(LUA_USE_C89)
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEC89
#else  
#define LUA_STRFTIMEOPTIONS	L_STRFTIMEC99
#endif
#endif					
#if !defined(l_time_t)		
#define l_timet			lua_Integer
#define l_pushtime(L,t)		lua_pushinteger(L,(lua_Integer)(t))
static time_t l_checktime (lua_State *L, int arg) {
lua_Integer t = luaL_checkinteger(L, arg);
luaL_argcheck(L, (time_t)t == t, arg, "time out-of-bounds");
return (time_t)t;
}
#endif				
#if !defined(l_gmtime)		
#if defined(LUA_USE_POSIX)	
#define l_gmtime(t,r)		gmtime_r(t,r)
#define l_localtime(t,r)	localtime_r(t,r)
#else				
#define l_gmtime(t,r)		((void)(r)->tm_sec, gmtime(t))
#define l_localtime(t,r)  	((void)(r)->tm_sec, localtime(t))
#endif				
#endif				
#if !defined(lua_tmpnam)	
#if defined(LUA_USE_POSIX)	
#include <unistd.h>
#define LUA_TMPNAMBUFSIZE	32
#if !defined(LUA_TMPNAMTEMPLATE)
#define LUA_TMPNAMTEMPLATE	"/tmp/lua_XXXXXX"
#endif
#define lua_tmpnam(b,e) { \
strcpy(b, LUA_TMPNAMTEMPLATE); \
e = mkstemp(b); \
if (e != -1) close(e); \
e = (e == -1); }
#else				
#define LUA_TMPNAMBUFSIZE	L_tmpnam
#define lua_tmpnam(b,e)		{ e = (tmpnam(b) == NULL); }
#endif				
#endif				
static int os_execute (lua_State *L) {
const char *cmd = luaL_optstring(L, 1, NULL);
int stat = system(cmd);
if (cmd != NULL)
return luaL_execresult(L, stat);
else {
lua_pushboolean(L, stat);  
return 1;
}
}
static int os_remove (lua_State *L) {
const char *filename = luaL_checkstring(L, 1);
return luaL_fileresult(L, remove(filename) == 0, filename);
}
static int os_rename (lua_State *L) {
const char *fromname = luaL_checkstring(L, 1);
const char *toname = luaL_checkstring(L, 2);
return luaL_fileresult(L, rename(fromname, toname) == 0, NULL);
}
static int os_tmpname (lua_State *L) {
char buff[LUA_TMPNAMBUFSIZE];
int err;
lua_tmpnam(buff, err);
if (err)
return luaL_error(L, "unable to generate a unique filename");
lua_pushstring(L, buff);
return 1;
}
static int os_getenv (lua_State *L) {
lua_pushstring(L, getenv(luaL_checkstring(L, 1)));  
return 1;
}
static int os_clock (lua_State *L) {
lua_pushnumber(L, ((lua_Number)clock())/(lua_Number)CLOCKS_PER_SEC);
return 1;
}
static void setfield (lua_State *L, const char *key, int value) {
lua_pushinteger(L, value);
lua_setfield(L, -2, key);
}
static void setboolfield (lua_State *L, const char *key, int value) {
if (value < 0)  
return;  
lua_pushboolean(L, value);
lua_setfield(L, -2, key);
}
static void setallfields (lua_State *L, struct tm *stm) {
setfield(L, "sec", stm->tm_sec);
setfield(L, "min", stm->tm_min);
setfield(L, "hour", stm->tm_hour);
setfield(L, "day", stm->tm_mday);
setfield(L, "month", stm->tm_mon + 1);
setfield(L, "year", stm->tm_year + 1900);
setfield(L, "wday", stm->tm_wday + 1);
setfield(L, "yday", stm->tm_yday + 1);
setboolfield(L, "isdst", stm->tm_isdst);
}
static int getboolfield (lua_State *L, const char *key) {
int res;
res = (lua_getfield(L, -1, key) == LUA_TNIL) ? -1 : lua_toboolean(L, -1);
lua_pop(L, 1);
return res;
}
#if !defined(L_MAXDATEFIELD)
#define L_MAXDATEFIELD	(INT_MAX / 2)
#endif
static int getfield (lua_State *L, const char *key, int d, int delta) {
int isnum;
int t = lua_getfield(L, -1, key);  
lua_Integer res = lua_tointegerx(L, -1, &isnum);
if (!isnum) {  
if (t != LUA_TNIL)  
return luaL_error(L, "field '%s' is not an integer", key);
else if (d < 0)  
return luaL_error(L, "field '%s' missing in date table", key);
res = d;
}
else {
if (!(-L_MAXDATEFIELD <= res && res <= L_MAXDATEFIELD))
return luaL_error(L, "field '%s' is out-of-bound", key);
res -= delta;
}
lua_pop(L, 1);
return (int)res;
}
static const char *checkoption (lua_State *L, const char *conv,
ptrdiff_t convlen, char *buff) {
const char *option = LUA_STRFTIMEOPTIONS;
int oplen = 1;  
for (; *option != '\0' && oplen <= convlen; option += oplen) {
if (*option == '|')  
oplen++;  
else if (memcmp(conv, option, oplen) == 0) {  
memcpy(buff, conv, oplen);  
buff[oplen] = '\0';
return conv + oplen;  
}
}
luaL_argerror(L, 1,
lua_pushfstring(L, "invalid conversion specifier '%%%s'", conv));
return conv;  
}
#define SIZETIMEFMT	250
static int os_date (lua_State *L) {
size_t slen;
const char *s = luaL_optlstring(L, 1, "%c", &slen);
time_t t = luaL_opt(L, l_checktime, 2, time(NULL));
const char *se = s + slen;  
struct tm tmr, *stm;
if (*s == '!') {  
stm = l_gmtime(&t, &tmr);
s++;  
}
else
stm = l_localtime(&t, &tmr);
if (stm == NULL)  
return luaL_error(L,
"time result cannot be represented in this installation");
if (strcmp(s, "*t") == 0) {
lua_createtable(L, 0, 9);  
setallfields(L, stm);
}
else {
char cc[4];  
luaL_Buffer b;
cc[0] = '%';
luaL_buffinit(L, &b);
while (s < se) {
if (*s != '%')  
luaL_addchar(&b, *s++);
else {
size_t reslen;
char *buff = luaL_prepbuffsize(&b, SIZETIMEFMT);
s++;  
s = checkoption(L, s, se - s, cc + 1);  
reslen = strftime(buff, SIZETIMEFMT, cc, stm);
luaL_addsize(&b, reslen);
}
}
luaL_pushresult(&b);
}
return 1;
}
static int os_time (lua_State *L) {
time_t t;
if (lua_isnoneornil(L, 1))  
t = time(NULL);  
else {
struct tm ts;
luaL_checktype(L, 1, LUA_TTABLE);
lua_settop(L, 1);  
ts.tm_sec = getfield(L, "sec", 0, 0);
ts.tm_min = getfield(L, "min", 0, 0);
ts.tm_hour = getfield(L, "hour", 12, 0);
ts.tm_mday = getfield(L, "day", -1, 0);
ts.tm_mon = getfield(L, "month", -1, 1);
ts.tm_year = getfield(L, "year", -1, 1900);
ts.tm_isdst = getboolfield(L, "isdst");
t = mktime(&ts);
setallfields(L, &ts);  
}
if (t != (time_t)(l_timet)t || t == (time_t)(-1))
return luaL_error(L,
"time result cannot be represented in this installation");
l_pushtime(L, t);
return 1;
}
static int os_difftime (lua_State *L) {
time_t t1 = l_checktime(L, 1);
time_t t2 = l_checktime(L, 2);
lua_pushnumber(L, (lua_Number)difftime(t1, t2));
return 1;
}
static int os_setlocale (lua_State *L) {
static const int cat[] = {LC_ALL, LC_COLLATE, LC_CTYPE, LC_MONETARY,
LC_NUMERIC, LC_TIME};
static const char *const catnames[] = {"all", "collate", "ctype", "monetary",
"numeric", "time", NULL};
const char *l = luaL_optstring(L, 1, NULL);
int op = luaL_checkoption(L, 2, "all", catnames);
lua_pushstring(L, setlocale(cat[op], l));
return 1;
}
static int os_exit (lua_State *L) {
int status;
if (lua_isboolean(L, 1))
status = (lua_toboolean(L, 1) ? EXIT_SUCCESS : EXIT_FAILURE);
else
status = (int)luaL_optinteger(L, 1, EXIT_SUCCESS);
if (lua_toboolean(L, 2))
lua_close(L);
if (L) exit(status);  
return 0;
}
static const luaL_Reg syslib[] = {
{"clock",     os_clock},
{"date",      os_date},
{"difftime",  os_difftime},
{"execute",   os_execute},
{"exit",      os_exit},
{"getenv",    os_getenv},
{"remove",    os_remove},
{"rename",    os_rename},
{"setlocale", os_setlocale},
{"time",      os_time},
{"tmpname",   os_tmpname},
{NULL, NULL}
};
LUAMOD_API int luaopen_os (lua_State *L) {
luaL_newlib(L, syslib);
return 1;
}
#define lparser_c
#define LUA_CORE
#include <string.h>
/* maximum number of local variables per function (must be smaller
than 250, due to the bytecode format) */
#define MAXVARS		200
#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)
/* because all strings are unified by the scanner, the parser
can use pointer equality for string equality */
#define eqstr(a,b)	((a) == (b))
typedef struct BlockCnt {
struct BlockCnt *previous;  
int firstlabel;  
int firstgoto;  
lu_byte nactvar;  
lu_byte upval;  
lu_byte isloop;  
} BlockCnt;
static void statement (LexState *ls);
static void expr (LexState *ls, expdesc *v);
static l_noret semerror (LexState *ls, const char *msg) {
ls->t.token = 0;  
luaX_syntaxerror(ls, msg);
}
static l_noret error_expected (LexState *ls, int token) {
luaX_syntaxerror(ls,
luaO_pushfstring(ls->L, "%s expected", luaX_token2str(ls, token)));
}
static l_noret errorlimit (FuncState *fs, int limit, const char *what) {
lua_State *L = fs->ls->L;
const char *msg;
int line = fs->f->linedefined;
const char *where = (line == 0)
? "main function"
: luaO_pushfstring(L, "function at line %d", line);
msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s",
what, limit, where);
luaX_syntaxerror(fs->ls, msg);
}
static void checklimit (FuncState *fs, int v, int l, const char *what) {
if (v > l) errorlimit(fs, l, what);
}
static int testnext (LexState *ls, int c) {
if (ls->t.token == c) {
luaX_next(ls);
return 1;
}
else return 0;
}
static void check (LexState *ls, int c) {
if (ls->t.token != c)
error_expected(ls, c);
}
static void checknext (LexState *ls, int c) {
check(ls, c);
luaX_next(ls);
}
#define check_condition(ls,c,msg)	{ if (!(c)) luaX_syntaxerror(ls, msg); }
static void check_match (LexState *ls, int what, int who, int where) {
if (!testnext(ls, what)) {
if (where == ls->linenumber)
error_expected(ls, what);
else {
luaX_syntaxerror(ls, luaO_pushfstring(ls->L,
"%s expected (to close %s at line %d)",
luaX_token2str(ls, what), luaX_token2str(ls, who), where));
}
}
}
static TString *str_checkname (LexState *ls) {
TString *ts;
check(ls, TK_NAME);
ts = ls->t.seminfo.ts;
luaX_next(ls);
return ts;
}
static void init_exp (expdesc *e, expkind k, int i) {
e->f = e->t = NO_JUMP;
e->k = k;
e->u.info = i;
}
static void codestring (LexState *ls, expdesc *e, TString *s) {
init_exp(e, VK, luaK_stringK(ls->fs, s));
}
static void checkname (LexState *ls, expdesc *e) {
codestring(ls, e, str_checkname(ls));
}
static int registerlocalvar (LexState *ls, TString *varname) {
FuncState *fs = ls->fs;
Proto *f = fs->f;
int oldsize = f->sizelocvars;
luaM_growvector(ls->L, f->locvars, fs->nlocvars, f->sizelocvars,
LocVar, SHRT_MAX, "local variables");
while (oldsize < f->sizelocvars)
f->locvars[oldsize++].varname = NULL;
f->locvars[fs->nlocvars].varname = varname;
luaC_objbarrier(ls->L, f, varname);
return fs->nlocvars++;
}
static void new_localvar (LexState *ls, TString *name) {
FuncState *fs = ls->fs;
Dyndata *dyd = ls->dyd;
int reg = registerlocalvar(ls, name);
checklimit(fs, dyd->actvar.n + 1 - fs->firstlocal,
MAXVARS, "local variables");
luaM_growvector(ls->L, dyd->actvar.arr, dyd->actvar.n + 1,
dyd->actvar.size, Vardesc, MAX_INT, "local variables");
dyd->actvar.arr[dyd->actvar.n++].idx = cast(short, reg);
}
static void new_localvarliteral_ (LexState *ls, const char *name, size_t sz) {
new_localvar(ls, luaX_newstring(ls, name, sz));
}
#define new_localvarliteral(ls,v) \
new_localvarliteral_(ls, "" v, (sizeof(v)/sizeof(char))-1)
static LocVar *getlocvar (FuncState *fs, int i) {
int idx = fs->ls->dyd->actvar.arr[fs->firstlocal + i].idx;
lua_assert(idx < fs->nlocvars);
return &fs->f->locvars[idx];
}
static void adjustlocalvars (LexState *ls, int nvars) {
FuncState *fs = ls->fs;
fs->nactvar = cast_byte(fs->nactvar + nvars);
for (; nvars; nvars--) {
getlocvar(fs, fs->nactvar - nvars)->startpc = fs->pc;
}
}
static void removevars (FuncState *fs, int tolevel) {
fs->ls->dyd->actvar.n -= (fs->nactvar - tolevel);
while (fs->nactvar > tolevel)
getlocvar(fs, --fs->nactvar)->endpc = fs->pc;
}
static int searchupvalue (FuncState *fs, TString *name) {
int i;
Upvaldesc *up = fs->f->upvalues;
for (i = 0; i < fs->nups; i++) {
if (eqstr(up[i].name, name)) return i;
}
return -1;  
}
static int newupvalue (FuncState *fs, TString *name, expdesc *v) {
Proto *f = fs->f;
int oldsize = f->sizeupvalues;
checklimit(fs, fs->nups + 1, MAXUPVAL, "upvalues");
luaM_growvector(fs->ls->L, f->upvalues, fs->nups, f->sizeupvalues,
Upvaldesc, MAXUPVAL, "upvalues");
while (oldsize < f->sizeupvalues)
f->upvalues[oldsize++].name = NULL;
f->upvalues[fs->nups].instack = (v->k == VLOCAL);
f->upvalues[fs->nups].idx = cast_byte(v->u.info);
f->upvalues[fs->nups].name = name;
luaC_objbarrier(fs->ls->L, f, name);
return fs->nups++;
}
static int searchvar (FuncState *fs, TString *n) {
int i;
for (i = cast_int(fs->nactvar) - 1; i >= 0; i--) {
if (eqstr(n, getlocvar(fs, i)->varname))
return i;
}
return -1;  
}
static void markupval (FuncState *fs, int level) {
BlockCnt *bl = fs->bl;
while (bl->nactvar > level)
bl = bl->previous;
bl->upval = 1;
}
static void singlevaraux (FuncState *fs, TString *n, expdesc *var, int base) {
if (fs == NULL)  
init_exp(var, VVOID, 0);  
else {
int v = searchvar(fs, n);  
if (v >= 0) {  
init_exp(var, VLOCAL, v);  
if (!base)
markupval(fs, v);  
}
else {  
int idx = searchupvalue(fs, n);  
if (idx < 0) {  
singlevaraux(fs->prev, n, var, 0);  
if (var->k == VVOID)  
return;  
idx  = newupvalue(fs, n, var);  
}
init_exp(var, VUPVAL, idx);  
}
}
}
static void singlevar (LexState *ls, expdesc *var) {
TString *varname = str_checkname(ls);
FuncState *fs = ls->fs;
singlevaraux(fs, varname, var, 1);
if (var->k == VVOID) {  
expdesc key;
singlevaraux(fs, ls->envn, var, 1);  
lua_assert(var->k != VVOID);  
codestring(ls, &key, varname);  
luaK_indexed(fs, var, &key);  
}
}
static void adjust_assign (LexState *ls, int nvars, int nexps, expdesc *e) {
FuncState *fs = ls->fs;
int extra = nvars - nexps;
if (hasmultret(e->k)) {
extra++;  
if (extra < 0) extra = 0;
luaK_setreturns(fs, e, extra);  
if (extra > 1) luaK_reserveregs(fs, extra-1);
}
else {
if (e->k != VVOID) luaK_exp2nextreg(fs, e);  
if (extra > 0) {
int reg = fs->freereg;
luaK_reserveregs(fs, extra);
luaK_nil(fs, reg, extra);
}
}
if (nexps > nvars)
ls->fs->freereg -= nexps - nvars;  
}
static void enterlevel (LexState *ls) {
lua_State *L = ls->L;
++L->nCcalls;
checklimit(ls->fs, L->nCcalls, LUAI_MAXCCALLS, "C levels");
}
#define leavelevel(ls)	((ls)->L->nCcalls--)
static void closegoto (LexState *ls, int g, Labeldesc *label) {
int i;
FuncState *fs = ls->fs;
Labellist *gl = &ls->dyd->gt;
Labeldesc *gt = &gl->arr[g];
lua_assert(eqstr(gt->name, label->name));
if (gt->nactvar < label->nactvar) {
TString *vname = getlocvar(fs, gt->nactvar)->varname;
const char *msg = luaO_pushfstring(ls->L,
"<goto %s> at line %d jumps into the scope of local '%s'",
getstr(gt->name), gt->line, getstr(vname));
semerror(ls, msg);
}
luaK_patchlist(fs, gt->pc, label->pc);
for (i = g; i < gl->n - 1; i++)
gl->arr[i] = gl->arr[i + 1];
gl->n--;
}
static int findlabel (LexState *ls, int g) {
int i;
BlockCnt *bl = ls->fs->bl;
Dyndata *dyd = ls->dyd;
Labeldesc *gt = &dyd->gt.arr[g];
for (i = bl->firstlabel; i < dyd->label.n; i++) {
Labeldesc *lb = &dyd->label.arr[i];
if (eqstr(lb->name, gt->name)) {  
if (gt->nactvar > lb->nactvar &&
(bl->upval || dyd->label.n > bl->firstlabel))
luaK_patchclose(ls->fs, gt->pc, lb->nactvar);
closegoto(ls, g, lb);  
return 1;
}
}
return 0;  
}
static int newlabelentry (LexState *ls, Labellist *l, TString *name,
int line, int pc) {
int n = l->n;
luaM_growvector(ls->L, l->arr, n, l->size,
Labeldesc, SHRT_MAX, "labels/gotos");
l->arr[n].name = name;
l->arr[n].line = line;
l->arr[n].nactvar = ls->fs->nactvar;
l->arr[n].pc = pc;
l->n = n + 1;
return n;
}
static void findgotos (LexState *ls, Labeldesc *lb) {
Labellist *gl = &ls->dyd->gt;
int i = ls->fs->bl->firstgoto;
while (i < gl->n) {
if (eqstr(gl->arr[i].name, lb->name))
closegoto(ls, i, lb);
else
i++;
}
}
static void movegotosout (FuncState *fs, BlockCnt *bl) {
int i = bl->firstgoto;
Labellist *gl = &fs->ls->dyd->gt;
/* correct pending gotos to current block and try to close it
with visible labels */
while (i < gl->n) {
Labeldesc *gt = &gl->arr[i];
if (gt->nactvar > bl->nactvar) {
if (bl->upval)
luaK_patchclose(fs, gt->pc, bl->nactvar);
gt->nactvar = bl->nactvar;
}
if (!findlabel(fs->ls, i))
i++;  
}
}
static void enterblock (FuncState *fs, BlockCnt *bl, lu_byte isloop) {
bl->isloop = isloop;
bl->nactvar = fs->nactvar;
bl->firstlabel = fs->ls->dyd->label.n;
bl->firstgoto = fs->ls->dyd->gt.n;
bl->upval = 0;
bl->previous = fs->bl;
fs->bl = bl;
lua_assert(fs->freereg == fs->nactvar);
}
static void breaklabel (LexState *ls) {
TString *n = luaS_new(ls->L, "break");
int l = newlabelentry(ls, &ls->dyd->label, n, 0, ls->fs->pc);
findgotos(ls, &ls->dyd->label.arr[l]);
}
static l_noret undefgoto (LexState *ls, Labeldesc *gt) {
const char *msg = isreserved(gt->name)
? "<%s> at line %d not inside a loop"
: "no visible label '%s' for <goto> at line %d";
msg = luaO_pushfstring(ls->L, msg, getstr(gt->name), gt->line);
semerror(ls, msg);
}
static void leaveblock (FuncState *fs) {
BlockCnt *bl = fs->bl;
LexState *ls = fs->ls;
if (bl->previous && bl->upval) {
int j = luaK_jump(fs);
luaK_patchclose(fs, j, bl->nactvar);
luaK_patchtohere(fs, j);
}
if (bl->isloop)
breaklabel(ls);  
fs->bl = bl->previous;
removevars(fs, bl->nactvar);
lua_assert(bl->nactvar == fs->nactvar);
fs->freereg = fs->nactvar;  
ls->dyd->label.n = bl->firstlabel;  
if (bl->previous)  
movegotosout(fs, bl);  
else if (bl->firstgoto < ls->dyd->gt.n)  
undefgoto(ls, &ls->dyd->gt.arr[bl->firstgoto]);  
}
static Proto *addprototype (LexState *ls) {
Proto *clp;
lua_State *L = ls->L;
FuncState *fs = ls->fs;
Proto *f = fs->f;  
if (fs->np >= f->sizep) {
int oldsize = f->sizep;
luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
while (oldsize < f->sizep)
f->p[oldsize++] = NULL;
}
f->p[fs->np++] = clp = luaF_newproto(L);
luaC_objbarrier(L, f, clp);
return clp;
}
static void codeclosure (LexState *ls, expdesc *v) {
FuncState *fs = ls->fs->prev;
init_exp(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
luaK_exp2nextreg(fs, v);  
}
static void open_func (LexState *ls, FuncState *fs, BlockCnt *bl) {
Proto *f;
fs->prev = ls->fs;  
fs->ls = ls;
ls->fs = fs;
fs->pc = 0;
fs->lasttarget = 0;
fs->jpc = NO_JUMP;
fs->freereg = 0;
fs->nk = 0;
fs->np = 0;
fs->nups = 0;
fs->nlocvars = 0;
fs->nactvar = 0;
fs->firstlocal = ls->dyd->actvar.n;
fs->bl = NULL;
f = fs->f;
f->source = ls->source;
f->maxstacksize = 2;  
enterblock(fs, bl, 0);
}
static void close_func (LexState *ls) {
lua_State *L = ls->L;
FuncState *fs = ls->fs;
Proto *f = fs->f;
luaK_ret(fs, 0, 0);  
leaveblock(fs);
luaM_reallocvector(L, f->code, f->sizecode, fs->pc, Instruction);
f->sizecode = fs->pc;
luaM_reallocvector(L, f->lineinfo, f->sizelineinfo, fs->pc, int);
f->sizelineinfo = fs->pc;
luaM_reallocvector(L, f->k, f->sizek, fs->nk, TValue);
f->sizek = fs->nk;
luaM_reallocvector(L, f->p, f->sizep, fs->np, Proto *);
f->sizep = fs->np;
luaM_reallocvector(L, f->locvars, f->sizelocvars, fs->nlocvars, LocVar);
f->sizelocvars = fs->nlocvars;
luaM_reallocvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
f->sizeupvalues = fs->nups;
lua_assert(fs->bl == NULL);
ls->fs = fs->prev;
luaC_checkGC(L);
}
static int block_follow (LexState *ls, int withuntil) {
switch (ls->t.token) {
case TK_ELSE: case TK_ELSEIF:
case TK_END: case TK_EOS:
return 1;
case TK_UNTIL: return withuntil;
default: return 0;
}
}
static void statlist (LexState *ls) {
while (!block_follow(ls, 1)) {
if (ls->t.token == TK_RETURN) {
statement(ls);
return;  
}
statement(ls);
}
}
static void fieldsel (LexState *ls, expdesc *v) {
FuncState *fs = ls->fs;
expdesc key;
luaK_exp2anyregup(fs, v);
luaX_next(ls);  
checkname(ls, &key);
luaK_indexed(fs, v, &key);
}
static void yindex (LexState *ls, expdesc *v) {
luaX_next(ls);  
expr(ls, v);
luaK_exp2val(ls->fs, v);
checknext(ls, ']');
}
struct ConsControl {
expdesc v;  
expdesc *t;  
int nh;  
int na;  
int tostore;  
};
static void recfield (LexState *ls, struct ConsControl *cc) {
FuncState *fs = ls->fs;
int reg = ls->fs->freereg;
expdesc key, val;
int rkkey;
if (ls->t.token == TK_NAME) {
checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
checkname(ls, &key);
}
else  
yindex(ls, &key);
cc->nh++;
checknext(ls, '=');
rkkey = luaK_exp2RK(fs, &key);
expr(ls, &val);
luaK_codeABC(fs, OP_SETTABLE, cc->t->u.info, rkkey, luaK_exp2RK(fs, &val));
fs->freereg = reg;  
}
static void closelistfield (FuncState *fs, struct ConsControl *cc) {
if (cc->v.k == VVOID) return;  
luaK_exp2nextreg(fs, &cc->v);
cc->v.k = VVOID;
if (cc->tostore == LFIELDS_PER_FLUSH) {
luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);  
cc->tostore = 0;  
}
}
static void lastlistfield (FuncState *fs, struct ConsControl *cc) {
if (cc->tostore == 0) return;
if (hasmultret(cc->v.k)) {
luaK_setmultret(fs, &cc->v);
luaK_setlist(fs, cc->t->u.info, cc->na, LUA_MULTRET);
cc->na--;  
}
else {
if (cc->v.k != VVOID)
luaK_exp2nextreg(fs, &cc->v);
luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);
}
}
static void listfield (LexState *ls, struct ConsControl *cc) {
expr(ls, &cc->v);
checklimit(ls->fs, cc->na, MAX_INT, "items in a constructor");
cc->na++;
cc->tostore++;
}
static void field (LexState *ls, struct ConsControl *cc) {
switch(ls->t.token) {
case TK_NAME: {  
if (luaX_lookahead(ls) != '=')  
listfield(ls, cc);
else
recfield(ls, cc);
break;
}
case '[': {
recfield(ls, cc);
break;
}
default: {
listfield(ls, cc);
break;
}
}
}
static void constructor (LexState *ls, expdesc *t) {
/* constructor -> '{' [ field { sep field } [sep] ] '}'
sep -> ',' | ';' */
FuncState *fs = ls->fs;
int line = ls->linenumber;
int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
struct ConsControl cc;
cc.na = cc.nh = cc.tostore = 0;
cc.t = t;
init_exp(t, VRELOCABLE, pc);
init_exp(&cc.v, VVOID, 0);  
luaK_exp2nextreg(ls->fs, t);  
checknext(ls, '{');
do {
lua_assert(cc.v.k == VVOID || cc.tostore > 0);
if (ls->t.token == '}') break;
closelistfield(fs, &cc);
field(ls, &cc);
} while (testnext(ls, ',') || testnext(ls, ';'));
check_match(ls, '}', '{', line);
lastlistfield(fs, &cc);
SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); 
SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh));  
}
static void parlist (LexState *ls) {
FuncState *fs = ls->fs;
Proto *f = fs->f;
int nparams = 0;
f->is_vararg = 0;
if (ls->t.token != ')') {  
do {
switch (ls->t.token) {
case TK_NAME: {  
new_localvar(ls, str_checkname(ls));
nparams++;
break;
}
case TK_DOTS: {  
luaX_next(ls);
f->is_vararg = 1;  
break;
}
default: luaX_syntaxerror(ls, "<name> or '...' expected");
}
} while (!f->is_vararg && testnext(ls, ','));
}
adjustlocalvars(ls, nparams);
f->numparams = cast_byte(fs->nactvar);
luaK_reserveregs(fs, fs->nactvar);  
}
static void body (LexState *ls, expdesc *e, int ismethod, int line) {
FuncState new_fs;
BlockCnt bl;
new_fs.f = addprototype(ls);
new_fs.f->linedefined = line;
open_func(ls, &new_fs, &bl);
checknext(ls, '(');
if (ismethod) {
new_localvarliteral(ls, "self");  
adjustlocalvars(ls, 1);
}
parlist(ls);
checknext(ls, ')');
statlist(ls);
new_fs.f->lastlinedefined = ls->linenumber;
check_match(ls, TK_END, TK_FUNCTION, line);
codeclosure(ls, e);
close_func(ls);
}
static int explist (LexState *ls, expdesc *v) {
int n = 1;  
expr(ls, v);
while (testnext(ls, ',')) {
luaK_exp2nextreg(ls->fs, v);
expr(ls, v);
n++;
}
return n;
}
static void funcargs (LexState *ls, expdesc *f, int line) {
FuncState *fs = ls->fs;
expdesc args;
int base, nparams;
switch (ls->t.token) {
case '(': {  
luaX_next(ls);
if (ls->t.token == ')')  
args.k = VVOID;
else {
explist(ls, &args);
luaK_setmultret(fs, &args);
}
check_match(ls, ')', '(', line);
break;
}
case '{': {  
constructor(ls, &args);
break;
}
case TK_STRING: {  
codestring(ls, &args, ls->t.seminfo.ts);
luaX_next(ls);  
break;
}
default: {
luaX_syntaxerror(ls, "function arguments expected");
}
}
lua_assert(f->k == VNONRELOC);
base = f->u.info;  
if (hasmultret(args.k))
nparams = LUA_MULTRET;  
else {
if (args.k != VVOID)
luaK_exp2nextreg(fs, &args);  
nparams = fs->freereg - (base+1);
}
init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams+1, 2));
luaK_fixline(fs, line);
fs->freereg = base+1;  /* call remove function and arguments and leaves
(unless changed) one result */
}
static void primaryexp (LexState *ls, expdesc *v) {
switch (ls->t.token) {
case '(': {
int line = ls->linenumber;
luaX_next(ls);
expr(ls, v);
check_match(ls, ')', '(', line);
luaK_dischargevars(ls->fs, v);
return;
}
case TK_NAME: {
singlevar(ls, v);
return;
}
default: {
luaX_syntaxerror(ls, "unexpected symbol");
}
}
}
static void suffixedexp (LexState *ls, expdesc *v) {
/* suffixedexp ->
primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
FuncState *fs = ls->fs;
int line = ls->linenumber;
primaryexp(ls, v);
for (;;) {
switch (ls->t.token) {
case '.': {  
fieldsel(ls, v);
break;
}
case '[': {  
expdesc key;
luaK_exp2anyregup(fs, v);
yindex(ls, &key);
luaK_indexed(fs, v, &key);
break;
}
case ':': {  
expdesc key;
luaX_next(ls);
checkname(ls, &key);
luaK_self(fs, v, &key);
funcargs(ls, v, line);
break;
}
case '(': case TK_STRING: case '{': {  
luaK_exp2nextreg(fs, v);
funcargs(ls, v, line);
break;
}
default: return;
}
}
}
static void simpleexp (LexState *ls, expdesc *v) {
/* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... |
constructor | FUNCTION body | suffixedexp */
switch (ls->t.token) {
case TK_FLT: {
init_exp(v, VKFLT, 0);
v->u.nval = ls->t.seminfo.r;
break;
}
case TK_INT: {
init_exp(v, VKINT, 0);
v->u.ival = ls->t.seminfo.i;
break;
}
case TK_STRING: {
codestring(ls, v, ls->t.seminfo.ts);
break;
}
case TK_NIL: {
init_exp(v, VNIL, 0);
break;
}
case TK_TRUE: {
init_exp(v, VTRUE, 0);
break;
}
case TK_FALSE: {
init_exp(v, VFALSE, 0);
break;
}
case TK_DOTS: {  
FuncState *fs = ls->fs;
check_condition(ls, fs->f->is_vararg,
"cannot use '...' outside a vararg function");
init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0));
break;
}
case '{': {  
constructor(ls, v);
return;
}
case TK_FUNCTION: {
luaX_next(ls);
body(ls, v, 0, ls->linenumber);
return;
}
default: {
suffixedexp(ls, v);
return;
}
}
luaX_next(ls);
}
static UnOpr getunopr (int op) {
switch (op) {
case TK_NOT: return OPR_NOT;
case '-': return OPR_MINUS;
case '~': return OPR_BNOT;
case '#': return OPR_LEN;
default: return OPR_NOUNOPR;
}
}
static BinOpr getbinopr (int op) {
switch (op) {
case '+': return OPR_ADD;
case '-': return OPR_SUB;
case '*': return OPR_MUL;
case '%': return OPR_MOD;
case '^': return OPR_POW;
case '/': return OPR_DIV;
case TK_IDIV: return OPR_IDIV;
case '&': return OPR_BAND;
case '|': return OPR_BOR;
case '~': return OPR_BXOR;
case TK_SHL: return OPR_SHL;
case TK_SHR: return OPR_SHR;
case TK_CONCAT: return OPR_CONCAT;
case TK_NE: return OPR_NE;
case TK_EQ: return OPR_EQ;
case '<': return OPR_LT;
case TK_LE: return OPR_LE;
case '>': return OPR_GT;
case TK_GE: return OPR_GE;
case TK_AND: return OPR_AND;
case TK_OR: return OPR_OR;
default: return OPR_NOBINOPR;
}
}
static const struct {
lu_byte left;  
lu_byte right; 
} priority[] = {  
{10, 10}, {10, 10},           
{11, 11}, {11, 11},           
{14, 13},                  
{11, 11}, {11, 11},           
{6, 6}, {4, 4}, {5, 5},   
{7, 7}, {7, 7},           
{9, 8},                   
{3, 3}, {3, 3}, {3, 3},   
{3, 3}, {3, 3}, {3, 3},   
{2, 2}, {1, 1}            
};
#define UNARY_PRIORITY	12  
static BinOpr subexpr (LexState *ls, expdesc *v, int limit) {
BinOpr op;
UnOpr uop;
enterlevel(ls);
uop = getunopr(ls->t.token);
if (uop != OPR_NOUNOPR) {
int line = ls->linenumber;
luaX_next(ls);
subexpr(ls, v, UNARY_PRIORITY);
luaK_prefix(ls->fs, uop, v, line);
}
else simpleexp(ls, v);
op = getbinopr(ls->t.token);
while (op != OPR_NOBINOPR && priority[op].left > limit) {
expdesc v2;
BinOpr nextop;
int line = ls->linenumber;
luaX_next(ls);
luaK_infix(ls->fs, op, v);
nextop = subexpr(ls, &v2, priority[op].right);
luaK_posfix(ls->fs, op, v, &v2, line);
op = nextop;
}
leavelevel(ls);
return op;  
}
static void expr (LexState *ls, expdesc *v) {
subexpr(ls, v, 0);
}
static void block (LexState *ls) {
FuncState *fs = ls->fs;
BlockCnt bl;
enterblock(fs, &bl, 0);
statlist(ls);
leaveblock(fs);
}
struct LHS_assign {
struct LHS_assign *prev;
expdesc v;  
};
static void check_conflict (LexState *ls, struct LHS_assign *lh, expdesc *v) {
FuncState *fs = ls->fs;
int extra = fs->freereg;  
int conflict = 0;
for (; lh; lh = lh->prev) {  
if (lh->v.k == VINDEXED) {  
if (lh->v.u.ind.vt == v->k && lh->v.u.ind.t == v->u.info) {
conflict = 1;
lh->v.u.ind.vt = VLOCAL;
lh->v.u.ind.t = extra;  
}
if (v->k == VLOCAL && lh->v.u.ind.idx == v->u.info) {
conflict = 1;
lh->v.u.ind.idx = extra;  
}
}
}
if (conflict) {
OpCode op = (v->k == VLOCAL) ? OP_MOVE : OP_GETUPVAL;
luaK_codeABC(fs, op, extra, v->u.info, 0);
luaK_reserveregs(fs, 1);
}
}
static void assignment (LexState *ls, struct LHS_assign *lh, int nvars) {
expdesc e;
check_condition(ls, vkisvar(lh->v.k), "syntax error");
if (testnext(ls, ',')) {  
struct LHS_assign nv;
nv.prev = lh;
suffixedexp(ls, &nv.v);
if (nv.v.k != VINDEXED)
check_conflict(ls, lh, &nv.v);
checklimit(ls->fs, nvars + ls->L->nCcalls, LUAI_MAXCCALLS,
"C levels");
assignment(ls, &nv, nvars+1);
}
else {  
int nexps;
checknext(ls, '=');
nexps = explist(ls, &e);
if (nexps != nvars)
adjust_assign(ls, nvars, nexps, &e);
else {
luaK_setoneret(ls->fs, &e);  
luaK_storevar(ls->fs, &lh->v, &e);
return;  
}
}
init_exp(&e, VNONRELOC, ls->fs->freereg-1);  
luaK_storevar(ls->fs, &lh->v, &e);
}
static int cond (LexState *ls) {
expdesc v;
expr(ls, &v);  
if (v.k == VNIL) v.k = VFALSE;  
luaK_goiftrue(ls->fs, &v);
return v.f;
}
static void gotostat (LexState *ls, int pc) {
int line = ls->linenumber;
TString *label;
int g;
if (testnext(ls, TK_GOTO))
label = str_checkname(ls);
else {
luaX_next(ls);  
label = luaS_new(ls->L, "break");
}
g = newlabelentry(ls, &ls->dyd->gt, label, line, pc);
findlabel(ls, g);  
}
static void checkrepeated (FuncState *fs, Labellist *ll, TString *label) {
int i;
for (i = fs->bl->firstlabel; i < ll->n; i++) {
if (eqstr(label, ll->arr[i].name)) {
const char *msg = luaO_pushfstring(fs->ls->L,
"label '%s' already defined on line %d",
getstr(label), ll->arr[i].line);
semerror(fs->ls, msg);
}
}
}
static void skipnoopstat (LexState *ls) {
while (ls->t.token == ';' || ls->t.token == TK_DBCOLON)
statement(ls);
}
static void labelstat (LexState *ls, TString *label, int line) {
FuncState *fs = ls->fs;
Labellist *ll = &ls->dyd->label;
int l;  
checkrepeated(fs, ll, label);  
checknext(ls, TK_DBCOLON);  
l = newlabelentry(ls, ll, label, line, luaK_getlabel(fs));
skipnoopstat(ls);  
if (block_follow(ls, 0)) {  
ll->arr[l].nactvar = fs->bl->nactvar;
}
findgotos(ls, &ll->arr[l]);
}
static void whilestat (LexState *ls, int line) {
FuncState *fs = ls->fs;
int whileinit;
int condexit;
BlockCnt bl;
luaX_next(ls);  
whileinit = luaK_getlabel(fs);
condexit = cond(ls);
enterblock(fs, &bl, 1);
checknext(ls, TK_DO);
block(ls);
luaK_jumpto(fs, whileinit);
check_match(ls, TK_END, TK_WHILE, line);
leaveblock(fs);
luaK_patchtohere(fs, condexit);  
}
static void repeatstat (LexState *ls, int line) {
int condexit;
FuncState *fs = ls->fs;
int repeat_init = luaK_getlabel(fs);
BlockCnt bl1, bl2;
enterblock(fs, &bl1, 1);  
enterblock(fs, &bl2, 0);  
luaX_next(ls);  
statlist(ls);
check_match(ls, TK_UNTIL, TK_REPEAT, line);
condexit = cond(ls);  
if (bl2.upval)  
luaK_patchclose(fs, condexit, bl2.nactvar);
leaveblock(fs);  
luaK_patchlist(fs, condexit, repeat_init);  
leaveblock(fs);  
}
static int exp1 (LexState *ls) {
expdesc e;
int reg;
expr(ls, &e);
luaK_exp2nextreg(ls->fs, &e);
lua_assert(e.k == VNONRELOC);
reg = e.u.info;
return reg;
}
static void forbody (LexState *ls, int base, int line, int nvars, int isnum) {
BlockCnt bl;
FuncState *fs = ls->fs;
int prep, endfor;
adjustlocalvars(ls, 3);  
checknext(ls, TK_DO);
prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs);
enterblock(fs, &bl, 0);  
adjustlocalvars(ls, nvars);
luaK_reserveregs(fs, nvars);
block(ls);
leaveblock(fs);  
luaK_patchtohere(fs, prep);
if (isnum)  
endfor = luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP);
else {  
luaK_codeABC(fs, OP_TFORCALL, base, 0, nvars);
luaK_fixline(fs, line);
endfor = luaK_codeAsBx(fs, OP_TFORLOOP, base + 2, NO_JUMP);
}
luaK_patchlist(fs, endfor, prep + 1);
luaK_fixline(fs, line);
}
static void fornum (LexState *ls, TString *varname, int line) {
FuncState *fs = ls->fs;
int base = fs->freereg;
new_localvarliteral(ls, "(for index)");
new_localvarliteral(ls, "(for limit)");
new_localvarliteral(ls, "(for step)");
new_localvar(ls, varname);
checknext(ls, '=');
exp1(ls);  
checknext(ls, ',');
exp1(ls);  
if (testnext(ls, ','))
exp1(ls);  
else {  
luaK_codek(fs, fs->freereg, luaK_intK(fs, 1));
luaK_reserveregs(fs, 1);
}
forbody(ls, base, line, 1, 1);
}
static void forlist (LexState *ls, TString *indexname) {
FuncState *fs = ls->fs;
expdesc e;
int nvars = 4;  
int line;
int base = fs->freereg;
new_localvarliteral(ls, "(for generator)");
new_localvarliteral(ls, "(for state)");
new_localvarliteral(ls, "(for control)");
new_localvar(ls, indexname);
while (testnext(ls, ',')) {
new_localvar(ls, str_checkname(ls));
nvars++;
}
checknext(ls, TK_IN);
line = ls->linenumber;
adjust_assign(ls, 3, explist(ls, &e), &e);
luaK_checkstack(fs, 3);  
forbody(ls, base, line, nvars - 3, 0);
}
static void forstat (LexState *ls, int line) {
FuncState *fs = ls->fs;
TString *varname;
BlockCnt bl;
enterblock(fs, &bl, 1);  
luaX_next(ls);  
varname = str_checkname(ls);  
switch (ls->t.token) {
case '=': fornum(ls, varname, line); break;
case ',': case TK_IN: forlist(ls, varname); break;
default: luaX_syntaxerror(ls, "'=' or 'in' expected");
}
check_match(ls, TK_END, TK_FOR, line);
leaveblock(fs);  
}
static void test_then_block (LexState *ls, int *escapelist) {
BlockCnt bl;
FuncState *fs = ls->fs;
expdesc v;
int jf;  
luaX_next(ls);  
expr(ls, &v);  
checknext(ls, TK_THEN);
if (ls->t.token == TK_GOTO || ls->t.token == TK_BREAK) {
luaK_goiffalse(ls->fs, &v);  
enterblock(fs, &bl, 0);  
gotostat(ls, v.t);  
while (testnext(ls, ';')) {}  
if (block_follow(ls, 0)) {  
leaveblock(fs);
return;  
}
else  
jf = luaK_jump(fs);
}
else {  
luaK_goiftrue(ls->fs, &v);  
enterblock(fs, &bl, 0);
jf = v.f;
}
statlist(ls);  
leaveblock(fs);
if (ls->t.token == TK_ELSE ||
ls->t.token == TK_ELSEIF)  
luaK_concat(fs, escapelist, luaK_jump(fs));  
luaK_patchtohere(fs, jf);
}
static void ifstat (LexState *ls, int line) {
FuncState *fs = ls->fs;
int escapelist = NO_JUMP;  
test_then_block(ls, &escapelist);  
while (ls->t.token == TK_ELSEIF)
test_then_block(ls, &escapelist);  
if (testnext(ls, TK_ELSE))
block(ls);  
check_match(ls, TK_END, TK_IF, line);
luaK_patchtohere(fs, escapelist);  
}
static void localfunc (LexState *ls) {
expdesc b;
FuncState *fs = ls->fs;
new_localvar(ls, str_checkname(ls));  
adjustlocalvars(ls, 1);  
body(ls, &b, 0, ls->linenumber);  
getlocvar(fs, b.u.info)->startpc = fs->pc;
}
static void localstat (LexState *ls) {
int nvars = 0;
int nexps;
expdesc e;
do {
new_localvar(ls, str_checkname(ls));
nvars++;
} while (testnext(ls, ','));
if (testnext(ls, '='))
nexps = explist(ls, &e);
else {
e.k = VVOID;
nexps = 0;
}
adjust_assign(ls, nvars, nexps, &e);
adjustlocalvars(ls, nvars);
}
static int funcname (LexState *ls, expdesc *v) {
int ismethod = 0;
singlevar(ls, v);
while (ls->t.token == '.')
fieldsel(ls, v);
if (ls->t.token == ':') {
ismethod = 1;
fieldsel(ls, v);
}
return ismethod;
}
static void funcstat (LexState *ls, int line) {
int ismethod;
expdesc v, b;
luaX_next(ls);  
ismethod = funcname(ls, &v);
body(ls, &b, ismethod, line);
luaK_storevar(ls->fs, &v, &b);
luaK_fixline(ls->fs, line);  
}
static void exprstat (LexState *ls) {
FuncState *fs = ls->fs;
struct LHS_assign v;
suffixedexp(ls, &v.v);
if (ls->t.token == '=' || ls->t.token == ',') { 
v.prev = NULL;
assignment(ls, &v, 1);
}
else {  
check_condition(ls, v.v.k == VCALL, "syntax error");
SETARG_C(getinstruction(fs, &v.v), 1);  
}
}
static void retstat (LexState *ls) {
FuncState *fs = ls->fs;
expdesc e;
int first, nret;  
if (block_follow(ls, 1) || ls->t.token == ';')
first = nret = 0;  
else {
nret = explist(ls, &e);  
if (hasmultret(e.k)) {
luaK_setmultret(fs, &e);
if (e.k == VCALL && nret == 1) {  
SET_OPCODE(getinstruction(fs,&e), OP_TAILCALL);
lua_assert(GETARG_A(getinstruction(fs,&e)) == fs->nactvar);
}
first = fs->nactvar;
nret = LUA_MULTRET;  
}
else {
if (nret == 1)  
first = luaK_exp2anyreg(fs, &e);
else {
luaK_exp2nextreg(fs, &e);  
first = fs->nactvar;  
lua_assert(nret == fs->freereg - first);
}
}
}
luaK_ret(fs, first, nret);
testnext(ls, ';');  
}
static void statement (LexState *ls) {
int line = ls->linenumber;  
enterlevel(ls);
switch (ls->t.token) {
case ';': {  
luaX_next(ls);  
break;
}
case TK_IF: {  
ifstat(ls, line);
break;
}
case TK_WHILE: {  
whilestat(ls, line);
break;
}
case TK_DO: {  
luaX_next(ls);  
block(ls);
check_match(ls, TK_END, TK_DO, line);
break;
}
case TK_FOR: {  
forstat(ls, line);
break;
}
case TK_REPEAT: {  
repeatstat(ls, line);
break;
}
case TK_FUNCTION: {  
funcstat(ls, line);
break;
}
case TK_LOCAL: {  
luaX_next(ls);  
if (testnext(ls, TK_FUNCTION))  
localfunc(ls);
else
localstat(ls);
break;
}
case TK_DBCOLON: {  
luaX_next(ls);  
labelstat(ls, str_checkname(ls), line);
break;
}
case TK_RETURN: {  
luaX_next(ls);  
retstat(ls);
break;
}
case TK_BREAK:   
case TK_GOTO: {  
gotostat(ls, luaK_jump(ls->fs));
break;
}
default: {  
exprstat(ls);
break;
}
}
lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
ls->fs->freereg >= ls->fs->nactvar);
ls->fs->freereg = ls->fs->nactvar;  
leavelevel(ls);
}
static void mainfunc (LexState *ls, FuncState *fs) {
BlockCnt bl;
expdesc v;
open_func(ls, fs, &bl);
fs->f->is_vararg = 1;  
init_exp(&v, VLOCAL, 0);  
newupvalue(fs, ls->envn, &v);  
luaX_next(ls);  
statlist(ls);  
check(ls, TK_EOS);
close_func(ls);
}
LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
Dyndata *dyd, const char *name, int firstchar) {
LexState lexstate;
FuncState funcstate;
LClosure *cl = luaF_newLclosure(L, 1);  
setclLvalue(L, L->top, cl);  
luaD_inctop(L);
lexstate.h = luaH_new(L);  
sethvalue(L, L->top, lexstate.h);  
luaD_inctop(L);
funcstate.f = cl->p = luaF_newproto(L);
funcstate.f->source = luaS_new(L, name);  
lua_assert(iswhite(funcstate.f));  
lexstate.buff = buff;
lexstate.dyd = dyd;
dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
luaX_setinput(L, &lexstate, z, funcstate.f->source, firstchar);
mainfunc(&lexstate, &funcstate);
lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
L->top--;  
return cl;  
}
#define lstate_c
#define LUA_CORE
#include <stddef.h>
#include <string.h>
#if !defined(LUAI_GCPAUSE)
#define LUAI_GCPAUSE	200  
#endif
#if !defined(LUAI_GCMUL)
#define LUAI_GCMUL	200 
#endif
#if !defined(luai_makeseed)
#include <time.h>
#define luai_makeseed()		cast(unsigned int, time(NULL))
#endif
typedef struct LX {
lu_byte extra_[LUA_EXTRASPACE];
lua_State l;
} LX;
typedef struct LG {
LX l;
global_State g;
} LG;
#define fromstate(L)	(cast(LX *, cast(lu_byte *, (L)) - offsetof(LX, l)))
#define addbuff(b,p,e) \
{ size_t t = cast(size_t, e); \
memcpy(b + p, &t, sizeof(t)); p += sizeof(t); }
static unsigned int makeseed (lua_State *L) {
char buff[4 * sizeof(size_t)];
unsigned int h = luai_makeseed();
int p = 0;
addbuff(buff, p, L);  
addbuff(buff, p, &h);  
addbuff(buff, p, luaO_nilobject);  
addbuff(buff, p, &lua_newstate);  
lua_assert(p == sizeof(buff));
return luaS_hash(buff, p, h);
}
void luaE_setdebt (global_State *g, l_mem debt) {
l_mem tb = gettotalbytes(g);
lua_assert(tb > 0);
if (debt < tb - MAX_LMEM)
debt = tb - MAX_LMEM;  
g->totalbytes = tb - debt;
g->GCdebt = debt;
}
CallInfo *luaE_extendCI (lua_State *L) {
CallInfo *ci = luaM_new(L, CallInfo);
lua_assert(L->ci->next == NULL);
L->ci->next = ci;
ci->previous = L->ci;
ci->next = NULL;
L->nci++;
return ci;
}
void luaE_freeCI (lua_State *L) {
CallInfo *ci = L->ci;
CallInfo *next = ci->next;
ci->next = NULL;
while ((ci = next) != NULL) {
next = ci->next;
luaM_free(L, ci);
L->nci--;
}
}
void luaE_shrinkCI (lua_State *L) {
CallInfo *ci = L->ci;
CallInfo *next2;  
while (ci->next != NULL && (next2 = ci->next->next) != NULL) {
luaM_free(L, ci->next);  
L->nci--;
ci->next = next2;  
next2->previous = ci;
ci = next2;  
}
}
static void stack_init (lua_State *L1, lua_State *L) {
int i; CallInfo *ci;
L1->stack = luaM_newvector(L, BASIC_STACK_SIZE, TValue);
L1->stacksize = BASIC_STACK_SIZE;
for (i = 0; i < BASIC_STACK_SIZE; i++)
setnilvalue(L1->stack + i);  
L1->top = L1->stack;
L1->stack_last = L1->stack + L1->stacksize - EXTRA_STACK;
ci = &L1->base_ci;
ci->next = ci->previous = NULL;
ci->callstatus = 0;
ci->func = L1->top;
setnilvalue(L1->top++);  
ci->top = L1->top + LUA_MINSTACK;
L1->ci = ci;
}
static void freestack (lua_State *L) {
if (L->stack == NULL)
return;  
L->ci = &L->base_ci;  
luaE_freeCI(L);
lua_assert(L->nci == 0);
luaM_freearray(L, L->stack, L->stacksize);  
}
static void init_registry (lua_State *L, global_State *g) {
TValue temp;
Table *registry = luaH_new(L);
sethvalue(L, &g->l_registry, registry);
luaH_resize(L, registry, LUA_RIDX_LAST, 0);
setthvalue(L, &temp, L);  
luaH_setint(L, registry, LUA_RIDX_MAINTHREAD, &temp);
sethvalue(L, &temp, luaH_new(L));  
luaH_setint(L, registry, LUA_RIDX_GLOBALS, &temp);
}
static void f_luaopen (lua_State *L, void *ud) {
global_State *g = G(L);
UNUSED(ud);
stack_init(L, L);  
init_registry(L, g);
luaS_init(L);
luaT_init(L);
luaX_init(L);
g->gcrunning = 1;  
g->version = lua_version(NULL);
luai_userstateopen(L);
}
static void preinit_thread (lua_State *L, global_State *g) {
G(L) = g;
L->stack = NULL;
L->ci = NULL;
L->nci = 0;
L->stacksize = 0;
L->twups = L;  
L->errorJmp = NULL;
L->nCcalls = 0;
L->hook = NULL;
L->hookmask = 0;
L->basehookcount = 0;
L->allowhook = 1;
resethookcount(L);
L->openupval = NULL;
L->nny = 1;
L->status = LUA_OK;
L->errfunc = 0;
}
static void close_state (lua_State *L) {
global_State *g = G(L);
luaF_close(L, L->stack);  
luaC_freeallobjects(L);  
if (g->version)  
luai_userstateclose(L);
luaM_freearray(L, G(L)->strt.hash, G(L)->strt.size);
freestack(L);
lua_assert(gettotalbytes(g) == sizeof(LG));
(*g->frealloc)(g->ud, fromstate(L), sizeof(LG), 0);  
}
LUA_API lua_State *lua_newthread (lua_State *L) {
global_State *g = G(L);
lua_State *L1;
lua_lock(L);
luaC_checkGC(L);
L1 = &cast(LX *, luaM_newobject(L, LUA_TTHREAD, sizeof(LX)))->l;
L1->marked = luaC_white(g);
L1->tt = LUA_TTHREAD;
L1->next = g->allgc;
g->allgc = obj2gco(L1);
setthvalue(L, L->top, L1);
api_incr_top(L);
preinit_thread(L1, g);
L1->hookmask = L->hookmask;
L1->basehookcount = L->basehookcount;
L1->hook = L->hook;
resethookcount(L1);
memcpy(lua_getextraspace(L1), lua_getextraspace(g->mainthread),
LUA_EXTRASPACE);
luai_userstatethread(L, L1);
stack_init(L1, L);  
lua_unlock(L);
return L1;
}
void luaE_freethread (lua_State *L, lua_State *L1) {
LX *l = fromstate(L1);
luaF_close(L1, L1->stack);  
lua_assert(L1->openupval == NULL);
luai_userstatefree(L, L1);
freestack(L1);
luaM_free(L, l);
}
LUA_API lua_State *lua_newstate (lua_Alloc f, void *ud) {
int i;
lua_State *L;
global_State *g;
LG *l = cast(LG *, (*f)(ud, NULL, LUA_TTHREAD, sizeof(LG)));
if (l == NULL) return NULL;
L = &l->l.l;
g = &l->g;
L->next = NULL;
L->tt = LUA_TTHREAD;
g->currentwhite = bitmask(WHITE0BIT);
L->marked = luaC_white(g);
preinit_thread(L, g);
g->frealloc = f;
g->ud = ud;
g->mainthread = L;
g->seed = makeseed(L);
g->gcrunning = 0;  
g->GCestimate = 0;
g->strt.size = g->strt.nuse = 0;
g->strt.hash = NULL;
setnilvalue(&g->l_registry);
g->panic = NULL;
g->version = NULL;
g->gcstate = GCSpause;
g->gckind = KGC_NORMAL;
g->allgc = g->finobj = g->tobefnz = g->fixedgc = NULL;
g->sweepgc = NULL;
g->gray = g->grayagain = NULL;
g->weak = g->ephemeron = g->allweak = NULL;
g->twups = NULL;
g->totalbytes = sizeof(LG);
g->GCdebt = 0;
g->gcfinnum = 0;
g->gcpause = LUAI_GCPAUSE;
g->gcstepmul = LUAI_GCMUL;
for (i=0; i < LUA_NUMTAGS; i++) g->mt[i] = NULL;
if (luaD_rawrunprotected(L, f_luaopen, NULL) != LUA_OK) {
close_state(L);
L = NULL;
}
return L;
}
LUA_API void lua_close (lua_State *L) {
L = G(L)->mainthread;  
lua_lock(L);
close_state(L);
}
#define lstring_c
#define LUA_CORE
#include <string.h>
#define MEMERRMSG       "not enough memory"
#if !defined(LUAI_HASHLIMIT)
#define LUAI_HASHLIMIT		5
#endif
int luaS_eqlngstr (TString *a, TString *b) {
size_t len = a->u.lnglen;
lua_assert(a->tt == LUA_TLNGSTR && b->tt == LUA_TLNGSTR);
return (a == b) ||  
((len == b->u.lnglen) &&  
(memcmp(getstr(a), getstr(b), len) == 0));  
}
unsigned int luaS_hash (const char *str, size_t l, unsigned int seed) {
unsigned int h = seed ^ cast(unsigned int, l);
size_t step = (l >> LUAI_HASHLIMIT) + 1;
for (; l >= step; l -= step)
h ^= ((h<<5) + (h>>2) + cast_byte(str[l - 1]));
return h;
}
unsigned int luaS_hashlongstr (TString *ts) {
lua_assert(ts->tt == LUA_TLNGSTR);
if (ts->extra == 0) {  
ts->hash = luaS_hash(getstr(ts), ts->u.lnglen, ts->hash);
ts->extra = 1;  
}
return ts->hash;
}
void luaS_resize (lua_State *L, int newsize) {
int i;
stringtable *tb = &G(L)->strt;
if (newsize > tb->size) {  
luaM_reallocvector(L, tb->hash, tb->size, newsize, TString *);
for (i = tb->size; i < newsize; i++)
tb->hash[i] = NULL;
}
for (i = 0; i < tb->size; i++) {  
TString *p = tb->hash[i];
tb->hash[i] = NULL;
while (p) {  
TString *hnext = p->u.hnext;  
unsigned int h = lmod(p->hash, newsize);  
p->u.hnext = tb->hash[h];  
tb->hash[h] = p;
p = hnext;
}
}
if (newsize < tb->size) {  
lua_assert(tb->hash[newsize] == NULL && tb->hash[tb->size - 1] == NULL);
luaM_reallocvector(L, tb->hash, tb->size, newsize, TString *);
}
tb->size = newsize;
}
void luaS_clearcache (global_State *g) {
int i, j;
for (i = 0; i < STRCACHE_N; i++)
for (j = 0; j < STRCACHE_M; j++) {
if (iswhite(g->strcache[i][j]))  
g->strcache[i][j] = g->memerrmsg;  
}
}
void luaS_init (lua_State *L) {
global_State *g = G(L);
int i, j;
luaS_resize(L, MINSTRTABSIZE);  
g->memerrmsg = luaS_newliteral(L, MEMERRMSG);
luaC_fix(L, obj2gco(g->memerrmsg));  
for (i = 0; i < STRCACHE_N; i++)  
for (j = 0; j < STRCACHE_M; j++)
g->strcache[i][j] = g->memerrmsg;
}
static TString *createstrobj (lua_State *L, size_t l, int tag, unsigned int h) {
TString *ts;
GCObject *o;
size_t totalsize;  
totalsize = sizelstring(l);
o = luaC_newobj(L, tag, totalsize);
ts = gco2ts(o);
ts->hash = h;
ts->extra = 0;
getstr(ts)[l] = '\0';  
return ts;
}
TString *luaS_createlngstrobj (lua_State *L, size_t l) {
TString *ts = createstrobj(L, l, LUA_TLNGSTR, G(L)->seed);
ts->u.lnglen = l;
return ts;
}
void luaS_remove (lua_State *L, TString *ts) {
stringtable *tb = &G(L)->strt;
TString **p = &tb->hash[lmod(ts->hash, tb->size)];
while (*p != ts)  
p = &(*p)->u.hnext;
*p = (*p)->u.hnext;  
tb->nuse--;
}
static TString *internshrstr (lua_State *L, const char *str, size_t l) {
TString *ts;
global_State *g = G(L);
unsigned int h = luaS_hash(str, l, g->seed);
TString **list = &g->strt.hash[lmod(h, g->strt.size)];
lua_assert(str != NULL);  
for (ts = *list; ts != NULL; ts = ts->u.hnext) {
if (l == ts->shrlen &&
(memcmp(str, getstr(ts), l * sizeof(char)) == 0)) {
if (isdead(g, ts))  
changewhite(ts);  
return ts;
}
}
if (g->strt.nuse >= g->strt.size && g->strt.size <= MAX_INT/2) {
luaS_resize(L, g->strt.size * 2);
list = &g->strt.hash[lmod(h, g->strt.size)];  
}
ts = createstrobj(L, l, LUA_TSHRSTR, h);
memcpy(getstr(ts), str, l * sizeof(char));
ts->shrlen = cast_byte(l);
ts->u.hnext = *list;
*list = ts;
g->strt.nuse++;
return ts;
}
TString *luaS_newlstr (lua_State *L, const char *str, size_t l) {
if (l <= LUAI_MAXSHORTLEN)  
return internshrstr(L, str, l);
else {
TString *ts;
if (l >= (MAX_SIZE - sizeof(TString))/sizeof(char))
luaM_toobig(L);
ts = luaS_createlngstrobj(L, l);
memcpy(getstr(ts), str, l * sizeof(char));
return ts;
}
}
TString *luaS_new (lua_State *L, const char *str) {
unsigned int i = point2uint(str) % STRCACHE_N;  
int j;
TString **p = G(L)->strcache[i];
for (j = 0; j < STRCACHE_M; j++) {
if (strcmp(str, getstr(p[j])) == 0)  
return p[j];  
}
for (j = STRCACHE_M - 1; j > 0; j--)
p[j] = p[j - 1];  
p[0] = luaS_newlstr(L, str, strlen(str));
return p[0];
}
Udata *luaS_newudata (lua_State *L, size_t s) {
Udata *u;
GCObject *o;
if (s > MAX_SIZE - sizeof(Udata))
luaM_toobig(L);
o = luaC_newobj(L, LUA_TUSERDATA, sizeludata(s));
u = gco2u(o);
u->len = s;
u->metatable = NULL;
setuservalue(L, u, luaO_nilobject);
return u;
}
#define lstrlib_c
#define LUA_LIB
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(LUA_MAXCAPTURES)
#define LUA_MAXCAPTURES		32
#endif
#define uchar(c)	((unsigned char)(c))
#define MAX_SIZET	((size_t)(~(size_t)0))
#define MAXSIZE  \
(sizeof(size_t) < sizeof(int) ? MAX_SIZET : (size_t)(INT_MAX))
static int str_len (lua_State *L) {
size_t l;
luaL_checklstring(L, 1, &l);
lua_pushinteger(L, (lua_Integer)l);
return 1;
}
static lua_Integer posrelat (lua_Integer pos, size_t len) {
if (pos >= 0) return pos;
else if (0u - (size_t)pos > len) return 0;
else return (lua_Integer)len + pos + 1;
}
static int str_sub (lua_State *L) {
size_t l;
const char *s = luaL_checklstring(L, 1, &l);
lua_Integer start = posrelat(luaL_checkinteger(L, 2), l);
lua_Integer end = posrelat(luaL_optinteger(L, 3, -1), l);
if (start < 1) start = 1;
if (end > (lua_Integer)l) end = l;
if (start <= end)
lua_pushlstring(L, s + start - 1, (size_t)(end - start) + 1);
else lua_pushliteral(L, "");
return 1;
}
static int str_reverse (lua_State *L) {
size_t l, i;
luaL_Buffer b;
const char *s = luaL_checklstring(L, 1, &l);
char *p = luaL_buffinitsize(L, &b, l);
for (i = 0; i < l; i++)
p[i] = s[l - i - 1];
luaL_pushresultsize(&b, l);
return 1;
}
static int str_lower (lua_State *L) {
size_t l;
size_t i;
luaL_Buffer b;
const char *s = luaL_checklstring(L, 1, &l);
char *p = luaL_buffinitsize(L, &b, l);
for (i=0; i<l; i++)
p[i] = tolower(uchar(s[i]));
luaL_pushresultsize(&b, l);
return 1;
}
static int str_upper (lua_State *L) {
size_t l;
size_t i;
luaL_Buffer b;
const char *s = luaL_checklstring(L, 1, &l);
char *p = luaL_buffinitsize(L, &b, l);
for (i=0; i<l; i++)
p[i] = toupper(uchar(s[i]));
luaL_pushresultsize(&b, l);
return 1;
}
static int str_rep (lua_State *L) {
size_t l, lsep;
const char *s = luaL_checklstring(L, 1, &l);
lua_Integer n = luaL_checkinteger(L, 2);
const char *sep = luaL_optlstring(L, 3, "", &lsep);
if (n <= 0) lua_pushliteral(L, "");
else if (l + lsep < l || l + lsep > MAXSIZE / n)  
return luaL_error(L, "resulting string too large");
else {
size_t totallen = (size_t)n * l + (size_t)(n - 1) * lsep;
luaL_Buffer b;
char *p = luaL_buffinitsize(L, &b, totallen);
while (n-- > 1) {  
memcpy(p, s, l * sizeof(char)); p += l;
if (lsep > 0) {  
memcpy(p, sep, lsep * sizeof(char));
p += lsep;
}
}
memcpy(p, s, l * sizeof(char));  
luaL_pushresultsize(&b, totallen);
}
return 1;
}
static int str_byte (lua_State *L) {
size_t l;
const char *s = luaL_checklstring(L, 1, &l);
lua_Integer posi = posrelat(luaL_optinteger(L, 2, 1), l);
lua_Integer pose = posrelat(luaL_optinteger(L, 3, posi), l);
int n, i;
if (posi < 1) posi = 1;
if (pose > (lua_Integer)l) pose = l;
if (posi > pose) return 0;  
if (pose - posi >= INT_MAX)  
return luaL_error(L, "string slice too long");
n = (int)(pose -  posi) + 1;
luaL_checkstack(L, n, "string slice too long");
for (i=0; i<n; i++)
lua_pushinteger(L, uchar(s[posi+i-1]));
return n;
}
static int str_char (lua_State *L) {
int n = lua_gettop(L);  
int i;
luaL_Buffer b;
char *p = luaL_buffinitsize(L, &b, n);
for (i=1; i<=n; i++) {
lua_Integer c = luaL_checkinteger(L, i);
luaL_argcheck(L, uchar(c) == c, i, "value out of range");
p[i - 1] = uchar(c);
}
luaL_pushresultsize(&b, n);
return 1;
}
static int writer (lua_State *L, const void *b, size_t size, void *B) {
(void)L;
luaL_addlstring((luaL_Buffer *) B, (const char *)b, size);
return 0;
}
static int str_dump (lua_State *L) {
luaL_Buffer b;
int strip = lua_toboolean(L, 2);
luaL_checktype(L, 1, LUA_TFUNCTION);
lua_settop(L, 1);
luaL_buffinit(L,&b);
if (lua_dump(L, writer, &b, strip) != 0)
return luaL_error(L, "unable to dump given function");
luaL_pushresult(&b);
return 1;
}
#define CAP_UNFINISHED	(-1)
#define CAP_POSITION	(-2)
typedef struct MatchState {
const char *src_init;  
const char *src_end;  
const char *p_end;  
lua_State *L;
int matchdepth;  
unsigned char level;  
struct {
const char *init;
ptrdiff_t len;
} capture[LUA_MAXCAPTURES];
} MatchState;
static const char *match (MatchState *ms, const char *s, const char *p);
#if !defined(MAXCCALLS)
#define MAXCCALLS	200
#endif
#define L_ESC		'%'
#define SPECIALS	"^$*+?.([%-"
static int check_capture (MatchState *ms, int l) {
l -= '1';
if (l < 0 || l >= ms->level || ms->capture[l].len == CAP_UNFINISHED)
return luaL_error(ms->L, "invalid capture index %%%d", l + 1);
return l;
}
static int capture_to_close (MatchState *ms) {
int level = ms->level;
for (level--; level>=0; level--)
if (ms->capture[level].len == CAP_UNFINISHED) return level;
return luaL_error(ms->L, "invalid pattern capture");
}
static const char *classend (MatchState *ms, const char *p) {
switch (*p++) {
case L_ESC: {
if (p == ms->p_end)
luaL_error(ms->L, "malformed pattern (ends with '%%')");
return p+1;
}
case '[': {
if (*p == '^') p++;
do {  
if (p == ms->p_end)
luaL_error(ms->L, "malformed pattern (missing ']')");
if (*(p++) == L_ESC && p < ms->p_end)
p++;  
} while (*p != ']');
return p+1;
}
default: {
return p;
}
}
}
static int match_class (int c, int cl) {
int res;
switch (tolower(cl)) {
case 'a' : res = isalpha(c); break;
case 'c' : res = iscntrl(c); break;
case 'd' : res = isdigit(c); break;
case 'g' : res = isgraph(c); break;
case 'l' : res = islower(c); break;
case 'p' : res = ispunct(c); break;
case 's' : res = isspace(c); break;
case 'u' : res = isupper(c); break;
case 'w' : res = isalnum(c); break;
case 'x' : res = isxdigit(c); break;
case 'z' : res = (c == 0); break;  
default: return (cl == c);
}
return (islower(cl) ? res : !res);
}
static int matchbracketclass (int c, const char *p, const char *ec) {
int sig = 1;
if (*(p+1) == '^') {
sig = 0;
p++;  
}
while (++p < ec) {
if (*p == L_ESC) {
p++;
if (match_class(c, uchar(*p)))
return sig;
}
else if ((*(p+1) == '-') && (p+2 < ec)) {
p+=2;
if (uchar(*(p-2)) <= c && c <= uchar(*p))
return sig;
}
else if (uchar(*p) == c) return sig;
}
return !sig;
}
static int singlematch (MatchState *ms, const char *s, const char *p,
const char *ep) {
if (s >= ms->src_end)
return 0;
else {
int c = uchar(*s);
switch (*p) {
case '.': return 1;  
case L_ESC: return match_class(c, uchar(*(p+1)));
case '[': return matchbracketclass(c, p, ep-1);
default:  return (uchar(*p) == c);
}
}
}
static const char *matchbalance (MatchState *ms, const char *s,
const char *p) {
if (p >= ms->p_end - 1)
luaL_error(ms->L, "malformed pattern (missing arguments to '%%b')");
if (*s != *p) return NULL;
else {
int b = *p;
int e = *(p+1);
int cont = 1;
while (++s < ms->src_end) {
if (*s == e) {
if (--cont == 0) return s+1;
}
else if (*s == b) cont++;
}
}
return NULL;  
}
static const char *max_expand (MatchState *ms, const char *s,
const char *p, const char *ep) {
ptrdiff_t i = 0;  
while (singlematch(ms, s + i, p, ep))
i++;
while (i>=0) {
const char *res = match(ms, (s+i), ep+1);
if (res) return res;
i--;  
}
return NULL;
}
static const char *min_expand (MatchState *ms, const char *s,
const char *p, const char *ep) {
for (;;) {
const char *res = match(ms, s, ep+1);
if (res != NULL)
return res;
else if (singlematch(ms, s, p, ep))
s++;  
else return NULL;
}
}
static const char *start_capture (MatchState *ms, const char *s,
const char *p, int what) {
const char *res;
int level = ms->level;
if (level >= LUA_MAXCAPTURES) luaL_error(ms->L, "too many captures");
ms->capture[level].init = s;
ms->capture[level].len = what;
ms->level = level+1;
if ((res=match(ms, s, p)) == NULL)  
ms->level--;  
return res;
}
static const char *end_capture (MatchState *ms, const char *s,
const char *p) {
int l = capture_to_close(ms);
const char *res;
ms->capture[l].len = s - ms->capture[l].init;  
if ((res = match(ms, s, p)) == NULL)  
ms->capture[l].len = CAP_UNFINISHED;  
return res;
}
static const char *match_capture (MatchState *ms, const char *s, int l) {
size_t len;
l = check_capture(ms, l);
len = ms->capture[l].len;
if ((size_t)(ms->src_end-s) >= len &&
memcmp(ms->capture[l].init, s, len) == 0)
return s+len;
else return NULL;
}
static const char *match (MatchState *ms, const char *s, const char *p) {
if (ms->matchdepth-- == 0)
luaL_error(ms->L, "pattern too complex");
init: 
if (p != ms->p_end) {  
switch (*p) {
case '(': {  
if (*(p + 1) == ')')  
s = start_capture(ms, s, p + 2, CAP_POSITION);
else
s = start_capture(ms, s, p + 1, CAP_UNFINISHED);
break;
}
case ')': {  
s = end_capture(ms, s, p + 1);
break;
}
case '$': {
if ((p + 1) != ms->p_end)  
goto dflt;  
s = (s == ms->src_end) ? s : NULL;  
break;
}
case L_ESC: {  
switch (*(p + 1)) {
case 'b': {  
s = matchbalance(ms, s, p + 2);
if (s != NULL) {
p += 4; goto init;  
}  
break;
}
case 'f': {  
const char *ep; char previous;
p += 2;
if (*p != '[')
luaL_error(ms->L, "missing '[' after '%%f' in pattern");
ep = classend(ms, p);  
previous = (s == ms->src_init) ? '\0' : *(s - 1);
if (!matchbracketclass(uchar(previous), p, ep - 1) &&
matchbracketclass(uchar(*s), p, ep - 1)) {
p = ep; goto init;  
}
s = NULL;  
break;
}
case '0': case '1': case '2': case '3':
case '4': case '5': case '6': case '7':
case '8': case '9': {  
s = match_capture(ms, s, uchar(*(p + 1)));
if (s != NULL) {
p += 2; goto init;  
}
break;
}
default: goto dflt;
}
break;
}
default: dflt: {  
const char *ep = classend(ms, p);  
if (!singlematch(ms, s, p, ep)) {
if (*ep == '*' || *ep == '?' || *ep == '-') {  
p = ep + 1; goto init;  
}
else  
s = NULL;  
}
else {  
switch (*ep) {  
case '?': {  
const char *res;
if ((res = match(ms, s + 1, ep + 1)) != NULL)
s = res;
else {
p = ep + 1; goto init;  
}
break;
}
case '+':  
s++;  
case '*':  
s = max_expand(ms, s, p, ep);
break;
case '-':  
s = min_expand(ms, s, p, ep);
break;
default:  
s++; p = ep; goto init;  
}
}
break;
}
}
}
ms->matchdepth++;
return s;
}
static const char *lmemfind (const char *s1, size_t l1,
const char *s2, size_t l2) {
if (l2 == 0) return s1;  
else if (l2 > l1) return NULL;  
else {
const char *init;  
l2--;  
l1 = l1-l2;  
while (l1 > 0 && (init = (const char *)memchr(s1, *s2, l1)) != NULL) {
init++;   
if (memcmp(init, s2+1, l2) == 0)
return init-1;
else {  
l1 -= init-s1;
s1 = init;
}
}
return NULL;  
}
}
static void push_onecapture (MatchState *ms, int i, const char *s,
const char *e) {
if (i >= ms->level) {
if (i == 0)  
lua_pushlstring(ms->L, s, e - s);  
else
luaL_error(ms->L, "invalid capture index %%%d", i + 1);
}
else {
ptrdiff_t l = ms->capture[i].len;
if (l == CAP_UNFINISHED) luaL_error(ms->L, "unfinished capture");
if (l == CAP_POSITION)
lua_pushinteger(ms->L, (ms->capture[i].init - ms->src_init) + 1);
else
lua_pushlstring(ms->L, ms->capture[i].init, l);
}
}
static int push_captures (MatchState *ms, const char *s, const char *e) {
int i;
int nlevels = (ms->level == 0 && s) ? 1 : ms->level;
luaL_checkstack(ms->L, nlevels, "too many captures");
for (i = 0; i < nlevels; i++)
push_onecapture(ms, i, s, e);
return nlevels;  
}
static int nospecials (const char *p, size_t l) {
size_t upto = 0;
do {
if (strpbrk(p + upto, SPECIALS))
return 0;  
upto += strlen(p + upto) + 1;  
} while (upto <= l);
return 1;  
}
static void prepstate (MatchState *ms, lua_State *L,
const char *s, size_t ls, const char *p, size_t lp) {
ms->L = L;
ms->matchdepth = MAXCCALLS;
ms->src_init = s;
ms->src_end = s + ls;
ms->p_end = p + lp;
}
static void reprepstate (MatchState *ms) {
ms->level = 0;
lua_assert(ms->matchdepth == MAXCCALLS);
}
static int str_find_aux (lua_State *L, int find) {
size_t ls, lp;
const char *s = luaL_checklstring(L, 1, &ls);
const char *p = luaL_checklstring(L, 2, &lp);
lua_Integer init = posrelat(luaL_optinteger(L, 3, 1), ls);
if (init < 1) init = 1;
else if (init > (lua_Integer)ls + 1) {  
lua_pushnil(L);  
return 1;
}
if (find && (lua_toboolean(L, 4) || nospecials(p, lp))) {
const char *s2 = lmemfind(s + init - 1, ls - (size_t)init + 1, p, lp);
if (s2) {
lua_pushinteger(L, (s2 - s) + 1);
lua_pushinteger(L, (s2 - s) + lp);
return 2;
}
}
else {
MatchState ms;
const char *s1 = s + init - 1;
int anchor = (*p == '^');
if (anchor) {
p++; lp--;  
}
prepstate(&ms, L, s, ls, p, lp);
do {
const char *res;
reprepstate(&ms);
if ((res=match(&ms, s1, p)) != NULL) {
if (find) {
lua_pushinteger(L, (s1 - s) + 1);  
lua_pushinteger(L, res - s);   
return push_captures(&ms, NULL, 0) + 2;
}
else
return push_captures(&ms, s1, res);
}
} while (s1++ < ms.src_end && !anchor);
}
lua_pushnil(L);  
return 1;
}
static int str_find (lua_State *L) {
return str_find_aux(L, 1);
}
static int str_match (lua_State *L) {
return str_find_aux(L, 0);
}
typedef struct GMatchState {
const char *src;  
const char *p;  
const char *lastmatch;  
MatchState ms;  
} GMatchState;
static int gmatch_aux (lua_State *L) {
GMatchState *gm = (GMatchState *)lua_touserdata(L, lua_upvalueindex(3));
const char *src;
gm->ms.L = L;
for (src = gm->src; src <= gm->ms.src_end; src++) {
const char *e;
reprepstate(&gm->ms);
if ((e = match(&gm->ms, src, gm->p)) != NULL && e != gm->lastmatch) {
gm->src = gm->lastmatch = e;
return push_captures(&gm->ms, src, e);
}
}
return 0;  
}
static int gmatch (lua_State *L) {
size_t ls, lp;
const char *s = luaL_checklstring(L, 1, &ls);
const char *p = luaL_checklstring(L, 2, &lp);
GMatchState *gm;
lua_settop(L, 2);  
gm = (GMatchState *)lua_newuserdata(L, sizeof(GMatchState));
prepstate(&gm->ms, L, s, ls, p, lp);
gm->src = s; gm->p = p; gm->lastmatch = NULL;
lua_pushcclosure(L, gmatch_aux, 3);
return 1;
}
static void add_s (MatchState *ms, luaL_Buffer *b, const char *s,
const char *e) {
size_t l, i;
lua_State *L = ms->L;
const char *news = lua_tolstring(L, 3, &l);
for (i = 0; i < l; i++) {
if (news[i] != L_ESC)
luaL_addchar(b, news[i]);
else {
i++;  
if (!isdigit(uchar(news[i]))) {
if (news[i] != L_ESC)
luaL_error(L, "invalid use of '%c' in replacement string", L_ESC);
luaL_addchar(b, news[i]);
}
else if (news[i] == '0')
luaL_addlstring(b, s, e - s);
else {
push_onecapture(ms, news[i] - '1', s, e);
luaL_tolstring(L, -1, NULL);  
lua_remove(L, -2);  
luaL_addvalue(b);  
}
}
}
}
static void add_value (MatchState *ms, luaL_Buffer *b, const char *s,
const char *e, int tr) {
lua_State *L = ms->L;
switch (tr) {
case LUA_TFUNCTION: {
int n;
lua_pushvalue(L, 3);
n = push_captures(ms, s, e);
lua_call(L, n, 1);
break;
}
case LUA_TTABLE: {
push_onecapture(ms, 0, s, e);
lua_gettable(L, 3);
break;
}
default: {  
add_s(ms, b, s, e);
return;
}
}
if (!lua_toboolean(L, -1)) {  
lua_pop(L, 1);
lua_pushlstring(L, s, e - s);  
}
else if (!lua_isstring(L, -1))
luaL_error(L, "invalid replacement value (a %s)", luaL_typename(L, -1));
luaL_addvalue(b);  
}
static int str_gsub (lua_State *L) {
size_t srcl, lp;
const char *src = luaL_checklstring(L, 1, &srcl);  
const char *p = luaL_checklstring(L, 2, &lp);  
const char *lastmatch = NULL;  
int tr = lua_type(L, 3);  
lua_Integer max_s = luaL_optinteger(L, 4, srcl + 1);  
int anchor = (*p == '^');
lua_Integer n = 0;  
MatchState ms;
luaL_Buffer b;
luaL_argcheck(L, tr == LUA_TNUMBER || tr == LUA_TSTRING ||
tr == LUA_TFUNCTION || tr == LUA_TTABLE, 3,
"string/function/table expected");
luaL_buffinit(L, &b);
if (anchor) {
p++; lp--;  
}
prepstate(&ms, L, src, srcl, p, lp);
while (n < max_s) {
const char *e;
reprepstate(&ms);  
if ((e = match(&ms, src, p)) != NULL && e != lastmatch) {  
n++;
add_value(&ms, &b, src, e, tr);  
src = lastmatch = e;
}
else if (src < ms.src_end)  
luaL_addchar(&b, *src++);
else break;  
if (anchor) break;
}
luaL_addlstring(&b, src, ms.src_end-src);
luaL_pushresult(&b);
lua_pushinteger(L, n);  
return 2;
}
#if !defined(lua_number2strx)	
#include <math.h>
#define SIZELENMOD	(sizeof(LUA_NUMBER_FRMLEN)/sizeof(char))
#define L_NBFD		((l_mathlim(MANT_DIG) - 1)%4 + 1)
static lua_Number adddigit (char *buff, int n, lua_Number x) {
lua_Number dd = l_mathop(floor)(x);  
int d = (int)dd;
buff[n] = (d < 10 ? d + '0' : d - 10 + 'a');  
return x - dd;  
}
static int num2straux (char *buff, int sz, lua_Number x) {
if (x != x || x == (lua_Number)HUGE_VAL || x == -(lua_Number)HUGE_VAL)
return l_sprintf(buff, sz, LUA_NUMBER_FMT, (LUAI_UACNUMBER)x);
else if (x == 0) {  
return l_sprintf(buff, sz, LUA_NUMBER_FMT "x0p+0", (LUAI_UACNUMBER)x);
}
else {
int e;
lua_Number m = l_mathop(frexp)(x, &e);  
int n = 0;  
if (m < 0) {  
buff[n++] = '-';  
m = -m;  
}
buff[n++] = '0'; buff[n++] = 'x';  
m = adddigit(buff, n++, m * (1 << L_NBFD));  
e -= L_NBFD;  
if (m > 0) {  
buff[n++] = lua_getlocaledecpoint();  
do {  
m = adddigit(buff, n++, m * 16);
} while (m > 0);
}
n += l_sprintf(buff + n, sz - n, "p%+d", e);  
lua_assert(n < sz);
return n;
}
}
static int lua_number2strx (lua_State *L, char *buff, int sz,
const char *fmt, lua_Number x) {
int n = num2straux(buff, sz, x);
if (fmt[SIZELENMOD] == 'A') {
int i;
for (i = 0; i < n; i++)
buff[i] = toupper(uchar(buff[i]));
}
else if (fmt[SIZELENMOD] != 'a')
return luaL_error(L, "modifiers for format '%%a'/'%%A' not implemented");
return n;
}
#endif				
#define MAX_ITEM        (120 + l_mathlim(MAX_10_EXP))
#define FLAGS	"-+ #0"
#define MAX_FORMAT	32
static void addquoted (luaL_Buffer *b, const char *s, size_t len) {
luaL_addchar(b, '"');
while (len--) {
if (*s == '"' || *s == '\\' || *s == '\n') {
luaL_addchar(b, '\\');
luaL_addchar(b, *s);
}
else if (iscntrl(uchar(*s))) {
char buff[10];
if (!isdigit(uchar(*(s+1))))
l_sprintf(buff, sizeof(buff), "\\%d", (int)uchar(*s));
else
l_sprintf(buff, sizeof(buff), "\\%03d", (int)uchar(*s));
luaL_addstring(b, buff);
}
else
luaL_addchar(b, *s);
s++;
}
luaL_addchar(b, '"');
}
static void checkdp (char *buff, int nb) {
if (memchr(buff, '.', nb) == NULL) {  
char point = lua_getlocaledecpoint();  
char *ppoint = (char *)memchr(buff, point, nb);
if (ppoint) *ppoint = '.';  
}
}
static void addliteral (lua_State *L, luaL_Buffer *b, int arg) {
switch (lua_type(L, arg)) {
case LUA_TSTRING: {
size_t len;
const char *s = lua_tolstring(L, arg, &len);
addquoted(b, s, len);
break;
}
case LUA_TNUMBER: {
char *buff = luaL_prepbuffsize(b, MAX_ITEM);
int nb;
if (!lua_isinteger(L, arg)) {  
lua_Number n = lua_tonumber(L, arg);  
nb = lua_number2strx(L, buff, MAX_ITEM, "%" LUA_NUMBER_FRMLEN "a", n);
checkdp(buff, nb);  
}
else {  
lua_Integer n = lua_tointeger(L, arg);
const char *format = (n == LUA_MININTEGER)  
? "0x%" LUA_INTEGER_FRMLEN "x"  
: LUA_INTEGER_FMT;  
nb = l_sprintf(buff, MAX_ITEM, format, (LUAI_UACINT)n);
}
luaL_addsize(b, nb);
break;
}
case LUA_TNIL: case LUA_TBOOLEAN: {
luaL_tolstring(L, arg, NULL);
luaL_addvalue(b);
break;
}
default: {
luaL_argerror(L, arg, "value has no literal form");
}
}
}
static const char *scanformat (lua_State *L, const char *strfrmt, char *form) {
const char *p = strfrmt;
while (*p != '\0' && strchr(FLAGS, *p) != NULL) p++;  
if ((size_t)(p - strfrmt) >= sizeof(FLAGS)/sizeof(char))
luaL_error(L, "invalid format (repeated flags)");
if (isdigit(uchar(*p))) p++;  
if (isdigit(uchar(*p))) p++;  
if (*p == '.') {
p++;
if (isdigit(uchar(*p))) p++;  
if (isdigit(uchar(*p))) p++;  
}
if (isdigit(uchar(*p)))
luaL_error(L, "invalid format (width or precision too long)");
*(form++) = '%';
memcpy(form, strfrmt, ((p - strfrmt) + 1) * sizeof(char));
form += (p - strfrmt) + 1;
*form = '\0';
return p;
}
static void addlenmod (char *form, const char *lenmod) {
size_t l = strlen(form);
size_t lm = strlen(lenmod);
char spec = form[l - 1];
strcpy(form + l - 1, lenmod);
form[l + lm - 1] = spec;
form[l + lm] = '\0';
}
static int str_format (lua_State *L) {
int top = lua_gettop(L);
int arg = 1;
size_t sfl;
const char *strfrmt = luaL_checklstring(L, arg, &sfl);
const char *strfrmt_end = strfrmt+sfl;
luaL_Buffer b;
luaL_buffinit(L, &b);
while (strfrmt < strfrmt_end) {
if (*strfrmt != L_ESC)
luaL_addchar(&b, *strfrmt++);
else if (*++strfrmt == L_ESC)
luaL_addchar(&b, *strfrmt++);  
else { 
char form[MAX_FORMAT];  
char *buff = luaL_prepbuffsize(&b, MAX_ITEM);  
int nb = 0;  
if (++arg > top)
luaL_argerror(L, arg, "no value");
strfrmt = scanformat(L, strfrmt, form);
switch (*strfrmt++) {
case 'c': {
nb = l_sprintf(buff, MAX_ITEM, form, (int)luaL_checkinteger(L, arg));
break;
}
case 'd': case 'i':
case 'o': case 'u': case 'x': case 'X': {
lua_Integer n = luaL_checkinteger(L, arg);
addlenmod(form, LUA_INTEGER_FRMLEN);
nb = l_sprintf(buff, MAX_ITEM, form, (LUAI_UACINT)n);
break;
}
case 'a': case 'A':
addlenmod(form, LUA_NUMBER_FRMLEN);
nb = lua_number2strx(L, buff, MAX_ITEM, form,
luaL_checknumber(L, arg));
break;
case 'e': case 'E': case 'f':
case 'g': case 'G': {
lua_Number n = luaL_checknumber(L, arg);
addlenmod(form, LUA_NUMBER_FRMLEN);
nb = l_sprintf(buff, MAX_ITEM, form, (LUAI_UACNUMBER)n);
break;
}
case 'q': {
addliteral(L, &b, arg);
break;
}
case 's': {
size_t l;
const char *s = luaL_tolstring(L, arg, &l);
if (form[2] == '\0')  
luaL_addvalue(&b);  
else {
luaL_argcheck(L, l == strlen(s), arg, "string contains zeros");
if (!strchr(form, '.') && l >= 100) {
luaL_addvalue(&b);  
}
else {  
nb = l_sprintf(buff, MAX_ITEM, form, s);
lua_pop(L, 1);  
}
}
break;
}
default: {  
return luaL_error(L, "invalid option '%%%c' to 'format'",
*(strfrmt - 1));
}
}
lua_assert(nb < MAX_ITEM);
luaL_addsize(&b, nb);
}
}
luaL_pushresult(&b);
return 1;
}
#if !defined(LUAL_PACKPADBYTE)
#define LUAL_PACKPADBYTE		0x00
#endif
#define MAXINTSIZE	16
#define NB	CHAR_BIT
#define MC	((1 << NB) - 1)
#define SZINT	((int)sizeof(lua_Integer))
static const union {
int dummy;
char little;  
} nativeendian = {1};
struct cD {
char c;
union { double d; void *p; lua_Integer i; lua_Number n; } u;
};
#define MAXALIGN	(offsetof(struct cD, u))
typedef union Ftypes {
float f;
double d;
lua_Number n;
char buff[5 * sizeof(lua_Number)];  
} Ftypes;
typedef struct Header {
lua_State *L;
int islittle;
int maxalign;
} Header;
typedef enum KOption {
Kint,		
Kuint,	
Kfloat,	
Kchar,	
Kstring,	
Kzstr,	
Kpadding,	
Kpaddalign,	
Knop		
} KOption;
static int digit (int c) { return '0' <= c && c <= '9'; }
static int getnum (const char **fmt, int df) {
if (!digit(**fmt))  
return df;  
else {
int a = 0;
do {
a = a*10 + (*((*fmt)++) - '0');
} while (digit(**fmt) && a <= ((int)MAXSIZE - 9)/10);
return a;
}
}
static int getnumlimit (Header *h, const char **fmt, int df) {
int sz = getnum(fmt, df);
if (sz > MAXINTSIZE || sz <= 0)
return luaL_error(h->L, "integral size (%d) out of limits [1,%d]",
sz, MAXINTSIZE);
return sz;
}
static void initheader (lua_State *L, Header *h) {
h->L = L;
h->islittle = nativeendian.little;
h->maxalign = 1;
}
static KOption getoption (Header *h, const char **fmt, int *size) {
int opt = *((*fmt)++);
*size = 0;  
switch (opt) {
case 'b': *size = sizeof(char); return Kint;
case 'B': *size = sizeof(char); return Kuint;
case 'h': *size = sizeof(short); return Kint;
case 'H': *size = sizeof(short); return Kuint;
case 'l': *size = sizeof(long); return Kint;
case 'L': *size = sizeof(long); return Kuint;
case 'j': *size = sizeof(lua_Integer); return Kint;
case 'J': *size = sizeof(lua_Integer); return Kuint;
case 'T': *size = sizeof(size_t); return Kuint;
case 'f': *size = sizeof(float); return Kfloat;
case 'd': *size = sizeof(double); return Kfloat;
case 'n': *size = sizeof(lua_Number); return Kfloat;
case 'i': *size = getnumlimit(h, fmt, sizeof(int)); return Kint;
case 'I': *size = getnumlimit(h, fmt, sizeof(int)); return Kuint;
case 's': *size = getnumlimit(h, fmt, sizeof(size_t)); return Kstring;
case 'c':
*size = getnum(fmt, -1);
if (*size == -1)
luaL_error(h->L, "missing size for format option 'c'");
return Kchar;
case 'z': return Kzstr;
case 'x': *size = 1; return Kpadding;
case 'X': return Kpaddalign;
case ' ': break;
case '<': h->islittle = 1; break;
case '>': h->islittle = 0; break;
case '=': h->islittle = nativeendian.little; break;
case '!': h->maxalign = getnumlimit(h, fmt, MAXALIGN); break;
default: luaL_error(h->L, "invalid format option '%c'", opt);
}
return Knop;
}
static KOption getdetails (Header *h, size_t totalsize,
const char **fmt, int *psize, int *ntoalign) {
KOption opt = getoption(h, fmt, psize);
int align = *psize;  
if (opt == Kpaddalign) {  
if (**fmt == '\0' || getoption(h, fmt, &align) == Kchar || align == 0)
luaL_argerror(h->L, 1, "invalid next option for option 'X'");
}
if (align <= 1 || opt == Kchar)  
*ntoalign = 0;
else {
if (align > h->maxalign)  
align = h->maxalign;
if ((align & (align - 1)) != 0)  
luaL_argerror(h->L, 1, "format asks for alignment not power of 2");
*ntoalign = (align - (int)(totalsize & (align - 1))) & (align - 1);
}
return opt;
}
static void packint (luaL_Buffer *b, lua_Unsigned n,
int islittle, int size, int neg) {
char *buff = luaL_prepbuffsize(b, size);
int i;
buff[islittle ? 0 : size - 1] = (char)(n & MC);  
for (i = 1; i < size; i++) {
n >>= NB;
buff[islittle ? i : size - 1 - i] = (char)(n & MC);
}
if (neg && size > SZINT) {  
for (i = SZINT; i < size; i++)  
buff[islittle ? i : size - 1 - i] = (char)MC;
}
luaL_addsize(b, size);  
}
static void copywithendian (volatile char *dest, volatile const char *src,
int size, int islittle) {
if (islittle == nativeendian.little) {
while (size-- != 0)
*(dest++) = *(src++);
}
else {
dest += size - 1;
while (size-- != 0)
*(dest--) = *(src++);
}
}
static int str_pack (lua_State *L) {
luaL_Buffer b;
Header h;
const char *fmt = luaL_checkstring(L, 1);  
int arg = 1;  
size_t totalsize = 0;  
initheader(L, &h);
lua_pushnil(L);  
luaL_buffinit(L, &b);
while (*fmt != '\0') {
int size, ntoalign;
KOption opt = getdetails(&h, totalsize, &fmt, &size, &ntoalign);
totalsize += ntoalign + size;
while (ntoalign-- > 0)
luaL_addchar(&b, LUAL_PACKPADBYTE);  
arg++;
switch (opt) {
case Kint: {  
lua_Integer n = luaL_checkinteger(L, arg);
if (size < SZINT) {  
lua_Integer lim = (lua_Integer)1 << ((size * NB) - 1);
luaL_argcheck(L, -lim <= n && n < lim, arg, "integer overflow");
}
packint(&b, (lua_Unsigned)n, h.islittle, size, (n < 0));
break;
}
case Kuint: {  
lua_Integer n = luaL_checkinteger(L, arg);
if (size < SZINT)  
luaL_argcheck(L, (lua_Unsigned)n < ((lua_Unsigned)1 << (size * NB)),
arg, "unsigned overflow");
packint(&b, (lua_Unsigned)n, h.islittle, size, 0);
break;
}
case Kfloat: {  
volatile Ftypes u;
char *buff = luaL_prepbuffsize(&b, size);
lua_Number n = luaL_checknumber(L, arg);  
if (size == sizeof(u.f)) u.f = (float)n;  
else if (size == sizeof(u.d)) u.d = (double)n;
else u.n = n;
copywithendian(buff, u.buff, size, h.islittle);
luaL_addsize(&b, size);
break;
}
case Kchar: {  
size_t len;
const char *s = luaL_checklstring(L, arg, &len);
luaL_argcheck(L, len <= (size_t)size, arg,
"string longer than given size");
luaL_addlstring(&b, s, len);  
while (len++ < (size_t)size)  
luaL_addchar(&b, LUAL_PACKPADBYTE);
break;
}
case Kstring: {  
size_t len;
const char *s = luaL_checklstring(L, arg, &len);
luaL_argcheck(L, size >= (int)sizeof(size_t) ||
len < ((size_t)1 << (size * NB)),
arg, "string length does not fit in given size");
packint(&b, (lua_Unsigned)len, h.islittle, size, 0);  
luaL_addlstring(&b, s, len);
totalsize += len;
break;
}
case Kzstr: {  
size_t len;
const char *s = luaL_checklstring(L, arg, &len);
luaL_argcheck(L, strlen(s) == len, arg, "string contains zeros");
luaL_addlstring(&b, s, len);
luaL_addchar(&b, '\0');  
totalsize += len + 1;
break;
}
case Kpadding: luaL_addchar(&b, LUAL_PACKPADBYTE);  
case Kpaddalign: case Knop:
arg--;  
break;
}
}
luaL_pushresult(&b);
return 1;
}
static int str_packsize (lua_State *L) {
Header h;
const char *fmt = luaL_checkstring(L, 1);  
size_t totalsize = 0;  
initheader(L, &h);
while (*fmt != '\0') {
int size, ntoalign;
KOption opt = getdetails(&h, totalsize, &fmt, &size, &ntoalign);
size += ntoalign;  
luaL_argcheck(L, totalsize <= MAXSIZE - size, 1,
"format result too large");
totalsize += size;
switch (opt) {
case Kstring:  
case Kzstr:    
luaL_argerror(L, 1, "variable-length format");
/* FALLTHROUGH */
default:  break;
}
}
lua_pushinteger(L, (lua_Integer)totalsize);
return 1;
}
static lua_Integer unpackint (lua_State *L, const char *str,
int islittle, int size, int issigned) {
lua_Unsigned res = 0;
int i;
int limit = (size  <= SZINT) ? size : SZINT;
for (i = limit - 1; i >= 0; i--) {
res <<= NB;
res |= (lua_Unsigned)(unsigned char)str[islittle ? i : size - 1 - i];
}
if (size < SZINT) {  
if (issigned) {  
lua_Unsigned mask = (lua_Unsigned)1 << (size*NB - 1);
res = ((res ^ mask) - mask);  
}
}
else if (size > SZINT) {  
int mask = (!issigned || (lua_Integer)res >= 0) ? 0 : MC;
for (i = limit; i < size; i++) {
if ((unsigned char)str[islittle ? i : size - 1 - i] != mask)
luaL_error(L, "%d-byte integer does not fit into Lua Integer", size);
}
}
return (lua_Integer)res;
}
static int str_unpack (lua_State *L) {
Header h;
const char *fmt = luaL_checkstring(L, 1);
size_t ld;
const char *data = luaL_checklstring(L, 2, &ld);
size_t pos = (size_t)posrelat(luaL_optinteger(L, 3, 1), ld) - 1;
int n = 0;  
luaL_argcheck(L, pos <= ld, 3, "initial position out of string");
initheader(L, &h);
while (*fmt != '\0') {
int size, ntoalign;
KOption opt = getdetails(&h, pos, &fmt, &size, &ntoalign);
if ((size_t)ntoalign + size > ~pos || pos + ntoalign + size > ld)
luaL_argerror(L, 2, "data string too short");
pos += ntoalign;  
luaL_checkstack(L, 2, "too many results");
n++;
switch (opt) {
case Kint:
case Kuint: {
lua_Integer res = unpackint(L, data + pos, h.islittle, size,
(opt == Kint));
lua_pushinteger(L, res);
break;
}
case Kfloat: {
volatile Ftypes u;
lua_Number num;
copywithendian(u.buff, data + pos, size, h.islittle);
if (size == sizeof(u.f)) num = (lua_Number)u.f;
else if (size == sizeof(u.d)) num = (lua_Number)u.d;
else num = u.n;
lua_pushnumber(L, num);
break;
}
case Kchar: {
lua_pushlstring(L, data + pos, size);
break;
}
case Kstring: {
size_t len = (size_t)unpackint(L, data + pos, h.islittle, size, 0);
luaL_argcheck(L, pos + len + size <= ld, 2, "data string too short");
lua_pushlstring(L, data + pos + size, len);
pos += len;  
break;
}
case Kzstr: {
size_t len = (int)strlen(data + pos);
lua_pushlstring(L, data + pos, len);
pos += len + 1;  
break;
}
case Kpaddalign: case Kpadding: case Knop:
n--;  
break;
}
pos += size;
}
lua_pushinteger(L, pos + 1);  
return n + 1;
}
static const luaL_Reg strlib[] = {
{"byte", str_byte},
{"char", str_char},
{"dump", str_dump},
{"find", str_find},
{"format", str_format},
{"gmatch", gmatch},
{"gsub", str_gsub},
{"len", str_len},
{"lower", str_lower},
{"match", str_match},
{"rep", str_rep},
{"reverse", str_reverse},
{"sub", str_sub},
{"upper", str_upper},
{"pack", str_pack},
{"packsize", str_packsize},
{"unpack", str_unpack},
{NULL, NULL}
};
static void createmetatable (lua_State *L) {
lua_createtable(L, 0, 1);  
lua_pushliteral(L, "");  
lua_pushvalue(L, -2);  
lua_setmetatable(L, -2);  
lua_pop(L, 1);  
lua_pushvalue(L, -2);  
lua_setfield(L, -2, "__index");  
lua_pop(L, 1);  
}
LUAMOD_API int luaopen_string (lua_State *L) {
luaL_newlib(L, strlib);
createmetatable(L);
return 1;
}
#define ltable_c
#define LUA_CORE
#include <math.h>
#include <limits.h>
#define MAXABITS	cast_int(sizeof(int) * CHAR_BIT - 1)
#define MAXASIZE	(1u << MAXABITS)
#define MAXHBITS	(MAXABITS - 1)
#define hashpow2(t,n)		(gnode(t, lmod((n), sizenode(t))))
#define hashstr(t,str)		hashpow2(t, (str)->hash)
#define hashboolean(t,p)	hashpow2(t, p)
#define hashint(t,i)		hashpow2(t, i)
#define hashmod(t,n)	(gnode(t, ((n) % ((sizenode(t)-1)|1))))
#define hashpointer(t,p)	hashmod(t, point2uint(p))
#define dummynode		(&dummynode_)
static const Node dummynode_ = {
{NILCONSTANT},  
{{NILCONSTANT, 0}}  
};
#if !defined(l_hashfloat)
static int l_hashfloat (lua_Number n) {
int i;
lua_Integer ni;
n = l_mathop(frexp)(n, &i) * -cast_num(INT_MIN);
if (!lua_numbertointeger(n, &ni)) {  
lua_assert(luai_numisnan(n) || l_mathop(fabs)(n) == cast_num(HUGE_VAL));
return 0;
}
else {  
unsigned int u = cast(unsigned int, i) + cast(unsigned int, ni);
return cast_int(u <= cast(unsigned int, INT_MAX) ? u : ~u);
}
}
#endif
static Node *mainposition (const Table *t, const TValue *key) {
switch (ttype(key)) {
case LUA_TNUMINT:
return hashint(t, ivalue(key));
case LUA_TNUMFLT:
return hashmod(t, l_hashfloat(fltvalue(key)));
case LUA_TSHRSTR:
return hashstr(t, tsvalue(key));
case LUA_TLNGSTR:
return hashpow2(t, luaS_hashlongstr(tsvalue(key)));
case LUA_TBOOLEAN:
return hashboolean(t, bvalue(key));
case LUA_TLIGHTUSERDATA:
return hashpointer(t, pvalue(key));
case LUA_TLCF:
return hashpointer(t, fvalue(key));
default:
lua_assert(!ttisdeadkey(key));
return hashpointer(t, gcvalue(key));
}
}
static unsigned int arrayindex (const TValue *key) {
if (ttisinteger(key)) {
lua_Integer k = ivalue(key);
if (0 < k && (lua_Unsigned)k <= MAXASIZE)
return cast(unsigned int, k);  
}
return 0;  
}
static unsigned int findindex (lua_State *L, Table *t, StkId key) {
unsigned int i;
if (ttisnil(key)) return 0;  
i = arrayindex(key);
if (i != 0 && i <= t->sizearray)  
return i;  
else {
int nx;
Node *n = mainposition(t, key);
for (;;) {  
if (luaV_rawequalobj(gkey(n), key) ||
(ttisdeadkey(gkey(n)) && iscollectable(key) &&
deadvalue(gkey(n)) == gcvalue(key))) {
i = cast_int(n - gnode(t, 0));  
return (i + 1) + t->sizearray;
}
nx = gnext(n);
if (nx == 0)
luaG_runerror(L, "invalid key to 'next'");  
else n += nx;
}
}
}
int luaH_next (lua_State *L, Table *t, StkId key) {
unsigned int i = findindex(L, t, key);  
for (; i < t->sizearray; i++) {  
if (!ttisnil(&t->array[i])) {  
setivalue(key, i + 1);
setobj2s(L, key+1, &t->array[i]);
return 1;
}
}
for (i -= t->sizearray; cast_int(i) < sizenode(t); i++) {  
if (!ttisnil(gval(gnode(t, i)))) {  
setobj2s(L, key, gkey(gnode(t, i)));
setobj2s(L, key+1, gval(gnode(t, i)));
return 1;
}
}
return 0;  
}
static unsigned int computesizes (unsigned int nums[], unsigned int *pna) {
int i;
unsigned int twotoi;  
unsigned int a = 0;  
unsigned int na = 0;  
unsigned int optimal = 0;  
for (i = 0, twotoi = 1;
twotoi > 0 && *pna > twotoi / 2;
i++, twotoi *= 2) {
if (nums[i] > 0) {
a += nums[i];
if (a > twotoi/2) {  
optimal = twotoi;  
na = a;  
}
}
}
lua_assert((optimal == 0 || optimal / 2 < na) && na <= optimal);
*pna = na;
return optimal;
}
static int countint (const TValue *key, unsigned int *nums) {
unsigned int k = arrayindex(key);
if (k != 0) {  
nums[luaO_ceillog2(k)]++;  
return 1;
}
else
return 0;
}
static unsigned int numusearray (const Table *t, unsigned int *nums) {
int lg;
unsigned int ttlg;  
unsigned int ause = 0;  
unsigned int i = 1;  
for (lg = 0, ttlg = 1; lg <= MAXABITS; lg++, ttlg *= 2) {
unsigned int lc = 0;  
unsigned int lim = ttlg;
if (lim > t->sizearray) {
lim = t->sizearray;  
if (i > lim)
break;  
}
for (; i <= lim; i++) {
if (!ttisnil(&t->array[i-1]))
lc++;
}
nums[lg] += lc;
ause += lc;
}
return ause;
}
static int numusehash (const Table *t, unsigned int *nums, unsigned int *pna) {
int totaluse = 0;  
int ause = 0;  
int i = sizenode(t);
while (i--) {
Node *n = &t->node[i];
if (!ttisnil(gval(n))) {
ause += countint(gkey(n), nums);
totaluse++;
}
}
*pna += ause;
return totaluse;
}
static void setarrayvector (lua_State *L, Table *t, unsigned int size) {
unsigned int i;
luaM_reallocvector(L, t->array, t->sizearray, size, TValue);
for (i=t->sizearray; i<size; i++)
setnilvalue(&t->array[i]);
t->sizearray = size;
}
static void setnodevector (lua_State *L, Table *t, unsigned int size) {
if (size == 0) {  
t->node = cast(Node *, dummynode);  
t->lsizenode = 0;
t->lastfree = NULL;  
}
else {
int i;
int lsize = luaO_ceillog2(size);
if (lsize > MAXHBITS)
luaG_runerror(L, "table overflow");
size = twoto(lsize);
t->node = luaM_newvector(L, size, Node);
for (i = 0; i < (int)size; i++) {
Node *n = gnode(t, i);
gnext(n) = 0;
setnilvalue(wgkey(n));
setnilvalue(gval(n));
}
t->lsizenode = cast_byte(lsize);
t->lastfree = gnode(t, size);  
}
}
typedef struct {
Table *t;
unsigned int nhsize;
} AuxsetnodeT;
static void auxsetnode (lua_State *L, void *ud) {
AuxsetnodeT *asn = cast(AuxsetnodeT *, ud);
setnodevector(L, asn->t, asn->nhsize);
}
void luaH_resize (lua_State *L, Table *t, unsigned int nasize,
unsigned int nhsize) {
unsigned int i;
int j;
AuxsetnodeT asn;
unsigned int oldasize = t->sizearray;
int oldhsize = allocsizenode(t);
Node *nold = t->node;  
if (nasize > oldasize)  
setarrayvector(L, t, nasize);
asn.t = t; asn.nhsize = nhsize;
if (luaD_rawrunprotected(L, auxsetnode, &asn) != LUA_OK) {  
setarrayvector(L, t, oldasize);  
luaD_throw(L, LUA_ERRMEM);  
}
if (nasize < oldasize) {  
t->sizearray = nasize;
for (i=nasize; i<oldasize; i++) {
if (!ttisnil(&t->array[i]))
luaH_setint(L, t, i + 1, &t->array[i]);
}
luaM_reallocvector(L, t->array, oldasize, nasize, TValue);
}
for (j = oldhsize - 1; j >= 0; j--) {
Node *old = nold + j;
if (!ttisnil(gval(old))) {
/* doesn't need barrier/invalidate cache, as entry was
already present in the table */
setobjt2t(L, luaH_set(L, t, gkey(old)), gval(old));
}
}
if (oldhsize > 0)  
luaM_freearray(L, nold, cast(size_t, oldhsize)); 
}
void luaH_resizearray (lua_State *L, Table *t, unsigned int nasize) {
int nsize = allocsizenode(t);
luaH_resize(L, t, nasize, nsize);
}
static void rehash (lua_State *L, Table *t, const TValue *ek) {
unsigned int asize;  
unsigned int na;  
unsigned int nums[MAXABITS + 1];
int i;
int totaluse;
for (i = 0; i <= MAXABITS; i++) nums[i] = 0;  
na = numusearray(t, nums);  
totaluse = na;  
totaluse += numusehash(t, nums, &na);  
na += countint(ek, nums);
totaluse++;
asize = computesizes(nums, &na);
luaH_resize(L, t, asize, totaluse - na);
}
Table *luaH_new (lua_State *L) {
GCObject *o = luaC_newobj(L, LUA_TTABLE, sizeof(Table));
Table *t = gco2t(o);
t->metatable = NULL;
t->flags = cast_byte(~0);
t->array = NULL;
t->sizearray = 0;
setnodevector(L, t, 0);
return t;
}
void luaH_free (lua_State *L, Table *t) {
if (!isdummy(t))
luaM_freearray(L, t->node, cast(size_t, sizenode(t)));
luaM_freearray(L, t->array, t->sizearray);
luaM_free(L, t);
}
static Node *getfreepos (Table *t) {
if (!isdummy(t)) {
while (t->lastfree > t->node) {
t->lastfree--;
if (ttisnil(gkey(t->lastfree)))
return t->lastfree;
}
}
return NULL;  
}
TValue *luaH_newkey (lua_State *L, Table *t, const TValue *key) {
Node *mp;
TValue aux;
if (ttisnil(key)) luaG_runerror(L, "table index is nil");
else if (ttisfloat(key)) {
lua_Integer k;
if (luaV_tointeger(key, &k, 0)) {  
setivalue(&aux, k);
key = &aux;  
}
else if (luai_numisnan(fltvalue(key)))
luaG_runerror(L, "table index is NaN");
}
mp = mainposition(t, key);
if (!ttisnil(gval(mp)) || isdummy(t)) {  
Node *othern;
Node *f = getfreepos(t);  
if (f == NULL) {  
rehash(L, t, key);  
return luaH_set(L, t, key);  
}
lua_assert(!isdummy(t));
othern = mainposition(t, gkey(mp));
if (othern != mp) {  
while (othern + gnext(othern) != mp)  
othern += gnext(othern);
gnext(othern) = cast_int(f - othern);  
*f = *mp;  
if (gnext(mp) != 0) {
gnext(f) += cast_int(mp - f);  
gnext(mp) = 0;  
}
setnilvalue(gval(mp));
}
else {  
if (gnext(mp) != 0)
gnext(f) = cast_int((mp + gnext(mp)) - f);  
else lua_assert(gnext(f) == 0);
gnext(mp) = cast_int(f - mp);
mp = f;
}
}
setnodekey(L, &mp->i_key, key);
luaC_barrierback(L, t, key);
lua_assert(ttisnil(gval(mp)));
return gval(mp);
}
const TValue *luaH_getint (Table *t, lua_Integer key) {
if (l_castS2U(key) - 1 < t->sizearray)
return &t->array[key - 1];
else {
Node *n = hashint(t, key);
for (;;) {  
if (ttisinteger(gkey(n)) && ivalue(gkey(n)) == key)
return gval(n);  
else {
int nx = gnext(n);
if (nx == 0) break;
n += nx;
}
}
return luaO_nilobject;
}
}
const TValue *luaH_getshortstr (Table *t, TString *key) {
Node *n = hashstr(t, key);
lua_assert(key->tt == LUA_TSHRSTR);
for (;;) {  
const TValue *k = gkey(n);
if (ttisshrstring(k) && eqshrstr(tsvalue(k), key))
return gval(n);  
else {
int nx = gnext(n);
if (nx == 0)
return luaO_nilobject;  
n += nx;
}
}
}
static const TValue *getgeneric (Table *t, const TValue *key) {
Node *n = mainposition(t, key);
for (;;) {  
if (luaV_rawequalobj(gkey(n), key))
return gval(n);  
else {
int nx = gnext(n);
if (nx == 0)
return luaO_nilobject;  
n += nx;
}
}
}
const TValue *luaH_getstr (Table *t, TString *key) {
if (key->tt == LUA_TSHRSTR)
return luaH_getshortstr(t, key);
else {  
TValue ko;
setsvalue(cast(lua_State *, NULL), &ko, key);
return getgeneric(t, &ko);
}
}
const TValue *luaH_get (Table *t, const TValue *key) {
switch (ttype(key)) {
case LUA_TSHRSTR: return luaH_getshortstr(t, tsvalue(key));
case LUA_TNUMINT: return luaH_getint(t, ivalue(key));
case LUA_TNIL: return luaO_nilobject;
case LUA_TNUMFLT: {
lua_Integer k;
if (luaV_tointeger(key, &k, 0)) 
return luaH_getint(t, k);  
}  
default:
return getgeneric(t, key);
}
}
TValue *luaH_set (lua_State *L, Table *t, const TValue *key) {
const TValue *p = luaH_get(t, key);
if (p != luaO_nilobject)
return cast(TValue *, p);
else return luaH_newkey(L, t, key);
}
void luaH_setint (lua_State *L, Table *t, lua_Integer key, TValue *value) {
const TValue *p = luaH_getint(t, key);
TValue *cell;
if (p != luaO_nilobject)
cell = cast(TValue *, p);
else {
TValue k;
setivalue(&k, key);
cell = luaH_newkey(L, t, &k);
}
setobj2t(L, cell, value);
}
static lua_Unsigned unbound_search (Table *t, lua_Unsigned j) {
lua_Unsigned i = j;  
j++;
while (!ttisnil(luaH_getint(t, j))) {
i = j;
if (j > l_castS2U(LUA_MAXINTEGER) / 2) {  
i = 1;
while (!ttisnil(luaH_getint(t, i))) i++;
return i - 1;
}
j *= 2;
}
while (j - i > 1) {
lua_Unsigned m = (i+j)/2;
if (ttisnil(luaH_getint(t, m))) j = m;
else i = m;
}
return i;
}
lua_Unsigned luaH_getn (Table *t) {
unsigned int j = t->sizearray;
if (j > 0 && ttisnil(&t->array[j - 1])) {
unsigned int i = 0;
while (j - i > 1) {
unsigned int m = (i+j)/2;
if (ttisnil(&t->array[m - 1])) j = m;
else i = m;
}
return i;
}
else if (isdummy(t))  
return j;  
else return unbound_search(t, j);
}
#if defined(LUA_DEBUG)
Node *luaH_mainposition (const Table *t, const TValue *key) {
return mainposition(t, key);
}
int luaH_isdummy (const Table *t) { return isdummy(t); }
#endif
#define ltablib_c
#define LUA_LIB
#include <limits.h>
#include <stddef.h>
#include <string.h>
#define TAB_R	1			
#define TAB_W	2			
#define TAB_L	4			
#define TAB_RW	(TAB_R | TAB_W)		
#define aux_getn(L,n,w)	(checktab(L, n, (w) | TAB_L), luaL_len(L, n))
static int checkfield (lua_State *L, const char *key, int n) {
lua_pushstring(L, key);
return (lua_rawget(L, -n) != LUA_TNIL);
}
static void checktab (lua_State *L, int arg, int what) {
if (lua_type(L, arg) != LUA_TTABLE) {  
int n = 1;  
if (lua_getmetatable(L, arg) &&  
(!(what & TAB_R) || checkfield(L, "__index", ++n)) &&
(!(what & TAB_W) || checkfield(L, "__newindex", ++n)) &&
(!(what & TAB_L) || checkfield(L, "__len", ++n))) {
lua_pop(L, n);  
}
else
luaL_checktype(L, arg, LUA_TTABLE);  
}
}
#if defined(LUA_COMPAT_MAXN)
static int maxn (lua_State *L) {
lua_Number max = 0;
luaL_checktype(L, 1, LUA_TTABLE);
lua_pushnil(L);  
while (lua_next(L, 1)) {
lua_pop(L, 1);  
if (lua_type(L, -1) == LUA_TNUMBER) {
lua_Number v = lua_tonumber(L, -1);
if (v > max) max = v;
}
}
lua_pushnumber(L, max);
return 1;
}
#endif
static int tinsert (lua_State *L) {
lua_Integer e = aux_getn(L, 1, TAB_RW) + 1;  
lua_Integer pos;  
switch (lua_gettop(L)) {
case 2: {  
pos = e;  
break;
}
case 3: {
lua_Integer i;
pos = luaL_checkinteger(L, 2);  
luaL_argcheck(L, 1 <= pos && pos <= e, 2, "position out of bounds");
for (i = e; i > pos; i--) {  
lua_geti(L, 1, i - 1);
lua_seti(L, 1, i);  
}
break;
}
default: {
return luaL_error(L, "wrong number of arguments to 'insert'");
}
}
lua_seti(L, 1, pos);  
return 0;
}
static int tremove (lua_State *L) {
lua_Integer size = aux_getn(L, 1, TAB_RW);
lua_Integer pos = luaL_optinteger(L, 2, size);
if (pos != size)  
luaL_argcheck(L, 1 <= pos && pos <= size + 1, 1, "position out of bounds");
lua_geti(L, 1, pos);  
for ( ; pos < size; pos++) {
lua_geti(L, 1, pos + 1);
lua_seti(L, 1, pos);  
}
lua_pushnil(L);
lua_seti(L, 1, pos);  
return 1;
}
static int tmove (lua_State *L) {
lua_Integer f = luaL_checkinteger(L, 2);
lua_Integer e = luaL_checkinteger(L, 3);
lua_Integer t = luaL_checkinteger(L, 4);
int tt = !lua_isnoneornil(L, 5) ? 5 : 1;  
checktab(L, 1, TAB_R);
checktab(L, tt, TAB_W);
if (e >= f) {  
lua_Integer n, i;
luaL_argcheck(L, f > 0 || e < LUA_MAXINTEGER + f, 3,
"too many elements to move");
n = e - f + 1;  
luaL_argcheck(L, t <= LUA_MAXINTEGER - n + 1, 4,
"destination wrap around");
if (t > e || t <= f || (tt != 1 && !lua_compare(L, 1, tt, LUA_OPEQ))) {
for (i = 0; i < n; i++) {
lua_geti(L, 1, f + i);
lua_seti(L, tt, t + i);
}
}
else {
for (i = n - 1; i >= 0; i--) {
lua_geti(L, 1, f + i);
lua_seti(L, tt, t + i);
}
}
}
lua_pushvalue(L, tt);  
return 1;
}
static void addfield (lua_State *L, luaL_Buffer *b, lua_Integer i) {
lua_geti(L, 1, i);
if (!lua_isstring(L, -1))
luaL_error(L, "invalid value (%s) at index %d in table for 'concat'",
luaL_typename(L, -1), i);
luaL_addvalue(b);
}
static int tconcat (lua_State *L) {
luaL_Buffer b;
lua_Integer last = aux_getn(L, 1, TAB_R);
size_t lsep;
const char *sep = luaL_optlstring(L, 2, "", &lsep);
lua_Integer i = luaL_optinteger(L, 3, 1);
last = luaL_optinteger(L, 4, last);
luaL_buffinit(L, &b);
for (; i < last; i++) {
addfield(L, &b, i);
luaL_addlstring(&b, sep, lsep);
}
if (i == last)  
addfield(L, &b, i);
luaL_pushresult(&b);
return 1;
}
static int pack (lua_State *L) {
int i;
int n = lua_gettop(L);  
lua_createtable(L, n, 1);  
lua_insert(L, 1);  
for (i = n; i >= 1; i--)  
lua_seti(L, 1, i);
lua_pushinteger(L, n);
lua_setfield(L, 1, "n");  
return 1;  
}
static int unpack (lua_State *L) {
lua_Unsigned n;
lua_Integer i = luaL_optinteger(L, 2, 1);
lua_Integer e = luaL_opt(L, luaL_checkinteger, 3, luaL_len(L, 1));
if (i > e) return 0;  
n = (lua_Unsigned)e - i;  
if (n >= (unsigned int)INT_MAX  || !lua_checkstack(L, (int)(++n)))
return luaL_error(L, "too many results to unpack");
for (; i < e; i++) {  
lua_geti(L, 1, i);
}
lua_geti(L, 1, e);  
return (int)n;
}
typedef unsigned int IdxT;
#if !defined(l_randomizePivot)		
#include <time.h>
#define sof(e)		(sizeof(e) / sizeof(unsigned int))
static unsigned int l_randomizePivot (void) {
clock_t c = clock();
time_t t = time(NULL);
unsigned int buff[sof(c) + sof(t)];
unsigned int i, rnd = 0;
memcpy(buff, &c, sof(c) * sizeof(unsigned int));
memcpy(buff + sof(c), &t, sof(t) * sizeof(unsigned int));
for (i = 0; i < sof(buff); i++)
rnd += buff[i];
return rnd;
}
#endif					
#define RANLIMIT	100u
static void set2 (lua_State *L, IdxT i, IdxT j) {
lua_seti(L, 1, i);
lua_seti(L, 1, j);
}
static int sort_comp (lua_State *L, int a, int b) {
if (lua_isnil(L, 2))  
return lua_compare(L, a, b, LUA_OPLT);  
else {  
int res;
lua_pushvalue(L, 2);    
lua_pushvalue(L, a-1);  
lua_pushvalue(L, b-2);  
lua_call(L, 2, 1);      
res = lua_toboolean(L, -1);  
lua_pop(L, 1);          
return res;
}
}
static IdxT partition (lua_State *L, IdxT lo, IdxT up) {
IdxT i = lo;  
IdxT j = up - 1;  
for (;;) {
while (lua_geti(L, 1, ++i), sort_comp(L, -1, -2)) {
if (i == up - 1)  
luaL_error(L, "invalid order function for sorting");
lua_pop(L, 1);  
}
while (lua_geti(L, 1, --j), sort_comp(L, -3, -1)) {
if (j < i)  
luaL_error(L, "invalid order function for sorting");
lua_pop(L, 1);  
}
if (j < i) {  
lua_pop(L, 1);  
set2(L, up - 1, i);
return i;
}
set2(L, i, j);
}
}
static IdxT choosePivot (IdxT lo, IdxT up, unsigned int rnd) {
IdxT r4 = (up - lo) / 4;  
IdxT p = rnd % (r4 * 2) + (lo + r4);
lua_assert(lo + r4 <= p && p <= up - r4);
return p;
}
static void auxsort (lua_State *L, IdxT lo, IdxT up,
unsigned int rnd) {
while (lo < up) {  
IdxT p;  
IdxT n;  
lua_geti(L, 1, lo);
lua_geti(L, 1, up);
if (sort_comp(L, -1, -2))  
set2(L, lo, up);  
else
lua_pop(L, 2);  
if (up - lo == 1)  
return;  
if (up - lo < RANLIMIT || rnd == 0)  
p = (lo + up)/2;  
else  
p = choosePivot(lo, up, rnd);
lua_geti(L, 1, p);
lua_geti(L, 1, lo);
if (sort_comp(L, -2, -1))  
set2(L, p, lo);  
else {
lua_pop(L, 1);  
lua_geti(L, 1, up);
if (sort_comp(L, -1, -2))  
set2(L, p, up);  
else
lua_pop(L, 2);
}
if (up - lo == 2)  
return;  
lua_geti(L, 1, p);  
lua_pushvalue(L, -1);  
lua_geti(L, 1, up - 1);  
set2(L, p, up - 1);  
p = partition(L, lo, up);
if (p - lo < up - p) {  
auxsort(L, lo, p - 1, rnd);  
n = p - lo;  
lo = p + 1;  
}
else {
auxsort(L, p + 1, up, rnd);  
n = up - p;  
up = p - 1;  
}
if ((up - lo) / 128 > n) 
rnd = l_randomizePivot();  
}  
}
static int sort (lua_State *L) {
lua_Integer n = aux_getn(L, 1, TAB_RW);
if (n > 1) {  
luaL_argcheck(L, n < INT_MAX, 1, "array too big");
if (!lua_isnoneornil(L, 2))  
luaL_checktype(L, 2, LUA_TFUNCTION);  
lua_settop(L, 2);  
auxsort(L, 1, (IdxT)n, 0);
}
return 0;
}
static const luaL_Reg tab_funcs[] = {
{"concat", tconcat},
#if defined(LUA_COMPAT_MAXN)
{"maxn", maxn},
#endif
{"insert", tinsert},
{"pack", pack},
{"unpack", unpack},
{"remove", tremove},
{"move", tmove},
{"sort", sort},
{NULL, NULL}
};
LUAMOD_API int luaopen_table (lua_State *L) {
luaL_newlib(L, tab_funcs);
#if defined(LUA_COMPAT_UNPACK)
lua_getfield(L, -1, "unpack");
lua_setglobal(L, "unpack");
#endif
return 1;
}
#define ltm_c
#define LUA_CORE
#include <string.h>
static const char udatatypename[] = "userdata";
LUAI_DDEF const char *const luaT_typenames_[LUA_TOTALTAGS] = {
"no value",
"nil", "boolean", udatatypename, "number",
"string", "table", "function", udatatypename, "thread",
"proto" 
};
void luaT_init (lua_State *L) {
static const char *const luaT_eventname[] = {  
"__index", "__newindex",
"__gc", "__mode", "__len", "__eq",
"__add", "__sub", "__mul", "__mod", "__pow",
"__div", "__idiv",
"__band", "__bor", "__bxor", "__shl", "__shr",
"__unm", "__bnot", "__lt", "__le",
"__concat", "__call"
};
int i;
for (i=0; i<TM_N; i++) {
G(L)->tmname[i] = luaS_new(L, luaT_eventname[i]);
luaC_fix(L, obj2gco(G(L)->tmname[i]));  
}
}
const TValue *luaT_gettm (Table *events, TMS event, TString *ename) {
const TValue *tm = luaH_getshortstr(events, ename);
lua_assert(event <= TM_EQ);
if (ttisnil(tm)) {  
events->flags |= cast_byte(1u<<event);  
return NULL;
}
else return tm;
}
const TValue *luaT_gettmbyobj (lua_State *L, const TValue *o, TMS event) {
Table *mt;
switch (ttnov(o)) {
case LUA_TTABLE:
mt = hvalue(o)->metatable;
break;
case LUA_TUSERDATA:
mt = uvalue(o)->metatable;
break;
default:
mt = G(L)->mt[ttnov(o)];
}
return (mt ? luaH_getshortstr(mt, G(L)->tmname[event]) : luaO_nilobject);
}
const char *luaT_objtypename (lua_State *L, const TValue *o) {
Table *mt;
if ((ttistable(o) && (mt = hvalue(o)->metatable) != NULL) ||
(ttisfulluserdata(o) && (mt = uvalue(o)->metatable) != NULL)) {
const TValue *name = luaH_getshortstr(mt, luaS_new(L, "__name"));
if (ttisstring(name))  
return getstr(tsvalue(name));  
}
return ttypename(ttnov(o));  
}
void luaT_callTM (lua_State *L, const TValue *f, const TValue *p1,
const TValue *p2, TValue *p3, int hasres) {
ptrdiff_t result = savestack(L, p3);
StkId func = L->top;
setobj2s(L, func, f);  
setobj2s(L, func + 1, p1);  
setobj2s(L, func + 2, p2);  
L->top += 3;
if (!hasres)  
setobj2s(L, L->top++, p3);  
if (isLua(L->ci))
luaD_call(L, func, hasres);
else
luaD_callnoyield(L, func, hasres);
if (hasres) {  
p3 = restorestack(L, result);
setobjs2s(L, p3, --L->top);
}
}
int luaT_callbinTM (lua_State *L, const TValue *p1, const TValue *p2,
StkId res, TMS event) {
const TValue *tm = luaT_gettmbyobj(L, p1, event);  
if (ttisnil(tm))
tm = luaT_gettmbyobj(L, p2, event);  
if (ttisnil(tm)) return 0;
luaT_callTM(L, tm, p1, p2, res, 1);
return 1;
}
void luaT_trybinTM (lua_State *L, const TValue *p1, const TValue *p2,
StkId res, TMS event) {
if (!luaT_callbinTM(L, p1, p2, res, event)) {
switch (event) {
case TM_CONCAT:
luaG_concaterror(L, p1, p2);
/* FALLTHROUGH */
case TM_BAND: case TM_BOR: case TM_BXOR:
case TM_SHL: case TM_SHR: case TM_BNOT: {
lua_Number dummy;
if (tonumber(p1, &dummy) && tonumber(p2, &dummy))
luaG_tointerror(L, p1, p2);
else
luaG_opinterror(L, p1, p2, "perform bitwise operation on");
}
/* FALLTHROUGH */
default:
luaG_opinterror(L, p1, p2, "perform arithmetic on");
}
}
}
int luaT_callorderTM (lua_State *L, const TValue *p1, const TValue *p2,
TMS event) {
if (!luaT_callbinTM(L, p1, p2, L->top, event))
return -1;  
else
return !l_isfalse(L->top);
}
#define lundump_c
#define LUA_CORE
#include <string.h>
#if !defined(luai_verifycode)
#define luai_verifycode(L,b,f)  
#endif
typedef struct {
lua_State *L;
ZIO *Z;
const char *name;
} LoadState;
static l_noret error(LoadState *S, const char *why) {
luaO_pushfstring(S->L, "%s: %s precompiled chunk", S->name, why);
luaD_throw(S->L, LUA_ERRSYNTAX);
}
#define LoadVector(S,b,n)	LoadBlock(S,b,(n)*sizeof((b)[0]))
static void LoadBlock (LoadState *S, void *b, size_t size) {
if (luaZ_read(S->Z, b, size) != 0)
error(S, "truncated");
}
#define LoadVar(S,x)		LoadVector(S,&x,1)
static lu_byte LoadByte (LoadState *S) {
lu_byte x;
LoadVar(S, x);
return x;
}
static int LoadInt (LoadState *S) {
int x;
LoadVar(S, x);
return x;
}
static lua_Number LoadNumber (LoadState *S) {
lua_Number x;
LoadVar(S, x);
return x;
}
static lua_Integer LoadInteger (LoadState *S) {
lua_Integer x;
LoadVar(S, x);
return x;
}
static TString *LoadString (LoadState *S) {
size_t size = LoadByte(S);
if (size == 0xFF)
LoadVar(S, size);
if (size == 0)
return NULL;
else if (--size <= LUAI_MAXSHORTLEN) {  
char buff[LUAI_MAXSHORTLEN];
LoadVector(S, buff, size);
return luaS_newlstr(S->L, buff, size);
}
else {  
TString *ts = luaS_createlngstrobj(S->L, size);
LoadVector(S, getstr(ts), size);  
return ts;
}
}
static void LoadCode (LoadState *S, Proto *f) {
int n = LoadInt(S);
f->code = luaM_newvector(S->L, n, Instruction);
f->sizecode = n;
LoadVector(S, f->code, n);
}
static void LoadFunction(LoadState *S, Proto *f, TString *psource);
static void LoadConstants (LoadState *S, Proto *f) {
int i;
int n = LoadInt(S);
f->k = luaM_newvector(S->L, n, TValue);
f->sizek = n;
for (i = 0; i < n; i++)
setnilvalue(&f->k[i]);
for (i = 0; i < n; i++) {
TValue *o = &f->k[i];
int t = LoadByte(S);
switch (t) {
case LUA_TNIL:
setnilvalue(o);
break;
case LUA_TBOOLEAN:
setbvalue(o, LoadByte(S));
break;
case LUA_TNUMFLT:
setfltvalue(o, LoadNumber(S));
break;
case LUA_TNUMINT:
setivalue(o, LoadInteger(S));
break;
case LUA_TSHRSTR:
case LUA_TLNGSTR:
setsvalue2n(S->L, o, LoadString(S));
break;
default:
lua_assert(0);
}
}
}
static void LoadProtos (LoadState *S, Proto *f) {
int i;
int n = LoadInt(S);
f->p = luaM_newvector(S->L, n, Proto *);
f->sizep = n;
for (i = 0; i < n; i++)
f->p[i] = NULL;
for (i = 0; i < n; i++) {
f->p[i] = luaF_newproto(S->L);
LoadFunction(S, f->p[i], f->source);
}
}
static void LoadUpvalues (LoadState *S, Proto *f) {
int i, n;
n = LoadInt(S);
f->upvalues = luaM_newvector(S->L, n, Upvaldesc);
f->sizeupvalues = n;
for (i = 0; i < n; i++)
f->upvalues[i].name = NULL;
for (i = 0; i < n; i++) {
f->upvalues[i].instack = LoadByte(S);
f->upvalues[i].idx = LoadByte(S);
}
}
static void LoadDebug (LoadState *S, Proto *f) {
int i, n;
n = LoadInt(S);
f->lineinfo = luaM_newvector(S->L, n, int);
f->sizelineinfo = n;
LoadVector(S, f->lineinfo, n);
n = LoadInt(S);
f->locvars = luaM_newvector(S->L, n, LocVar);
f->sizelocvars = n;
for (i = 0; i < n; i++)
f->locvars[i].varname = NULL;
for (i = 0; i < n; i++) {
f->locvars[i].varname = LoadString(S);
f->locvars[i].startpc = LoadInt(S);
f->locvars[i].endpc = LoadInt(S);
}
n = LoadInt(S);
for (i = 0; i < n; i++)
f->upvalues[i].name = LoadString(S);
}
static void LoadFunction (LoadState *S, Proto *f, TString *psource) {
f->source = LoadString(S);
if (f->source == NULL)  
f->source = psource;  
f->linedefined = LoadInt(S);
f->lastlinedefined = LoadInt(S);
f->numparams = LoadByte(S);
f->is_vararg = LoadByte(S);
f->maxstacksize = LoadByte(S);
LoadCode(S, f);
LoadConstants(S, f);
LoadUpvalues(S, f);
LoadProtos(S, f);
LoadDebug(S, f);
}
static void checkliteral (LoadState *S, const char *s, const char *msg) {
char buff[sizeof(LUA_SIGNATURE) + sizeof(LUAC_DATA)]; 
size_t len = strlen(s);
LoadVector(S, buff, len);
if (memcmp(s, buff, len) != 0)
error(S, msg);
}
static void fchecksize (LoadState *S, size_t size, const char *tname) {
if (LoadByte(S) != size)
error(S, luaO_pushfstring(S->L, "%s size mismatch in", tname));
}
#define checksize(S,t)	fchecksize(S,sizeof(t),#t)
static void checkHeader (LoadState *S) {
checkliteral(S, LUA_SIGNATURE + 1, "not a");  
if (LoadByte(S) != LUAC_VERSION)
error(S, "version mismatch in");
if (LoadByte(S) != LUAC_FORMAT)
error(S, "format mismatch in");
checkliteral(S, LUAC_DATA, "corrupted");
checksize(S, int);
checksize(S, size_t);
checksize(S, Instruction);
checksize(S, lua_Integer);
checksize(S, lua_Number);
if (LoadInteger(S) != LUAC_INT)
error(S, "endianness mismatch in");
if (LoadNumber(S) != LUAC_NUM)
error(S, "float format mismatch in");
}
LClosure *luaU_undump(lua_State *L, ZIO *Z, const char *name) {
LoadState S;
LClosure *cl;
if (*name == '@' || *name == '=')
S.name = name + 1;
else if (*name == LUA_SIGNATURE[0])
S.name = "binary string";
else
S.name = name;
S.L = L;
S.Z = Z;
checkHeader(&S);
cl = luaF_newLclosure(L, LoadByte(&S));
setclLvalue(L, L->top, cl);
luaD_inctop(L);
cl->p = luaF_newproto(L);
LoadFunction(&S, cl->p, NULL);
lua_assert(cl->nupvalues == cl->p->sizeupvalues);
luai_verifycode(L, buff, cl->p);
return cl;
}
#define lutf8lib_c
#define LUA_LIB
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#define MAXUNICODE	0x10FFFF
#define iscont(p)	((*(p) & 0xC0) == 0x80)
static lua_Integer u_posrelat (lua_Integer pos, size_t len) {
if (pos >= 0) return pos;
else if (0u - (size_t)pos > len) return 0;
else return (lua_Integer)len + pos + 1;
}
static const char *utf8_decode (const char *o, int *val) {
static const unsigned int limits[] = {0xFF, 0x7F, 0x7FF, 0xFFFF};
const unsigned char *s = (const unsigned char *)o;
unsigned int c = s[0];
unsigned int res = 0;  
if (c < 0x80)  
res = c;
else {
int count = 0;  
while (c & 0x40) {  
int cc = s[++count];  
if ((cc & 0xC0) != 0x80)  
return NULL;  
res = (res << 6) | (cc & 0x3F);  
c <<= 1;  
}
res |= ((c & 0x7F) << (count * 5));  
if (count > 3 || res > MAXUNICODE || res <= limits[count])
return NULL;  
s += count;  
}
if (val) *val = res;
return (const char *)s + 1;  
}
static int utflen (lua_State *L) {
int n = 0;
size_t len;
const char *s = luaL_checklstring(L, 1, &len);
lua_Integer posi = u_posrelat(luaL_optinteger(L, 2, 1), len);
lua_Integer posj = u_posrelat(luaL_optinteger(L, 3, -1), len);
luaL_argcheck(L, 1 <= posi && --posi <= (lua_Integer)len, 2,
"initial position out of string");
luaL_argcheck(L, --posj < (lua_Integer)len, 3,
"final position out of string");
while (posi <= posj) {
const char *s1 = utf8_decode(s + posi, NULL);
if (s1 == NULL) {  
lua_pushnil(L);  
lua_pushinteger(L, posi + 1);  
return 2;
}
posi = s1 - s;
n++;
}
lua_pushinteger(L, n);
return 1;
}
static int codepoint (lua_State *L) {
size_t len;
const char *s = luaL_checklstring(L, 1, &len);
lua_Integer posi = u_posrelat(luaL_optinteger(L, 2, 1), len);
lua_Integer pose = u_posrelat(luaL_optinteger(L, 3, posi), len);
int n;
const char *se;
luaL_argcheck(L, posi >= 1, 2, "out of range");
luaL_argcheck(L, pose <= (lua_Integer)len, 3, "out of range");
if (posi > pose) return 0;  
if (pose - posi >= INT_MAX)  
return luaL_error(L, "string slice too long");
n = (int)(pose -  posi) + 1;
luaL_checkstack(L, n, "string slice too long");
n = 0;
se = s + pose;
for (s += posi - 1; s < se;) {
int code;
s = utf8_decode(s, &code);
if (s == NULL)
return luaL_error(L, "invalid UTF-8 code");
lua_pushinteger(L, code);
n++;
}
return n;
}
static void pushutfchar (lua_State *L, int arg) {
lua_Integer code = luaL_checkinteger(L, arg);
luaL_argcheck(L, 0 <= code && code <= MAXUNICODE, arg, "value out of range");
lua_pushfstring(L, "%U", (long)code);
}
static int utfchar (lua_State *L) {
int n = lua_gettop(L);  
if (n == 1)  
pushutfchar(L, 1);
else {
int i;
luaL_Buffer b;
luaL_buffinit(L, &b);
for (i = 1; i <= n; i++) {
pushutfchar(L, i);
luaL_addvalue(&b);
}
luaL_pushresult(&b);
}
return 1;
}
static int byteoffset (lua_State *L) {
size_t len;
const char *s = luaL_checklstring(L, 1, &len);
lua_Integer n  = luaL_checkinteger(L, 2);
lua_Integer posi = (n >= 0) ? 1 : len + 1;
posi = u_posrelat(luaL_optinteger(L, 3, posi), len);
luaL_argcheck(L, 1 <= posi && --posi <= (lua_Integer)len, 3,
"position out of range");
if (n == 0) {
while (posi > 0 && iscont(s + posi)) posi--;
}
else {
if (iscont(s + posi))
return luaL_error(L, "initial position is a continuation byte");
if (n < 0) {
while (n < 0 && posi > 0) {  
do {  
posi--;
} while (posi > 0 && iscont(s + posi));
n++;
}
}
else {
n--;  
while (n > 0 && posi < (lua_Integer)len) {
do {  
posi++;
} while (iscont(s + posi));  
n--;
}
}
}
if (n == 0)  
lua_pushinteger(L, posi + 1);
else  
lua_pushnil(L);
return 1;
}
static int iter_aux (lua_State *L) {
size_t len;
const char *s = luaL_checklstring(L, 1, &len);
lua_Integer n = lua_tointeger(L, 2) - 1;
if (n < 0)  
n = 0;  
else if (n < (lua_Integer)len) {
n++;  
while (iscont(s + n)) n++;  
}
if (n >= (lua_Integer)len)
return 0;  
else {
int code;
const char *next = utf8_decode(s + n, &code);
if (next == NULL || iscont(next))
return luaL_error(L, "invalid UTF-8 code");
lua_pushinteger(L, n + 1);
lua_pushinteger(L, code);
return 2;
}
}
static int iter_codes (lua_State *L) {
luaL_checkstring(L, 1);
lua_pushcfunction(L, iter_aux);
lua_pushvalue(L, 1);
lua_pushinteger(L, 0);
return 3;
}
#define UTF8PATT	"[\0-\x7F\xC2-\xF4][\x80-\xBF]*"
static const luaL_Reg funcs[] = {
{"offset", byteoffset},
{"codepoint", codepoint},
{"char", utfchar},
{"len", utflen},
{"codes", iter_codes},
{"charpattern", NULL},
{NULL, NULL}
};
LUAMOD_API int luaopen_utf8 (lua_State *L) {
luaL_newlib(L, funcs);
lua_pushlstring(L, UTF8PATT, sizeof(UTF8PATT)/sizeof(char) - 1);
lua_setfield(L, -2, "charpattern");
return 1;
}
#define lvm_c
#define LUA_CORE
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAXTAGLOOP	2000
#if !defined(l_intfitsf)
#define NBM		(l_mathlim(MANT_DIG))
#if ((((LUA_MAXINTEGER >> (NBM / 4)) >> (NBM / 4)) >> (NBM / 4)) \
>> (NBM - (3 * (NBM / 4))))  >  0
#define l_intfitsf(i)  \
(-((lua_Integer)1 << NBM) <= (i) && (i) <= ((lua_Integer)1 << NBM))
#endif
#endif
int luaV_tonumber_ (const TValue *obj, lua_Number *n) {
TValue v;
if (ttisinteger(obj)) {
*n = cast_num(ivalue(obj));
return 1;
}
else if (cvt2num(obj) &&  
luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
*n = nvalue(&v);  
return 1;
}
else
return 0;  
}
int luaV_tointeger (const TValue *obj, lua_Integer *p, int mode) {
TValue v;
again:
if (ttisfloat(obj)) {
lua_Number n = fltvalue(obj);
lua_Number f = l_floor(n);
if (n != f) {  
if (mode == 0) return 0;  
else if (mode > 1)  
f += 1;  
}
return lua_numbertointeger(f, p);
}
else if (ttisinteger(obj)) {
*p = ivalue(obj);
return 1;
}
else if (cvt2num(obj) &&
luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
obj = &v;
goto again;  
}
return 0;  
}
static int forlimit (const TValue *obj, lua_Integer *p, lua_Integer step,
int *stopnow) {
*stopnow = 0;  
if (!luaV_tointeger(obj, p, (step < 0 ? 2 : 1))) {  
lua_Number n;  
if (!tonumber(obj, &n)) 
return 0;  
if (luai_numlt(0, n)) {  
*p = LUA_MAXINTEGER;
if (step < 0) *stopnow = 1;
}
else {  
*p = LUA_MININTEGER;
if (step >= 0) *stopnow = 1;
}
}
return 1;
}
void luaV_finishget (lua_State *L, const TValue *t, TValue *key, StkId val,
const TValue *slot) {
int loop;  
const TValue *tm;  
for (loop = 0; loop < MAXTAGLOOP; loop++) {
if (slot == NULL) {  
lua_assert(!ttistable(t));
tm = luaT_gettmbyobj(L, t, TM_INDEX);
if (ttisnil(tm))
luaG_typeerror(L, t, "index");  
}
else {  
lua_assert(ttisnil(slot));
tm = fasttm(L, hvalue(t)->metatable, TM_INDEX);  
if (tm == NULL) {  
setnilvalue(val);  
return;
}
}
if (ttisfunction(tm)) {  
luaT_callTM(L, tm, t, key, val, 1);  
return;
}
t = tm;  
if (luaV_fastget(L,t,key,slot,luaH_get)) {  
setobj2s(L, val, slot);  
return;
}
}
luaG_runerror(L, "'__index' chain too long; possible loop");
}
void luaV_finishset (lua_State *L, const TValue *t, TValue *key,
StkId val, const TValue *slot) {
int loop;  
for (loop = 0; loop < MAXTAGLOOP; loop++) {
const TValue *tm;  
if (slot != NULL) {  
Table *h = hvalue(t);  
lua_assert(ttisnil(slot));  
tm = fasttm(L, h->metatable, TM_NEWINDEX);  
if (tm == NULL) {  
if (slot == luaO_nilobject)  
slot = luaH_newkey(L, h, key);  
setobj2t(L, cast(TValue *, slot), val);  
invalidateTMcache(h);
luaC_barrierback(L, h, val);
return;
}
}
else {  
if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
luaG_typeerror(L, t, "index");
}
if (ttisfunction(tm)) {
luaT_callTM(L, tm, t, key, val, 0);
return;
}
t = tm;  
if (luaV_fastset(L, t, key, slot, luaH_get, val))
return;  
}
luaG_runerror(L, "'__newindex' chain too long; possible loop");
}
static int l_strcmp (const TString *ls, const TString *rs) {
const char *l = getstr(ls);
size_t ll = tsslen(ls);
const char *r = getstr(rs);
size_t lr = tsslen(rs);
for (;;) {  
int temp = strcoll(l, r);
if (temp != 0)  
return temp;  
else {  
size_t len = strlen(l);  
if (len == lr)  
return (len == ll) ? 0 : 1;  
else if (len == ll)  
return -1;  
len++;
l += len; ll -= len; r += len; lr -= len;
}
}
}
static int LTintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
if (!l_intfitsf(i)) {
if (f >= -cast_num(LUA_MININTEGER))  
return 1;  
else if (f > cast_num(LUA_MININTEGER))  
return (i < cast(lua_Integer, f));  
else  
return 0;
}
#endif
return luai_numlt(cast_num(i), f);  
}
static int LEintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
if (!l_intfitsf(i)) {
if (f >= -cast_num(LUA_MININTEGER))  
return 1;  
else if (f >= cast_num(LUA_MININTEGER))  
return (i <= cast(lua_Integer, f));  
else  
return 0;
}
#endif
return luai_numle(cast_num(i), f);  
}
static int LTnum (const TValue *l, const TValue *r) {
if (ttisinteger(l)) {
lua_Integer li = ivalue(l);
if (ttisinteger(r))
return li < ivalue(r);  
else  
return LTintfloat(li, fltvalue(r));  
}
else {
lua_Number lf = fltvalue(l);  
if (ttisfloat(r))
return luai_numlt(lf, fltvalue(r));  
else if (luai_numisnan(lf))  
return 0;  
else  
return !LEintfloat(ivalue(r), lf);  
}
}
static int LEnum (const TValue *l, const TValue *r) {
if (ttisinteger(l)) {
lua_Integer li = ivalue(l);
if (ttisinteger(r))
return li <= ivalue(r);  
else  
return LEintfloat(li, fltvalue(r));  
}
else {
lua_Number lf = fltvalue(l);  
if (ttisfloat(r))
return luai_numle(lf, fltvalue(r));  
else if (luai_numisnan(lf))  
return 0;  
else  
return !LTintfloat(ivalue(r), lf);  
}
}
int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r) {
int res;
if (ttisnumber(l) && ttisnumber(r))  
return LTnum(l, r);
else if (ttisstring(l) && ttisstring(r))  
return l_strcmp(tsvalue(l), tsvalue(r)) < 0;
else if ((res = luaT_callorderTM(L, l, r, TM_LT)) < 0)  
luaG_ordererror(L, l, r);  
return res;
}
int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r) {
int res;
if (ttisnumber(l) && ttisnumber(r))  
return LEnum(l, r);
else if (ttisstring(l) && ttisstring(r))  
return l_strcmp(tsvalue(l), tsvalue(r)) <= 0;
else if ((res = luaT_callorderTM(L, l, r, TM_LE)) >= 0)  
return res;
else {  
L->ci->callstatus |= CIST_LEQ;  
res = luaT_callorderTM(L, r, l, TM_LT);
L->ci->callstatus ^= CIST_LEQ;  
if (res < 0)
luaG_ordererror(L, l, r);
return !res;  
}
}
int luaV_equalobj (lua_State *L, const TValue *t1, const TValue *t2) {
const TValue *tm;
if (ttype(t1) != ttype(t2)) {  
if (ttnov(t1) != ttnov(t2) || ttnov(t1) != LUA_TNUMBER)
return 0;  
else {  
lua_Integer i1, i2;  
return (tointeger(t1, &i1) && tointeger(t2, &i2) && i1 == i2);
}
}
switch (ttype(t1)) {
case LUA_TNIL: return 1;
case LUA_TNUMINT: return (ivalue(t1) == ivalue(t2));
case LUA_TNUMFLT: return luai_numeq(fltvalue(t1), fltvalue(t2));
case LUA_TBOOLEAN: return bvalue(t1) == bvalue(t2);  
case LUA_TLIGHTUSERDATA: return pvalue(t1) == pvalue(t2);
case LUA_TLCF: return fvalue(t1) == fvalue(t2);
case LUA_TSHRSTR: return eqshrstr(tsvalue(t1), tsvalue(t2));
case LUA_TLNGSTR: return luaS_eqlngstr(tsvalue(t1), tsvalue(t2));
case LUA_TUSERDATA: {
if (uvalue(t1) == uvalue(t2)) return 1;
else if (L == NULL) return 0;
tm = fasttm(L, uvalue(t1)->metatable, TM_EQ);
if (tm == NULL)
tm = fasttm(L, uvalue(t2)->metatable, TM_EQ);
break;  
}
case LUA_TTABLE: {
if (hvalue(t1) == hvalue(t2)) return 1;
else if (L == NULL) return 0;
tm = fasttm(L, hvalue(t1)->metatable, TM_EQ);
if (tm == NULL)
tm = fasttm(L, hvalue(t2)->metatable, TM_EQ);
break;  
}
default:
return gcvalue(t1) == gcvalue(t2);
}
if (tm == NULL)  
return 0;  
luaT_callTM(L, tm, t1, t2, L->top, 1);  
return !l_isfalse(L->top);
}
#define tostring(L,o)  \
(ttisstring(o) || (cvt2str(o) && (luaO_tostring(L, o), 1)))
#define isemptystr(o)	(ttisshrstring(o) && tsvalue(o)->shrlen == 0)
static void copy2buff (StkId top, int n, char *buff) {
size_t tl = 0;  
do {
size_t l = vslen(top - n);  
memcpy(buff + tl, svalue(top - n), l * sizeof(char));
tl += l;
} while (--n > 0);
}
void luaV_concat (lua_State *L, int total) {
lua_assert(total >= 2);
do {
StkId top = L->top;
int n = 2;  
if (!(ttisstring(top-2) || cvt2str(top-2)) || !tostring(L, top-1))
luaT_trybinTM(L, top-2, top-1, top-2, TM_CONCAT);
else if (isemptystr(top - 1))  
cast_void(tostring(L, top - 2));  
else if (isemptystr(top - 2)) {  
setobjs2s(L, top - 2, top - 1);  
}
else {
size_t tl = vslen(top - 1);
TString *ts;
for (n = 1; n < total && tostring(L, top - n - 1); n++) {
size_t l = vslen(top - n - 1);
if (l >= (MAX_SIZE/sizeof(char)) - tl)
luaG_runerror(L, "string length overflow");
tl += l;
}
if (tl <= LUAI_MAXSHORTLEN) {  
char buff[LUAI_MAXSHORTLEN];
copy2buff(top, n, buff);  
ts = luaS_newlstr(L, buff, tl);
}
else {  
ts = luaS_createlngstrobj(L, tl);
copy2buff(top, n, getstr(ts));
}
setsvalue2s(L, top - n, ts);  
}
total -= n-1;  
L->top -= n-1;  
} while (total > 1);  
}
void luaV_objlen (lua_State *L, StkId ra, const TValue *rb) {
const TValue *tm;
switch (ttype(rb)) {
case LUA_TTABLE: {
Table *h = hvalue(rb);
tm = fasttm(L, h->metatable, TM_LEN);
if (tm) break;  
setivalue(ra, luaH_getn(h));  
return;
}
case LUA_TSHRSTR: {
setivalue(ra, tsvalue(rb)->shrlen);
return;
}
case LUA_TLNGSTR: {
setivalue(ra, tsvalue(rb)->u.lnglen);
return;
}
default: {  
tm = luaT_gettmbyobj(L, rb, TM_LEN);
if (ttisnil(tm))  
luaG_typeerror(L, rb, "get length of");
break;
}
}
luaT_callTM(L, tm, rb, rb, ra, 1);
}
lua_Integer luaV_div (lua_State *L, lua_Integer m, lua_Integer n) {
if (l_castS2U(n) + 1u <= 1u) {  
if (n == 0)
luaG_runerror(L, "attempt to divide by zero");
return intop(-, 0, m);   
}
else {
lua_Integer q = m / n;  
if ((m ^ n) < 0 && m % n != 0)  
q -= 1;  
return q;
}
}
lua_Integer luaV_mod (lua_State *L, lua_Integer m, lua_Integer n) {
if (l_castS2U(n) + 1u <= 1u) {  
if (n == 0)
luaG_runerror(L, "attempt to perform 'n%%0'");
return 0;   
}
else {
lua_Integer r = m % n;
if (r != 0 && (m ^ n) < 0)  
r += n;  
return r;
}
}
#define NBITS	cast_int(sizeof(lua_Integer) * CHAR_BIT)
lua_Integer luaV_shiftl (lua_Integer x, lua_Integer y) {
if (y < 0) {  
if (y <= -NBITS) return 0;
else return intop(>>, x, -y);
}
else {  
if (y >= NBITS) return 0;
else return intop(<<, x, y);
}
}
static LClosure *getcached (Proto *p, UpVal **encup, StkId base) {
LClosure *c = p->cache;
if (c != NULL) {  
int nup = p->sizeupvalues;
Upvaldesc *uv = p->upvalues;
int i;
for (i = 0; i < nup; i++) {  
TValue *v = uv[i].instack ? base + uv[i].idx : encup[uv[i].idx]->v;
if (c->upvals[i]->v != v)
return NULL;  
}
}
return c;  
}
static void pushclosure (lua_State *L, Proto *p, UpVal **encup, StkId base,
StkId ra) {
int nup = p->sizeupvalues;
Upvaldesc *uv = p->upvalues;
int i;
LClosure *ncl = luaF_newLclosure(L, nup);
ncl->p = p;
setclLvalue(L, ra, ncl);  
for (i = 0; i < nup; i++) {  
if (uv[i].instack)  
ncl->upvals[i] = luaF_findupval(L, base + uv[i].idx);
else  
ncl->upvals[i] = encup[uv[i].idx];
ncl->upvals[i]->refcount++;
}
if (!isblack(p))  
p->cache = ncl;  
}
void luaV_finishOp (lua_State *L) {
CallInfo *ci = L->ci;
StkId base = ci->u.l.base;
Instruction inst = *(ci->u.l.savedpc - 1);  
OpCode op = GET_OPCODE(inst);
switch (op) {  
case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: case OP_IDIV:
case OP_BAND: case OP_BOR: case OP_BXOR: case OP_SHL: case OP_SHR:
case OP_MOD: case OP_POW:
case OP_UNM: case OP_BNOT: case OP_LEN:
case OP_GETTABUP: case OP_GETTABLE: case OP_SELF: {
setobjs2s(L, base + GETARG_A(inst), --L->top);
break;
}
case OP_LE: case OP_LT: case OP_EQ: {
int res = !l_isfalse(L->top - 1);
L->top--;
if (ci->callstatus & CIST_LEQ) {  
lua_assert(op == OP_LE);
ci->callstatus ^= CIST_LEQ;  
res = !res;  
}
lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_JMP);
if (res != GETARG_A(inst))  
ci->u.l.savedpc++;  
break;
}
case OP_CONCAT: {
StkId top = L->top - 1;  
int b = GETARG_B(inst);      
int total = cast_int(top - 1 - (base + b));  
setobj2s(L, top - 2, top);  
if (total > 1) {  
L->top = top - 1;  
luaV_concat(L, total);  
}
setobj2s(L, ci->u.l.base + GETARG_A(inst), L->top - 1);
L->top = ci->top;  
break;
}
case OP_TFORCALL: {
lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_TFORLOOP);
L->top = ci->top;  
break;
}
case OP_CALL: {
if (GETARG_C(inst) - 1 >= 0)  
L->top = ci->top;  
break;
}
case OP_TAILCALL: case OP_SETTABUP: case OP_SETTABLE:
break;
default: lua_assert(0);
}
}
#define RA(i)	(base+GETARG_A(i))
#define RB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define RKB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgK, \
ISK(GETARG_B(i)) ? k+INDEXK(GETARG_B(i)) : base+GETARG_B(i))
#define RKC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgK, \
ISK(GETARG_C(i)) ? k+INDEXK(GETARG_C(i)) : base+GETARG_C(i))
#define dojump(ci,i,e) \
{ int a = GETARG_A(i); \
if (a != 0) luaF_close(L, ci->u.l.base + a - 1); \
ci->u.l.savedpc += GETARG_sBx(i) + e; }
#define donextjump(ci)	{ i = *ci->u.l.savedpc; dojump(ci, i, 1); }
#define Protect(x)	{ {x;}; base = ci->u.l.base; }
#define checkGC(L,c)  \
{ luaC_condGC(L, L->top = (c),   \
Protect(L->top = ci->top));   \
luai_threadyield(L); }
#define vmfetch()	{ \
i = *(ci->u.l.savedpc++); \
if (L->hookmask & (LUA_MASKLINE | LUA_MASKCOUNT)) \
Protect(luaG_traceexec(L)); \
ra = RA(i);  \
lua_assert(base == ci->u.l.base); \
lua_assert(base <= L->top && L->top < L->stack + L->stacksize); \
}
#define vmdispatch(o)	switch(o)
#define vmcase(l)	case l:
#define vmbreak		break
#define gettableProtected(L,t,k,v)  { const TValue *slot; \
if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
else Protect(luaV_finishget(L,t,k,v,slot)); }
#define settableProtected(L,t,k,v) { const TValue *slot; \
if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
Protect(luaV_finishset(L,t,k,v,slot)); }
void luaV_execute (lua_State *L) {
CallInfo *ci = L->ci;
LClosure *cl;
TValue *k;
StkId base;
ci->callstatus |= CIST_FRESH;  
newframe:  
lua_assert(ci == L->ci);
cl = clLvalue(ci->func);  
k = cl->p->k;  
base = ci->u.l.base;  
for (;;) {
Instruction i;
StkId ra;
vmfetch();
vmdispatch (GET_OPCODE(i)) {
vmcase(OP_MOVE) {
setobjs2s(L, ra, RB(i));
vmbreak;
}
vmcase(OP_LOADK) {
TValue *rb = k + GETARG_Bx(i);
setobj2s(L, ra, rb);
vmbreak;
}
vmcase(OP_LOADKX) {
TValue *rb;
lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
rb = k + GETARG_Ax(*ci->u.l.savedpc++);
setobj2s(L, ra, rb);
vmbreak;
}
vmcase(OP_LOADBOOL) {
setbvalue(ra, GETARG_B(i));
if (GETARG_C(i)) ci->u.l.savedpc++;  
vmbreak;
}
vmcase(OP_LOADNIL) {
int b = GETARG_B(i);
do {
setnilvalue(ra++);
} while (b--);
vmbreak;
}
vmcase(OP_GETUPVAL) {
int b = GETARG_B(i);
setobj2s(L, ra, cl->upvals[b]->v);
vmbreak;
}
vmcase(OP_GETTABUP) {
TValue *upval = cl->upvals[GETARG_B(i)]->v;
TValue *rc = RKC(i);
gettableProtected(L, upval, rc, ra);
vmbreak;
}
vmcase(OP_GETTABLE) {
StkId rb = RB(i);
TValue *rc = RKC(i);
gettableProtected(L, rb, rc, ra);
vmbreak;
}
vmcase(OP_SETTABUP) {
TValue *upval = cl->upvals[GETARG_A(i)]->v;
TValue *rb = RKB(i);
TValue *rc = RKC(i);
settableProtected(L, upval, rb, rc);
vmbreak;
}
vmcase(OP_SETUPVAL) {
UpVal *uv = cl->upvals[GETARG_B(i)];
setobj(L, uv->v, ra);
luaC_upvalbarrier(L, uv);
vmbreak;
}
vmcase(OP_SETTABLE) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
settableProtected(L, ra, rb, rc);
vmbreak;
}
vmcase(OP_NEWTABLE) {
int b = GETARG_B(i);
int c = GETARG_C(i);
Table *t = luaH_new(L);
sethvalue(L, ra, t);
if (b != 0 || c != 0)
luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));
checkGC(L, ra + 1);
vmbreak;
}
vmcase(OP_SELF) {
const TValue *aux;
StkId rb = RB(i);
TValue *rc = RKC(i);
TString *key = tsvalue(rc);  
setobjs2s(L, ra + 1, rb);
if (luaV_fastget(L, rb, key, aux, luaH_getstr)) {
setobj2s(L, ra, aux);
}
else Protect(luaV_finishget(L, rb, rc, ra, aux));
vmbreak;
}
vmcase(OP_ADD) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (ttisinteger(rb) && ttisinteger(rc)) {
lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
setivalue(ra, intop(+, ib, ic));
}
else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_numadd(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_ADD)); }
vmbreak;
}
vmcase(OP_SUB) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (ttisinteger(rb) && ttisinteger(rc)) {
lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
setivalue(ra, intop(-, ib, ic));
}
else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_numsub(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SUB)); }
vmbreak;
}
vmcase(OP_MUL) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (ttisinteger(rb) && ttisinteger(rc)) {
lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
setivalue(ra, intop(*, ib, ic));
}
else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_nummul(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MUL)); }
vmbreak;
}
vmcase(OP_DIV) {  
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_numdiv(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_DIV)); }
vmbreak;
}
vmcase(OP_BAND) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Integer ib; lua_Integer ic;
if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
setivalue(ra, intop(&, ib, ic));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BAND)); }
vmbreak;
}
vmcase(OP_BOR) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Integer ib; lua_Integer ic;
if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
setivalue(ra, intop(|, ib, ic));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BOR)); }
vmbreak;
}
vmcase(OP_BXOR) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Integer ib; lua_Integer ic;
if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
setivalue(ra, intop(^, ib, ic));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BXOR)); }
vmbreak;
}
vmcase(OP_SHL) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Integer ib; lua_Integer ic;
if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
setivalue(ra, luaV_shiftl(ib, ic));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHL)); }
vmbreak;
}
vmcase(OP_SHR) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Integer ib; lua_Integer ic;
if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
setivalue(ra, luaV_shiftl(ib, -ic));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHR)); }
vmbreak;
}
vmcase(OP_MOD) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (ttisinteger(rb) && ttisinteger(rc)) {
lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
setivalue(ra, luaV_mod(L, ib, ic));
}
else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
lua_Number m;
luai_nummod(L, nb, nc, m);
setfltvalue(ra, m);
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MOD)); }
vmbreak;
}
vmcase(OP_IDIV) {  
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (ttisinteger(rb) && ttisinteger(rc)) {
lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
setivalue(ra, luaV_div(L, ib, ic));
}
else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_numidiv(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_IDIV)); }
vmbreak;
}
vmcase(OP_POW) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
lua_Number nb; lua_Number nc;
if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
setfltvalue(ra, luai_numpow(L, nb, nc));
}
else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_POW)); }
vmbreak;
}
vmcase(OP_UNM) {
TValue *rb = RB(i);
lua_Number nb;
if (ttisinteger(rb)) {
lua_Integer ib = ivalue(rb);
setivalue(ra, intop(-, 0, ib));
}
else if (tonumber(rb, &nb)) {
setfltvalue(ra, luai_numunm(L, nb));
}
else {
Protect(luaT_trybinTM(L, rb, rb, ra, TM_UNM));
}
vmbreak;
}
vmcase(OP_BNOT) {
TValue *rb = RB(i);
lua_Integer ib;
if (tointeger(rb, &ib)) {
setivalue(ra, intop(^, ~l_castS2U(0), ib));
}
else {
Protect(luaT_trybinTM(L, rb, rb, ra, TM_BNOT));
}
vmbreak;
}
vmcase(OP_NOT) {
TValue *rb = RB(i);
int res = l_isfalse(rb);  
setbvalue(ra, res);
vmbreak;
}
vmcase(OP_LEN) {
Protect(luaV_objlen(L, ra, RB(i)));
vmbreak;
}
vmcase(OP_CONCAT) {
int b = GETARG_B(i);
int c = GETARG_C(i);
StkId rb;
L->top = base + c + 1;  
Protect(luaV_concat(L, c - b + 1));
ra = RA(i);  
rb = base + b;
setobjs2s(L, ra, rb);
checkGC(L, (ra >= rb ? ra + 1 : rb));
L->top = ci->top;  
vmbreak;
}
vmcase(OP_JMP) {
dojump(ci, i, 0);
vmbreak;
}
vmcase(OP_EQ) {
TValue *rb = RKB(i);
TValue *rc = RKC(i);
Protect(
if (luaV_equalobj(L, rb, rc) != GETARG_A(i))
ci->u.l.savedpc++;
else
donextjump(ci);
)
vmbreak;
}
vmcase(OP_LT) {
Protect(
if (luaV_lessthan(L, RKB(i), RKC(i)) != GETARG_A(i))
ci->u.l.savedpc++;
else
donextjump(ci);
)
vmbreak;
}
vmcase(OP_LE) {
Protect(
if (luaV_lessequal(L, RKB(i), RKC(i)) != GETARG_A(i))
ci->u.l.savedpc++;
else
donextjump(ci);
)
vmbreak;
}
vmcase(OP_TEST) {
if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra))
ci->u.l.savedpc++;
else
donextjump(ci);
vmbreak;
}
vmcase(OP_TESTSET) {
TValue *rb = RB(i);
if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb))
ci->u.l.savedpc++;
else {
setobjs2s(L, ra, rb);
donextjump(ci);
}
vmbreak;
}
vmcase(OP_CALL) {
int b = GETARG_B(i);
int nresults = GETARG_C(i) - 1;
if (b != 0) L->top = ra+b;  
if (luaD_precall(L, ra, nresults)) {  
if (nresults >= 0)
L->top = ci->top;  
Protect((void)0);  
}
else {  
ci = L->ci;
goto newframe;  
}
vmbreak;
}
vmcase(OP_TAILCALL) {
int b = GETARG_B(i);
if (b != 0) L->top = ra+b;  
lua_assert(GETARG_C(i) - 1 == LUA_MULTRET);
if (luaD_precall(L, ra, LUA_MULTRET)) {  
Protect((void)0);  
}
else {
CallInfo *nci = L->ci;  
CallInfo *oci = nci->previous;  
StkId nfunc = nci->func;  
StkId ofunc = oci->func;  
StkId lim = nci->u.l.base + getproto(nfunc)->numparams;
int aux;
if (cl->p->sizep > 0) luaF_close(L, oci->u.l.base);
for (aux = 0; nfunc + aux < lim; aux++)
setobjs2s(L, ofunc + aux, nfunc + aux);
oci->u.l.base = ofunc + (nci->u.l.base - nfunc);  
oci->top = L->top = ofunc + (L->top - nfunc);  
oci->u.l.savedpc = nci->u.l.savedpc;
oci->callstatus |= CIST_TAIL;  
ci = L->ci = oci;  
lua_assert(L->top == oci->u.l.base + getproto(ofunc)->maxstacksize);
goto newframe;  
}
vmbreak;
}
vmcase(OP_RETURN) {
int b = GETARG_B(i);
if (cl->p->sizep > 0) luaF_close(L, base);
b = luaD_poscall(L, ci, ra, (b != 0 ? b - 1 : cast_int(L->top - ra)));
if (ci->callstatus & CIST_FRESH)  
return;  
else {  
ci = L->ci;
if (b) L->top = ci->top;
lua_assert(isLua(ci));
lua_assert(GET_OPCODE(*((ci)->u.l.savedpc - 1)) == OP_CALL);
goto newframe;  
}
}
vmcase(OP_FORLOOP) {
if (ttisinteger(ra)) {  
lua_Integer step = ivalue(ra + 2);
lua_Integer idx = intop(+, ivalue(ra), step); 
lua_Integer limit = ivalue(ra + 1);
if ((0 < step) ? (idx <= limit) : (limit <= idx)) {
ci->u.l.savedpc += GETARG_sBx(i);  
chgivalue(ra, idx);  
setivalue(ra + 3, idx);  
}
}
else {  
lua_Number step = fltvalue(ra + 2);
lua_Number idx = luai_numadd(L, fltvalue(ra), step); 
lua_Number limit = fltvalue(ra + 1);
if (luai_numlt(0, step) ? luai_numle(idx, limit)
: luai_numle(limit, idx)) {
ci->u.l.savedpc += GETARG_sBx(i);  
chgfltvalue(ra, idx);  
setfltvalue(ra + 3, idx);  
}
}
vmbreak;
}
vmcase(OP_FORPREP) {
TValue *init = ra;
TValue *plimit = ra + 1;
TValue *pstep = ra + 2;
lua_Integer ilimit;
int stopnow;
if (ttisinteger(init) && ttisinteger(pstep) &&
forlimit(plimit, &ilimit, ivalue(pstep), &stopnow)) {
lua_Integer initv = (stopnow ? 0 : ivalue(init));
setivalue(plimit, ilimit);
setivalue(init, intop(-, initv, ivalue(pstep)));
}
else {  
lua_Number ninit; lua_Number nlimit; lua_Number nstep;
if (!tonumber(plimit, &nlimit))
luaG_runerror(L, "'for' limit must be a number");
setfltvalue(plimit, nlimit);
if (!tonumber(pstep, &nstep))
luaG_runerror(L, "'for' step must be a number");
setfltvalue(pstep, nstep);
if (!tonumber(init, &ninit))
luaG_runerror(L, "'for' initial value must be a number");
setfltvalue(init, luai_numsub(L, ninit, nstep));
}
ci->u.l.savedpc += GETARG_sBx(i);
vmbreak;
}
vmcase(OP_TFORCALL) {
StkId cb = ra + 3;  
setobjs2s(L, cb+2, ra+2);
setobjs2s(L, cb+1, ra+1);
setobjs2s(L, cb, ra);
L->top = cb + 3;  
Protect(luaD_call(L, cb, GETARG_C(i)));
L->top = ci->top;
i = *(ci->u.l.savedpc++);  
ra = RA(i);
lua_assert(GET_OPCODE(i) == OP_TFORLOOP);
goto l_tforloop;
}
vmcase(OP_TFORLOOP) {
l_tforloop:
if (!ttisnil(ra + 1)) {  
setobjs2s(L, ra, ra + 1);  
ci->u.l.savedpc += GETARG_sBx(i);  
}
vmbreak;
}
vmcase(OP_SETLIST) {
int n = GETARG_B(i);
int c = GETARG_C(i);
unsigned int last;
Table *h;
if (n == 0) n = cast_int(L->top - ra) - 1;
if (c == 0) {
lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
c = GETARG_Ax(*ci->u.l.savedpc++);
}
h = hvalue(ra);
last = ((c-1)*LFIELDS_PER_FLUSH) + n;
if (last > h->sizearray)  
luaH_resizearray(L, h, last);  
for (; n > 0; n--) {
TValue *val = ra+n;
luaH_setint(L, h, last--, val);
luaC_barrierback(L, h, val);
}
L->top = ci->top;  
vmbreak;
}
vmcase(OP_CLOSURE) {
Proto *p = cl->p->p[GETARG_Bx(i)];
LClosure *ncl = getcached(p, cl->upvals, base);  
if (ncl == NULL)  
pushclosure(L, p, cl->upvals, base, ra);  
else
setclLvalue(L, ra, ncl);  
checkGC(L, ra + 1);
vmbreak;
}
vmcase(OP_VARARG) {
int b = GETARG_B(i) - 1;  
int j;
int n = cast_int(base - ci->func) - cl->p->numparams - 1;
if (n < 0)  
n = 0;  
if (b < 0) {  
b = n;  
Protect(luaD_checkstack(L, n));
ra = RA(i);  
L->top = ra + n;
}
for (j = 0; j < b && j < n; j++)
setobjs2s(L, ra + j, base - n + j);
for (; j < b; j++)  
setnilvalue(ra + j);
vmbreak;
}
vmcase(OP_EXTRAARG) {
lua_assert(0);
vmbreak;
}
}
}
}
#define lzio_c
#define LUA_CORE
#include <string.h>
int luaZ_fill (ZIO *z) {
size_t size;
lua_State *L = z->L;
const char *buff;
lua_unlock(L);
buff = z->reader(L, z->data, &size);
lua_lock(L);
if (buff == NULL || size == 0)
return EOZ;
z->n = size - 1;  
z->p = buff;
return cast_uchar(*(z->p++));
}
void luaZ_init (lua_State *L, ZIO *z, lua_Reader reader, void *data) {
z->L = L;
z->reader = reader;
z->data = data;
z->n = 0;
z->p = NULL;
}
size_t luaZ_read (ZIO *z, void *b, size_t n) {
while (n) {
size_t m;
if (z->n == 0) {  
if (luaZ_fill(z) == EOZ)  
return n;  
else {
z->n++;  
z->p--;
}
}
m = (n <= z->n) ? n : z->n;  
memcpy(b, z->p, m);
z->n -= m;
z->p += m;
b = (char *)b + m;
n -= m;
}
return 0;
}
};
};
#undef next
#undef PI
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "../MQ2Plugin.h"
PreSetup("MQ2Lua");
namespace Lua5 {
extern "C" {
#define loadlib_c
#define LUA_LIB
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined (LUA_IGMARK)
#define LUA_IGMARK		"-"
#endif
#if !defined(LUA_CSUBSEP)
#define LUA_CSUBSEP		LUA_DIRSEP
#endif
#if !defined(LUA_LSUBSEP)
#define LUA_LSUBSEP		LUA_DIRSEP
#endif
#define LUA_POF		"luaopen_"
#define LUA_OFSEP	"_"
static const int CLIBS = 0;
#define LIB_FAIL	"open"
#define setprogdir(L)           ((void)0)
static void lsys_unloadlib (void *lib);
static void *lsys_load (lua_State *L, const char *path, int seeglb);
static lua_CFunction lsys_sym (lua_State *L, void *lib, const char *sym);
#if defined(LUA_USE_DLOPEN)	
#include <dlfcn.h>
#if defined(__GNUC__)
#define cast_func(p) (__extension__ (lua_CFunction)(p))
#else
#define cast_func(p) ((lua_CFunction)(p))
#endif
static void lsys_unloadlib (void *lib) {
dlclose(lib);
}
static void *lsys_load (lua_State *L, const char *path, int seeglb) {
void *lib = dlopen(path, RTLD_NOW | (seeglb ? RTLD_GLOBAL : RTLD_LOCAL));
if (lib == NULL) lua_pushstring(L, dlerror());
return lib;
}
static lua_CFunction lsys_sym (lua_State *L, void *lib, const char *sym) {
lua_CFunction f = cast_func(dlsym(lib, sym));
if (f == NULL) lua_pushstring(L, dlerror());
return f;
}
#elif defined(LUA_DL_DLL)	
#include <windows.h>
#if !defined(LUA_LLE_FLAGS)
#define LUA_LLE_FLAGS	0
#endif
#undef setprogdir
static void setprogdir (lua_State *L) {
char buff[MAX_PATH + 1];
char *lb;
DWORD nsize = sizeof(buff)/sizeof(char);
DWORD n = GetModuleFileNameA(NULL, buff, nsize);  
if (n == 0 || n == nsize || (lb = strrchr(buff, '\\')) == NULL)
luaL_error(L, "unable to get ModuleFileName");
else {
*lb = '\0';  
luaL_gsub(L, lua_tostring(L, -1), LUA_EXEC_DIR, buff);
lua_remove(L, -2);  
}
}
static void pusherror (lua_State *L) {
int error = GetLastError();
char buffer[128];
if (FormatMessageA(FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_FROM_SYSTEM,
NULL, error, 0, buffer, sizeof(buffer)/sizeof(char), NULL))
lua_pushstring(L, buffer);
else
lua_pushfstring(L, "system error %d\n", error);
}
static void lsys_unloadlib (void *lib) {
FreeLibrary((HMODULE)lib);
}
static void *lsys_load (lua_State *L, const char *path, int seeglb) {
HMODULE lib = LoadLibraryExA(path, NULL, LUA_LLE_FLAGS);
(void)(seeglb);  
if (lib == NULL) pusherror(L);
return lib;
}
static lua_CFunction lsys_sym (lua_State *L, void *lib, const char *sym) {
lua_CFunction f = (lua_CFunction)GetProcAddress((HMODULE)lib, sym);
if (f == NULL) pusherror(L);
return f;
}
#else				
#undef LIB_FAIL
#define LIB_FAIL	"absent"
#define DLMSG	"dynamic libraries not enabled; check your Lua installation"
static void lsys_unloadlib (void *lib) {
(void)(lib);  
}
static void *lsys_load (lua_State *L, const char *path, int seeglb) {
(void)(path); (void)(seeglb);  
lua_pushliteral(L, DLMSG);
return NULL;
}
static lua_CFunction lsys_sym (lua_State *L, void *lib, const char *sym) {
(void)(lib); (void)(sym);  
lua_pushliteral(L, DLMSG);
return NULL;
}
#endif				
#if !defined(LUA_PATH_VAR)
#define LUA_PATH_VAR    "LUA_PATH"
#endif
#if !defined(LUA_CPATH_VAR)
#define LUA_CPATH_VAR   "LUA_CPATH"
#endif
#define AUXMARK         "\1"	
static int noenv (lua_State *L) {
int b;
lua_getfield(L, LUA_REGISTRYINDEX, "LUA_NOENV");
b = lua_toboolean(L, -1);
lua_pop(L, 1);  
return b;
}
static void setpath (lua_State *L, const char *fieldname,
const char *envname,
const char *dft) {
const char *nver = lua_pushfstring(L, "%s%s", envname, LUA_VERSUFFIX);
const char *path = getenv(nver);  
if (path == NULL)  
path = getenv(envname);  
if (path == NULL || noenv(L))  
lua_pushstring(L, dft);  
else {
path = luaL_gsub(L, path, LUA_PATH_SEP LUA_PATH_SEP,
LUA_PATH_SEP AUXMARK LUA_PATH_SEP);
luaL_gsub(L, path, AUXMARK, dft);
lua_remove(L, -2); 
}
setprogdir(L);
lua_setfield(L, -3, fieldname);  
lua_pop(L, 1);  
}
static void *checkclib (lua_State *L, const char *path) {
void *plib;
lua_rawgetp(L, LUA_REGISTRYINDEX, &CLIBS);
lua_getfield(L, -1, path);
plib = lua_touserdata(L, -1);  
lua_pop(L, 2);  
return plib;
}
static void addtoclib (lua_State *L, const char *path, void *plib) {
lua_rawgetp(L, LUA_REGISTRYINDEX, &CLIBS);
lua_pushlightuserdata(L, plib);
lua_pushvalue(L, -1);
lua_setfield(L, -3, path);  
lua_rawseti(L, -2, luaL_len(L, -2) + 1);  
lua_pop(L, 1);  
}
static int gctm (lua_State *L) {
lua_Integer n = luaL_len(L, 1);
for (; n >= 1; n--) {  
lua_rawgeti(L, 1, n);  
lsys_unloadlib(lua_touserdata(L, -1));
lua_pop(L, 1);  
}
return 0;
}
#define ERRLIB		1
#define ERRFUNC		2
static int lookforfunc (lua_State *L, const char *path, const char *sym) {
void *reg = checkclib(L, path);  
if (reg == NULL) {  
reg = lsys_load(L, path, *sym == '*');  
if (reg == NULL) return ERRLIB;  
addtoclib(L, path, reg);
}
if (*sym == '*') {  
lua_pushboolean(L, 1);  
return 0;  
}
else {
lua_CFunction f = lsys_sym(L, reg, sym);
if (f == NULL)
return ERRFUNC;  
lua_pushcfunction(L, f);  
return 0;  
}
}
static int ll_loadlib (lua_State *L) {
const char *path = luaL_checkstring(L, 1);
const char *init = luaL_checkstring(L, 2);
int stat = lookforfunc(L, path, init);
if (stat == 0)  
return 1;  
else {  
lua_pushnil(L);
lua_insert(L, -2);
lua_pushstring(L, (stat == ERRLIB) ?  LIB_FAIL : "init");
return 3;  
}
}
static int readable (const char *filename) {
FILE *f = fopen(filename, "r");  
if (f == NULL) return 0;  
fclose(f);
return 1;
}
static const char *pushnexttemplate (lua_State *L, const char *path) {
const char *l;
while (*path == *LUA_PATH_SEP) path++;  
if (*path == '\0') return NULL;  
l = strchr(path, *LUA_PATH_SEP);  
if (l == NULL) l = path + strlen(path);
lua_pushlstring(L, path, l - path);  
return l;
}
static const char *searchpath (lua_State *L, const char *name,
const char *path,
const char *sep,
const char *dirsep) {
luaL_Buffer msg;  
luaL_buffinit(L, &msg);
if (*sep != '\0')  
name = luaL_gsub(L, name, sep, dirsep);  
while ((path = pushnexttemplate(L, path)) != NULL) {
const char *filename = luaL_gsub(L, lua_tostring(L, -1),
LUA_PATH_MARK, name);
lua_remove(L, -2);  
if (readable(filename))  
return filename;  
lua_pushfstring(L, "\n\tno file '%s'", filename);
lua_remove(L, -2);  
luaL_addvalue(&msg);  
}
luaL_pushresult(&msg);  
return NULL;  
}
static int ll_searchpath (lua_State *L) {
const char *f = searchpath(L, luaL_checkstring(L, 1),
luaL_checkstring(L, 2),
luaL_optstring(L, 3, "."),
luaL_optstring(L, 4, LUA_DIRSEP));
if (f != NULL) return 1;
else {  
lua_pushnil(L);
lua_insert(L, -2);
return 2;  
}
}
static const char *findfile (lua_State *L, const char *name,
const char *pname,
const char *dirsep) {
const char *path;
lua_getfield(L, lua_upvalueindex(1), pname);
path = lua_tostring(L, -1);
if (path == NULL)
luaL_error(L, "'package.%s' must be a string", pname);
return searchpath(L, name, path, ".", dirsep);
}
static int checkload (lua_State *L, int stat, const char *filename) {
if (stat) {  
lua_pushstring(L, filename);  
return 2;  
}
else
return luaL_error(L, "error loading module '%s' from file '%s':\n\t%s",
lua_tostring(L, 1), filename, lua_tostring(L, -1));
}
static int searcher_Lua (lua_State *L) {
const char *filename;
const char *name = luaL_checkstring(L, 1);
filename = findfile(L, name, "path", LUA_LSUBSEP);
if (filename == NULL) return 1;  
return checkload(L, (luaL_loadfile(L, filename) == LUA_OK), filename);
}
static int loadfunc (lua_State *L, const char *filename, const char *modname) {
const char *openfunc;
const char *mark;
modname = luaL_gsub(L, modname, ".", LUA_OFSEP);
mark = strchr(modname, *LUA_IGMARK);
if (mark) {
int stat;
openfunc = lua_pushlstring(L, modname, mark - modname);
openfunc = lua_pushfstring(L, LUA_POF"%s", openfunc);
stat = lookforfunc(L, filename, openfunc);
if (stat != ERRFUNC) return stat;
modname = mark + 1;  
}
openfunc = lua_pushfstring(L, LUA_POF"%s", modname);
return lookforfunc(L, filename, openfunc);
}
static int searcher_C (lua_State *L) {
const char *name = luaL_checkstring(L, 1);
const char *filename = findfile(L, name, "cpath", LUA_CSUBSEP);
if (filename == NULL) return 1;  
return checkload(L, (loadfunc(L, filename, name) == 0), filename);
}
static int searcher_Croot (lua_State *L) {
const char *filename;
const char *name = luaL_checkstring(L, 1);
const char *p = strchr(name, '.');
int stat;
if (p == NULL) return 0;  
lua_pushlstring(L, name, p - name);
filename = findfile(L, lua_tostring(L, -1), "cpath", LUA_CSUBSEP);
if (filename == NULL) return 1;  
if ((stat = loadfunc(L, filename, name)) != 0) {
if (stat != ERRFUNC)
return checkload(L, 0, filename);  
else {  
lua_pushfstring(L, "\n\tno module '%s' in file '%s'", name, filename);
return 1;
}
}
lua_pushstring(L, filename);  
return 2;
}
static int searcher_preload (lua_State *L) {
const char *name = luaL_checkstring(L, 1);
lua_getfield(L, LUA_REGISTRYINDEX, LUA_PRELOAD_TABLE);
if (lua_getfield(L, -1, name) == LUA_TNIL)  
lua_pushfstring(L, "\n\tno field package.preload['%s']", name);
return 1;
}
static void findloader (lua_State *L, const char *name) {
int i;
luaL_Buffer msg;  
luaL_buffinit(L, &msg);
if (lua_getfield(L, lua_upvalueindex(1), "searchers") != LUA_TTABLE)
luaL_error(L, "'package.searchers' must be a table");
for (i = 1; ; i++) {
if (lua_rawgeti(L, 3, i) == LUA_TNIL) {  
lua_pop(L, 1);  
luaL_pushresult(&msg);  
luaL_error(L, "module '%s' not found:%s", name, lua_tostring(L, -1));
}
lua_pushstring(L, name);
lua_call(L, 1, 2);  
if (lua_isfunction(L, -2))  
return;  
else if (lua_isstring(L, -2)) {  
lua_pop(L, 1);  
luaL_addvalue(&msg);  
}
else
lua_pop(L, 2);  
}
}
static int ll_require (lua_State *L) {
const char *name = luaL_checkstring(L, 1);
lua_settop(L, 1);  
lua_getfield(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
lua_getfield(L, 2, name);  
if (lua_toboolean(L, -1))  
return 1;  
lua_pop(L, 1);  
findloader(L, name);
lua_pushstring(L, name);  
lua_insert(L, -2);  
lua_call(L, 2, 1);  
if (!lua_isnil(L, -1))  
lua_setfield(L, 2, name);  
if (lua_getfield(L, 2, name) == LUA_TNIL) {   
lua_pushboolean(L, 1);  
lua_pushvalue(L, -1);  
lua_setfield(L, 2, name);  
}
return 1;
}
#if defined(LUA_COMPAT_MODULE)
static void set_env (lua_State *L) {
lua_Debug ar;
if (lua_getstack(L, 1, &ar) == 0 ||
lua_getinfo(L, "f", &ar) == 0 ||  
lua_iscfunction(L, -1))
luaL_error(L, "'module' not called from a Lua function");
lua_pushvalue(L, -2);  
lua_setupvalue(L, -2, 1);
lua_pop(L, 1);  
}
static void dooptions (lua_State *L, int n) {
int i;
for (i = 2; i <= n; i++) {
if (lua_isfunction(L, i)) {  
lua_pushvalue(L, i);  
lua_pushvalue(L, -2);  
lua_call(L, 1, 0);
}
}
}
static void modinit (lua_State *L, const char *modname) {
const char *dot;
lua_pushvalue(L, -1);
lua_setfield(L, -2, "_M");  
lua_pushstring(L, modname);
lua_setfield(L, -2, "_NAME");
dot = strrchr(modname, '.');  
if (dot == NULL) dot = modname;
else dot++;
lua_pushlstring(L, modname, dot - modname);
lua_setfield(L, -2, "_PACKAGE");
}
static int ll_module (lua_State *L) {
const char *modname = luaL_checkstring(L, 1);
int lastarg = lua_gettop(L);  
luaL_pushmodule(L, modname, 1);  
if (lua_getfield(L, -1, "_NAME") != LUA_TNIL)
lua_pop(L, 1);  
else {  
lua_pop(L, 1);
modinit(L, modname);
}
lua_pushvalue(L, -1);
set_env(L);
dooptions(L, lastarg);
return 1;
}
static int ll_seeall (lua_State *L) {
luaL_checktype(L, 1, LUA_TTABLE);
if (!lua_getmetatable(L, 1)) {
lua_createtable(L, 0, 1); 
lua_pushvalue(L, -1);
lua_setmetatable(L, 1);
}
lua_pushglobaltable(L);
lua_setfield(L, -2, "__index");  
return 0;
}
#endif
static const luaL_Reg pk_funcs[] = {
{"loadlib", ll_loadlib},
{"searchpath", ll_searchpath},
#if defined(LUA_COMPAT_MODULE)
{"seeall", ll_seeall},
#endif
{"preload", NULL},
{"cpath", NULL},
{"path", NULL},
{"searchers", NULL},
{"loaded", NULL},
{NULL, NULL}
};
static const luaL_Reg ll_funcs[] = {
#if defined(LUA_COMPAT_MODULE)
{"module", ll_module},
#endif
{"require", ll_require},
{NULL, NULL}
};
static void createsearcherstable (lua_State *L) {
static const lua_CFunction searchers[] =
{searcher_preload, searcher_Lua, searcher_C, searcher_Croot, NULL};
int i;
lua_createtable(L, sizeof(searchers)/sizeof(searchers[0]) - 1, 0);
for (i=0; searchers[i] != NULL; i++) {
lua_pushvalue(L, -2);  
lua_pushcclosure(L, searchers[i], 1);
lua_rawseti(L, -2, i+1);
}
#if defined(LUA_COMPAT_LOADERS)
lua_pushvalue(L, -1);  
lua_setfield(L, -3, "loaders");  
#endif
lua_setfield(L, -2, "searchers");  
}
static void createclibstable (lua_State *L) {
lua_newtable(L);  
lua_createtable(L, 0, 1);  
lua_pushcfunction(L, gctm);
lua_setfield(L, -2, "__gc");  
lua_setmetatable(L, -2);
lua_rawsetp(L, LUA_REGISTRYINDEX, &CLIBS);  
}
LUAMOD_API int luaopen_package (lua_State *L) {
createclibstable(L);
luaL_newlib(L, pk_funcs);  
createsearcherstable(L);
setpath(L, "path", LUA_PATH_VAR, LUA_PATH_DEFAULT);
setpath(L, "cpath", LUA_CPATH_VAR, LUA_CPATH_DEFAULT);
lua_pushliteral(L, LUA_DIRSEP "\n" LUA_PATH_SEP "\n" LUA_PATH_MARK "\n"
LUA_EXEC_DIR "\n" LUA_IGMARK "\n");
lua_setfield(L, -2, "config");
luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_LOADED_TABLE);
lua_setfield(L, -2, "loaded");
luaL_getsubtable(L, LUA_REGISTRYINDEX, LUA_PRELOAD_TABLE);
lua_setfield(L, -2, "preload");
lua_pushglobaltable(L);
lua_pushvalue(L, -2);  
luaL_setfuncs(L, ll_funcs, 1);  
lua_pop(L, 1);  
return 1;  
}
};
};
using namespace Lua5;
#define WIN32LEAN_AND_MEAN
#include <windows.h>
double ootimer_freq;
static int lua_timer(lua_State *L)
{
LARGE_INTEGER now;
QueryPerformanceCounter(&now);
lua_pushnumber(L, (double)now.QuadPart * ootimer_freq);
return 1;
}
int luaopen_timer (lua_State *L) {
static const luaL_Reg lib [] =
{
{"now", lua_timer},
{NULL, NULL}
};
LARGE_INTEGER now;
QueryPerformanceFrequency(&now);
ootimer_freq = 1.0 / now.QuadPart;
luaL_newlib(L, lib);
return 1;
}
lua_State *_L;
struct MQ2IDXVAR {
MQ2TYPEVAR Type;
char Member[32];
};
BOOL dataType(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (MQ2Type* pType = FindMQ2DataType(szIndex))
{
Ret.Ptr = pType;
Ret.Type = pTypeType;
return true;
}
return false;
}
BOOL dataXTarget(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"XTarget", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataCombatAbilityReady(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"CombatAbilityReady", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataAltAbilityReady(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"AltAbilityReady", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataItemReady(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"ItemReady", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataSpellReady(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"SpellReady", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataMyBuff(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"Buff", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataMyAura(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if (pCharData)
{
Ret.Ptr = pCharData;
Ret.Type = pCharacterType;
return pCharacterType ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"Aura", (PCHAR)szIndex, Ret) : 0;
}
return false;
}
BOOL dataTargetBuff(PCHAR szIndex, MQ2TYPEVAR &Ret)
{
if(dataTarget("", Ret))
return Ret.Type ? Ret.Type->GetMember(Ret.VarPtr, (PCHAR)"Buff", (PCHAR)szIndex, Ret) : 0;
return false;
}
static int push_litvar(lua_State *L, MQ2TYPEVAR &t) {
if (t.Type == pIntType || t.Type == pBoolType || t.Type == pByteType) {
lua_pushinteger(L, t.Int);
} else if (t.Type == pFloatType)
lua_pushnumber(L, t.Float);
else if(t.Type == pDoubleType)
lua_pushnumber(L, t.Double);
// timestamps are always milliseconds int 64 bits
else if(t.Type == pInt64Type )
lua_pushnumber(L, (double)t.Int64);
else if(t.Type == pTimeStampType)
lua_pushnumber(L, (double)t.Int64/1000.0);
else if (t.Type == pStringType)
lua_pushstring(L, (const char*)t.Ptr);
else
return 0;
return 1;
}
static int push_typevar(lua_State *L, MQ2TYPEVAR &t) {
int rv = push_litvar(L, t);
if (!rv && t.Type) {
// it is something that is not a lua native type, that can be indexed
// in the future; push it as first level indexable
MQ2TYPEVAR *p = (MQ2TYPEVAR*)lua_newuserdata(L, sizeof(MQ2TYPEVAR));
*p = t;
luaL_getmetatable( L, "imq2" );
lua_setmetatable( L, -2 );
// WriteChatf("Pushed type: %s",t.Type->GetName());
rv = 1;
}
return rv;
}
static int mq2_lookup(lua_State *L, MQ2TYPEVAR& t, const char* member, const char* index, bool isglobal=false) {
MQ2TYPEVAR r = t;
// if we can index now, great! superb, keep going
if ((member[0] != '.') && t.Type && t.Type->GetMember(t.VarPtr, (PCHAR)member, (PCHAR)index, r)) {
// WriteChatf("Index on %s with %s.%s worked!", t.Type? t.Type->GetName() : "nil", member, index);
return push_typevar(L, r);
}
// WriteChatf("Index on %s with %s.%s needs array index or is unknown",t.Type->GetName(), member, index);
// otherwise, we are a bit screwed, and need to wait for an
// additional index that might still work for us
if (!index[0] && t.Type) {
MQ2IDXVAR *p = (MQ2IDXVAR*)lua_newuserdata(L, sizeof(MQ2IDXVAR));
p->Type = t;
strncpy_s(p->Member, member + (member[0]=='.'), sizeof(p->Member));
luaL_getmetatable( L, "lmq2" );
lua_setmetatable( L, -2 );
// WriteChatf("waiting for array index for, udata=%lx", (long)lua_touserdata(L,-1));
return 1;
}
return 0;
}
static int imq2_load( lua_State *L ) {
MQ2TYPEVAR &t = *(MQ2TYPEVAR*)lua_touserdata( L, -2 );
const char* i = lua_tostring( L, -1 );
if (!i) {
// WriteChatf("Index on %s nil!",t.Type->GetName());
return 0;
}
return mq2_lookup(L, t, i, "");
}
static int imq2_ton( lua_State *L ) {
MQ2TYPEVAR &t = *(MQ2TYPEVAR*)lua_touserdata( L, -1 );
char data[MAX_STRING];
t.Type->ToString(t.VarPtr, data);
lua_pushnumber(L, atol(data));
return 1;
}
static int imq2_tos( lua_State *L ) {
MQ2TYPEVAR &t = *(MQ2TYPEVAR*)lua_touserdata( L, -1 );
char data[MAX_STRING];
if(t.Type->ToString(t.VarPtr, data))
lua_pushstring(L, data);
else
lua_pushstring(L, "");
return 1;
}
static int gmq2_tos( lua_State *L ) {
fMQData *t = (fMQData*)lua_touserdata( L, -1 );
MQ2TYPEVAR v;
// if we index a global, we call a function to get the type & pointer
if ((*t)((PCHAR)"", v)) {
char data[MAX_STRING];
if (v.Type && v.Type->ToString(v.VarPtr, data)) {
lua_pushstring(L, data);
} else {
lua_pushstring(L, "");
}
} else {
lua_pushstring(L, "");
}
return 1;
}
static int gmq2_load( lua_State *L ) {
fMQData *t = (fMQData*)lua_touserdata( L, -2 );
const char* i = lua_tostring( L, -1 );
if (!i) {
// WriteChatf("gmq2_load: with non-string");
return 0;
}
MQ2TYPEVAR rv;
// if we index a global, we call a function to get the type & pointer
// WriteChatf("gmq2_load: %s",i);
if ((*t)((PCHAR)i, rv)) {
int r = push_litvar(L, rv);
// WriteChatf("for %s returned %d", i, r);
if (!r) {
MQ2TYPEVAR tv = rv;
if ((*t)((PCHAR)"", tv) && tv.Type == rv.Type && (tv.Type != pGroundType) && tv.Type) {
// WriteChatf("for %s pushing indea", i);
r = mq2_lookup(L, rv, i, "", true);
} else {
r = push_typevar(L, rv);
// WriteChatf("for %s returned %d", tv.Type->GetName(), r);
}
}
return r;
} else {
// WriteChatf("Indexing with %s failed", i);
return 0;
}
}
static int lmq2_load( lua_State *L ) {
MQ2IDXVAR *t = (MQ2IDXVAR*)lua_touserdata( L, -2 );
const char* i = lua_tostring( L, -1 );
if (!t || !i) {
// WriteChatf("lmq2_load: with non-string");
return 0;
}
return mq2_lookup(L, t->Type, (PCHAR)t->Member, (PCHAR)i);
}
static int imq2_eq( lua_State *L ) {
MQ2TYPEVAR *t0 = (MQ2TYPEVAR*)lua_touserdata( L, -2 );
MQ2TYPEVAR *t1 = (MQ2TYPEVAR*)lua_touserdata( L, -1 );
if (t0 && t1 && t0->Ptr == t1->Ptr)
lua_pushinteger(L, 1);
else
lua_pushnil(L);
return 1;
}
static int lmq2_push( lua_State *L, MQ2Type *mq2 ) {
MQ2Type **p = (MQ2Type**)lua_newuserdata(L, sizeof(MQ2Type*)*2);
p[0] = mq2;
p[1] = 0;
luaL_getmetatable( L, "lmq2" );
lua_setmetatable( L, -2 );
return 1;
}
#define AddMQ2Data(name,value) { \
fMQData *f = (fMQData*)lua_newuserdata(L, sizeof(fMQData)); \
*f = value; \
lua_pushvalue(L, -2); \
lua_setmetatable( L, -2 ); \
lua_setglobal( L, name ); \
}
static void lerror(lua_State *L, int status)
{
if (status && L && !lua_isnil(L, -1)) {
const char *msg = lua_tostring(L, -1);
if (!msg) msg = "<unknown error>";
SyntaxError((PCHAR)msg);
MacroLog(0, (PCHAR)msg);
lua_pop(L, 1);
}
}
static int lmq2_cmd(lua_State *L) {
const char* i = lua_tostring( L, -1 );
if (i) {
PCHARINFO pCharInfo=GetCharInfo();
if (!pCharInfo || !pCharInfo->pSpawn)
return 0;
DoCommand(pCharInfo->pSpawn, (PCHAR)i);
return 0;
}
return 0;
}
static bool logfini = false;
static FILE *logfp = 0;
static int lmq2_echo(lua_State *L) {
const char* i = lua_tostring( L, 1 );
if (i) {
const char* i = lua_tostring( L, 1 );
if ((lua_gettop(L) != 2) || (lua_isnumber(L,2) && (lua_tonumber(L,2) != 0)))
WriteChatf("%s",i);
if (lua_gettop(L) == 2) {
if (!logfini) {
PCHARINFO pCharInfo=GetCharInfo();
if (!pCharInfo || !pCharInfo->pSpawn)
return 0;
CHAR fn[MAX_STRING];
strcpy_s(fn,gszINIPath);
strcat_s(fn,"\\Logs\\");
strcat_s(fn, pCharInfo->pSpawn->Name);
strcat_s(fn, ".log");
logfp = fopen(fn, "ab");
logfini = true;
}
if (logfp) {
static time_t past=0;
time_t now = time(0);
char *tmps = ctime(&now);
if (!tmps) tmps = "";
char *tmpe = tmps + strlen(tmps);
while((tmpe>tmps) && ((tmpe[-1] == 10) || (tmpe[-1] == 13))) {
tmpe--;
*tmpe = 0;
}
fprintf(logfp, "%s: %s\n", tmps, i);
// don't flush more than once a second
if (now != past) {
fflush(logfp);
past = now;
}
}
}
}
return 0;
}
static void lstop(lua_State *L, lua_Debug *ar)
{
(void)ar;  
lua_sethook(L, NULL, 0, 0);
luaL_where(L, 0);
lua_pushfstring(L, "%sinterrupted!", lua_tostring(L, -1));
lua_error(L);
}
static void laction(int i)
{
signal(i, SIG_DFL); /* if another SIGINT happens before lstop,
terminate process (default action) */
lua_sethook(_L, lstop, LUA_MASKCALL | LUA_MASKRET | LUA_MASKCOUNT, 1);
}
static int traceback(lua_State *L)
{
if (!lua_isstring(L, 1)) { 
if (lua_isnoneornil(L, 1) ||
!luaL_callmeta(L, 1, "__tostring") ||
!lua_isstring(L, -1))
return 1;  
lua_remove(L, 1);  
}
luaL_traceback(L, L, lua_tostring(L, 1), 1);
return 1;
}
static int docall(lua_State *L, int narg, int clear)
{
int status;
int base = lua_gettop(L) - narg;  
lua_pushcclosure(L, traceback, 0);  
lua_insert(L, base);  
signal(SIGINT, laction);
status = lua_pcall(L, narg, (clear ? 0 : LUA_MULTRET), base);
signal(SIGINT, SIG_DFL);
lua_remove(L, base);  
if (status != LUA_OK) lua_gc(L, LUA_GCCOLLECT, 0);
return status;
}
VOID LuaCmd(PSPAWNINFO pChar, PCHAR s)
{
if (!_L) return;
if (s[0] == '-') {
CHAR fn[MAX_STRING];
strcpy_s(fn,gszINIPath);
strcat_s(fn,"\\");
strcat_s(fn, s+1);
strcat_s(fn, ".lua");
int e = luaL_loadfile(_L, fn) || docall(_L, 0, 1);
if (e)
lerror(_L, e);
} else
lerror(_L, luaL_loadbuffer(_L, s, strlen(s), "cmdline") || docall(_L, 0, 1));
}
// Called once, when the plugin is to initialize
PLUGIN_API VOID InitializePlugin(VOID)
{
DebugSpewAlways("Initializing MQ2Lua");
AddCommand("/lua",LuaCmd,0,0,0);
// Add commands, macro parameters, hooks, etc.
// AddCommand("/mycommand",MyCommand);
// AddParm("$myparm(x)",MyParm);
// AddXMLFile("MQUI_MyXMLFile.xml");
// bmMyBenchmark=AddMQ2Benchmark("My Benchmark Name");
}
// Called once, when the plugin is to shutdown
PLUGIN_API VOID ShutdownPlugin(VOID)
{
DebugSpewAlways("Shutting down MQ2Lua");
RemoveCommand("/lua");
if (_L) {
lua_close(_L);
_L = 0;
}
if (logfp)
fclose(logfp);
logfp = 0;
logfini = 0;
// Remove commands, macro parameters, hooks, etc.
// RemoveMQ2Benchmark(bmMyBenchmark);
// RemoveParm("$myparm(x)");
// RemoveCommand("/mycommand");
// RemoveXMLFile("MQUI_MyXMLFile.xml");
}
double chat_time;
PLUGIN_API DWORD OnIncomingChat(PCHAR Line, DWORD Color) {
if (_L) {
LARGE_INTEGER pre, post;
QueryPerformanceCounter(&pre);
lua_getglobal(_L, "addchat");
lua_pushstring(_L, Line);
lerror(_L, docall(_L, 1, 1));
QueryPerformanceCounter(&post);
chat_time += (double)(post.QuadPart - pre.QuadPart) * ootimer_freq;
}
return 0;
}
double prev_exec;
PLUGIN_API void OnPulse() {
if (_L) {
LARGE_INTEGER pre, post;
QueryPerformanceCounter(&pre);
double ct = chat_time; chat_time = 0;
double pe = prev_exec;
lua_getglobal(_L, "main");
lua_pushnumber(_L, pe);
lua_pushnumber(_L, ct);
lerror(_L, docall(_L, 2, 1));
QueryPerformanceCounter(&post);
prev_exec = (double)(post.QuadPart - pre.QuadPart) * ootimer_freq;
}
}
PLUGIN_API VOID SetGameState(DWORD GameState)
{
if (GameState==GAMESTATE_INGAME)
{
if (!_L && pCharData) {
_L = luaL_newstate();
lua_State *L = _L;
lua_gc(L, LUA_GCSTOP, 0);
luaL_openlibs(L);
lua_gc(L, LUA_GCRESTART, -1);
// construct the metatable for MQ2 objects
luaL_newmetatable( L, "lmq2" );
lua_pushcclosure( L, lmq2_load, 0 );
lua_setfield( L, -2, "__index" );
lua_pushcclosure( L, imq2_eq, 0 );
lua_setfield( L, -2, "__eq" );
lua_pushcclosure( L, imq2_tos, 0 );
lua_setfield( L, -2, "__tostring" );
lua_setglobal(L, "lmq2");
luaL_newmetatable( L, "imq2" );
lua_pushcclosure( L, imq2_load, 0 );
lua_setfield( L, -2, "__index" );
lua_pushcclosure( L, imq2_eq, 0 );
lua_setfield( L, -2, "__eq" );
lua_pushcclosure( L, imq2_tos, 0 );
lua_setfield( L, -2, "__tostring" );
lua_setglobal(L, "imq2");
// and globals
luaL_newmetatable( L, "gmq2" );
lua_pushcclosure( L, gmq2_load, 0 );
lua_setfield( L, -2, "__index" );
lua_pushcclosure( L, gmq2_tos, 0 );
lua_setfield( L, -2, "__tostring" );
AddMQ2Data("Spawn", dataSpawn);
AddMQ2Data("Target", dataTarget);
AddMQ2Data("Me", dataCharacter);
AddMQ2Data("Spell", dataSpell);
AddMQ2Data("Switch", dataSwitch);
AddMQ2Data("Ground", dataGroundItem);
AddMQ2Data("GroundItemCount", dataGroundItemCount);
AddMQ2Data("Merchant", dataMerchant);
AddMQ2Data("PointMerchant", dataPointMerchant);
AddMQ2Data("Mercenary", dataMercenary);
AddMQ2Data("Pet", dataPet);
AddMQ2Data("Window", dataWindow);
AddMQ2Data("Menu", dataMenu);
AddMQ2Data("Macro", dataMacro);
AddMQ2Data("EverQuest", dataEverQuest);
AddMQ2Data("MacroQuest", dataMacroQuest);
AddMQ2Data("Math", dataMath);
AddMQ2Data("Zone", dataZone);
AddMQ2Data("Group", dataGroup);
AddMQ2Data("String", dataString);
AddMQ2Data("Int", dataInt);
AddMQ2Data("Bool", dataBool);
AddMQ2Data("Float", dataFloat);
AddMQ2Data("Corpse", dataCorpse);
AddMQ2Data("If", dataIf);
AddMQ2Data("Cursor", dataCursor);
AddMQ2Data("NearestSpawn", dataNearestSpawn);
AddMQ2Data("Type", dataType);
AddMQ2Data("Time", dataTime);
AddMQ2Data("GameTime", dataGameTime);
AddMQ2Data("Ini", dataIni);
AddMQ2Data("Heading", dataHeading);
AddMQ2Data("Defined", dataDefined);
AddMQ2Data("LastSpawn", dataLastSpawn);
AddMQ2Data("FindItem", dataFindItem);
AddMQ2Data("FindItemBank", dataFindItemBank);
AddMQ2Data("InvSlot", dataInvSlot);
AddMQ2Data("SelectedItem", dataSelectedItem);
AddMQ2Data("FindItemCount", dataFindItemCount);
AddMQ2Data("FindItemBankCount", dataFindItemBankCount);
//AddMQ2Data("GroupLeader",dataGroupLeader);
//AddMQ2Data("GroupLeaderName",dataGroupLeaderName);
AddMQ2Data("Skill", dataSkill);
AddMQ2Data("AltAbility", dataAltAbility);
AddMQ2Data("Raid", dataRaid);
AddMQ2Data("NamingSpawn", dataNamingSpawn);
AddMQ2Data("SpawnCount", dataSpawnCount);
AddMQ2Data("LineOfSight", dataLineOfSight);
AddMQ2Data("Plugin", dataPlugin);
AddMQ2Data("Select", dataSelect);
AddMQ2Data("DoorTarget", dataDoorTarget);
AddMQ2Data("ItemTarget", dataItemTarget);
AddMQ2Data("DynamicZone", dataDynamicZone);
AddMQ2Data("Friends", dataFriends);
AddMQ2Data("Task", dataTask);
AddMQ2Data("Mount", dataMount);
AddMQ2Data("Illusion", dataIllusion);
AddMQ2Data("Familiar", dataFamiliar);
AddMQ2Data("Alias", dataAlias);
#if !defined(ROF2EMU) && !defined(UFEMU)
AddMQ2Data("AdvLoot", dataAdvLoot);
#endif
AddMQ2Data("Alert", dataAlert);
AddMQ2Data("Range", dataRange);
AddMQ2Data("XTarget", dataXTarget);
AddMQ2Data("CombatAbilityReady", dataCombatAbilityReady);
AddMQ2Data("AltAbilityReady", dataAltAbilityReady);
AddMQ2Data("ItemReady", dataItemReady);
AddMQ2Data("SpellReady", dataSpellReady);
AddMQ2Data("MyBuff", dataMyBuff);
AddMQ2Data("MyAura", dataMyAura);
AddMQ2Data("TargetBuff", dataTargetBuff);
lua_setglobal(L, "gmq2");
lua_pushcclosure(L, lmq2_cmd, 0);
lua_setglobal(L, "eq");
lua_pushcclosure(L, lmq2_echo, 0);
lua_setglobal(L, "mq");
luaL_requiref(L, "timer", luaopen_timer, 1);
CHAR fn[MAX_STRING];
strcpy_s(fn,gszINIPath);
strcat_s(fn,"\\init.lua");
lerror(L, luaL_loadfile(L, fn) || docall(L, 0, 1));
}
}
}
/*

#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <time.h>
#include <locale.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <intrin.h>
#include <limits.h>
#include <stdint.h>
#include <assert.h>
#include <setjmp.h>
#include <stdarg.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <limits.h>
#include <stddef.h>
#include <stdarg.h>
#include <stddef.h>
#include <signal.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/wait.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stddef.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <locale.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stddef.h>
#include <locale.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <errno.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include <limits.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <math.h>
#include <limits.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>
#include <time.h>
#include <string.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <windows.h>
#include <windows.h>
*/
