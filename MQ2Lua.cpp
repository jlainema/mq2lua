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
	#include "lua/src/lapi.c"
	#include "lua/src/lauxlib.c"
	#include "lua/src/lbaselib.c"
	#include "lua/src/lbitlib.c"
	#include "lua/src/lcode.c"
	#include "lua/src/lcorolib.c"
	#include "lua/src/lctype.c"
	#include "lua/src/ldblib.c"
	#include "lua/src/ldebug.c"
	#include "lua/src/ldo.c"
	#include "lua/src/ldump.c"
	#include "lua/src/lfunc.c"
	#include "lua/src/lgc.c"
	#include "lua/src/linit.c"
	#include "lua/src/liolib.c"
	#include "lua/src/llex.c"
	#include "lua/src/lmathlib.c"
	#include "lua/src/lmem.c"
	#include "lua/src/lobject.c"
	#include "lua/src/lopcodes.c"
	#include "lua/src/loslib.c"
	#include "lua/src/lparser.c"
	#include "lua/src/lstate.c"
	#include "lua/src/lstring.c"
	#include "lua/src/lstrlib.c"
	#include "lua/src/ltable.c"
	#include "lua/src/ltablib.c"
	#include "lua/src/ltm.c"
	#include "lua/src/lundump.c"
	#include "lua/src/lutf8lib.c"
	#include "lua/src/lvm.c"
	#include "lua/src/lzio.c"
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
#include "lua/src/loadlib.c"
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
	(void)ar;  /* unused arg. */
	lua_sethook(L, NULL, 0, 0);
	/* Avoid luaL_error -- a C hook doesn't add an extra frame. */
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
	if (!lua_isstring(L, 1)) { /* Non-string error object? Try metamethod. */
		if (lua_isnoneornil(L, 1) ||
			!luaL_callmeta(L, 1, "__tostring") ||
			!lua_isstring(L, -1))
			return 1;  /* Return non-string error object. */
		lua_remove(L, 1);  /* Replace object by result of __tostring metamethod. */
	}
	luaL_traceback(L, L, lua_tostring(L, 1), 1);
	return 1;
}

static int docall(lua_State *L, int narg, int clear)
{
	int status;
	int base = lua_gettop(L) - narg;  /* function index */
	lua_pushcclosure(L, traceback, 0);  /* push traceback function */
	lua_insert(L, base);  /* put it under chunk and args */
	signal(SIGINT, laction);
	status = lua_pcall(L, narg, (clear ? 0 : LUA_MULTRET), base);
	signal(SIGINT, SIG_DFL);
	lua_remove(L, base);  /* remove traceback function */
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



