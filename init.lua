require "coroutine"
require "os"
require "math"
--[[
Full dot stacking:
- high mana dots; 1/target, unless way too much mana
- dot order scambled; so all targets get started as evenly as possible

- dot target is always biased random from all targets without the dot,
  with the bias as how long the target will live
]]

blocked_spawns = {
	"a resting Bloodmoon orc",
	"a snoring Bloodmoon orc",
	"an idle daydream",
	"a vivid daydream",
	"a distracting daydream",
	"fervent disciple",
	"a Drogan eradicator",
	"an emaciated ghoul",
        "a ravenous ghoul",
	"an unmotivated zombie",
	"a complacent zombie",
	"an apathetic zombie",
	"a deteriorating mummy",
	"a crumbling mummy",
	"an unravelling mummy",
}
for i,v in ipairs(blocked_spawns) do
  blocked_spawns[v] = i
end
-- TODO: when group changes, all turned abilities should be revalidated

Now = 0                     -- current time in seconds
Delta = 0                   -- frame time in seconds
me = ""                     -- my name
group = {}                  -- people in my group
guild = {}                  -- people in my guild (with caveats)
GroupCheck = 0              -- timer to re-check my group composition
GroupExt = nil              -- single out of group name; bit of a haxy thing, dis here
LastMeleeOut = 0            -- when I last dealt melee damage
LastMeleeIn = 0             -- when I was last dealt melee damage
LastCombatChange = 0        -- when a target was last added to combat
DoNotMove = nil
Casting = nil
-- AllowEscape = 1

CombatDistance = 55         -- when we attack on incoming
DelayedCombatDistance = 125 -- if delayed on xtarget for a few secs, distance to attack
COMBAT_RADIUS = 40          -- when declaring attack, area of aggressives to add to combat at the same time
FOLLOW_DISTANCE = 450       -- follow leader/target when distance below this
REBUFF_VALID = 90           -- cache target buffs for this many seconds without retargeting to refresh real state
TANK_COUNT = 6              -- tank this many critters, CC the rest
MATCH_ALWAYS = nil          -- at encode, always match near spawns
BERSERKER_HEAL_DIFF = 15    -- % of health berserkers are expected to be below ordinary people
AUTO_AAs = nil
AAsRunning = nil
DoMelee = nil
CombatSpells = nil
LastMoved = 0
LastCombatActive = 0
LastNonCombat = 0
ToyDelay = 0
DisableCombat = nil

-- different class sets for buffing 
MeleeClasses = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Ranger=1, Rogue=1, Monk=1, Berserker=1, Paladin=1 }
TankClasses = { ["Shadow Knight"]=1, Warrior=1, Paladin=1, Enchanter=1, Pet=1 }
TankClassesNoSK = { Warrior=1, Paladin=1, Enchanter=1, Pet=1 }
CasterClasses = { ["Shadow Knight"]=1,Paladin=1, Beastlord=1, Ranger=1, Shaman=1, Magician=1, Enchanter=1, Necromancer=1, Druid=1,Ranger=1}
CasterClassesNoPet = { ["Shadow Knight"]=1,Paladin=1, Beastlord=1, Ranger=1, Shaman=1, Magician=1, Enchanter=1, Necromancer=1, Druid=1,Ranger=1,NoPet=1}
ManaClasses = { ["Shadow Knight"]=1,Paladin=1, Beastlord=1, Ranger=1, Shaman=1, Magician=1, Enchanter=1, Druid=1,Ranger=1,NoPet=1}
AllClasses = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Rogue=1, Ranger=1, Monk=1, Berserker=1, Paladin=1, Shaman=1, Druid=1, Magician=1, Enchanter=1, Necromancer=1, Cleric=1 }
AllClassesNoPet = { NoPet=1, ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Rogue=1, Ranger=1, Monk=1, Berserker=1, Paladin=1, Druid=1, Shaman=1, Magician=1, Enchanter=1, Necromancer=1, Cleric=1 }
AllClassesNoYaulp = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Rogue=1, Paladin=1, Ranger=1, Monk=1, Berserker=1, Shaman=1, Druid=1, Magician=1, Enchanter=1, Necromancer=1 }
AllClassesNoNec = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Rogue=1, Ranger=1, Monk=1, Berserker=1, Paladin=1, Shaman=1, Druid=1, Magician=1, Enchanter=1, Cleric=1, NoPet=1 }
AllClassesNoEnc = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Rogue=1, Ranger=1, Monk=1, Berserker=1, Paladin=1, Shaman=1, Druid=1, Magician=1, Necromancer=1, Cleric=1, NoPet=1 }
HealerClasses = { Paladin=1, Shaman=1, Druid=1, Cleric=1}
ResserClasses = {Necromancer=1, Paladin=1, Shaman=1, Druid=1, Cleric=1}

NoncasterClasses = { ["Shadow Knight"]=1, Warrior=1, Beastlord=1, Bard=1, Ranger=1, Rogue=1, Monk=1, Berserker=1, Paladin=1, Shaman=1, Druid=1, Cleric=1, Pet=1 }

LINE_MODROD = {"Summoned: Small Modulation Shard", "Summoned: Medium Modulation Shard", "Summoned: Large Modulation Shard", "Summoned: Giant Modulation Shard", "Summoned: Glowing Modulation Shard", levels={86,88,90,105,110}}

TankingOrder = { ["Shadow Knight"]=1, Warrior=1, Paladin=1, Enchanter=2, Ranger=2, Bard=3, Monk=3, Cleric=3, Beastlord=4, Rogue=4, Berserker=4, Shaman=5, Druid=6, Magician=6, Necromancer=6 }

-- generic AA purchase list
GenAAs = {
  "Combat Stability", -- +AC
  "Combat Agility", -- +Avoidance
  "Armor of Wisdom", -- +AC
  "Physical Enhancement", -- +AC, +HP
  "Spell Casting Reinforcement", -- longer buffs
  "Weapon Affinity",
  "Veteran's Wrath",

  "Focus:", -- any focus, increase skill/spell damage
  
  "Mnemonic Retention", -- more spell gems
  "Gift of Mana", -- sp free spells
  "Improved Bash", -- improve bash

  "Burst of Power", -- +melee
  "Combat Fury", -- +melee crit
  
  "General Sturdiness", -- +hp
  "Natural Durability", -- +hp

  "Critical Affliction", -- dot crit chance
  "Destructive Cascade", -- dot crit strength
  "Destructive Fury", -- DD crit strength
  "Fury of Magic", -- DD crit chance

  "Companion's Fortification",
  "Companion's Fury",
  "Expansive Mind",
  "Mental Clarity",
  "Mental Stamina",
}

AllAAs = { "" }

CurrentZone = -1
ZoneFollowDisable = 0

STOP_ALL = -1
STOP = 0
OK = 1
RANGE = 2
COMBAT = 3
FAIL = 4
NOTYET = 5
UNKNOWN = 6
AGAIN = 666
SpellReturns = {"OK", "RANGE", "COMBAT", "FAIL", "NOTYET", "UNKNOWN"}

DEBUG_CAST = nil -- name of the player to debug for
DEBUG_TURNS = nil
ONLY_LEADER_DPS = nil
DEBUG_LOG = nil
BOXING = 1 -- cuts reporting on non-main assist accounts when non-nil
Named = 0 -- amount of named being fought
CombatTargets = 0 -- total amount of combat targets
MEMGEM = 8 -- spell gem used to transient spells
HasMage = nil
Focus = nil
Hold = nil
Balance = nil
InFluxTime = 6 -- when added, delay before it is fixed in kill slot

pending_handlers = {}
frame_handlers = {}
frame_next = {}
triggers = {{}, {}, {}} -- by default, three priorities of triggers
spell_set = {} -- cleaned up my spell names
buff_set = {}
autotargets = {}

-- the main trigger function; called every time eq sends text one line at a time
function addchat(l)
  if DEBUG_LOG and (Me.Name == DEBUG_LOG) then mq(l,0) end
  for i,t in pairs(triggers) do
    for p,f in pairs(t) do
      local m = string.match(l, p)
      if m then if f(l, m) then return end end
    end
  end
end

-- returns the group filtered in table order
function group_order(how)
  if type(how) ~= "table" and group[1] then how = {[group[group[1]]]=1} end
  local order = {}
  for i,v in ipairs(group.IDs) do if how[Spawn[v].Class.Name] then table.insert(order,{v,Spawn[v].Class.Name}) end end
  table.sort(order, function (a,b)
    if how[a[2]] == how[b[2]] then return a[1] > b[1] end
    return how[a[2]] > how[b[2]]
  end)
  for i,v in ipairs(order) do order[v[1]] = #order - i + 1 end
  order.max = #order
  order.me = order[group.IDs[1]] or 0
  return order
end

-- tries to return a name without progression postfixes. Are there more?
function strip_spell(n)
  -- some spells phaze between upto 5 stages, longnamed ABCDE
  if spell_set[n] then return spell_set[n] end
  if type(n) ~= "string" then return n end

  local o = n
  -- generic prefixes
  n = n:match("^Hand of the (.*)$") or n
  n = n:match("^Hand of (.*)$") or n
  n = n:match("^Voice of (.*)$") or n
  n = n:match("^Masterful (.*)$") or n
  n = n:match("^A Gracious (.*)$") or n
  n = n:match("^a gracious (.*)$") or n
  n = n:match("^a Gracious (.*)$") or n
  n = n:match("^Gracious (.*)$") or n
  n = n:match("^Invoke (.*)$") or n
  n = n:match("^Group (.*)$") or n
	
  n = n:match("(.*) Azia$") or n
  n = n:match("(.*) Beza$") or n
  n = n:match("(.*) Caza$") or n
  n = n:match("(.*) Dena$") or n
  n = n:match("(.*) Ena$") or n
  n = n:match("^Azia (.*)$") or n
  n = n:match("^Beza (.*)$") or n
  n = n:match("^Caza (.*)$") or n
  n = n:match("^Dena (.*)$") or n
  n = n:match("^Ena (.*)$") or n
  n = n:match("(.*) Root$") or n
  n = n:match("(.*) Third$") or n
  n = n:match("(.*) Fifth$") or n
  n = n:match("(.*) Octave$") or n
  n = n:match("(.*) Watch$") or n
  n = n:match("(.*) Recourse$") or n
  n = n:match("(.*) Persistence$") or n
  n = n:match("(.*) Forbearance$") or n

  -- gift of mana trailers, (Azia) style trailers
  n = n:match("(.*) %(.-%)$") or n

  -- Roman numerals, any capitalized trailer, really
  n = n:match("(.*) %u+$") or n
	
  -- TBM spells are not roman...
  n = n:match("(.*) %d+$") or n

  -- the generic postfixes
  n = n:match("(.*) Rk.$") or n
  n = n:match("(.*) Suffering$") or n
  n = n:match("(.*) Recourse$") or n
  n = n:match("(.*) Effect$") or n

  -- Roman numerals (again), any capitalized trailer, really
  n = n:match("(.*) %u+$") or n

  -- only do this whole checkup once per spell, this is expensive
  spell_set[o] = n
	
  return n
end

-- as of spring 2020, Spawn.Dead is not always correct; also check clean name
function is_dead(spwn)
  if spwn then
    if spwn.Dead == 1 then return 1 end
    if string.find(spwn.CleanName, "'s corpse") then return 1 end
  end
end

function group_add(spwn)
  local sid = spwn.ID
  if not spwn or type(sid) ~= "number" then return end 
  local n = spwn.CleanName
  if type(spwn.Master.ID) ~= "number" then 
    if type(spwn.Owner.ID) ~= "number" then table.insert(group, n) end
	-- include mercs in group.Names
	table.insert(group.Names, n)
  end
  group[n] = spwn.Class.Name
  table.insert(group.IDs, sid)
  if type(spwn.Master.ID) == "number" then
    group[n.." Owner"] = spwn.Master.CleanName
    group[n] = "Pet"
    table.insert(group.Pets, sid) 
  elseif group[n] == "Magician" and spwn.Level>85 then HasMage = (HasMage or 0) + 1 end
  if type(spwn.Pet.ID) == "number" then group_add(spwn.Pet) end
end

function is_grouped(spwn)
  if type(spwn) == "string" then
	  local pi = spwn:find('`')
		if pi then spwn = spwn:sub(1,pi-1) end
	  return group[spwn]
	end
  for i,v in ipairs(group.IDs) do
    if spwn and spwn.ID == v then return 1 end
  end
end

function is_pet(spwn) 
  if type(spwn) == "string" then return group[spwn] == "Pet" end
  return spwn and spwn.Master and type(spwn.Master.ID) == "number"
end

function retime_buff(buff, ts, durations)
  if not buff then return end
  local tn = buff.Name
  if buff.SpellType == "Detrimental" and (buff.Name ~= "Resurrection Sickness" and buff.Name ~= "Revival Sickness" and not buff.Name:find("Alliance")) then 
    ts.detrimental = Now+1
    -- for NPC detrimentals, follow dot stacking (only track self cast).
    if (ts == buff_set.target) and (Target.Type == "NPC") and
       (type(Me.Book[buff.Name])=="number") and (buff.Caster ~= me) then return end
  end
  local bn = strip_spell(tn)
  local nd = buff.Duration
  if durations then
    nd = 18
    if durations[tn] then nd = durations[tn] end
  end
  if type(nd) == "userdata" then nd = nd.Seconds end
  nd = nd + Now
  -- make sure, of the same name buffs only the longest counts (Dichotomic Fury is like this, at least)
  if (ts[bn] or 0) < nd then ts[bn] = nd end
  ts.uptimes[bn] = (ts.uptimes[bn] or 0) + (Now - (ts.last_active or Now))
end

function setup_buffs(id, buffs, durations, retime)
  local bs = buff_set[id]
  if not bs then
    bs = {uptimes={}, nbuffs=0, last_active=Now}
    buff_set[id] = bs
  end
  -- clean up active buffs; so when buffs are dispelled/they drop, they register <= 0 active left
  if not retime then for i,v in pairs(bs) do if type(v) == "number" and v > Now then bs[i] = Now end end
  else bs.last_active = retime end
  if (id == Target.ID) and type(Target.BuffsPopulated) == "number" then
    bs.nbuffs = Target.BuffsPopulated
  end
  -- how many buff slots there are again?
  for i = 1,60 do retime_buff(buffs[i], bs, durations) end
  bs.last_active = Now
  bs.id = id
end

function HealQueue_add(spwn,bias)
  local hps = spwn.PctHPs
  local lim = HealQueue.limit or 80
  if Charms[spwn.ID] then return 0 end -- do no heal charmed pets, they are a dime a dozen
  if spwn.Class.Name == "Berserker" or spwn.Class.Name == "Shaman" then lim = lim - BERSERKER_HEAL_DIFF end
  if (type(hps) == "number") and (hps < lim) and (spwn.Distance3D < (HealQueue.distance or 225)) then
    local id = spwn.ID
    while hps < 100 do
      table.insert(HealQueue, {id,hps+bias})
      hps = hps + (HealQueue.amount or 25)
    end
    return 1
  end
  return 0
end

function sort_combatarray()
  local changes = #CombatArray > 1
  while changes do
    changes = nil
    for i = 1,#CombatArray - 1 do
      if (CombatArray[i][5]>Now-InFluxTime or FlipOrder) or (CombatArray[i][3] ~= CombatArray[i+1][3]) then
        -- the entry is still in flux; so swap if needed
        if CombatOrderFunc(CombatArray[i], CombatArray[i+1]) then
          local no = CombatArray[i]
          CombatArray[i] = CombatArray[i+1]
          CombatArray[i+1] = no
          changes = 1
        end
      end
    end
    if Group.MainAssist and (Group.MainAssist.ID == Me.ID) then
      if (CombatArray[1][6] ~= 0) and (CombatArray[1][5]<Now-InFluxTime) then
        -- encode main assist target to force everyone one same target (-named) when timing sucks
        CombatArray[1][6] = 0
        encode(CombatArray[1][1])        
      end
    end
  end
  if #CombatArray > 1 and AddsFirst and CombatArray[1][3] == 1 and CombatArray[2][3] == 0 then
    -- one last swap to put named after the first add
    local no = CombatArray[1]
    CombatArray[1] = CombatArray[2]
    CombatArray[2] = no
  end
end

NextSelfBuffCheck = 0
RawNow = 0
InitialNow = nil
function main()
  Delta = RawNow	
  RawNow = timer.now()
  Delta = RawNow - Delta
  if not InitialNow then
    -- sync clocks for everyone, but base it on the current hour to increase resolution;
    -- you need to know this, not to load up exactly on the hour :-)
    math.randomseed(RawNow*1000000)
    InitialNow = -RawNow
    InitCombatActive = InitialNow + RawNow
    mq("Initializing clock to "..(InitialNow + RawNow),1)
	LastCombatActive = InitialNow+RawNow
  end
  Now = InitialNow + RawNow

  -- clean up death confirmation if I am no longer dead
  if not is_dead(Me) then HasConfirmedDeath = nil end
  if Me.Moving == 1 then LastMoved = Now end

  -- detect an external target change, do not autotarget for 6s;
  -- this is needed so you can choose targets while scripts are helping
  if type(Target.ID) ~= "number" then
  elseif #autotargets == 0 then
    table.insert(autotargets, Target.ID)
  elseif group and group.IDs then
    local tid = Target.ID
    for i,v in ipairs(autotargets) do
      if tid == v then tid = nil break end
    end
    for i,v in ipairs(group.IDs) do
      if tid == v then tid = nil break end
    end
    if tid then
      table.insert(autotargets, tid)
      AutotargetDelay = Now + 6
    end
  end
  
  -- if we changed zones, disable follow for a short while, clean up buff sets and combat targets
  if tostring(Zone) ~= CurrentZone then
    CurrentZone, ZoneFollowDisable = tostring(Zone), Now+1
    buff_set = {target={uptimes={}, last_active=Now, nbuffs=-1}} -- active buff names for me, pet, target and checked spawn IDs
    buff_set.me = {uptimes={}, last_active=Now, nbuffs=-1}
    buff_set.pet = {uptimes={}, last_active=Now, nbuffs=-1}
    CombatTargets = 0
    CombatArray = {}
    Charms = {}
  end

  -- we need to organize the combat array based on combat priority
  sort_combatarray()
	
  if Now > GroupCheck then 
    me = Me.Name
    GroupCheck = Now + 6
    -- TODO: when group changes, all turned abilities should be revalidated
    HasMage = nil
    group = {IDs={}, Names={}, Pets={}}
    group_add(Me)    
    local gs = Group.GroupSize
    if type(gs) == "number" then for i = 1,gs-1 do group_add(Group.Member[i]) end end
    if GroupExt then
      local spwn = Spawn["pc "..GroupExt]
      if spwn and type(spwn.ID) == "number" then group_add(spwn) end

      -- Also add full raid, if we are so inclined
      if RaidGroup then
        local trig = "%[.-%] (.-) %(.-%) <(.-)>"
        if #guild == 0 then
          local gn = Me.Guild
          if type(gn) ~= "string" then gn = "Swift Death" end
          triggers[1][trig] = function (l,m)
            local n,m = l:match(trig)
            if m == gn then table.insert(guild, n) end
          end
          mq("/ guild "..gn, 1)
          eq("/ guild "..gn)				
        else
          if #guild == RaidGroup then triggers[1][trig] = nil else RaidGroup = #guild end
        end

        for i,v in ipairs(guild) do
          if not group[v] then
            local spwn = Spawn["pc "..v]
            if spwn and type(spwn.ID) == "number" then
              group_add(spwn)
            else
              -- we really need those people in the group, since raid n stuff [out of zone ress?]
              table.insert(group,v)
              group[v] = "Out of Zone"
            end
          end
        end
      end
    end
    group.Order = group_order(TankingOrder)
  end
  
  -- fill up heal queue; it is sorted order of targets needing healing as spawn id's,
  -- repeated with the expectation that each heal adds +25%; 
  if HealQueue and group and group.IDs then
    while #HealQueue > 0 do table.remove(HealQueue) end
    HealValues = {}
    -- add all HPs to HealQueue
    local pclim = HealQueue.pclim or 95
    local petlim = HealQueue.petlim or 95
	if HealQueue.dynamic then
	  pclim = pclim - (100-Me.PctMana)*HealQueue.dynamic
	  petlim = petlim - (100-Me.PctMana)*HealQueue.dynamic
	end
    local grp = group.IDs
    local individuals = 0
    for i = 1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]
      if id == Me.ID then spwn = Me end
      local did = 0
      if spwn and not is_dead(spwn) then
        local lim = pclim
        if spwn.Class.Name == "Berserker" then lim = lim - BERSERKER_HEAL_DIFF end
        local isplayer = (not spwn.Master or type(spwn.Master.ID)~="number")
        if (not HealQueue.petonly and isplayer) and (spwn.PctHPs < lim) and (not HealQueue.undeadonly or (tostring(spwn.Body) == "Undead")) then did = HealQueue_add(spwn, -25) end
        if (not HealQueue.pconly and not isplayer) and
           (not HealQueue.undeadonly or (tostring(spwn.Body) == "Undead")) and (spwn.PctHPs < petlim) then
          did = HealQueue_add(spwn,0)
        end
        individuals = individuals + did
      end
    end
    table.sort(HealQueue, function (a,b) return a[2]<b[2] end)
    HealQueue.individuals = individuals
  end

  -- in addition, setup self buffs / pet buffs a couple of times per second
  if Now > NextSelfBuffCheck then
    NextSelfBuffCheck = Now + 0.5
    local pid,mid = (Me.Pet and Me.Pet.ID), Me.ID
    if type(pid) == "number" then setup_buffs(pid, Me.PetBuff, {}) buff_set.pet = buff_set[pid] end
    local pt = buff_set[mid] and buff_set[mid].last_active
    setup_buffs(mid, Me[".Buff"])
    setup_buffs(mid, Me.Song, nil, pt)
    for a=1,2 do if MyAura[a] then retime_buff({Name=tostring(MyAura[a]), Duration=12}, buff_set[mid]) end end
    if Me.ActiveDisc.Name then retime_buff(Me.ActiveDisc, buff_set[mid]) end
    buff_set.me = buff_set[mid]
  end

  local tid = Target.ID
  if type(tid) == "number" and (Target.BuffsPopulated ~= 0) then
    if not buff_set.target or (Target.BuffsPopulated ~= buff_set.target.nbuffs)
       or (buff_set[tid] and buff_set[tid].last_active < Now-0.5)
       or tid ~= buff_set.target.id
       or buff_set.target.spell_landed then
      if tid ~= Me.ID and tid ~= (Me.Pet and Me.Pet.ID) then
        setup_buffs(tid, TargetBuff, Target.BuffDuration)
      end
      buff_set.target = buff_set[tid]
			if buff_set.target then buff_set.target.spell_landed = nil end
    end
  end

  for i,v in pairs(frame_handlers) do
    if v then
      local n,a = coroutine.resume(v[1])

      if not n then
        mq("error: "..tostring(a), 1)
      elseif a == STOP_ALL then
        return
      elseif a == AGAIN then
        if not frame_next[i] then
          frame_next[i] = v
        else
          table.insert(frame_next, v)
        end
      elseif type(a) == "string" then
        -- someone else is going to restart the coroutine with returned id
        pending_handlers[a] = v
      else
        if v[2] then
          -- mq("going to start new run next frame = "..tostring(n)..", "..tostring(a), 1)
          v[1] = coroutine.create(v[2])
          table.insert(frame_next, v)
        end
        -- mq("coroutine ended with "..tostring(r), 1)
      end
    end
  end
  if cleanup then frame_handlers,cleanup = {},nil else frame_handlers = frame_next end
  frame_next = {}
end

dps_all = {}
dps_out = {}
dps_in = {}

-- automatic time keeping:
 -- new source checks timing: if last_time is further than 3s away, add 0.5s to combat time;
 -- otherwise add Now-last_time, and set last_time
dps_ours = {} -- by source
dps_theirs = {} -- by source

function pnum(n)
  local s = ""
  if type(n) ~= "number" then return type(n) end
  if n < 0 then
    s = "-"
    n = -n
  end
  if n < 10 then
    return s..string.format("%.2f",n)
  elseif n < 1000 then
    return s..string.format("%.1f",n)
  elseif n < 1000000 then
    return s..string.format("%.1fk",n/1000)
  elseif n < 1000000000 then
    return s..string.format("%.1fM",n/1000000)
  else
    return s..string.format("%.1fG",n/10000000000)
  end
end

DPS_TIMESTAMP = 0
function dps_add(t, n, v, w)
  if DPS_TIMESTAMP ~= Now then
		if DPS_TIMESTAMP < Now-3 then 
			DPS_COMBATTIME = DPS_COMBATTIME+0.25 -- combat bootstrap time
		else
			DPS_COMBATTIME = DPS_COMBATTIME + (Now-DPS_TIMESTAMP)
		end
		DPS_TIMESTAMP = Now
	end
	n = strip_spell(n)
  local d = t[n]
  if d then d[1] = d[1] + v
  else d = {v,Now,n} t[n] = d end
	if w then d[w] = (d[w] or 0) + v end
end

-- DPS_LOG = 1
-- Mage hit a magmite for 55980 points of cold damage by  63^46866^'Surge of Ice . (Critical)
function dps_melee(l, m)
  local src, dst, total, ext = string.match(l, "(.*)"..m.."(.*) for (%d+) points of .*damage(.*)")
  if ext then
    ext = ext:match(" by .-'(.*).[.]")
    -- if ext and dps_all[ext] then src = ext end
    if src == "You" then src,LastMeleeOut = me,Now end
    if dst == "YOU" then dst,LastMeleeIn = me,Now end
    if DPS_LOG then msg = m..": "..src.." -> "..dst..", "..total mq(msg,msg) end

    local n = tonumber(total)
		dps_add(is_grouped(src) and dps_ours or dps_theirs, ext or "melee", n, src)
    dps_add(dps_all, src, n)
    dps_add(dps_out, src, n, dst)
    dps_add(dps_in, dst, n, src)
    return 1
  end
end

-- A magmite has taken 28185 damage from your  63^57196^'Strangulate .
-- A magmite has taken 64078 damage from  63^56670^'Polybiad Venom Rk. II  by Necro. (Twincast)
function dps_dot(l, m)
  local src, total, t = string.match(l, "(.*)"..m.."(%d+) .*damage(.*)")

  if t then
    t = t:match(" from (.*)") or t
		local mine = t:match("your .-'(.+).[.]")
    if mine then
      dst = me
			-- mq("My dot "..t..": -> "..mine)
			t = mine
    else
			-- mq("Other dot "..t)
      ext, dst = t:match(".-'(.+). by (.*)[.]")
      if not dst then dst = "<unk>" else t=ext end
    end
    if DPS_LOG and dst and src then msg = m..": "..dst.." -> "..src..", "..total mq(msg,msg) end
    if dst and src then
      local n = tonumber(total)
			dps_add(is_grouped(dst) and dps_ours or dps_theirs, t, n, dst)
      dps_add(dps_all, dst, n)
      dps_add(dps_out, dst, n, src)
      dps_add(dps_in, src, n, dst)
      return 1
    end
  end
end

function dps_ds(l, m)
  local dst, who, total = string.match(l, "(.*)"..m.."(.*) %g- for (%d+) points")

  if total then
    dst = ((dst == "YOU are") and me) or dst:match("(.*) is")
    src = ((who == "YOUR") and me) or who:match("(.*)'s")
    dst,src = src,dst

    if DPS_LOG and dst and src then msg = m..": "..src.." -> "..dst..", "..total mq(msg,msg) end

    -- mq(m..": "..src.." -> "..dst..", "..total)
    local n = tonumber(total)
		dps_add(is_grouped(src) and dps_ours or dps_theirs, "ds", n)
    dps_add(dps_all, src, n)
    dps_add(dps_out, src, n, dst)
    dps_add(dps_in, dst, n, src)
    return 1
  end
end

-- missed dps message; well, could be, at least: if there is you or a group member on a line
-- with a number, we might be missing reporting, so log that always
function dps_miss(l, m)
  local msg
  if l:find("singing") then return end
  if l:find("You") or l:find("YOU") then
    msg = "MISSED: "..l
  else for i,v in ipairs(group) do
    if l:find(v) then msg = "MISSED: "..l break end
  end end
  if msg then
    mq(msg, msg)
    return 1
  end
end

function try_pet(p)
  local on = p.." Owner"
  if group[on] then return group[on] end
  p = split_spawn(p)[1]
  for d,n in ipairs(group.Names) do
    if n and p and n:find(p) then return n end
  end
end

function dps_sort(a,b)
  return a[1] > b[1]
end

avg_dps = {}
avg_hp = {}
uptimes = {}
DPS_COMBATTIME = 0
DPS_REPORTS = 0
function dps_finish(n,s,do_ret)
  if ONLY_LEADER_DPS and Group.Leader.ID ~= Me.ID then if not do_ret then mq(n,1) end return do_ret end

  if s < 1 then s = 1 end

  local gdps, mdps = 0, 0
  -- gather all dps out from Group
  local pets = 0
  local all = ""
  local pall = {}
  local arr = {}
  for i,v in pairs(dps_out) do
    local gn = try_pet(i)
    if gn then 
      if not pall[gn] then pall[gn] = {v[1],0,gn}
      else pall[gn][1] = pall[gn][1] + v[1] end
      if gn ~= i then pall[gn][2] = pall[gn][2]+v[1] end
      gdps = gdps + v[1]
    elseif not i:find("'s corpse") then
      mdps = mdps + v[1]
      table.insert(arr, {v[1] / s, i, 0})
    end
  end

  local hdps = {0,0,"None"}
  local ldps = {1000000000000.0,0,"None"}

  for i,v in pairs(pall) do
    if hdps[1] < v[1] then hdps = v end
	if ldps[1] > v[1] then ldps = v end
  end

  if do_ret then return {gdps, hdps, ldps} end

  table.sort(arr, dps_sort)
  if CombatTargets == 0 then
	  dps_out = {}
	  -- also sum buff uptimes
	  for i,v in pairs(buff_set) do
			if v.uptimes then
				local b = uptimes[i]
				if not b then b = {} uptimes[i] = b end
				for n,t in pairs(v.uptimes) do 
					if t > s then t = s end
					b[n] = (b[n] or 0) + t
				end
			end
	  end

	  for i,v in pairs(pall) do
			if not i:find("'s corpse") then
				local a = avg_dps[i]
				if not a then a = {0,i,0,0} avg_dps[i] = a end
				a[1] = a[1] + v[1]
				a[3] = a[3] + v[2]
				a[4] = a[4] + s
				table.insert(arr, {v[1] / s, i, v[2] / s})
			end
	  end
  end
  
  local msg = n.." "..pnum(gdps / s).."/"..pnum(mdps / s).." in "..pnum(s).."s "
  if #arr > 0 then
    msg = msg .. arr[1][2] ..": ".. pnum(arr[1][1])
    -- only show pet DPS if it is significant; it is already summed to owner DPS
    if arr[1][3] > arr[1][1]*0.05 then msg = msg.."/-"..pnum(arr[1][3]) end
    msg = msg .. " .. "
    msg = msg .. arr[#arr][2] ..": ".. pnum(arr[#arr][1])
    if pall[me] then
      msg = msg .. ", "
      msg = msg .. me .. ": ".. pnum(pall[me][1]/s)
    end
  end
  mq(msg,1)

	if CombatTargets == 0 then
		-- also top 6 sources of damage in DPS out, and sum of DPS in:
		msg = "IN: "
		local din = {}
		for i,v in pairs(dps_theirs) do
			if v[3] == "melee" then
				for _i,_v in pairs(v) do
					if type(_i) == "string" then table.insert(din,{_v,0,_i}) end
				end
			else
				table.insert(din, v)
			end
		end
		table.sort(din, dps_sort)
		for i,v in ipairs(din) do
			msg = msg.." "..v[3]..": "..pnum(tonumber(v[1])/s)..", "
		end
		mq(msg,1)
		dps_theirs={}
		
		local cd = (DPS_COMBATTIME>1 and DPS_COMBATTIME) or 1
		local dout = {}
		for i,v in pairs(dps_ours) do table.insert(dout, v) end
		-- for i,v in pairs(dps_theirs) do table.insert(dout, v) end
		table.sort(dout, dps_sort)
		msg = ""
		local t5s = 0
		local rs = 0
		local ts = 0
		function square(a) return a*a end
		local ri = #dout - math.floor(math.random(square(#dout-4)) ^ 0.5+0.999)
		for i,v in ipairs(dout) do
			-- mq(v[3]..": "..pnum(tonumber(v[1])/cd),1)
			ts = ts+v[1]
			if i <= 3 then
				t5s = t5s+v[1]
			  msg = msg.." "..v[3]..": "..pnum(v[1]/cd)..", "
			elseif i == ri then
				rs = v[1]
			  msg = msg.." +"..ri.."/"..#dout.." "..v[3]..": "..pnum(v[1]/cd)				
			end
		end
		msg = pnum(ts/cd).." DPS:"..pnum(DPS_COMBATTIME*100/Now).."%, top3 "..pnum(t5s*100/ts).."%: "..msg.." / "..pnum(rs*100/ts).."%"
		mq(msg,1)
	end
	
  -- also averages every 10 deaths
  DPS_REPORTS = DPS_REPORTS + 1
  if (DPS_REPORTS % 10) == 1 then
    dps_uptimes("me",1)
    dps_uptimes("target",1)
    arr = {}
    local l=0
    local tot,tpet=0,0
    for i,a in pairs(avg_dps) do if a[4]>l then l = a[4] end tot = tot + a[1] / a[4] tpet = tpet + a[3] / a[4] table.insert(arr, {a[1] / a[4], i, a[3] / a[4]}) end
    table.sort(arr, dps_sort)
    msg = pnum(tot).."/-"..pnum(tpet).." DPS in "..pnum(l).."s/"..pnum(l*100/Now).."%:"
    for i,v in pairs(arr) do
      msg = msg .. " " .. v[2] ..": ".. pnum(v[1])
      -- only show pet DPS if it is significant; it is already summed to owner DPS
      if v[3] > v[1]*0.05 then msg = msg.."/-"..pnum(v[3]) end
    end
    mq(msg,1)
  end
end

function dps_uptimes(filter, brief)
  -- report buff uptimes in percentages, sorted by best uptime
  if DPS_COMBATTIME == 0 then return end

  local arr = {}
  for i,v in pairs(uptimes) do if not filter or filter==i then for n,t in pairs(v) do 
    local pn = n
    if not filter then pn = i..":"..n end
    table.insert(arr, {t*100/DPS_COMBATTIME, pn})
  end end end
  table.sort(arr, dps_sort)
  if #arr == 0 then return end
  -- for everything with > 99% uptime, just tell these are great, TYVM

  if not brief then 
    mq(pnum(DPS_COMBATTIME*100/Now).."%: BUFF UPTIMES, total "..pnum(DPS_COMBATTIME).."s of combat for "..(filter or "all")..":",0)
  end

  local i = 1
  local hdr = ""
  local msg = ""
  local lim = 90
  while lim >= 0 do
    while arr[i] and (arr[i][1] >= lim) do
      msg = msg .. hdr .. arr[i][2] .. "(" .. pnum(arr[i][1]).. ")"
      hdr = ", "
      i = i + 1
    end
    if msg ~= "" then 
      mq((filter or "all").."/"..lim.."%+: "..msg,0) 
    end
    msg = ""
    if not brief then break else lim = lim - 10 end
  end
  while arr[i] do mq(pnum(arr[i][1]).."%: "..arr[i][2],1) i = i + 1 end
end


-- no heal counters (yet?)
function dps_heal(l, m)
  return 1
end

function dps_start(l, m) return 1 end
function dps_end(l, m) return 1 end
function dps_split(l, m) return 1 end
function dps_loot(l, m) return 1 end
function dps_won(l, m) mq(l,l) return 1 end

function dps_interrupt(l, m) return 1 end
function dps_too_early(l, m) return 1 end
function dps_gain(l, m) return 1 end
function dps_party(l, m) return 2 end

dps_triggers = {
  [" slash.- "] = dps_melee,
  [" smash.- "] = dps_melee,
  [" crush.- "] = dps_melee,
  [" punch.- "] = dps_melee,
  [" sting.- "] = dps_melee,
  [" maul.- "] = dps_melee,
  [" pierce.- "] = dps_melee,
  [" backstab.- "] = dps_melee,
  [" strike.- "] = dps_melee,
  [" bash.- "] = dps_melee,
  [" kick.- "] = dps_melee,
  [" bite.- "] = dps_melee,
  [" claw.- "] = dps_melee,
  [" hit.- "] = dps_melee,
  [" gore.- "] = dps_melee,
  [" shoot.- "] = dps_melee,
  [" rend.- "] = dps_melee,
  [" frenz.- on "] = dps_melee,
  [" ha.- taken "] = dps_dot,
  [" tormented by "] = dps_ds,
  [" burned by "] = dps_ds,
  [" healed "] = dps_heal,
  [" casting "] = dps_start,
  [" has worn off"] = dps_end,
  [" rolled a "] = dps_split,
  [" won the "] = dps_won,
  [" looted a "] = dps_loot,
  [" is interrupted."] = dps_interrupt,
  [" can use "] = dps_too_early,
  [" have gained "] = dps_gain,
  -- [" group, "] = dps_party,
  
}
function dps_generic(l,m)
  local rv
  for p,f in pairs(dps_triggers) do
    local m = string.match(l, p)
    if m then rv = (f(l, m) or rv) if rv == 1 then return 1 end end
  end
  if not rv then dps_miss(l,m) end
end
-- two level dps triggers for faster scan; first match any number,
-- then the rest of the line to categorize the message in detail
triggers[1]["%d"] = dps_generic

AutotargetDelay = 0
function set_target(ID, wait)
  if AutotargetDelay > Now then return end
  local ptid = Target.ID
  local stop = Now + 3
  -- do not switch target to a new one before buffs are populated
  while type(Target.ID) == "number" and Target.BuffsPopulated == 0 and not is_dead(Target) and stop > Now and wait do coroutine.yield(AGAIN) end
  if ID ~= Target.ID then
    if not Spawn[ID] then return end
    eq("/target id "..tostring(ID))
    table.insert(autotargets, ID)
    stop = Now + 1
    while stop > Now do
      if not Spawn[ID] then return end
      if Target.ID == ID then break end
      coroutine.yield(AGAIN)
    end
  end
  stop = Now + 2
  while wait and (stop>Now) do
    if not Spawn[ID] then return end
    if Target.ID ~= ID then break end
    if Target.BuffsPopulated ~= 0 then 
      -- allow one more round to make sure they got populated
      stop = Now
    end		
    coroutine.yield(AGAIN)
  end
  if Target.ID == ID then autotargets = {ID} end
end

function validate_spell(b)
  -- find the spells actual name
  if b.real_name then return end
  if #b > 1 and type(b[1]) == "table" then
    if b[1].real_name then return end
    local rv
    for i,v in ipairs(b) do rv = validate_spell(b[i]) or rv end
	if not b.set then return rv end
  end
  local n = b[1]
  if type(n) == "table" then n = n[1] end
  if tonumber(n) then 
    n = tonumber(n)
    if n < 11 then
      b.real_name, b.cast_name, b.cast_type, b.cast_time, b.cast_id = "", n, "/doability", 0, n
    elseif Me.AltAbility[n] then
      b.real_name, b.cast_name, b.cast_type, b.cast_time, b.recast_time, b.cast_id = strip_spell(Me.AltAbility[n].Spell.Name), n, "/alt activate", Me.AltAbility[n].Spell.MyCastTime, Me.AltAbility[n].MyReuseTime, n
    end
  elseif type(Me.Book[n]) == "number" then b.cast_name, b.cast_type, b.book_index = n, "/cast", Me.Book[n]
  elseif type(Me.Book[n.." Rk. II"]) == "number" then b.cast_name, b.cast_type, b.book_index = n.." Rk. II", "/cast", Me.Book[n.." Rk. II"]
  elseif type(Me.Book[n.." Rk. III"]) == "number" then b.cast_name, b.cast_type, b.book_index = n.." Rk. III", "/cast", Me.Book[n.." Rk. III"]
  elseif type(Me.CombatAbility[n]) == "number" then b.cast_name,b.cast_type = n, "/disc"
  elseif type(Me.CombatAbility[n.." Rk. II"]) == "number" then b.cast_name,b.cast_type = n.." Rk. II", "/disc"
  elseif type(Me.CombatAbility[n.." Rk. III"]) == "number" then b.cast_name,b.cast_type = n.." Rk. III", "/disc"
  else
    local itm = FindItem[n]
    if (type(itm) == "userdata") and (itm.Name == n) then
      b.real_name, b.cast_name, b.cast_type, b.cast_time, b.cast_id = strip_spell(itm.Clicky.Spell.Name), itm.Name, "/cast item", itm.CastTime, '"'..itm.Name..'"'
    else
      if b.is_spell then return b end
      b.real_name = n
    end
  end

  if b.book_index and string.find(Me.Book[b.book_index].TargetType, "Group") then b.group_target = 1 end
  if not b.real_name then 
    if not b.cast_name then return end
    b.real_name = strip_spell(b.cast_name) 
  end

  local cls = b.turned
  if cls then
    -- count the amount of the same classes as me in the group; this is how many people are participating in turn
    -- then sort them and give them their turns
    -- TODO: drop the turned effect, as if it is on for some, and off for others to start with -> DEATH! RAMPAGE!
    if type(cls) ~= "table" and group[1] then cls = {[group[group[1]]]=1} end
    local turns = {}
    for i,v in ipairs(group) do if cls[group[v]] then
      table.insert(turns,v)
      -- mq(v.." is part of turns")
    else
      -- mq(v.." is not in turns [class = "..group[v].."]")
    end	end
    table.sort(turns, function (a,b)
      if type(cls[group[a]])=="number" and type(cls[group[b]])=="number" and cls[group[a]] ~= cls[group[b]] then return cls[group[a]] > cls[group[b]] end
      return a > b
    end)
    for i,v in ipairs(turns) do turns[v] = #turns - i + 1 end
    b.turn_max = #turns		
    b.turn_me = turns[group[1]]

    if b.self then 
      b.turn_max = b.turn_max+1
      b.turn_self = b.turn_me + 1
    else
      b.turn_self = 0
    end
    b.turn_orig = b.turn_me
    mq(b.real_name.." turn initialized to "..(b.turn_me or "nil").."/"..(b.turn_max or "nil")..", self="..(b.turn_self or "nil"),1)
  end


  if (not b.cast_time) and b.cast_name then 
    b.cast_time = Spell[b.cast_name].MyCastTime
    if not b.recast_time then b.recast_time = Spell[b.cast_name].RecastTime end
    b.cast_id = Spell[b.cast_name].ID
  end

  if b.cast_type == "/cast" then
    if type(Me.Gem[b.cast_name]) == "number" then b.cast_id = Me.Gem[b.cast_name]
    else b.cast_id = MEMGEM end
  end

  mq("Spell "..n.." is "..tostring(b.cast_type).." "..tostring(b.cast_name)..", ("..tostring(b.real_name).."/"..tostring(b.cast_id)..") = "..pnum(b.cast_time or 0).."s", 1)
  return b
end

-- TaskSelectWnd
-- TSEL_AcceptButton
-- /nomodkey /notify TaskSelectWnd TSEL_AcceptButton leftmouseup
function check_buffs_target(b, tgt)
  if type(tgt)=="number" then
    if buff_set[tgt] then
      tgt = buff_set[tgt]
    else
      local acq = Now + 1
      while acq > Now do
        set_target(tgt,1)
        if buff_set[tgt] then break end
        coroutine.yield(AGAIN)
      end
      tgt = buff_set[tgt]
    end
    if not tgt then return 6,"<tgt>" end
  end
  if not b.real_name then validate_spell(b) end
  if type(b[1]) == "table" then
    -- longest of every listed buff
    local on = 0
    local m
    for i,v in ipairs(b) do
      local e,mt = check_buffs_target(v, tgt)
      if e > on then on,m = e,mt end
    end
    return on,m
  end
  if not b.real_name then return 12,"<nil>" end -- block unknown spells...
  if not tgt then return 6,"<act>" end -- unknown targets likely wont have buffs or smth
  if type(tgt.last_active) ~= "number" or (tgt.last_active+30 < Now) then set_target(tgt.id,1) end
  if type(b.check) == "table" then validate_spell(b.check) b.check = b.real_name end
  local r = tgt[b.check or b.real_name]
  if r and r > Now then return r - Now, (b.check or b.real_name) end
  -- r = tgt[strip_spell(b[1])]
  -- if r and r > Now then return r - Now, b[1] end
  --[[
  if buff_aliases[ b[1] ] then
    for i,v in ipairs(buff_aliases[ b[1] ]) do
      r = tgt[v]
      if r and r > Now then return r - Now,v end
      -- r = tgt[strip_spell(v)]
      -- if r and r > Now then return r - Now,strip_spell(v) end			
    end
  end ]]
  return 0, (b.check or b.real_name)
end

function check_buffs_name(b, tgt)
  if not tgt then return 0 end -- unknown targets likely wont have buffs or smth
  local r = tgt[b]
  if r then return r - Now end
  --[[
  if buff_aliases[b] then
    for i,v in ipairs(buff_aliases[b]) do
      r = tgt[v]
      if r then return r - Now end
    end
  end ]]
  return 0
end

-- spell casting setup
function cast_start() CastStarted = OK return 1 end
function cast_ok() CastStarted, CastReturn = OK, OK return 1 end
function cast_fail(l,m) CastStarted, CastReturn = OK, FAIL SpellReturns[FAIL] = l return 1 end
function cast_immune(l,m) CastStarted, CastReturn = OK, IMMUNE if DEBUG_CAST == Me.Name then mq(l,l) end return 1 end
function cast_notyet(l,m) CastStarted, CastReturn = OK, NOTYET SpellReturns[NOTYET] = l return 1 end
function cast_miss(l,m) mq("CastMiss: "..l,1) end
function cast_log(l,m) mq(l,1) end

CastMissTrigger = { ["^You"] = cast_miss }

CastTriggers = {
  -- ["^"] = cast_log,
  ["^You begin "] = cast_start,
  ["^You activate "] = cast_ok,
  ["^Your .* flickers "] = cast_ok,
  ["^You can't cast spells while stunned"] = cast_notyet,
  ["^You .CANNOT. cast spells, you have been silenced"] = cast_notyet,
  ["^Spell recast time not yet met"] = cast_notyet,
  ["^You must first select a target for this spell"] = cast_notyet,
  ["^You must first target a group member"] = cast_immune,
  ["^You cannot see your target"] = cast_notyet,
  ["^You are too distracted to use a skill"] = cast_notyet,
  ["^You can use the ability "] = cast_notyet,
  ["^You haven't recovered yet"] = cast_notyet,
  ["^Your target is out of range, get closer"] = cast_notyet,
  ["^You must be standing to cast a spell"] = cast_fail,
  ["^The target does not meet "] = cast_immune,
  ["^You do not have "] = cast_immune,
  ["^Your gate is too unstable, and collapses"] = cast_fail,
  [" has fallen to the ground"] = cast_fail,
  ["^Your .*spell fizzles"] = cast_fail,
  ["^Your target is immune "] = cast_immune,
  ["^Your target cannot be mesmerized"] = cast_immune,
  ["^Your casting has been interrupted"] = cast_fail,
  ["^Your .* is interrupted"] = cast_fail,
  ["^Your .*spell did not take hold"] = cast_immune,
  ["^Your .*spell would not have taken hold"] = cast_immune,
  ["^Your .*spell is too powerful for your intended target"] = cast_immune,
  ["^You can not summon a mount here"] = cast_immune,
  ["^You can only cast "] = cast_notyet,
  ["^Insufficient Mana to cast this "] = cast_notyet,
  ["^Spell recovery time not yet met"] = cast_notyet,
  ["^Your target resisted the "] = cast_fail,
  ["^You resist the"] = cast_fail,
  ["^You are stunned"] = cast_notyet,
}
SpellMemCooldown = 0
CombatTargets = 0
CombatArray = {}
LastEnd = 0
PendingSpells = {}

CastID = 0
function cast_raw(cmd,spell,tgt)
  CastID = CastID + 1
  local c = "c"..CastID
  table.insert(PendingSpells,{cmd,c,spell,tgt or Target.ID})
  coroutine.yield(c)
  -- mq("cast_raw "..c.." returns "..(CastReturn or "<nil>"))
  return CastReturn
end

  function statusline(spell)
--[[
statusline:                          HRP = healing required per second
delta    action                       T+E           HPS   DPS  HRP 
(+1.25s) RESULT True Death Strike     (10.5s+ 8.2)   78   666k/68.7k HDPS: 140k, LPDS:  45k
]]
	local spwn = Spawn[CastTarget] or Me
    local tgt = tostring(spwn.Name)
    local aggro = tostring(Target.PctAggro)
    local hps = tostring(spwn.PctHPs)
	local CDPS
	local DPS
	if CombatTargets > 0 then
	  local v = CombatArray[1]
      local n = v[4]
      local t = Now - v[5]
	  local tid = (tgt ~= "nil" and Target.ID) or Me.ID
	  local h = (buff_set[tid] and buff_set[tid].health and buff_set[tid].health[3]) or 100
	  local dps = dps_finish(n, t, 1)
      CDPS = "("..pnum(t).."s+"..pnum(h)..") "..
		dps[2][3]..": "..
		pnum(dps[2][1])..", "..
		dps[3][3]..": "..
		pnum(dps[3][1])
	  DPS = pnum(dps[1]/t)

	else
	  DPS = "("..pnum(Now-LastCombatActive).."s)"
	  CDPS = ""
	end
	local sn = (spell.real_name or spell[1])
	if #sn > (MaximumSpellLength or 0) then MaximumSpellLength = #sn end 
	sn = sn -- ..string.rep(' ', MaximumSpellLength-#sn) -- pad spell name + result to fixed length
	mq(DPS.." "..hps.."hp/"..aggro.."% (+"..pnum(Now-LastEnd).."s) "..sn.." "..tostring((CastReturn and SpellReturns[CastReturn]) or "CASTING").." -- "..CDPS.." [at "..tgt.."]",1)
  end

CDPS = "0k"
CastTarget = 0
function do_cast()
  while #PendingSpells == 0 do coroutine.yield(AGAIN) end

  local in_now = Now
  DoNotMove = 1
  coroutine.yield(AGAIN)
  while (Me.Moving == 1) or (Me.Stunned == 1) do coroutine.yield(AGAIN) end

  SitTimer = Now + 2
  if (Me.Ducking == 1) then eq("/keypress duck") delay(0.1) end
  if (Me.Standing == 0) then eq("/stand") delay(0.1) end

  if DEBUG_CAST == me then mq("Activating spell...", "Activating spell...") end
  local pretgt = tostring(Target.Name)
  local preaggro = tostring(Target.PctAggro)
  local prehps = tostring(Target.PctHPs)
  local try_start = Now
  local spell = PendingSpells[1][3]
  local skip_cast
  if type(PendingSpells[1][4]) == "number" then
    local tid = PendingSpells[1][4]
    -- group spells are always cast on master, and then may land on the pet (and should, too)
    if spell.group_target and Spawn[tid] and Spawn[tid].Master and type(Spawn[tid].Master.ID) == "number" then
      set_target(Spawn[tid].Master.ID)
    else
      set_target(tid)
      skip_cast = (Target.ID ~= tid)
    end
  end

  -- mq("setup + target in "..pnum(Now-in_now).."s",1)
  in_now = Now
  
  local preid = Target.ID or Me.ID
  local prebuff = buff_set[preid]
  CastTarget = preid
  
  triggers[1].CastTriggers = CastTriggers
  triggers[3].CastMiss = CastMissTrigger
  CastReturn = nil
  CastStarted = nil
  SpellReturns[FAIL] = "FAIL"
  SpellReturns[NOTYET] = "NOTYET"
  local is_bard = (Me.Class.Name == "Bard")
  
  
  if not skip_cast then
    local timeout=Now+0.75
	if is_bard then 
	  eq("/stopsong")
      while (type(Me.Casting.ID) == "number") and (timeout > Now) do coroutine.yield(AGAIN) end
	end
    eq(PendingSpells[1][1])
    -- if it is not instant, make sure it starts by a couple of retries
    local next = Now+0.25
    if spell and spell.cast_time and (spell.cast_time > 0) then
      while (type(Me.Casting.ID) ~= "number") and (timeout > Now) do
        coroutine.yield(AGAIN)
        if Now > next then
		  if Target.ID ~= CastTarget then set_target(CastTarget) end
          eq(PendingSpells[1][1])
          next = Now+0.25
		  statusline(spell)
        end
      end
    else 
	  -- mq("Instant "..PendingSpells[1][1])
	end
  end
  local PrevGCD = Now - (LastEnd or Now)
  local PrevCast = Now

  if #CombatArray > 0 then
    local tgta = ""
    for i=1,#CombatArray do tgta = tgta.." "..tostring(CombatArray[i][1]) end
    if PreviousCombatArray ~= tgta then
	  local nn = (Spawn[preid] and tostring(Spawn[preid].CleanName)) or "<none>"
	  mq("CombatArray: "..tgta..", was "..(PreviousCombatArray or "<nil>").." - target "..nn,1)
	  PreviousCombatArray = tgta
	end
  end
  
  local was_casting
  local stop = Now + ((spell and spell.cast_time) or 0) + 1
  if stop > Now+30 then stop = Now+30 end -- IDK, some spells have bugged cast times?
  local need_drop
  local next = Now+3
  while not need_drop and not skip_cast and (stop>Now) and (is_bard or (not CastReturn and (type(Me.Casting.ID) == "number"))) do 
    was_casting = 1
    coroutine.yield(AGAIN)
	if has_spawn and not Spawn[preid] or (is_dead(Spawn[preid]) and not spell.ress and not spell.blocks) then set_target(Me.ID) need_drop=1 break end
	if spell.debuff and check_buffs_target(spell, preid)>0 then need_drop=1 break end
	if Now > next then statusline(spell) next = Now + 1 end
  end
  DoNotMove = nil

  if need_drop and not spell.ress then
    -- force it out; by memming a spell and ducking on/off (this memspell fixes a bug present in early 2020)
	-- first, timeout for another 1s, perhaps this is just latency
    -- eq("/memspell "..MEMGEM.." '"..Me.Book[1].Name.."'")
    eq("/keypress duck")
    eq("/keypress duck")
  end
  if is_bard then eq("/stopsong") end

  if DEBUG_CAST == me then mq("Cast finished!", "Cast Finished!") end
  triggers[1].CastTriggers = nil
  triggers[3].CastMiss = nil

  -- something went wrong, but just report is as OK, probably just latency
  if not CastReturn and not skip_cast then 
    if Now >= (in_now+((spell and spell.cast_time) or 0)) then CastReturn = OK else CastReturn = NOTYET end
  end 

  statusline(spell)

  if spell and (not spell.recast_time) and (spell.cast_type == "/cast item") then
    local stop = Now + 0.25
    mq("Trying to find recast for "..spell.cast_name..": "..PendingSpells[1][1],1)
    while not spell.recast_time and (stop>Now) do 
      local tmr = FindItem[spell.cast_name]
      if tmr then tmr = tmr.TimerReady end
      if type(tmr) ~= "number" then stop=Now
      elseif tmr>0 then spell.recast_time = tmr
      else coroutine.yield(AGAIN) end
    end
  end

  LastEnd = Now
  local r = PendingSpells[1][2]
  if r and pending_handlers[r] then
    table.insert(frame_next, pending_handlers[r]) 
    pending_handlers[r] = nil
  end
  table.remove(PendingSpells,1)
end
table.insert(frame_handlers, {coroutine.create(do_cast), do_cast})

function cast_spell(spell,wouldcast,tgt,nomem)
  if type(spell[1]) == "table" then 
    -- pick the first spell that is not in cooldown to cast
    for i,v in ipairs(spell) do
      local sp = cast_spell(v,v)
      if sp then return cast_spell(sp,wouldcast,tgt) end
    end
    return
  end
  if spell.need_targets and spell.need_targets > CombatTargets then return end
  
  if is_dead(Me) then return end
  if not spell.cast_type then validate_spell(spell) end
  if not spell[1] or not spell.cast_type or not spell.cast_name then return end
  if DEBUG_CAST == me then mq(tostring(spell[1]).."/"..tostring(spell.real_name),1) end
  local n, id = spell.cast_name, spell.cast_id
  local do_cast
  if tonumber(n) then
    if tonumber(n) < 11 then do_cast = 1
    elseif AltAbilityReady[n] == 1 then do_cast = 1 end
  elseif spell.cast_type == "/cast item" then
    if ItemReady[n] == 1 then do_cast = 1 end
  elseif spell.cast_type == "/disc" then
    if CombatAbilityReady[n] == 1 then do_cast = 1 end
  elseif (id ~= MEMGEM) or (Me.Gem[n] == MEMGEM) then
    if Me.Class.Name == "Bard" then
      do_cast = 1
    elseif (Me.SpellInCooldown == 0) and (Me.GemTimer[id] == 0) then
      if (Me.Gem[id].Mana <= Me.CurrentMana) or (check_buffs_name("Gift of Mana") > 0) then do_cast = 1 end
    end
  elseif (SpellMemCooldown < Now) and (CombatTargets < 2 or spell.summon) and (type(Target.PctAggro) ~= "number" or Target.PctAggro<65) then
    if nomem and wouldcast then return wouldcast end
    local Wait = Now + 4
    while Wait > Now and type(Cursor.ID) == "number" do
      if Wait < Now then eq("/autoinv") Wait = Now+4 end
      coroutine.yield(AGAIN)
    end
    if Me.Moving == 1 then return end		
    FoundTarget = Now+3
    if type(Me.Casting.ID) ~= "number" then eq('/memspell '..MEMGEM..' "'..n..'"') end
    while 1 do coroutine.yield(AGAIN) if (Me.Gem[n] == MEMGEM) or (Now > Wait) then break end end
    -- whatever tried to cast this one, let it retry later by not memming new stuff for N seconds + recast
    if Me.Gem[n] == MEMGEM then
      FoundTarget = Now+1
      SpellMemCooldown = Now + Me.Gem[MEMGEM].RecastTime + 12
      SpellMemCooldownName = n
      if Me.Gem[MEMGEM].RecastTime < 3 then
        local Timeout = Now+3
        while ((Me.SpellInCooldown ~= 0) or (Me.GemTimer[MEMGEM] ~= 0)) and Timeout > Now do coroutine.yield(AGAIN) end
        do_cast = 1
      end
    end
  end

  if not do_cast then return end
  if wouldcast then return wouldcast end

  Casting = 1
  if Me.Class.Name == "Bard" then
    eq("/stopsong")
    local stopwait = Now+1
    while type(Me.Casting.ID) == "number" and (Now < stopwait) do coroutine.yield(AGAIN) end
  end
  
  -- we are going to cast!
  local rv = cast_raw(spell.cast_type..' '..spell.cast_id, spell,tgt or Target.ID)
  if rv == OK and n == SpellMemCooldownName then 
    if DEBUG_CAST == Me.Name then mq(n.." cooldown met", 1) end
    SpellMemCooldown = 0
  end
  Casting = nil
  return rv
end

function convert_effects(c,p)
  if #c == 0 then c[1] = {p[1]} validate_spell(c[1]) 
  else for i,v in ipairs(c) do
    local s = (type(v) == "table" and v) or {v}
    validate_spell(s)
    c[i] = s
  end end
  c.converted = 1
end

function toturn(p)
  if not p.turned then return 1 end
  if p.timed and p.turn_max then
    local single = (p.return_time and p.return_time*p.turn_max) or (p.recast_time and p.recast_time/p.turn_max) or 1
    local round = (Now - p.timed) / single
    local lim = ((p.cast_time or 1) * 0.5 + 0.5) / single
    local fround = (round - math.floor(round)) * p.turn_max
    local frac = fround - math.floor(fround)
    if (frac < lim) or (frac > (1-lim)) then return p.turn_max+1 end
    return math.floor(fround) + 1
  end
  return p.turn_me or 1
end

-- return true if it is my turn to cast p
function myturn(p)
  if p.timed then return toturn(p) == p.turn_me end
  if p.turn_me and (p.turn_self == p.turn_me) then return 1 end
  return p.turn_me == 1
end

function check_turns(p,cast,tgt)
  if p.timed then return myturn(p) and cast and cast(p) end

  if not cast then
    -- mq(p[1].." got blocked by "..v[1])
    if p.turn_me and not p.turn_on then
      p.turn_on = 1
      if DEBUG_TURNS then mq(p.real_name.." turn changed to "..p.turn_me.."/"..p.turn_max,1) end
    end
    return
  else
    if p.turn_on then 
      p.turn_on = nil 
      if p.turn_me == 1 then p.turn_me = p.turn_max else p.turn_me = p.turn_me - 1 end
      if DEBUG_TURNS then mq("Switch: seen, now expired "..p.real_name,1) end
      return 
    end
    if p.turn_me and ((p.turn_me ~= 1) and (p.turn_self ~= p.turn_me)) then return end
  end

  if p.turn_me and (p.turn_self == p.turn_me) then
    -- try to cast the self version; it will turn to next after this succeeds,
    -- and the group version will stay on hold
    if DEBUG_TURNS then mq(p.real_name.." trying to cast self version at "..p.turn_me,1) end
    return cast(p.me[#p.me],nil,tgt)
  end	

  return cast(p,nil,tgt)
end

function id_to_turn(p,id)
  local hid = math.floor((id * 104729) / 65536)
  if not p.turn_max then mq("Invalid turn for "..tostring(p[1])) return 1 end
  return math.fmod(hid+p.nturn,p.turn_max)+1
end

function handle_out_of_range()
  if PetTank and (not PetBackoffDelay or (PetBackoffDelay<Now)) then
    PetBackoffDelay=Now+1
    -- eq("/pet back")
  end
end

function match_classes(p,spwn)
  return (not p.classes or (p.classes[tostring(spwn.Class.Name)] or (p.classes.Pet and spwn.Master and type(spwn.Master.ID)=="number"))) and
         (not p.classes or not p.classes.NoPet or (not spwn.Master or type(spwn.Master.ID)~="number")) 
end

SingleHPDelay = 0
MultiHPDelay = 0
HasDeadPlayers = 0
DefaultSkipCosts = {"Gift of Mana"}
function check_pipeline(p,cast,pipe,target)
  if DelayCast or p.song then return end
  if p.ooc and p.ooc < 0 then p.ooc = 900 end -- convert to 15 min out of combat max buff upkeep
  if p.next_try and (Now < p.next_try) then return end
  if is_dead(Me) then return end
  if p.swarm and Hold then return end
  if (cast == cast_swap) and p.ooc and ((p.ooc > (Now-LastCombatActive)) or (p.ooc < 0)) then cast = cast_spell end

  -- convert turn sets into spell casting
  if p.turned and p.set and #p > 1 then
	validate_spell(p)
	local mt = 1
	if p.turn_me and p.turn_me <= #p then mt = p.turn_me end
	local rs = p[mt]
	mq(p[1].real_name .. " as " .. rs.real_name.." for set-turned as "..tostring(mt),1)
	p[1] = rs
	for i=2,#p do p[i] = nil end
  end

  -- as long as anyone is dead, do not do ooc stuff (buffing, really)
  if p.ooc and HasDeadPlayers > Now and not p.ress then return end
  if HasDeadPlayers < Now then
    HasDeadPlayers = Now
    local grp = group
    for i=1,#grp do
      local spwn = Spawn["pccorpse "..grp[i]]
      if spwn and not spwn.Master.ID and is_dead(spwn) and spwn.Distance3D < 96 then HasDeadPlayers = Now + 3 end
    end
  end
  
  if p.item then
    for i,n in ipairs(p.item) do
      local itm = FindItem[n]
	    if Cursor.Name == n then eq("/autoinv") end
      if (type(itm) == "userdata") and (itm.Name == n) then return end
    end
    if p.item.do_target then target = Me.ID end
    -- only retry failed item casts every 12s
    if not p.retarget then p.retarget = 12 end
  end

  if p.require_item then
    local had
    for i,n in ipairs(p.require_item) do
      local itm = FindItem[n]
	  if Cursor.Name == n then eq("/autoinv") end
      if (type(itm) == "userdata") and (itm.Name == n) then had = 1 break end
    end
    if not p.retarget then p.retarget = 12 end
    if not had then p.next_try = Now + p.retarget return end
  end
	
  if p.mainassist and Me.ID ~= Group.MainAssist.ID then return end
  if p.notmainassist and Me.ID == Group.MainAssist.ID then return end
  if p.partial_rune and Me.PctHPs < p.partial_rune and not p.force_cast and CombatTargets>0 then p.force_cast = 1 end

  if (p.aoe or p.pbaoe) and LastCombatChange > Now - 3 then return end -- Do not cast aoe before combat has stabilized a bit 
  if p.aoe and type(Target.X) == "number" and SpawnCount["loc "..Target.X.." "..Target.Y.." npc targetable radius "..p.aoe] ~= CombatTargets then return end
  if p.pbaoe then
    local hits = SpawnCount["npc targetable radius "..p.pbaoe]
    if (hits ~= CombatTargets) or (hits == 0) then return end
  end
  if p.body and (tostring(Target.Body) ~= p.body) then return end
  if p.targethasmana then if (type(Target.MaxMana) ~= "number") or (Target.MaxMana<p.targethasmana) then return end end
  if p.targethpbelow then if (type(Target.PctHPs) ~= "number") or (Target.PctHPs > p.targethpbelow) then return end end
  if p.offturnoverride and check_buffs_target(pipe[p.offturnoverride].target, buff_set.target)>0 and not myturn(pipe[p.offturnoverride]) then return check_turns(p, cast) end
  if p.onturnoverride and check_buffs_target(pipe[p.onturnoverride].target, buff_set.target)>0 and myturn(pipe[p.onturnoverride]) then return check_turns(p, cast) end
  if p.minaggro and type(Target.PctAggro) == "number" and Target.PctAggro > p.minaggro then return end
  if p.maxaggro and (type(Target.PctAggro) ~= "number" or Target.PctAggro < p.maxaggro) then return end
  if p.minrange and type(Target.Distance3D) == "number" and Target.Distance3D < p.minrange then return end
  if p.maxrange and type(Target.Distance3D) == "number" and Target.Distance3D > p.maxrange then return end
  if p.myhphigher and (Me.PctHPs < p.myhphigher) then return end
  if p.myhplower and (Me.PctHPs > p.myhplower) then return end
  if p.pethplower and ((type(Me.Pet.ID)~="number") or (Me.Pet.PctHPs > p.pethplower)) then return end
  if p.myendhigher and (Me.PctEndurance < p.myendhigher) then return end
  if p.myendlower and (Me.PctEndurance > p.myendlower) then return end
  local mymanascale = 1
  if CombatTargets > 1 then mymanascale = mymanascale/CombatTargets end
  if p.mymanahigher and (Me.PctMana < p.mymanahigher*mymanascale) and (check_buffs_target(SkipCosts or DefaultSkipCosts, buff_set.me)<=0) and (Named < 1) then return end
  if p.mymanaabs and Me.PctMana < p.mymanaabs then return end
  if p.mymanalower and (Me.PctMana > p.mymanalower) then return end
  if p.totbelow and (not Target.AggroHolder or type(Target.AggroHolder.PctHPs) ~= "number" or Target.AggroHolder.PctHPs > p.totbelow) then return end
  if p.selfcure and check_buffs_name("detrimental", buff_set.me) <= 0 then return end
  if p.named and (Target.Named ~= 1) then return end
  if p.notnamed and (Named > 0) then return end
  if p.multiples and (CombatTargets < p.multiples) then return end
  if p.notsteady and (not DoMelee or DoMelee == 1) then return end
  -- singles is for doing dangerous stuff, so do not do it while named in camp
  if p.singles and ((CombatTargets > p.singles) or (p.singles == 1 and Named > 0)) then return end
  if p.lastmoved and ((LastMoved+p.lastmoved > Now) or (LastCombatActive+p.lastmoved > Now)) then return end
  if p.summon and type(Me.Pet.ID) == "number" then return end

  if p.targetqueue then
    local rv
    if not p.cast_name then validate_spell(p) end
    if #p.targetqueue > 0 then
      local spwn
      local targets = #p.targetqueue
      local grp
      local turns = p.turn_max or 1
      local myturn = toturn(p)
      local tq = p.targetqueue
      local targets = 0
      if p.group then
        local uniques = {}
        for i,v in ipairs(p.targetqueue) do
          if type(v) == "table" then v = v[1] end
          if not uniques[v] then uniques[v],targets = 1, targets + 1 end
        end
        -- if we are not the primary, and there are multiples, pretend an extra target
        if myturn ~= 1 and targets > 1 then targets = targets+1 end
      end

      if targets > turns and p.group and cast(p.group,1) then 
        p = p.group
        grp = 1
      elseif myturn <= #p.targetqueue then
        if type(p.targetqueue[myturn]) == "number" then
          spwn = Spawn[p.targetqueue[myturn]]
        else
          local pt = p.targetqueue[myturn]
          spwn = Spawn[pt[1]]
          if p.crit and spwn and type(spwn.PctHPs) == "number" and (spwn.PctHPs < (p.critlimit or 40)) and cast(p.crit,1) then p = p.crit end
        end
      end
      if (not spwn or type(spwn.ID) ~= "number") and (p.targetqueue and #p.targetqueue >= myturn) then
        table.remove(p.targetqueue, myturn)
      end
      if (grp or (spwn and (type(spwn.ID) == "number"))) and cast(p,1) then
        -- mq("Casting "..p[1].." at "..spwn.CleanName..", targets left="..targets)
        if not grp then
          set_target(spwn.ID,1)
          if p.hot and ((check_buffs_target(p,buff_set.target) > 0) or (type(p.hot)=="table" and p.hot[spwn.ID] and p.hot[spwn.ID] > Now)) then
            if p.crit and cast(p.crit,1) then p = p.crit
            elseif p.group and cast(p.group,1) then p = p.group
            else return end
          end
        end
        if type(Target.PctHPs) == "number" then
          mq("TargetQueue "..(p.real_name or p[1].real_name or "").." at "..tostring(Target.CleanName)..", targets left="..targets..", HPs="..tostring(Target.PctHPs))
        end
        rv = cast_spell(p)
        -- if we failed, stop rest of the pipe as we likely changed targets if not grp target
        if rv ~= OK and not grp then return OK end
      end
      if rv == OK then			
        if spwn and type(spwn.ID) == "number" and #tq > myturn then table.remove(tq,myturn) end
        while grp and #tq > 0 do table.remove(tq) end
		if (type(p.hot) == "table") and (type(spwn.ID) == "number") then
		  p.hot[spwn.ID] = Now + (p.hot.duration or 30)
		end
      end
    end
    return rv
  end

  if p.requires then
    local had_requires = 0
    local requires = p.requires
    for b,s in pairs(buff_set) do
      local pb = requires[b]
      if pb then
        if not pb.converted then convert_effects(pb, p) end
        for i,v in ipairs(pb) do
          if check_buffs_target(v, s) > 0 then had_requires = had_requires + 1 end
        end
      end
    end
    if had_requires < (p.count or 1) then return end
  end

  if p.overrides then
    local had_requires = 0
    local requires = p.overrides
    for b,s in pairs(buff_set) do
      local pb = requires[b]
      if pb then
        if not pb.converted then convert_effects(pb, p) end
        for i,v in ipairs(pb) do
          if check_buffs_target(v, s) > 0 then had_requires = had_requires + 1 end
        end
      end
    end
    if had_requires >= (p.count or 1) then return check_turns(p,cast) end
    return
  end

  if p.blocks and not p.force_cast then
    local had_requires = 0
    local requires = p.blocks
    for b,s in pairs(buff_set) do
      local pb = requires[b]
      if pb then
        if not pb.converted then convert_effects(pb, p) end
        for i,v in ipairs(pb) do
          if check_buffs_target(v, s) > (p.check_left or p.cast_time or 1) then had_requires = had_requires + 1 end
        end
      end
    end
    if had_requires >= (p.count or 1) then return end
    if requires.pet and (not Me.Pet or type(Me.Pet.ID) ~= "number") then return end
    if requires.pet and requires.do_target then target = Me.Pet.ID end
    if requires.me and requires.do_target then target = Me.ID end
  end

  if p.buff then
    if cast ~= cast_spell then return end
    if (p.turn_me and not p.nturn) and (p.turn_me ~= 1) then return end
    -- check if we have applied this to all targets
    local grp = group.IDs
    local pa = p.buff
    local mn = 0
    for i=1,#grp do
      local id = grp[i]
      if (not p.nturn or (id_to_turn(p,id) == p.turn_me)) and (not pa[id] or (pa[id] < Now) or p.cyclic) then
        local spwn = Spawn[id]
        if not p.charm and Charms[id] then spwn = nil end -- only buff charms when charm is set
        if spwn and not is_dead(spwn) and (not p.maxrange or (spwn.Distance < p.maxrange)) and match_classes(p,spwn) and
           (not p.group_target or ((not spwn.Master or type(spwn.Master.ID)~="number") or (tostring(spwn.Master.Class.Name) ~= "Druid"))) then -- Druid pets are never group targets
          mn = mn + 1
          local active,check
          if not p.cyclic then 
            if DEBUG_CAST == me then mq(p[1].." "..i..":"..id.." checking buffs...",1) end
            if not buff_set[id] and DEBUG_CAST == me then mq("buff_set not found for "..id,1) end
            active,check = check_buffs_target(p, id)
          else
            if mn ~= p.cyclic and (not p.offcycle_mainassist or (Group.MainAssist and id ~= Group.MainAssist.ID)) then active,check=666,"off cycle"
            else active,check = 0,p.check or p[1] end
          end
          if p.cure then if active > 0 then active = 0 else active = 12 end end
          if active > 45 then active = 45 end
          if p.once then
            -- redo once per half an hour if in same zone
            if p.once[id] == tostring(Zone) then active = 3600 p.once[id] = nil end
          end
          if active > 0 then
            if DEBUG_CAST == me then mq(p[1].." "..i..":"..id.." duration = "..pnum(active).."s",1) end
            pa[id] = Now + (p.recheck or active)
          elseif cast(p,1,nil,1) then -- nomem here; going to mem later after checks
            if DEBUG_CAST == me then mq(p[1].." "..i..":"..id.." (CAST, match as "..check..")",1) end
            -- recheck the target active status (bugs sometimes)
            local timeout = Now + 0.5
            while active <= 0 and timeout > Now do
              set_target(id,1)
              coroutine.yield(AGAIN)
              active,check = check_buffs_target(p, id)
            end
            if active > 0 then
              if active > 90 then active = 90 end
              if DEBUG_CAST == me then mq(p[1].." "..i..":"..id.." duration = "..pnum(active).."s -- LATE CATCH!",1) end
              pa[id] = Now + (p.recheck or active)
            else
	      local rv
              -- if we have not targeted the target in 6s, do so now
              -- if buff_set[id] and (Now-6 > buff_set[id].last_active) then set_target(id,1) end
              if not p.book_index or ((Target.ID ~= id) or (Me.Book[p.book_index].StacksTarget == 1)) then
                if SpellMemCooldown <= Now then rv = cast(p,nil,id) else rv=NOTYET end
                if rv == IMMUNE then pa[id] = Now + 300
                      if DEBUG_CAST == me then mq("Immune!",1) end
                else
                      local bl,chk = check_buffs_target(p, buff_set[id])
                      if bl <= 3 then bl = 3 end
                      if DEBUG_CAST == me then mq(p[1].." resulting duration = "..pnum(bl).."s as "..chk,1) end
                      if bl > 120 then bl = 120 end
                      pa[id] = Now + (p.recheck or bl)
                end
                if p.once and rv == OK then p.once[id] = tostring(Zone) end
                if p.cyclic and (rv ~= NOTYET) and (rv ~= FAIL) and (mn == p.cyclic) then p.cyclic = p.cyclic + 1 end
              else
                -- do not recheck immediately
                pa[id] = Now + 1
              end
              -- we changed targets and tried to cast, so block rest of the pipe
              if not p.cyclic then return OK end
            end
          else
            pa[id] = Now + 3
          end
        end
      end
    end
    if p.cyclic and mn < p.cyclic then p.cyclic = 1 end
    return
  end
	
  if (p.offtargets or p.alltargets) and cast(p,1) then
    if cast ~= cast_spell then return end
    if (p.turn_me and not p.nturn) and (p.turn_me ~= 1) then return end
    -- check if we have applied this to all non-primary targets, and if not, apply it to the one that dies last first
    -- TODO: cast, then check for the actual debuff length in a second pass for recast delay
    local mx = p.first or 1
    if (CombatTargets < mx) then return end
    local pa = p.offtargets or p.alltargets
    local st,en,ct
    if p.offtargets then ct = "offtarget" else ct = "ontarget" end
    local cs = p
    if p.group and ((en-st)*cn > 0) and cast(p.group,1) and (not p.group.targets or (p.group.targets < CombatTargets) ) and (not p.group.maxrange or SpawnCount["npc targetable radius "..p.group.maxrange] == CombatTargets) then cs = p.group end
    local sum=0
    local changed_target
	local range = CombatTargets - mx + 1
	if p.nmax and range > p.nmax then range = p.nmax end 
    for ki=1,range do
	  local i = (ki + (group.Order[Me.ID] or 0) - 1) % range + mx
      local id = CombatArray[i][1]
	  if not buff_set[id] then set_target(id) end
      if (not p.nturn or (id_to_turn(p,id) == p.turn_me) or (p.always_n and CombatTargets<=p.turn_max)) and (not pa[id] or (pa[id] < Now)) and (not p.health or not buff_set[id] or not buff_set[id].health or ((p.health > 0 and buff_set[id].health[3]>p.health) or (p.health < 0 and buff_set[id].health[3]<-p.health))) then
        local spwn = Spawn[id]
        if pa.body and spwn then
          local bd = pa.body
          if type(bd) == "string" then bd = {bd} end
          local pass
          local sbd = tostring(spwn.Body)
          for i,v in ipairs(bd) do
            if sbd == v then pass = 1 end
          end
          if not pass then spwn = nil end
        end
        if p.charm and spwn and spwn.PctHPs < 10 then spwn = nil end -- only charm > 10hp 
        if spwn and not is_dead(spwn) and (not p.maxrange or (spwn.Distance < p.maxrange)) then
          local active,aname = check_buffs_target(p, buff_set[id])
          if DEBUG_CAST == me then mq(ct.." "..i..":"..id.." checking buffs = "..active.." as "..aname,1) end
		  if p.nocheck then active = 0 end
          if p.group then
            local gla = check_buffs_target(p.group, buff_set[id]) or 0
            if gla > active then active = gla end
          end
		  
          local acheck = active - ((p.lmax and 12) or (p.cast_time or 1))
		  if p.rdelay and not pa[id] then acheck=math.random()*p.rdelay*0.75 + p.rdelay/4 end
		  if p.debuff and buff_set[id] and buff_set[id].nbuffs ~= 0 and buff_set[id].nbuffs < p.debuff then acheck=3 end
		  
          if acheck > 0 then
            -- if DEBUG_CAST == me then mq(ct.." "..i..":"..id.." duration = "..pnum(active).."s",1) end
            pa[id] = Now + acheck
          else
            if DEBUG_CAST == me then mq(ct.." "..i..":"..id.." (CAST)",1) end
            local skip
            if p.lmax then
              local lvl = spwn.Level
              if p.lmax < lvl or (spwn.Named == 1 and not p.charm) then  -- do not try to mezz nameds
                skip = 1
                pa[id] = Now + 240
              end
            end
            if not skip then
              if i > TANK_COUNT then eq("/attack off") end
              set_target(id,1)
              changed_target=1

              -- late check, sometimes bugs
              active = (not p.nocheck and check_buffs_target(p, buff_set[id])) or 0
              if p.group then
                local gla = check_buffs_target(p.group, buff_set[id]) or 0
                if gla > active then active = gla end
              end
              if active > 0 then
                pa[id] = Now + active
              else
                if cs == p.group and p.group.maxrange and type(Target.Distance) == "number" and Target.Distance > p.group.maxrange then cs = p end
                if p.cyclic and #cs > 1 then cs = cs[p.cyclic] end
                local rv = cast(cs,nil,id)
                if rv == IMMUNE then pa[id] = Now + 240 
                  if DEBUG_CAST == me then mq("Immune!",1) end
                else
                  local bl = check_buffs_target(cs, buff_set[id])
                  if bl <= 12 then bl = 12 end
                  pa[id] = Now + (p.nocheck or p.recheck or cs.nocheck or (bl - ((p.lmax and 12) or 0)))
                  if bl > Now and p.charm == "announce" then encode(id, "CHARMING") end
                end
                if p.group and p.group.targets then p.group.targets = CombatTargets end
	        if rv == OK and p.cyclic then
		  p.cyclic = p.cyclic + 1
		  if #p < p.cyclic then p.cyclic = 1 end
	        end
                -- we changed targets and tried to cast, so block rest of the pipe
                return OK
              end
            end
          end
        end
      end
    end
    return (changed_target and OK) or nil
  end


  if p.dtime and buff_set.target and buff_set.target.health and p.dtime > buff_set.target.health then return end
  if p.giftspell and (check_buffs_name("Gift of Mana", buff_set.me) <= 0) then return end
  if p.nogift and (check_buffs_name("Gift of Mana", buff_set.me) > 0) then return end
  if p.petlow and ((type(Me.Pet.PctHPs) ~= "number") or (Me.Pet.PctHPs > 80)) then return end
  if p.petdown and ((type(Me.Pet.PctHPs) ~= "number") or (Me.Pet.PctHPs > 90)) then return end
  if p.petcrit and ((type(Me.Pet.PctHPs) ~= "number") or (Me.Pet.PctHPs > 50)) then return end

  if p.groupcure then
    if not p.skiprecast and not cast(p,1) then return end
    -- check all group Mana; if any is below 75(or so), match
    local grp = group.IDs
    for i=1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]
      if spwn and buff_set[id] and check_buffs_name("detrimental", buff_set[id]) and not is_dead(spwn) and spwn.Distance < 125 and match_classes(p, spwn) then target = id end
    end
    if not target then return end
  end

  if p.anymanalow then
    if not p.skiprecast and not cast(p,1) then return end
    -- check all group Mana; if any is below 75(or so), match
    local grp = group.IDs
    local minv, mini = p.anymanalow
    for i=1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]
      if spwn and type(spwn.PctMana) == "number" and not is_dead(spwn) and spwn.Class.Name ~= "Bard" and not spwn.Master.ID and spwn.Distance < 125 and spwn.PctMana < minv and spwn.PctMana ~= 0 and match_classes(p, spwn) then
        minv = spwn.PctMana
        mini = spwn.ID
      end
    end
    if not mini then return end
    target = mini
  end

  if p.anymanahigh then
    if not p.skiprecast and not cast(p,1) then return end
    -- check all group Mana; if any is below 75(or so), match
    local grp = group.IDs
    local minv, mini = p.anymanahigh
    for i=1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]
	  if spwn and spwn.Class.Name ~= "Bard" then 
        if spwn and type(spwn.PctMana) == "number" and not is_dead(spwn) and not spwn.Master.ID and spwn.Distance < 125 and spwn.PctMana > minv and match_classes(p, spwn) then
          minv = spwn.PctMana
          mini = spwn.ID
          break
        end
	  end
    end
    if not mini then return end
    target = mini
  end

  if p.anyendlow then
    if not p.skiprecast and not cast(p,1) then return end
    -- check all group Mana; if any is below 75(or so), match
    local grp = group.IDs
    local minv, mini = p.anyendlow
    for i=1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]
      if spwn and type(spwn.PctEndurance) == "number" and not spwn.Master.ID and not is_dead(spwn) and spwn.Distance < 125 and spwn.PctEndurance < minv and spwn.PctEndurance ~= 0 and match_classes(p, spwn) then
        minv = spwn.PctEndurance
        mini = spwn.ID
      end
    end
    if not mini then return end
    target = mini
  end

  if p.anyhplow then
    if not p.skiprecast and not cast(p,1) then return end
    local grp = group.IDs
    local minv, mini = p.anyhplow
    for i=1,#grp do
      local id = grp[i]
      local spwn = Spawn[id]	  
      if spwn and type(spwn.PctHPs) == "number" and not is_dead(spwn) and spwn.Distance < 125 and spwn.PctHPs < minv and match_classes(p, spwn) then
		if id ~= Me.ID or not p.notself then 
		  minv = spwn.PctHPs
          mini = spwn.ID
		end
      end
    end
    if not mini and not target then return end
	if not p.notarget then target = mini end
  end

  if p.groupext and GroupExt then
    if not p.skiprecast and not cast(p,1) then return end
    local ext = Spawn["pc "..GroupExt]
    if not ext or type(ext.ID) ~= "number" then return end
    set_target(ext.ID)
    mq("GroupExt spell "..p[1].." passed!")
  end

  if p.ress then
    local pass = nil
    local grp = group
    local id
    local gn
    for i=1,#grp do
      local spwn = Spawn["pccorpse "..grp[i]]
      if spwn and not spwn.Master.ID and is_dead(spwn) and spwn.Distance3D < 96 and (not p.ress[id] or p.ress[id] < Now) and match_classes(p, spwn) and (math.random(5) == 1) then
        gn = grp[i]
        id = spwn.ID
        pass = 1
        set_target(id)
        if Me.Gem[p[1]] then
          SpellMemCooldown = Now + 12
          SpellMemCooldownName = p[1]		
		    end
        break
      end
    end
    if not pass then return end
    if (not p.ress[id] or p.ress[id] < Now) and Target.ID == id then
      p.ress[id] = Now + math.random(100)/10+5
      eq("/corpsedrag")
    end
    p.next_try = Now+1
    local rv
    if type(Target.Distance) == "number" and Target.Distance < 10 then
      rv = check_turns(p,cast,id)
      if rv == OK then
        p.ress[id] = Now + 6
        local ci = string.find(gn, "'")
        if ci then gn = string.sub(gn,1,ci-1) end
        table.insert(frame_handlers, {coroutine.create(function()
          local d = 0.5
	  for i=1,4 do
            local delay = Now+d
            while delay > Now do coroutine.yield(AGAIN) end
	    if (not Spawn[id]) or (type(Spawn[id].ID) ~= "number") then break end
            eq(";tell "..gn.." accept")
	    d = d + 0.5
	  end
          return STOP
        end)})
      end
    end
    return rv
  end

  if p.anyonefurther then
    local grp = group
    local dst, pid = p.anyonefurther
    for i,v in ipairs(grp) do
      local spwn = Spawn["pc "..v]
      if spwn and spwn.Distance > dst and match_classes(p, spwn) then
        dst = spwn.Distance
        pid = spwn.ID
      end
    end
    if not pid then return end
    if not p.notarget then set_target(pid) end
    return cast(p)
  end


  p.prev_bl = nil

  if not p.timed and p.turn_me and (p.turn_me ~= p.turn_orig) and (p.turn_me ~= 1) and p.recast_time and (not p.prev_active or ((Now - p.prev_active) > p.recast_time)) then
    if not p.prev_active then
      p.prev_active = Now
    else
      mq("-- RESET inactive turned: "..p.real_name.." recast = "..p.recast_time, 1)
      p.prev_active = nil
      validate_spell(p)
    end
  end

  if p.last_target and CombatTargets >= p.last_target and cast(p,1) then 
    local lt = CombatTargets+1-p.last_target
	if lt > TANK_COUNT then lt = TANK_COUNT end
	target = CombatArray[lt][1]
  elseif target and Target.ID ~= target and not p.notarget and not p.group_target then else target = nil end
 
 
  -- and (type(Target.ID) ~= "number" or Target.LineOfSight ~= 1) 
  if p.splash and cast(p,1) and not target and not p.notarget then target = Me.ID end
  
  if not p.debuff and not p.group_target and not p.notarget and (type(Target.Distance) == "number" and (Target.Distance > 125 or Target.LineOfSight ~= 1)) then
    handle_out_of_range()
    return
  end

  local rv = check_turns(p,cast,target)

  if DEBUG_TURNS and (rv == OK) and p.turn_me then mq("Turn: cast success for "..p.real_name,1) end

  if rv == OK or rv == NOTYET then p.next_try = Now+(p.retarget or 0.5) p.force_cast = nil end

  return rv
end

function cast_swap(p) 
  if p.cycle then
    -- we are out of combat, and can swap in cycled spells; if this spell is not blocked, mem it and
    -- return OK to stop memming the other cycled spells
    local rv = cast_spell(p, OK)
    p.next_try = Now
    return rv
  end
  return 
end

function cast_pipeline(pipe, cast, target)
  if not pipe then return end
  if not cast then cast = cast_spell end
  local orig_cast = cast
  local hadok
  local nf = function() end
  for n,p in ipairs(pipe) do
    if Me.SpellInCooldown ~= p.WAIT_GCD then
      if p.GCD and p.NextCast and p.NextCast>Now then cast = nf end
      for i,v in ipairs(p) do
        if not v.real_name and (type(v[1]) ~= "table" or not v[1].real_name) then validate_spell(v) end
        local result = check_pipeline(v,cast,p,target)
        if result and (cast ~= nf) then
          hadok = result
          if p.GCD then p.NextCast = Now + p.GCD end
          cast = nf -- do all spells without casting for correct turn management
        end
      end
      -- return to true casting type for the next pipeline in turn
      cast = orig_cast
    end
  end
  return hadok
end

function spawn_distance(a,b)
  local ax = a.X
  local ay = a.Y
  local bx = b.X
  local by = b.Y

  return math.sqrt((ax-bx)*(ax-bx) + (ay-by)*(ay-by))
end

CombatSortOrder = -1 -- 1 -> kill adds/easies first, -1 -> kill named/hard first
AddsFirst = 1 -- if set, kill adds before named (otherwise in CombatSortOrder)

function CombatOrderFunc(l,r)
  local c = l[3] - r[3]
  if c ~= 0 then return CombatSortOrder*c > 0 end
  c = l[2] - r[2]
  if c ~= 0 then return CombatSortOrder*c > 0 end
  c = l[1] - r[1]
  return CombatSortOrder*c > 0
end

RunDelay = 0
WalkDelay = 0
StopDelay = 0
AlreadyFollowing = nil
LastFollowTarget = nil
LastFollowFinal = 0
LastFollowDistance = 0

function target_follow(timeout, id, loc)
  if ZoneFollowDisable>Now or DoNotMove then
    if (Me.Moving == 1) and (StopDelay < Now) then
      eq("/keypress forward")
      eq("/keypress right")
      StopDelay = Now+1
    end
    return
  end
  if (Me.Class.Name ~= "Bard") and (type(Me.Casting.ID) == "number") then return end
  
  local start = Now
  local dist = 7
  local tgt
  local name
  if not loc then 
    if not id then id = Target.ID end
    tgt = Spawn[id]
    if (type(tgt) ~= "userdata") or is_dead(tgt) then return end 
    name = tgt.Name
    if not timeout then timeout = 6 end
    if type(Group.MainAssist.ID) ~= "number" or id ~= Group.MainAssist.ID then dist = tgt.MaxRangeTo*0.85 end
    if type(dist) ~= "number" then return end
    if dist < 7 then dist = 7 end
  else
    tgt = loc
  end
  local first_in = 1
  local nd
  local mind = 25
  local MindDelay = Now
  if AlreadyFollowing then return end
  AlreadyFollowing = 1
  local startloc = {X=Me.X,Y=Me.Y}
  local prex = Me.XTarget
  local ploc = {X=Me.X, Y=Me.Y}
  local xtgts = has_xtargets()

  if LastFollowTarget == id and LastFollowDistance > dist and LastFollowFinal > Now then
	dist = LastFollowDistance
  end  

  while 1 do
    if type(tgt) ~= "userdata" and not loc then break end
    if has_xtargets() ~= xtgts or DoNotMove then break end
    if Me.XTarget ~= prex then break end
    local moved = spawn_distance(Me, ploc)
    if not loc and name ~= tgt.Name then break end
    nd = spawn_distance(Me, tgt)
    if moved*2 >= nd then break end -- do not overshoot
    if nd > mind and mind < 25 and MindDelay < Now then 
      dist = dist + 1 
      MindDelay = Now + 0.1
    end
    if nd < mind then mind = nd end
    if (nd <= dist) or (Now > start+timeout) or (ZoneFollowDisable>Now) or nd > FOLLOW_DISTANCE then break end
    if type(id) == "number" then
      eq("/squelch /face nolook fast id "..id)
    else
      if not loc then
        eq("/squelch /face nolook fast "..id)
      else
        eq("/squelch /face nolook fast loc "..loc.Y..","..loc.X)			
      end
    end
    if (nd < 5) and (Me.Running == 1) and (Now>RunDelay) then
      eq("/keypress ctrl+r")
      RunDelay = Now+1
    end
    if (nd > 5) and (Me.Running == 0) and (Now>WalkDelay) then
      eq("/keypress ctrl+r")
      WalkDelay = Now+1
    end
    if first_in then
      if Me.Standing ~= 1 then eq("/stand") end
      eq("/keypress forward hold")
      first_in = nil
      StopDelay=Now+1
    end
    ploc.X,ploc.Y = Me.X, Me.Y
    coroutine.yield(AGAIN)
    if not loc then tgt = Spawn[id] end
  end

  if nd < dist then
	LastFollowTarget = id
	LastFollowFinal = Now + 1.5
	LastFollowDistance = nd
  end

  if (Me.Moving == 1) or (not first_in) then
    if (not first_in) or ((StopDelay < Now) and (Me.Moving==1)) then
      eq("/keypress forward")
      eq("/keypress right")
      StopDelay = Now+1
    end
  end
  if spawn_distance(Me,startloc) > 3 then LastMoved = Now end
  coroutine.yield(AGAIN)  
  AlreadyFollowing = nil
  return nd < dist
end

-- Should correctly offtank on pets until mezzed, when a mezzer is available
MezzedTargets = {}

function attack_index(id)
  local ord = group.Order[id]
  local ai = 1
  if ord and ord > 0 then
	local tgts = CombatTargets
	if tgts > TANK_COUNT then tgts = TANK_COUNT end
    ai = (ord-1) % tgts + 1
  end
  if MezzedTargets[CombatArray[ai][1]] and (MezzedTargets[CombatArray[ai][1]] > Now) then ai = 1 end
  return ai
end

function pet_attack_index()
  return (PetTank and type(Me.Pet.ID) == "number" and attack_index(Me.Pet.ID)) or 1
end

function my_attack_index()
  return attack_index(Me.ID)
end


function turn_attack_on()
  if check_buffs_name("Sarnak Finesse", buff_set.target) <= 0 then
    if Me.Combat ~= 1 then 
      if DoMelee and (TankClasses[Me.Class.Name] or type(Target.PctAggro) ~= "number" or Target.PctAggro < 90) then
        eq("/attack on")
      end
    else
      if not TankClasses[Me.Class.Name] and type(Target.PctAggro) == "number" and Target.PctAggro > 90 then
        -- eq("/attack off")
      end
    end
  else 
    if Me.Combat ~= 0 then eq("/attack off") end
  end

  if ((type(Me.Pet.ID) == "number") or PetTank) and not PetPacify then
    local attack_index = pet_attack_index()
	if #CombatArray < attack_index then return end
    local id = CombatArray[attack_index][1]

    if ((XTarget[1].TargetType ~= "Pet Target") or (XTarget[1].ID ~= id)) and Spawn[id] and (Spawn[id].LineOfSight == 1) then
      set_target(id, 1)
	  local doit
      if not Target.Mezzed or not Target.Mezzed.ID then 
        if BuffSpells and BuffSpells[1].combat and Target.PctHPs and (BuffSpells[1].combat < Target.PctHPs) then return end
        if not Hold or (type(Target.Distance) == "number" and Target.Distance < Hold) then doit = 1 end
      else
        MezzedTargets[id] = Now + 2
        if attack_index ~= 1 then set_target(CombatArray[1][1]) 
          if not Hold or (type(Target.Distance) == "number" and Target.Distance < Hold) then doit=1 end
        end
      end
	  if doit and (not PetTank or not PetTank.fragile or (type(Target.PctHPs)=="number" and Target.PctHPs ~=0 and Target.PctHPs < 99)) then eq("/pet attack") end
    end
  end
end

Quests = {}

FellowshipInsignia = {"Fellowship Registration Insignia"}
PrimaryAnchorTransportDevice = {"Primary Anchor Transport Device"}
function in_combat_targets(id)
  for i,v in ipairs(CombatArray) do
    if id == v[1] then return id end
  end
  return nil
end

encode_vals = {"!","1",".","?",["!"]=0,["1"]=1,["."]=2,["?"]=3}
function encode(ID,MSG)
  local coded = ""
  if not ID then ID = Target.ID end
  if type(ID) ~= "number" then return end
  local spwn = Spawn[ID]
  if not spwn or spwn.Type == "PC" then return end
  for i,v in ipairs(group.IDs) do if ID == v and MSG ~= "CHARMING" then return end end
  while ID ~= 0 do
    coded = coded .. encode_vals[ID % 4 + 1]
    ID = math.floor(ID/4)
  end
  if not MSG then MSG="GOING" end
  eq("/g "..MSG..coded)
end

function basename(s)
  while 1 do
    local n = string.find(s, " ")
    if not n then break end
    s = string.sub(s,n+1)
  end
  return s
end
function split_spawn(s)
  local na = {}
  while 1 do
    local n = string.find(s, " ")
    if not n then table.insert(na,s) break end
    local l = string.sub(s,1,n-1)
    if l ~= "a" and l ~= "an" and l ~= "A" and l ~= "An" then table.insert(na,l) end
    s = string.sub(s,n+1)
  end
  return na
end
function match_spawn(a,b)
  local ar = split_spawn(a)
  local br = split_spawn(b)

  for _,v in ipairs(ar) do
    for _,i in ipairs(br) do
      if v == i then return 1 end
    end
  end
end

Charms={}
triggers[1][" tell.*'CHARMING(.*)'$"] = function(l,m)
  local id = 0
  local M = 1
  while m do
    local s
    if #m > 1 then s,m = string.sub(m,1,1), string.sub(m,2) else s,m = m end 
    id = id + encode_vals[s] * M
    M = M * 4
  end
  local j
  for j = 1,#CombatArray do if CombatArray[j][1] == id then table.remove(CombatArray,j) CombatTargets = CombatTargets-1 break end end
  Charms[id] = Now + 2
end

triggers[1][" tell.*'toying (.*)'$"] = function(l,m) ToyDelay = Now+42 end
triggers[1][" tell.*'toyed up(.*)'$"] = function(l,m) ToyDelay = Now end

triggers[1][" tell.*'GOING(.*)'$"] = function(l,m)
  local id = 0
  local M = 1
  while m do
    local s
    if #m > 1 then s,m = string.sub(m,1,1), string.sub(m,2) else s,m = m end 
    id = id + encode_vals[s] * M
    M = M * 4
  end
  local ict = Now
  if CombatTargets>0 and CombatArray[1] then ict = CombatArray[1][5] end 
  local j
  for j = 1,#CombatArray do if CombatArray[j][1] == id then table.remove(CombatArray,j) CombatTargets = CombatTargets-1 break end end
  local spwn = Spawn[id]
  if spwn and not is_dead(spwn) and (spawn_distance(spwn, Me) < 250) then
    table.insert(CombatArray, 1, {id,spwn.Level,spwn.Named,spwn.CleanName,ict,0,spwn.PctHPs})
    Named = Named + spwn.Named
    CombatTargets = CombatTargets + 1
    -- TODO: also add all spawns in COMBAT_RADIUS from encoded, that we can see
    local n,pretargets = 1,CombatTargets
    local bsn = spwn.CleanName
    while 1 do
      local sn = spwn.NearestSpawn[n]
      if not sn or type(sn.ID) ~= "number" or spawn_distance(sn, spwn) > COMBAT_RADIUS or n > 10 then
        break
      end
      if sn.Type == "NPC" and sn.LineOfSight == 1 and (MATCH_ALWAYS or (sn.Aggressive == 1) or (match_spawn(bsn,sn.CleanName))) then
        local nn = sn.ID
        local had = nil
        for j = 1,#CombatArray do if CombatArray[j][1] == nn then had=1 break end end
		local sns = sn.CleanName
		for i,v in ipairs(blocked_spawns) do
		  if sns:find(v) then had = 1 end
		end
        if not had then
          table.insert(CombatArray, {nn,sn.Level,sn.Named,sn.CleanName,ict,1,sn.PctHPs})
          Named = Named + sn.Named
          CombatTargets = CombatTargets + 1
        end
      end
      n = n + 1
    end
    mq("-- decode passed "..(CombatTargets-pretargets).." additional spawns")
    if PetTank and not PetTank.fragile then
      table.insert(frame_handlers, {coroutine.create(function()
        set_target(id,1)
        eq("/pet attack")
        return STOP
      end)})
    end
  end
end
eq("/custombind delete startcombat")
eq("/custombind add startcombat")
eq("/custombind set startcombat /lua encode()")
eq("/bind startcombat 1")

LastSpellLanded = 0
PetAttackDelay = 0
FacingTimer = 0
SitTimer = 0
BandolierTimer = 0
PreviousCombatTargets = 0
AutoinvDelay = 0
XTO = nil
Delayed = {}
CombatCursorTime = 0

function has_xtargets()
  local xo = 0
  if not XTO then XTO=0 for i = 1, 5 do if XTarget[i].TargetType ~= "Auto Hater" then XTO = XTO + 1 end end end
  local xt = (Me.XTarget and (Me.XTarget + XTO)) or 0

  for i = 1, xt do
    local id = XTarget[i] and XTarget[i].ID
    id = id and Spawn[id]
    if id and not is_dead(id) and not is_grouped(Spawn[id]) then return 1 end
  end
end

AVG_COMBAT_LENGTH = 30
function maintain_combat()
  -- we may have something to combat!
  if is_dead(Me) then return end
  
  local adds = (PreviousCombatTargets ~= CombatTargets)
  local xo = 0
  if not XTO then XTO=0 for i = 1, 5 do if XTarget[i].TargetType ~= "Auto Hater" then XTO = XTO + 1 end end end
  local xt = (Me.XTarget and (Me.XTarget + XTO)) or 0
  local pf = CombatArray[1]

  for i = 1, xt do
    local id = XTarget[i] and XTarget[i].ID
    local had
    for j = 1,#CombatArray do
      if CombatArray[j][1] == id then
        local bs = buff_set[id]
        local tps = bs and Spawn[id] and Spawn[id].PctHPs
        if bs and tps then 
          if (tps > 99) then bs.health = {Now,100,AVG_COMBAT_LENGTH}
          elseif not bs.health then
			if tps == 0 then tps=100 end -- assume the broken-as-of spring 2021 targeting-to-0-health issue in EQ
		    bs.health = {Now,tps,tps*0.01*AVG_COMBAT_LENGTH}
          elseif tps > 0 then
            if tps >= bs.health[2] then
              bs.health[2] = tps
              if tps > 99 then bs.health[1] = Now-0.01 end
              bs.health[3] = tps*0.01*AVG_COMBAT_LENGTH
            else
              local hps = (bs.health[2] - tps) / (Now - bs.health[1]) 
              bs.health[3] = tps / hps
            end
          end
        end
        had=1
        break
      end
    end
    if Charms[id] then
       if Charms[id] > Now then had=1 end
       if Charms[id] < Now-60 then Charms[id] = nil end
    end
    
    if not had and not is_dead(Me) and id and Spawn[id] and (ZoneFollowDisable<Now) then


      local spwn = Spawn[id]

      local sn = spwn.CleanName
      for i,v in ipairs(blocked_spawns) do
	if sn:find(v) then had = 1 end
      end
      if not had and not is_dead(spwn) and not is_grouped(spwn) and (not Charms[id] or Charms[id] < Now) then
        if (spawn_distance(spwn, Me) < CombatDistance) or (Delayed[spwn.ID] and Delayed[spwn.ID]<Now) then
          LastCombatChange = Now
          local n = spwn.Named
		  local ct = Now
          if not CombatArray[1] then
            -- clean up buff uptimes, going into new combat
            local next = next
            for i,v in pairs(buff_set) do if next(v.uptimes) ~= nil then v.uptimes = {} end end
          else ct = CombatArray[1][5] end
          if n == 1 then mq("Adding Named "..spwn.CleanName, 1) end
          table.insert(CombatArray, {id,spwn.Level,n,spwn.CleanName,ct,1,spwn.PctHPs})
          Named = n + Named
          adds = 1
        elseif (spawn_distance(spwn, Me) < DelayedCombatDistance) and not Delayed[spwn.ID] then
          local ni = Now + InFluxTime/2
          Delayed[spwn.ID] = ni
          LastCombatChange = Now
        end
      end
    end
  end
  PreviousCombatTargets = CombatTargets
  CombatTargets = #CombatArray


  if type(Cursor.ID) ~= "number" then CombatCursorTime = 0
  else
    if CombatTargets > 0 then CombatCursorTime = CombatCursorTime + Delta end
    if CombatCursorTime > 120 then
      -- destroy item on cursor... EEEKS
      eq("/destroy")
    end
  end

  -- for tanks, switch between 2h/1h modes based on HP
  if ((AltAbilityReady[970] == 1) or (AltAbilityReady[969] == 1)) and (Me.ID ~= Group.MainTank.ID) then
    -- switch between 2h/1h
    local lim,tgts = 70,1
    if (check_buffs_name("Lich Sting", buff_set.me)>0 or check_buffs_name("Mortal Coil", buff_set.me)>0) and ((Named==0) and (CombatTargets < 4)) then lim,tgts = 45,3 end
    if check_buffs_name("Defensive Proficiency", buff_set.me)<=0 then
      if (Me.PctHPs < lim) and ((CombatTargets > tgts) or (Named > 0)) and (BandolierTimer < Now) then
        eq("/bandolier activate 1h")
        BandolierTimer = Now + 0.5
      end
    else
      if (Me.PctHPs > lim) and (CombatTargets <= tgts) and (Named == 0) and (BandolierTimer < Now) then
        eq("/bandolier activate 2h")
        BandolierTimer = Now + 0.5
      end
    end
  end

  -- and perform actual combat, if we have targets
  if CombatTargets == 0 then
    LastNonCombat = Now
	PreviousCombatArray = nil
    Named = 0 -- make sure Named are not mistracked
    if (Me.Combat == 1) and (FacingTimer < Now) then
      FacingTimer = Now + 0.25
      eq("/attack off")
    end

    if HasMage and #CombatPipeline > 1 then
      local p = CombatPipeline[2][#(CombatPipeline[2])]
      if tostring(p[1]):find("Modulation") and not FindItem[p[1]] and AutoinvDelay < Now then
        AutoinvDelay = Now + 6
        eq("/g need modrod")
      end
    end

    if StopDelay < Now then
      if (type(Group.MainAssist.ID) ~= "number") or (Group.MainAssist.Distance < 75) then
        cast_pipeline(CombatPipeline, cast_swap)
      end
    end

    if Group.MainAssist.ID and (Group.MainAssist.ID ~= Me.ID) then 
      if (DoMelee ~= 3) or (Me.ID ~= Group.MainTank.ID) then target_follow(2, Group.MainAssist.ID) end
    elseif GroupExt and type(Spawn["pc "..GroupExt]) == "userdata" and Me.ID ~= Group.MainAssist.ID then
      target_follow(2, Spawn["pc " .. GroupExt].ID)
    end

    if (LastMoved < Now-3) and ((Me.PctHPs < 85) or (Me.PctEndurance < 95) or ((Me.PctMana ~= 0) and (Me.PctMana < 85))) and ((Me.Moving == 0) and (Me.Standing == 1) and (SitTimer < Now)) and (type(Me.Casting.ID) ~= "number" or Me.Class.Name == "Bard") then
      SitTimer = Now + 3
      eq("/sit")
    end

    return
  end
  LastCombatActive = Now
  FoundTarget = Now+3

  -- remove dead targets, we've won!
  for i,v in ipairs(CombatArray) do
    local spwn = Spawn["id "..v[1]]
    if (not spwn) or is_dead(spwn) or is_grouped(spwn) or (spwn.ID ~= v[1]) or is_dead(Me) or ((DoMelee == 3) and (spwn.Distance3D > CombatDistance)) or (AllowEscape and spwn.Distance3D > DelayedCombatDistance) then
      if is_grouped(spwn) then
         if Me.Pet and type(Me.Pet.ID) == "number" then eq("/pet regroup") end
         Charms[v[1]] = Now + 2
      end
      local n = v[4]
      local t = Now - v[5]
      local id = v[1]
      Named = Named - v[3]
      table.remove(CombatArray,i)
      Delayed[id] = nil
      buff_set[id] = nil
      CombatTargets = CombatTargets - 1
      if i == 1 then dps_finish(n, t) end
      adds = 1
      if CombatPipeline then for n,p in ipairs(CombatPipeline) do for ii,iv in ipairs(p) do
        if not iv.once and (i==1) then iv.next_try = nil end
        if (iv.turnperkill and iv.turn_me) or (iv.turnpercombat and CombatTargets==0) then
          if iv.turn_me == 1 then iv.turn_me = iv.turn_max else iv.turn_me = iv.turn_me - 1 end
          iv.prev_active = Now
        end
        local ta = iv.offtargets or iv.alltargets
        if ta then ta[id] = nil end
      end end end
      if CombatTargets == 0 then 
        do_autoaas()
        return
      end
      if i == 1 then
	    if CombatArray[1][5] > v[5] then CombatArray[1][5] = v[5] end
        CombatArray[1][6] = 1
        for i,v in pairs(buff_set) do if next(v.uptimes) ~= nil then v.uptimes = {} end end
      end
    end
  end

  -- if we need to stay this one out
  if DisableCombat then return end
  if InitialCombatDelay and (LastNonCombat+InitialCombatDelay > Now) then return end

  sort_combatarray()

  local MA = Group.MainAssist.ID
  -- target the thing which will soon die
  local mai = my_attack_index()
  if not CombatArray[mai] then return end
  
  if (Target.ID ~= CombatArray[mai][1]) and not Balance and not Focus and (DoMelee or (Me.ID ~= MA)) then
    set_target(CombatArray[mai][1])
    if not Focus and DoMelee then eq("/squelch /face nolook fast") end
  end

  if adds then turn_attack_on() end

  if Focus then
    set_target(Focus)
  elseif Balance then
    -- rather than killing the primary target, we always hit the target with most hp left
    local xt = (Me.XTarget and (Me.XTarget + XTO)) or 0

    local chp = 0
    local cid
    for i = 1, xt do
      local id = XTarget[i] and XTarget[i].ID
      local spwn = id and Spawn[id]
      if spwn and not is_dead(spwn) and spwn.PctHPs > chp then
        cid = id
        chp = spwn.PctHPs
      end
    end
    if cid and Target.ID ~= cid then
      set_target(cid)
      if DoMelee then eq("/squelch /face nolook fast") end
    end
  end

  -- make sure we have a target during combat pipe execution
  if CombatArray[mai] and cast_pipeline(CombatPipeline, nil, CombatArray[mai][1]) then LastSpellLanded = Now end

  if FacingTimer > Now or not CombatArray[mai] or CombatTargets == 0 then return end
  FacingTimer = Now + 0.25
  if AltAbility["Companion's Discipline"] and (Target.ID == CombatArray[mai][1]) then 
    if not Hold or (type(Target.Distance) == "number" and Target.Distance < Hold) then eq("/pet swarm") end
  end
  local pai = pet_attack_index()
  if PetTank and type(Me.Pet.PctHPs) == "number" then
    local back
	-- also pet back if I have not seen the target for 3s (flip between on/off sessions; 1s back, 3s in
	local spwn = CombatArray[pai] and Spawn[CombatArray[pai][1]]
	if spwn then
	  if spwn.LineOfSight ~= 1 then
	    if not LastTargetMissing then LastTargetMissing = Now end
	  else LastTargetMissing = nil end
	end
	if LastTargetMissing then
	  local phaze = (Now-LastTargetMissing) / 4
	  phaze = phaze - math.floor(phaze)
	  if phaze > 0.75 then back=1 end
	end
    if Me.Pet.PctHPs < 70 or (PetTank.fragile and Me.Pet.PctHPs < 90) then back = 1 end
	if back then eq("/pet regroup") end
  end
  -- also sit in combat if far away
  if Target.Distance and (Target.PctAggro < 65) and (CombatTargets==1) and (not DoMelee or DoMelee == 1) and (Target.Distance > Target.MaxRangeTo) then
    if (LastMoved < Now-3) and ((Me.PctHPs < 80) or (Me.PctEndurance < 95) or ((Me.PctMana ~= 0) and (Me.PctMana < 85))) and ((Me.Moving == 0) and (Me.Standing == 1) and (SitTimer < Now)) and (type(Me.Casting.ID) ~= "number") then
      SitTimer = Now + 3
      eq("/sit")
    end	
  end

  if Hold and type(Target.Distance) == "number" and Target.Distance > Hold and PetTank then eq("/pet regroup")
  elseif ((Me.Combat == 0) and (check_buffs_name("Sarnak Finesse", buff_set.target)<=0)) or ((Me.Combat == 1) and (check_buffs_name("Sarnak Finesse", buff_set.target)>0))
  or ((XTarget[1].TargetType == "Pet Target") and (XTarget[1].ID ~= CombatArray[pai][1]))
  or (type(Target.PctAggro) == "number" and Target.PctAggro > 85) then turn_attack_on() end

  local disable
  if (Me.ID ~= MA or (DoMelee and (DoMelee>1))) and Target.Distance then
    if (DoMelee and DoMelee >= 2) and (Target.Distance > Target.MaxRangeTo/2) and (Target.LineOfSight == 1) then 
      if (DoMelee ~= 3) and CombatArray[mai] and Target.ID == CombatArray[mai][1] then target_follow(2) disable = 1 end
    elseif ((Target.LineOfSight ~= 1) or (DoMelee == 1) or not DoMelee) and (type(MA) == "number") then 
      if Me.ID ~= Group.MainTank.ID then target_follow(2, MA) end
      if Me.ID == MA and Target.LineOfSight == 0 then target_follow(2) disable = 1 end
    end
  end
  -- if not disable and DoMelee and CombatArray[1] and Target.ID == CombatArray[1][1] and (Me.ID ~= MA or Me.ID == (Group.Tank and Group.Tank.ID)) then eq("/squelch /face nolook fast") end
  if not disable and DoMelee and CombatArray[mai] and Target.ID == CombatArray[mai][1] then eq("/squelch /face nolook fast") end
end

ClassFilenames = {["Ranger"]="-rng", ["Cleric"] ="-cle", ["Shadow Knight"]="-sk", ["Bard"]="-brd", ["Beastlord"]="-bst", ["Berserker"]="-ber", ["Paladin"]="-pal", ["Warrior"]="-war", ["Shaman"]="-sha", ["Druid"]="-dru", ["Necromancer"]="-nec", ["Enchanter"]="-enc", ["Magician"]="-mag"}
function target_and_action(l, m)
  local t, c = m:match("(.-) (.+)")
  if c then
    table.insert(frame_handlers, {coroutine.create(function()
      if t and #t > 0 then
        local id = Spawn[t].ID
        if id then set_target(id) end
      end
      if c:sub(1,1) == "/" then eq(c) else
        if c == "qf" then
          finish_quest(t)
        else
          eq("/say "..c)
          -- if it opens tasks window, accept it, too
          local End = Now + 2
          while End > Now do
            if Window["TaskSelectWnd"].Open == 1 then
              eq("/nomodkey /notify TaskSelectWnd TSEL_AcceptButton leftmouseup")
              break
            end
            coroutine.yield(AGAIN)
          end
        end
      end
      return STOP
    end)})
  elseif m == "gate" then
    table.insert(frame_handlers, {coroutine.create(function()
      local sp = {"Gate"}
	  if Me.AltAbility[1217] then sp = {1217} end
      local rv = cast_spell(sp)
      if rv ~= OK then cast_spell(sp) end
      return STOP
    end)})
  elseif m == "throne" then
    table.insert(frame_handlers, {coroutine.create(function()
      local sp = {511}
      local rv = cast_spell(sp)
      if rv ~= OK then cast_spell(sp) end
      return STOP
    end)})
  elseif m == "origin" then
    table.insert(frame_handlers, {coroutine.create(function()
      local sp = {331}
      local rv = cast_spell(sp)
      if rv ~= OK then cast_spell(sp) end
      return STOP
    end)})
  elseif m == "door" then
    eq("/doortarget")
    table.insert(frame_handlers, {coroutine.create(function()
      local delay = Now+0.75
      while delay > Now do coroutine.yield(AGAIN) end
      eq("/click left door")
      delay = Now+1
      while delay > Now do
        if Window.LargeDialogWindow.Open == 1 then
          eq("/notify LargeDialogWindow LDW_YesButton LeftMouseUp")
          delay = Now
        end
        coroutine.yield(AGAIN)
      end
      return STOP
    end)})
  elseif m == "drop" then
    for i = 2,#group do eq("/taskremove "..group[i]) end
    eq("/taskquit")
  elseif m == "loot" then
    table.insert(frame_handlers, {coroutine.create(function()
      while AdvLoot.PCount > 0 or (AdvLoot.LootInProgress ~= 0) or (Now<AdvLootDelay) do
        -- loot nodrop things
        if Window["ConfirmationDialogBox"] and Window["ConfirmationDialogBox"].Open ~= 0 and (CloseDelay < Now) then 
          CloseDelay = Now + 1
          eq("/nomodkey /notify ConfirmationDialogBox Yes_Button leftmouseup") 
        end
        if AdvLoot.LootInProgress ~= 1 and AdvLoot.PCount > 0 then
          AdvLootDelay = Now + 1
          eq("/advloot personal 1 loot")
        end
        coroutine.yield(AGAIN)
      end
      return STOP
    end)})		
  elseif Quests.items and Quests.items[m] then
    for i,v in ipairs(Quests.items[m]) do cast_raw(v) end
  elseif Quests.givers and Quests.givers[m] then
    local q = Quests
    local giver = q.givers[m]
    table.insert(frame_handlers, {coroutine.create(function()
      local id = Spawn[m].ID
      -- go through all items from the givers list, and hand them over to the giver (which is targeted)
      local gives
      for i,v in ipairs(giver) do
        if id then set_target(id) end
        eq("/itemnotify \""..v.."\" LeftMouseUp")
        local Timeout = Now + 2
        while not Cursor.ID and (Timeout > Now) do 
          local notices
          if Window["QuantityWnd"].Open == 1 and not notices then
            eq("/nomodkey /notify QuantityWnd QTYW_Accept_Button LeftMouseUp")
            notices = 1
          end
          coroutine.yield(AGAIN)
        end
        eq("/click left target")
        Timeout = Now + 2
        if not gives and Cursor.ID then gives = 1 end
        while Cursor.ID and (Timeout > Now) do coroutine.yield(AGAIN) end
      end
      local Timeout = Now + 2
      while gives and (Window["GiveWnd"].Open == 0) and (Timeout>Now) do coroutine.yield(AGAIN) end
      if Window["GiveWnd"].Open == 1 then eq("/notify GiveWnd GVW_Give_Button LeftMouseUp") end
      return STOP
    end)})
  elseif m:sub(1,1) == "-" then
    mq("Matching load "..m,1)
    if m == "-" or (m:sub(2,2) == "-") then
      local cm = ClassFilenames[Me.Class.Name]
      local lv = Me.Level -- math.floor((Me.Level+1)/5)*5
      if m:sub(2,2) == "-" then lv = m:sub(3,-1) end 
      eq("/lua -init")
      m = cm..lv
    end
    eq("/lua "..m)
  end
end

-- quest propagator
triggers[1][" tell.*'dq (.+)'"] = target_and_action
triggers[1][" tell.*'focus(.*)'"] = function(l,m) if m ~= "" then Focus = m else Focus = nil end end
triggers[1][" tell.*'hold(.*)'"] = function(l,m) if m ~= "" then Hold = tonumber(m) else Hold = nil end end
triggers[1][" tell.*'Fate'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  cast_spell({"Shield of Fate"})
  return STOP
end)})
end

triggers[1][" tell.*'FSI'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  cast_spell(FellowshipInsignia)
  return STOP
end)})
end

triggers[1][" tell.*'BMI'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  cast_spell({"Mirror Fragment of Anashti Sul"})
  return STOP
end)})
end

triggers[1][" tell.*'GH'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  cast_spell(PrimaryAnchorTransportDevice)
  return STOP
end)})
end

function aa_delay()
  local End = Now + 0.5
  while ((End > Now) or (MakingCampsite and End+10>Now)) and not is_dead(Me) do coroutine.yield(AGAIN) end
  if Window.AAWindow.Open ~= 1 then
    mq("AAWindow is not open, trying to open it")
    eq("/keypress v")
  end
  while (Window.AAWindow.Open ~= 1) and not is_dead(Me) do coroutine.yield(AGAIN) end
end

AAWindows = {
  {"Focus", 5},
  {"Class", 3},
  {"Arch", 2},
  {"General", 1},
}

function do_autoaas() 
  if AAsRunning or not AUTO_AAs or is_dead(Me) or Me.AAPoints < 200 then return end
  AAsRunning = 1
  table.insert(frame_handlers, {coroutine.create(function()
    local preaas = Me.AAPoints
    for _i,w in ipairs(AAWindows) do
      aa_delay()
      local aaw = Window.AAWindow[".Child"]
      mq("AAW_"..w[1].."List")
      aaw = aaw["AAW_"..w[1].."List"]
      aaw = aaw[".List"]
      -- go through every AA in the list; if it has a value, try to buy it
      for i = 1,100 do
        local si = tostring(i)
        local n = aaw[si..",1"]
        if not n or n == "" then break end
        for _i, an in ipairs(AUTO_AAs) do
		  if preaas > Me.AAPoints then break end
          if n:find(an) or (an=="") then
            local c = tonumber(aaw[si..",3"])
            if type(c) == "number" and c < Me.AAPoints then
              aa_delay()
              eq("/notify AAWindow AAW_Subwindows tabselect "..w[2])
              aa_delay()
              eq("/notify AAWindow AAW_"..w[1].."List listselect "..i)
              aa_delay()
              if type(Window.ConfirmationDialogBox)=="userdata" and Window.ConfirmationDialogBox.Open == 1 then eq("/notify ConfirmationDialogBox No_Button LeftMouseUp") end		
              eq("/notify AAWindow AAW_BuyAllButton LeftMouseUp")
              aa_delay()
              if type(Window.ConfirmationDialogBox)=="userdata" and Window.ConfirmationDialogBox.Open == 1 then eq("/notify ConfirmationDialogBox Yes_Button LeftMouseUp") aa_delay() end
            end
          end
        end
      end
    end
	if preaas ~= Me.AAPoints then AAsRunning = nil end
    return STOP
  end)})	
end
--[[
/keypress v -- open aa window
/notify AAWindow AAW_Subwindows tabselect 5 -- number
aaw = Window.AAWindow[".Child"].AAW_FocusList[".List"]

mq(aaw["2,1"].. " / "..aaw["2,3"])
  
]]
-- triggers[1][" tell.*'a'"] = function() eq("/g a") end

triggers[1][" tell.*'Trash'"] = function() CombatSortOrder,OrderFlip=1,1 end
triggers[1][" tell.*'Named'"] = function() CombatSortOrder,OrderFlip,AddsFirst=-1,1,nil end
triggers[1][" tell.*'Bal'"] = function(l,m) Balance = 1 end
triggers[1][" tell.*'Focus'"] = function(l,m) Balance = nil end
triggers[1][" tell.*'all match'"] = function(l,m) MATCH_ALWAYS = 1 end

triggers[1][" tell.*'Steady'"] = function() DoMelee = 1 end
triggers[1][" tell.*'Rush'"] = function() DoMelee = 2 end
triggers[1][" tell.*'Gather'"] = function() DoMelee = 3 CombatSortOrder = 1 end
triggers[1][" tell.*'Here'"] = function() CombatTargets = 0 CombatArray={} eq("/pet back") end


function do_button(window,state,timeout,dismiss)
  timeout = timeout or 0.5
  local Timeout = Now+timeout
  while (not Window[window] or (Window[window].Open == 0)) and (Now<Timeout) do coroutine.yield(AGAIN) end
  Timeout = Now+timeout/2 while (Timeout > Now) do coroutine.yield(AGAIN) end
  local rv
  while Window[window] and (Window[window].Open == 1) do
    rv = 1
    eq("/notify "..window.." "..state.." LeftMouseUp")
    if not dismiss then break end
    Timeout = Now + timeout
    while (Window[window] and (Window[window].Open == 1)) and (Now<Timeout) do coroutine.yield(AGAIN) end
  end
  return rv
end
function do_confirm(state,timeout) return do_button("ConfirmationDialogBox", "CD_"..state.."_Button", timeout, 1) end

triggers[1][" tell.*'accept'"] = function()	table.insert(frame_handlers, {coroutine.create(function()
  local Stop=Now+5
  while Stop > Now and (type(Window.ConfirmationDialogBox) ~= "userdata" or (Window.ConfirmationDialogBox.Open ~= 1)) do coroutine.yield(AGAIN) end
  if type(Window.ConfirmationDialogBox) == "userdata" and Window.ConfirmationDialogBox.Open ~= 1 then return STOP end
  HasConfirmedDeath = do_confirm("Yes")
  Stop = Now+0.5
  while Stop > Now do coroutine.yield(AGAIN) end
  if type(Window.RespawnWnd) == "userdata" and Window.RespawnWnd.Open == 1 then
    eq("/notify RespawnWnd RW_OptionsList listselect 2")
    Stop = Now+0.5
    while Stop > Now do coroutine.yield(AGAIN) end
    eq("/notify RespawnWnd RW_SelectButton LeftMouseUp")
  end
  return STOP
end)})
end



triggers[1][" tell.*'Shrink'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  if Me.Class.Name == "Shaman" then
    cast_spell({9503})
    return STOP
  elseif Me.Class.Name == "Beastlord" then
    cast_spell({7025})
    return STOP	
  end
  if type(FindItem["Ring of the Ancients"].ID) == "number" then
    local id = Me.ID
    set_target(id)
    eq("/cast item \"Ring of the Ancients\"")
    if type(Me.Pet.ID) == "number" then
      local Stop=Now+1
      while Stop > Now do coroutine.yield(AGAIN) end
      set_target(Me.Pet.ID)
      eq("/cast item \"Ring of the Ancients\"")
    end
  end
  return STOP
end)})
end

triggers[1]["^(.-) tell.*'ge'"] = function(l,m) if not group[m] then
  mq(m) 
  eq("/lua -init")
  local cm = ClassFilenames[Me.Class.Name]
  if cm then eq("/lua "..cm..Me.Level.."oog") end
  eq("/lua GroupExt='"..m.."'")
end end

triggers[1]["^(.-) tell.*'stein'"] = function(l,m)
  eq("/target galdorin")
  eq("/say My stinky stein has rough dirty lips,")
  eq("/say but she loves a deep carouse.")
  eq("/say Beer or ale are her great trips.")
  eq("/say No matter how many vows")
  eq("/say I make or break, my drinking glass")
  eq("/say reminds me of my lovely Brasse.")
end

triggers[1]["^(.-) tell.*'kno'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  cast_spell({"Drunkard's Stein"})
  return STOP
end)})
end

triggers[1][" tell.*'Absorb'"] = function() table.insert(frame_handlers, {coroutine.create(function()
  if Absorb then cast_spell(Absorb) end
  return STOP
end)})
end

triggers[1][" tell.*'cleanup'"] = function() cleanup = 1 end

CombatPipeline = {}
table.insert(frame_handlers, {coroutine.create(maintain_combat), maintain_combat})

function pso(N,t)
  if not t then t = "Collectible" end
  local cX, cY = N,1
  table.insert(frame_handlers, {coroutine.create(function()
    while 1 do
      mq("processing "..cX..","..cY..": "..t)
      while Me.Inventory[22+cX].Item[cY] and ((Me.Inventory[22+cX].Item[cY].Type == t) or (Me.Inventory[22+cX].Item[cY][t] == 1)) and (Me.Inventory[22+cX].Item[cY].NoTrade==0) do
        eq("/itemnotify in pack"..cX.." "..cY.." RightMouseUp")
        mq("Collecting "..cX..","..cY)
        local Timeout = Now + 1
        while (Timeout > Now) do coroutine.yield(AGAIN) end
        cY = cY + 1
        if cY > Me.Inventory[22+cX].Container then cX = cX + 1 cY = 1 end
        if not Me.Inventory[22+cX] then return STOP end
      end
      local nX, nY = cX, cY
      while (not Me.Inventory[22+nX].Item[nY]) or not ((Me.Inventory[22+nX].Item[nY].Type == t) or (Me.Inventory[22+nX].Item[nY][t] == 1)) or (Me.Inventory[22+nX].Item[nY].NoTrade==1) do
        nY = nY + 1
        if nY > Me.Inventory[22+nX].Container then 
          mq("Swapping container at "..nX..","..nY)
          nX = nX + 1 nY = 1
        end
        if (not Me.Inventory[22+nX]) or (Me.Inventory[22+nX].Container == 0) then 
          mq("End at "..nX..","..nY)
          return STOP
        end
      end

      mq("Collecting "..nX..","..nY)
      eq("/itemnotify in pack"..nX.." "..nY.." RightMouseUp")
      local Timeout = Now + 2
      while (Timeout > Now) do coroutine.yield(AGAIN) end

      if Me.Inventory[22+cX].Item[cY] then				
        eq("/itemnotify in pack"..cX.." "..cY.." LeftMouseUp")
        local Timeout = Now + 2
        while not Cursor.ID and (Timeout > Now) do 
          local notices
          if Window["QuantityWnd"].Open == 1 and not notices then
            eq("/nomodkey /notify QuantityWnd QTYW_Accept_Button LeftMouseUp")
            notices = 1
          end
          coroutine.yield(AGAIN)
        end
      end

      local cid = Cursor.ID
      eq("/itemnotify in pack"..nX.." "..nY.." LeftMouseUp")
      local Timeout = Now + 2
      while (Cursor.ID == cid) and (Timeout > Now) do 
        local notices
        if Window["QuantityWnd"].Open == 1 and not notices then
          mq("Accepting quantity")
          eq("/nomodkey /notify QuantityWnd QTYW_Accept_Button LeftMouseUp")
          notices = 1
        end
        coroutine.yield(AGAIN)
      end

      if Cursor.ID then 
        eq("/itemnotify in pack"..cX.." "..cY.." LeftMouseUp")
        local Timeout = Now + 2
        cid = Cursor.ID
        while (Cursor.ID == cid) and (Timeout > Now) do 
          coroutine.yield(AGAIN)
        end
      end

      if Me.Inventory[22+cX].Item[cY] then
        cY = cY + 1
        if cY > Me.Inventory[22+cX].Container then cX = cX + 1 cY = 1 end
        if not Me.Inventory[22+cX] then return STOP end
      end
    end
  end)})
end

function pinv(filter)
  table.insert(frame_handlers, {coroutine.create(function()
    local trig = "%[.-%] (.-) %(.-%) <(.-)>" 
    local gn = Me.Guild
    triggers[1][trig] = function (l,m)
      local n,m = l:match("%[.-%] (.-) %(.-%) <(.-)>" )
      local ignore
      if m == gn then
        if filter and l:match(filter) then ignore = 1 end
      elseif type(gn) ~= "string" then
      	n = l:match("%[.-%] (.-) %(.-%)")
      	if n and type(Spawn[n].Distance) == "number" and Spawn[n].Distance < 25 then
        	if filter and l:match(filter) then ignore = 1 end
      	else
					ignore = 1
      	end
      else
        ignore = 1
      end
      if not ignore then eq("/invite "..n) end
      if ignore then mq("player "..(n or "nil").." in guild "..(m or "nil")) end
    end

    if type(gn) == "string" then eq("/ guild "..gn)
	  else eq("/ ") end
	  local Timeout = Now + 6
	  while (Timeout > Now) do coroutine.yield(AGAIN) end
	  triggers[1][trig] = nil
    return STOP
  end)})
end
triggers[1]["To join the group,"] = function () eq("/invite") end

function make_campsite() 
  table.insert(frame_handlers, {coroutine.create(function()
          if not Window.FellowshipWnd or Window.FellowshipWnd.Open ~= 1 then eq("/keypress ctrl+shift+f") end
          while not Window.FellowshipWnd or Window.FellowshipWnd.Open ~= 1 do coroutine.yield(AGAIN) end
          MakingCampsite = 1
          eq("/notify FellowshipWnd FP_Subwindows tabselect 2")
          local Timeout
          FoundTarget = Now+5
          Timeout = Now+0.5 while (Timeout > Now) do coroutine.yield(AGAIN) end
          eq("/notify FellowshipWnd FP_RefreshList LeftMouseUp")
          Timeout = Now+0.5 while (Timeout > Now) do coroutine.yield(AGAIN) end
          eq("/notify FellowshipWnd FP_DestroyCampsite LeftMouseUp")
          do_confirm("Yes")
          for i=1,4 do -- as of early 2020, camsite is not always created; try a few times
            Timeout = Now+0.5 while (Timeout > Now) do coroutine.yield(AGAIN) end
            eq("/notify FellowshipWnd FP_CampsiteKitList listselect 1")
            Timeout = Now+0.5 while (Timeout > Now) do coroutine.yield(AGAIN) end
            eq("/notify FellowshipWnd FP_CreateCampsite LeftMouseUp")
          end
          MakingCampsite = nil
          return STOP
  end)})
end

function delay(s)
      local nxt = Now + (s or 0)
      while Now <= nxt do coroutine.yield(AGAIN) end
end

function tbm_buy(l,m) table.insert(frame_handlers, {coroutine.create(function()
  l = l or math.floor(Me.Level / 5) * 5
  local s = TBM_LEVELS[l]
  if #s > 1 then buy_array({[s[2]]=22}) end
  local buy_tab={}
  for n,v in ipairs(TBM_setups[Me.Class.Name]) do
    if m and n > m then break end
    local n = s[1].." "..v[1]
    buy_tab[n] = (buy_tab[n] or 0) + #v - 2
  end
  buy_array(buy_tab)
end)})end

function tbm_notify(itm)
      local found
      for p = 1,10 do
        if Me.Inventory[22+p] then
          for n = 1,50 do
            local ii = Me.Inventory[22+p].Item[n]
            if ii and ii.Name == itm then
              found = 1
              eq("/itemnotify in pack"..p.." "..n.." leftmouseup")
              delay(0.5)
            end
            if found then break end
          end
          if found then break end
        end
      end
  return found
end

function tbm_sell(l) table.insert(frame_handlers, {coroutine.create(function()
  l = (l and (l-5)) or math.floor(Me.Level / 5) * 5 - 5
  local s = TBM_LEVELS[l]
  for n,v in ipairs(TBM_setups[Me.Class.Name]) do
    local itm = s[1].." "..v[1]
    local cnt = #v - 2
    for i = 1,cnt do
      tbm_notify(itm) delay(1.5)
      eq("/notify MerchantWnd MW_Sell_Button leftmouseup") delay(0.25)
      if Window.ConfirmationDialogBox and (Window.ConfirmationDialogBox.Open == 1) then do_confirm("Yes") delay(0.25) end
    end
  end
end)})end

function tbm_equip(l,m) table.insert(frame_handlers, {coroutine.create(function()
  l = l or math.floor(Me.Level / 5) * 5
  local s = TBM_LEVELS[l]
  for n,v in ipairs(TBM_setups[Me.Class.Name]) do
    local itm = s[1].." "..v[1]
    if m and n>m then break end
    for i = 3,#v do
      eq('/itemnotify '..v[i].." rightmouseheld") delay(0.5)
      local found = tbm_notify(itm)
      if found then
        eq('/notify ItemDisplayWindow IDW_Socket_Slot_'..v[2]..'_Item leftmouseup') delay(0.5)
        if Window.ConfirmationDialogBox and (Window.ConfirmationDialogBox.Open == 1) then
          do_confirm("Yes") delay(0.5)
          eq("/autoinv") delay(0.5)
        end
      else
        mq("Did not find "..itm)
      end
      local d = Window.ItemDisplayWindow.DoClose delay(0.5)
    end
  end
end)})end

function bs()
  gather_spells(buy_array, spell_train_handler)
end

function buy_array(items, handler)
table.insert(frame_handlers, {coroutine.create(function()
	if handler then handler() end
	local clist = Window["MerchantWnd"][".Child"]
	local array = clist.MW_ItemList[".List"]
	function delay(s)
		local n = Now+s
		while n >= Now do coroutine.yield(AGAIN) end
	end
	for n,v in pairs(items) do
		local cnt = 1
		if type(v) == "number" then cnt=v end
		if type(n) == "string" then v=n end
		if type(v) == "table" then v=v[1] end
		if v then
			local i = array[v..",2"]
			if type(i) == "number" and i>0 then
				if not items[n] or (type(items[n] ~= "table" or not items[n].skip_buy)) then while 1 do
					local pc = FindItemCount[v]
					if not pc or pc >= cnt then mq("done "..v) break end
					local cc = cnt - pc
					mq(i.." = "..v..", count "..cc)
					eq("/notify MerchantWnd MW_ItemList listselect "..i)
					while not string.find(clist.MW_SelectedItemLabel.Text, v) do delay(0) end 
					eq("/notify MerchantWnd MW_Buy_Button leftmouseup")
					while FindItemCount[v] == pc do 
						if Window.QuantityWnd and (Window.QuantityWnd.Open == 1) then
							eq("/notify QuantityWnd QTYW_slider newvalue "..cc) delay(0.25)
							eq("/notify QuantityWnd QTYW_Accept_Button leftmouseup") delay(0.25)
						end
						if Window.ConfirmationDialogBox and (Window.ConfirmationDialogBox.Open == 1) then do_confirm("Yes") end
						coroutine.yield(AGAIN) 
					end
					coroutine.yield(AGAIN) 
				end end
				if handler then handler(v,items[n]) end
			end
		end
	end
	mq("DONE!")
end)})
end

function spell_train_handler(item,skip_slot)
	if not item then CURRENT_SLOT = 1 return end
	local sp = FindItem[item]
	if sp and sp.Spell and type(sp.Spell.Level) == "number" and sp.Spell.Level <= Me.Level then
		delay(0.1)
		eq('/itemnotify "'..sp.Name..'" rightmouseup')
		local timestep = Now+3
		while FindItemCount[sp.Name] == 1 do 
			if timestep < Now then
				timestep = Now+3
				mq("Still memorizing "..sp.Name.."...",1)
				eq("/autoinv")
				delay(0.1)
				eq('/itemnotify "'..sp.Name..'" rightmouseup')
			end
			coroutine.yield(AGAIN) 
		end
	end
	delay(0.5)
	if Me.Book[item.." Rk. II"] then item = item.." Rk. II" end
	if Me.Book[item.." Rk. III"] then item = item.." Rk. III" end
	if not skip_slot and Me.Book[item] then
		local Wait = Now + 15
		eq('/memspell '..CURRENT_SLOT..' "'..item..'"')
		while 1 do coroutine.yield(AGAIN) if (Me.Gem[item] == CURRENT_SLOT) or (Now > Wait) then break end end
		if Me.Gem[item] == CURRENT_SLOT then
		    CURRENT_SLOT = CURRENT_SLOT+1
		    if CURRENT_SLOT == MEMGEM then CURRENT_SLOT = CURRENT_SLOT + 1 end
		end
	end
end

AlreadyInserted = {}
function found_spell(t, n, v)
	if type(n) == "table" then
		for _,a in ipairs(n) do
			if type(a) == "table" then a = a[1] end
			found_spell(t,a,v)
		end
		return
	end
	if AlreadyInserted[n] then return end
	AlreadyInserted[n] = 1
	if not Me.Book[n] and not Me.Book[n.. " Rk. II"] and not Me.Book[n.. " Rk. III"] then
		if v then t[n] = v else table.insert(t,n) end
	else
		if v then v.skip_buy=1 else v = {skip_buy=1} end
		t[n] = v
	end
end

function gather_spells(executor, handler)
table.insert(frame_handlers, {coroutine.create(function()
  DelayCast = 1
  local snames = {}
  for i,v in ipairs(CombatPipeline) do
    for _,s in ipairs(v) do
      for ni,n in ipairs(s) do
        if type(n) == "string" or type(n) == "table" then 
          if not s.use_gem and (s.buff or s.blocks or (s.singles == 0) or s.skip_gem) then
            found_spell(snames, n, {skip_gem=1})
          else
		    local st
		    -- also skip gem if spell-turned and offturn
			if s.turned and #s > 1 then 
			  validate_spell(s)
			  if s.turn_me and ((s.turn_me <= #s and s.turn_me) or 1) ~= ni then st = {skip_gem=1} end
			end
            found_spell(snames,n,st)
          end
        end
      end
      if s.crit then for _,n in ipairs(s.crit) do found_spell(snames,n) end end
      if s.group then for _,n in ipairs(s.group) do found_spell(snames,n) end end
    end
  end
  if PurchaseSpells then for i,v in ipairs(PurchaseSpells) do found_spell(snames, v, {skip_gem=1}) end end
  if executor then executor(snames, handler) else
    local gem = 1
    for i,v in pairs(snames) do
      if (type(v) == "table") and v.skip_buy and not v.skip_gem then v = i end
      if type(v) == "string" and (Me.Book[v] or Me.Book[v.. " Rk. II"] or Me.Book[v.. " Rk. III"]) then
        if Me.Book[v.." Rk. II"] then v = v.." Rk. II" end
        if Me.Book[v.." Rk. III"] then v = v.." Rk. III" end
        mq("GEM"..gem..": "..v)
        local Wait = Now + 15
        eq('/memspell '..gem..' "'..v..'"')
        while 1 do coroutine.yield(AGAIN) if (Me.Gem[v] == gem) or (Now > Wait) then break end end
        if Me.Gem[v] == gem then
          gem = gem+1
          if gem == MEMGEM then gem = gem + 1 end
        end
      else
 	if type(v) == "string" then i = v end
	mq("Others "..i)
      end
    end
  end
  DelayCast = nil
  AlreadyInserted = {}
  mq("DONE!")
end)})
end

function sb(ss) if ss then table.insert(frame_handlers, {coroutine.create(function()
  local w = Window["BarterSearchWnd"]
  if not w or w.Open ~= 1 then eq("/barter") return end
  local child = w[".Child"]
  local array = child.BTRSRCH_InventoryList[".List"]
  local item = array[ss..",2"]
  if type(item) == "number" and item>0 then
	eq("/notify BarterSearchWnd BTRSRCH_InventoryList listselect "..item)
  end
  delay(0.5)
  eq("/notify BarterSearchWnd BTRSRCH_SearchButton leftmouseup")
  delay(3)
  array = child.BTRSRCH_BuyLineList[".List"]
  
  local selected,sprice,maxn
  for i=1,50 do
    local n = array[i..",2"]
    if n == ss then
	  local cnt = array[i..",3"]
	  local price = array[i..",4"]
	  local button = array[i..",6"]
	  local pl = string.len(price)
	  while (pl ~= 0) and (string.sub(price, pl, pl) ~= "p") do
        price = string.sub(price,1,-2)
		pl = string.len(price)
      end
	  if string.sub(price, pl, pl) == "p" then
	    price = tonumber(string.sub(price,1,-2))
		cnt = tonumber(cnt)
		if (not sprice) or (sprice < price) or (sprice == price and cnt > maxn) then
		  selected = i
		  maxn = cnt
		  sprice = price
		end
	  end
	  -- mq(array[i..",2"])
	end
  end
  if sprice then
	triggers[1]["You've sold (.-) "] = function(l,m) SoldSuccess=tonumber(m) end
	local itc = FindItemCount[ss]
	while itc > 0 and maxn > 0 do
      mq("Selected index "..selected..", price = "..sprice.." max= "..maxn)
	  eq("/notify BarterSearchWnd BTRSRCH_BuyLineList listselect "..selected)
	  delay(0.5)
	  while type(Me.Casting.ID) == "number" do coroutine.yield(AGAIN) end
	  eq("/notify BarterSearchWnd SellButton leftmouseup")
	  local timeout = Now + 2
	  while (not Window.QuantityWnd or (Window.QuantityWnd.Open ~= 1)) and timeout>Now do
	    coroutine.yield(AGAIN)
	  end
	  if Window.QuantityWnd and (Window.QuantityWnd.Open == 1) then 
	    local m = 20
		if m > itc then m = itc end
		if m > maxn then m = maxn end
	    eq("/notify QuantityWnd QTYW_slider newvalue "..m)
	    delay(1)
	    eq("/notify QuantityWnd QTYW_Accept_Button leftmouseup")
	    SoldSuccess = nil
	    local timeout = Now + 15
	    while not SoldSuccess and timeout>Now do coroutine.yield(AGAIN) end
		if not SoldSuccess then break end
	    mq("Sold "..SoldSuccess.." units!")
		itc = itc - SoldSuccess
		maxn = maxn - SoldSuccess
  	  else break end
	end
	triggers[1]["You've sold (.-) "] = nil
  end
  mq("DONE!")
end)}) end end

-- 



-- Name, aug slot, item_slot(s)
TBM_KNIGHT = {
  {"Sulstone of the Knight",3,13},
  {"Sulstone of the Defender",4,14},
  -- {"Sulstone of the Gallant",4,13}, -- the whip will not take this...
  {"Sulstone of the Vengeful",4,20},
  {"Sulstone of the Sturdy",4,0,1,3,4,5,6,8,11,15,16},
  {"Sulstone of the Sturdy",3,2,7,9,10,12,17,18,19}
}

TBM_CASTER = {
  {"Sulstone of the Focused",3,13},
  {"Sulstone of the Defender",4,14},
  {"Sulstone of the Gallant",4,13},
  {"Sulstone of the Vengeful",4,20},
  {"Sulstone of the Valiant",4,0,1,3,4,5,6,8,11,15,16},
  {"Sulstone of the Valiant",3,2,7,9,10,12,17,18,19}
}

TBM_HEALER = {
  {"Sulstone of the Compassionate",3,13},
  {"Sulstone of the Defender",4,14},
  {"Sulstone of the Gallant",4,13},
  {"Sulstone of the Vengeful",4,20},
  {"Sulstone of the Valiant",4,0,1,3,4,5,6,8,11,15,16},
  {"Sulstone of the Valiant",3,2,7,9,10,12,17,18,19}
}

TBM_BARD = {
  {"Sulstone of the Elements",3,13,14},
  {"Sulstone of the Gallant",4,13,14},
  {"Sulstone of the Vengeful",4,20},
  {"Sulstone of the Valiant",4,0,1,3,4,5,6,8,11,15,16},
  {"Sulstone of the Valiant",3,2,7,9,10,12,17,18,19}
}

TBM_BEAST = {
  {"Sulstone of the Fist",3,13,14},
  {"Sulstone of the Gallant",4,13,14},
  {"Sulstone of the Vengeful",4,20},
  {"Sulstone of the Valiant",4,0,1,3,4,5,6,8,11,15,16},
  {"Sulstone of the Valiant",3,2,7,9,10,12,17,18,19}
}

TBM_LEVELS = {
  [75] = {"Elegant"},
  [80] = {"Stalwart", "Class X Augmentation Distiller"},
  [85] = {"Extravagant", "Class XI Augmentation Distiller"},
  [90] = {"Glorious", "Class XII Augmentation Distiller"},
  [95] = {"Regal", "Class XIII Augmentation Distiller"},
  [100] = {"August", "Class XIV Augmentation Distiller"},
}

TBM_setups = {
 ["Shadow Knight"] = TBM_KNIGHT,
 Druid = TBM_HEALER,
 Shaman = TBM_HEALER,
 Necromancer = TBM_CASTER,
 Magician = TBM_CASTER,
 Enchanter = TBM_CASTER,
 Beastlord = TBM_BEAST,
 Bard = TBM_BARD
}

TBM = {
  "Abettor's Earring",
  "Abjurer's Earring",
  "Acolyte's Ring",
  "Footman's Ring",
  "Ball of Everliving Golem",
  "Bright Sapphire Cloak",
  "Courtier's Mask",
  "Footman's Belt",
  "Footman's Trinket",
  "Privateering Pauldrons",
  "Woven Flesh Necklace",

  "Armsman's Wand",
  "Armsman's Rod",
  "Armsman's Buckler",
  

  "Preserving Protector's Boots",
  "Preserving Protector's Greaves",
  "Preserving Protector's Helm",
  "Preserving Protector's Breastplate",
  "Preserving Protector's Vambraces",
  "Preserving Protector's Gauntlets",
  "Preserving Protector's Bracer",
  "Deathscent Bracer",

  "Deathscent Cloth Wristguard",
  "Preserving Sponsor's Wristguard",
  "Preserving Sponsor's Gloves",
  "Preserving Sponsor's Cap",
  "Preserving Sponsor's Pants",
  "Preserving Sponsor's Sleeves",
  "Preserving Sponsor's Robe",
  "Preserving Sponsor's Boots",

  "Deathscent Chain Wristguard",
  "Preserving Dredger's Coat",
  "Preserving Dredger's Boots",
  "Preserving Dredger's Coif",
  "Preserving Dredger's Gauntlets",
  "Preserving Dredger's Leggings",
  "Preserving Dredger's Sleeves",
  "Preserving Dredger's Wristguard",

  "Deathscent Leather Wristguard",
  "Preserving Sifter's Armwraps",
  "Preserving Sifter's Boots",
  "Preserving Sifter's Gloves",
  "Preserving Sifter's Cowl",
  "Preserving Sifter's Leggings",
  "Preserving Sifter's Tunic",
  "Preserving Sifter's Wristguard",

}

function bb()
  buy_array(TBM)
end
