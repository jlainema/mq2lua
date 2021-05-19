DoMelee=1
HealQueue = { limit=85, dynamic=0.25 }
if Group.Leader.ID == Me.ID then
	DoMelee=nil
	pinv() -- invite everyone, too
end

CombatPipeline = {
  {
		-- SK initial spells
		{"Leering Corpse", summon=1, ooc=-1}, --
		{"Disease Cloud", offtargets={}}, -- conjuration
		{"Clinging Darkness", retarget=100}, -- alteration
		{"Ward Undead", retarget=100}, -- evocation
		{"Sense the Dead", retarget=100}, -- divination
		{"Endure Cold", retarget=100}, -- abjuration
		
		-- SHA initial spells
		{"Minor Healing", targetqueue=HealQueue, ooc=900}, -- alteration
		{"Flash of Light", retargets=100}, -- divination
		{"Inner Fire", buff={}, ooc=-1}, -- abjuration
		{"Sicken", offtargets={}}, -- conjuration
		{"Frost Rift", retarget=100}, -- evocation
		{"Spirit of Wolf", buff={}, ooc=-1},

		-- BRD initial songs
		{"Selo's Accelerando", song=1, ooc=-1}, -- Percussion
		{"Chords of Dissonance", song=1}, -- Stringed
		{"Hymn of Restoration", song=1, ooc=-1}, -- Stringed
		{"Cassindra's Chant of Clarity", song=1, ooc=-1}, -- Singing
		{"Anthem de Arms", song=1, ooc=-1}, -- Singing
		{"Denon's Disruptive Discord", song=1}, -- Brass
		{"Solon's Song of the Sirens", song=1}, -- Wind, 27
  }
}
