Scriptname tox_fmo_pRef_inventory extends ReferenceAlias  
{Advances smithing xp based on value of added items}
import debug
import math

bool property crafting auto
{ true = crafting at a forge.
 false = tampering at a grinding stone or at a workbench. 
 toggled via script, no need to initialize in the CK }

;
;  http://www.uesp.net/wiki/Skyrim:Leveling#Skill_XP
;
; 	Skill Use Mult                 1
;   Skill Use Offset               0
;   Skill Improve Mult             0.25
;   Skill Improve Offset           300
;   Total base XP from 15 -> 100   91,601
; 
; The skill XP needed for the next skill level is almost proportional
; to the square of the skill level:
;     Skill Improve Mult * skill level^1.95 + Skill Improve Offset
;
; Using a skill gives XP according to:
;     Skill Use Mult * base XP + Skill Use Offset (?)
;
; Sources of XP
; 25 + (3 * item value^0.65) base XP for constructing an item.
; 25 + (8 * item value delta^0.6) base XP for improving an item.

int _AMMO = 42 ; arrows
int _ARMO = 26 ; armor and clothing
int _LIGT = 31 ; light (torch, lantern)
int _MISC = 32 ;
int _SCRL = 23 ; scroll
int _SGEM = 52 ; soul gems
int _WEAP = 41 ; weapon
string smithing = "smithing"

; AVSmithing must be low enough to give "no xp"
; 160/100000 = .0016
; 
; 160/100000 * 1000 = 1.6 (xpBackMult in v0.1)
; 
; This is close to the Update value (which is 1), and the reason why it seems to work ok.
; However, the smithing skill goes up a bit too fast with the fix, so the multiplier must be 
; something like:
;  
; 160/100000 * 630 = 1.008

int xpBackMult = 625

Event OnItemAdded(Form base, int count, ObjectReference ref, ObjectReference from)
	float crafted 
	int ft = base.getType()

	if	ft == _SCRL || ft == _LIGT || ft == _MISC || ft == _SGEM ||\
		ft == _WEAP || ft == _AMMO || ft == _ARMO
		
	crafted = itemConstructedXp( base.getGoldValue())

	if GetActorReference() && crafting && crafted >= 1 
		game.AdvanceSkill(smithing, crafted)
								; I assumed that the item was removed after improving (tampering). It is not.
								;elseif actorRef && !crafting && improved >= 1
								;	game.AdvanceSkill(smithing, improved)
	endif

	endif
EndEvent


float Function itemConstructedXp(int value)
	int flat = 25
	if value == 0 
		flat = 0
	endif
	return (flat + 3 * pow(value, 0.65)) * xpBackMult
endFunction

;Function Say(String What) 
;	debug.trace("FMO " + What)
;endFunction

; Using the sharpening wheel and tampering table does not remove items from the inventory.
; For this reason, instead of manually administering a dose of XP, the PC is given a perk 
; that increases their smithing XP gain by virtue of EPModSkillUsage_IsAdvanceSkill (think 
; of it as a temporary  warrior doom stone that improves only smithing and only when using
; tampering tables and grinding stones). The perk is added and removed in the second ref-
; alias of the _fmo_quest
;
;int previousValue
;Event OnItemRemoved(Form base, int count, ObjectReference ref, ObjectReference to)
;	int ft = base.getType()
;	if ( ft == _ARMO || ft == _WEAP ) && to == none
;		previousValue = base.getGoldValue()
;		;say("Removed " + base + " ( "+ base.getGoldValue() +")")
;	endif
;EndEvent
