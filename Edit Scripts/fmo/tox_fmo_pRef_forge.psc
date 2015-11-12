Scriptname tox_fmo_pRef_forge extends ReferenceAlias  
{Populates and vacates the inventory monitor alias on forge-menu events}
import debug

tox_fmo_pRef_inventory property raInventory auto ; the second refAlias, InventoryMonitor
FormList property _fmo_apparatus_create auto
FormList property _fmo_apparatus_improve auto
Perk property _fmo_doomWarriorPerk auto

Event OnInit()
	RegisterForMenu("Crafting Menu")
endEvent

Event OnPlayerLoadGame()
	RegisterForMenu("Crafting Menu")
EndEvent

Event OnMenuOpen(string menuName)
	raInventory.crafting = isUsingForge()	; true - creating, false - improving
	raInventory.forceRefTo( getActorRef())	; PC
	if ! raInventory.crafting
		getActorRef().addPerk(_fmo_doomWarriorPerk)
	endif
endEvent

Event OnMenuClose(string menuName)
	raInventory.clear()
	if ! raInventory.crafting
		getActorRef().removePerk(_fmo_doomWarriorPerk)
	endif
endEvent
bool Function isUsingForge() 
	ObjectReference anvil
	ObjectReference wheel
	float toAnvil
	float toWheel
	; I can see how an alchemy-on-the spot mod could muck this up 
	; but tox_fmo_pRef_inventory verifies form type as well.
	anvil = findClosest(_fmo_apparatus_create,55)
	wheel = findClosest(_fmo_apparatus_improve,55)
	if !anvil && wheel
		return false
		endif
	if !wheel && anvil
		return true
		endif
	if !wheel && !anvil
		anvil = findClosest(_fmo_apparatus_create,200)
		wheel = findClosest(_fmo_apparatus_improve,200)
		
		if !wheel && !anvil
		trace("FMO Could find neither of {anvil,forge} or {grinding stone,workbench}.")
		trace("FMO Using a DLC apparatus? XP will be calculated as if the item was crated and not improved.")
		int k = _fmo_apparatus_create.getSize()
		while k > 0
			k -= 1
			trace( "FMO _fmo_apparatus_create["+k+"]" + _fmo_apparatus_create.getAt(k) )
			endWhile
		k = _fmo_apparatus_improve.getSize()
		while k > 0
			k -= 1
			trace( "FMO _fmo_apparatus_improve["+k+"]" + _fmo_apparatus_improve.getAt(k))
			endWhile
		return true
		endif
	endif
	toAnvil = getActorRef().GetDistance(anvil)
	toWheel = getActorRef().GetDistance(wheel)
	return toAnvil < toWheel
endFunction

ObjectReference Function findClosest(FormList fl, int radius)
	return game.FindClosestReferenceOfAnyTypeInListFromRef(fl, getActorRef(), radius)
endFunction
