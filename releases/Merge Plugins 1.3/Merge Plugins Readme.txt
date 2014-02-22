//====================================================================
|  Merge Plugins TES5Edit Script
|  Created by matortheeternal 
|  http://skyrim.nexusmods.com/mods/37981  
|
\\====================================================================



//=======Installation=====================================================
Copy the contents of the included Edit Scripts folder into TES5Edit's Edit Scripts folder.
(TES5Edit's Edit Scripts folder should be in the same directory as TES5Edit.exe.)

If you don't have TES5Edit you can get it here: http://skyrim.nexusmods.com/mods/25859
NOTE: These scripts will only work with the most up-to-date version of TES5Edit!



//=======Tutorial========================================================
1. Open TES5Edit and load the mods you want to merge.
2. Right click on each mod you want to merge and click "Check for Errors"
    You should fix any errors this finds before merging.
    See the Troubleshooting section if you see "Invalid or out of order subrecord" errors.
    Some files can be merged even if they have certain errors, but it's better fix them if you can.
3. Hold control and click on each of the mods you want to merge so they're highlighted.
4. Right on one of the mods and click "Apply Script".
5. Choose Merge Plugins v1.3 from the dropdown menu and click OK.
6. When asked if you want to renumber FormIDs click yes.
7. When prompted to type the name of an existing file leave the window blank and click OK.
8. Enter the name you wish to use for your merged ESP file and click OK.
9. After the script is finished, close TES5Edit and save only the merged file you made.



//=======FAQ==========================================================
You can find a lengthier FAQ section on the mod's page on Nexus Mods, so check that out if your
question isn't answered here.  And there's always the comments section as well!  :)

Q: Right clicking isn't doing anything!
A: Wait for the background loader to finish loading the mods.

Q: I have Invalid or out of order subrecord errors, what do?
A: These are symptomatic of corrupted data.  You can try to repair the data by opening the ESPs
in question in the creation kit as the active file and saving them.  If they persist, however, you should
not merge the files.  These errors will cause CTDs whenever Skyrim starts if TES5Edit attempts to 
copy them.

Q: I'm getting CTDs why?
A: If a merge is giving you CTDs it usually means that one or more of the files you merged doesn't like
being merged.  That is to say, TES5Edit wasn't able to copy some of the data from it into the merged 
file.  You can try to isolate the problematic file and not merge it by merging mods one at a time, which
I highly recommend for everyone who doesn't have experience merging plugin files.

Q: What mods don't merge well?
A: Armor mods merge great.  So do weapon mods.  So do most small "fix" type mods.  Player home 
mods don't merge well.  Mods which add an MCM menu don't merge well.  Mods which have invalid or
out of order subrecord errors will cause CTDs.  Other mods are still being tested.

Q: Stuff is invisible/turned pink after merging.  What do?
A: Extract the BSAs associated with the mods you merged.

Q: I've never merged stuff before and I don't want to screw up my game.  Help me?
A: Start by trying small merges to get a feel for the process.  Merge two basic mods (like armor mods)
together and see if it works.  If it does you can continue merging mods so as to get a larger and larger
merge file.  Keep regular backups so you can go back if a merge doesn't merge right.

Q: Will this make my game explode?
A: Only if you don't use it right.  The more material you read the better suited you will be to using this
tool properly and correctly.  You should also read the FAQ section on the mod page and consider reading
the comments section as well.  Oh and, the merging user reports thread will save your life.  :)

Q: What are the advantages of merging?
A: It can help you game stay stable.  It allows you to bypass the 250 mod limit.  And it's the panacea for
disorganized modder syndrome (DMS).