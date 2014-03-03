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
1. Unpack the BSAs associated with the mods you want to merge.
2. Open TES5Edit and load the mods you want to merge.
3. Right click on each mod you want to merge and click "Check for Errors"
   - You should fix any errors this finds before merging.
   - See the FAQ if you see "Invalid or out of order subrecord" errors.
   - Some files can be merged even if they have certain errors, but it's better fix them if you can.
4. Hold control and click on each of the mods you want to merge so they're highlighted.
   - You don't have to do this as of v1.4, but it can allow you to merge faster.
5. Right on one of the mods and click "Apply Script".
6. Choose Merge Plugins v1.6 from the dropdown menu and click OK.
7. Make sure the correct checkboxes for the mods you want to merge are ticked.
   - Leave the merge method and advanced merge settings at their defaults.  You can read up on how
     to use these on the Nexus Mods page.
8. Click OK, then in the next window choose the ESP you want to merge into or -- CREATE NEW FILE --.
   - If you use Two-pass copying you can't merge into an existing ESP.
9. After the script is finished, verify the merge worked correctly.  If a window comes up saying some
   records failed to copy make note of which records failed to copy and consider why these records would
   fail (Check for Errors!).  Also verify the masters of the merged ESP don't include the files you 
   merged.
10. Close TES5Edit and save ONLY the merged file you made.
11. Disable the merged ESPs from your load order, and consider moving them out of your data directory
    into a temporary folder to reduce clutter.
12. Enable the merged ESP and give it a test.  If things don't work you can troubleshoot to the best
    of your ability, and restore your old ESPs if necessary.



//=======FAQ==========================================================
You can find a lengthier FAQ section on the mod's page on Nexus Mods, so check that out if your
question isn't answered here.  And there's always the comments section as well!  :)

Q: SkyRe? SUM? AutomaticVariants?
A: ESPs associated with third party patchers will be filled with corrupted data... so you 
shouldn't touch anything even remotely related to third party patchers in TES5Edit with a 10 
foot pole.  Hell, you just plain shouldn't use third party patchers.  But what do I know...

Q: Right clicking isn't doing anything!
A: Wait for the background loader to finish loading the mods.

Q: I have Invalid or out of order subrecord errors, what do?
A: These are symptomatic of corrupted data.  You can try to repair the data by opening the ESPs
in question in the creation kit as the active file and saving them.  If the errors persist you 
should not merge the files.  These errors will cause CTDs whenever Skyrim starts if TES5Edit 
attempts to copy them.

Q: I'm getting CTDs why?
A: If a merge is giving you CTDs it usually means that one or more of the files you merged doesn't like
being merged.  That is to say, TES5Edit wasn't able to copy some of the data from it into the merged 
file.  You can try to isolate the problematic file and not merge it by merging mods one at a time, which
I highly recommend for everyone who doesn't have experience merging plugin files.

Q: What mods don't merge well?
A: As of v1.6 almost everything merges well.  Mods which have invalid or out of order subrecord errors 
will still cause CTDs.  A few finicky mods will not merge well, but far fewer than in earlier versions
of the script.

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
A: It can help your game stay stable.  It allows you to work beneath the 250 esp limit.  And it's the 
panacea for disorganized modder syndrome (DMS).