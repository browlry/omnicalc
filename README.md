## Omnicalc

I'm a high school math teacher with an irrational attachment to our old TI-83 Plus graphing calculators.

Our students have a hard time with the learning curve on the 83+ (even when just using the calculator for simple arithmetic), but much less difficulty with the TI-84+. This appears to be because the TI-84+ lets you scroll up the home screen using the up arrow, while the TI-83+ only has `[2nd]` `[Entry]` to view previous entries.

I already install Omnicalc on these calculators (I like the thousands separators and parentheses assistant). Since Omnicalc already re-maps some keys (like re-directing `[Entry]` to its Entries menu), I thought it would be a fun project to see if I can modify Omnicalc to re-map the Up arrow to `[Entry]`. This would make the button more like the up arrow in Bash or Powershell. 

## Credits

This repo is mostly the source code created by Michael Vincent of Detached Solutions and posted to https://www.ticalc.org/pub/83plus/flash/programs/omnicalc.zip, with just a few lines of code added by me.

The original Omnicalc home page can be viewed here:
https://detachedsolutions.com/omnicalc/

The original Omnicalc users manual may be viewed here:
https://detachedsolutions.com/omnicalc/manual/

## Building

Most people will be fine with the latest release, but if you want to make some more changes, here's the build process I used.

You will need:
 - ZDS I (Zilog Developer Studio), available [here](https://www.zilog.com/index.php?option=com_zcm&task=view&soft_id=19&Itemid=74).
 - Texas Instruments Wappsign (Windows App Sign), available as part of the [TI 83 Plus Flash Debugger installer](https://isa.umh.es/calc/TI/TI83-84SDK/83psdk_setup.exe). (I tried Wabbitsign, but the I got a NACK error when I tried to transfer the resulting 8xk to TiLem or a real calculator).

 1. Launch ZDS (I put mine in Compatibility mode for Windows XP).
 2. Go to `File` > `Open Project`.
 3. Navigate to `omnicalc.zws` and open it.
 4. Ignore the message about invalid emulator names.
 5. If prompted "Project has been moved from original location. Do you want to move it to the current location?", click "Yes".
 6. Go to `Build` menu > `Build`. Then click `Build` menu > `Rebuild all`. You should see "omnicalc.ld - 0 error(s), 0 warnings(s)" in the box at the bottom of the screen.
   - This creates a file called `omnicalc.hex` (an unsigned flash app) in the same folder as `omnicalc.zws`.
 7. Go to the folder where you installed TI 83 Plus Flash Debugger. Open the `Utils` folder and launch `Wappsign.exe`.
 8. For "Application", select the `omnicalc.hex` file you created in step 6.
 9. For "Key file", select `0104.key`, which is in the same folder as Wappsign. (This is the freeware signing key).
 10. Click "Sign". You should see "omnicalc.8xk successfully generated".
 10. Test out `omnicalc.8xk` in an emulator, then send it to your real TI-83 Plus calculator with the [TI Silverlink cable](https://epsstore.ti.com/OA_HTML/ibeCCtpItmDspRte.jsp?section=10412&item=114932&sitex=10023%3a22372%3aUS).
