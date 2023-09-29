# Siege of Avalon : Open Source and re-released #

_"Siege of Avalon : Open Source is an attempt to keep this great isometric RPG game alive by continuing development on it."_

![Siege of Avalon in HD with room for party of 4.](SoAOS_HD.png)Siege of Avalon in HD with room for party of 4.

## Repository now updated to the latest state - the unofficial SoAmigos 1.11 patch ##

I have pulled the changes we made for the official re-release (+150 commits by mostly me and the guys at General Arcade), but also a collective commit that covers +30 commits made by Seppi, that now brings the unofficial patch version to 1.11.

That includes improvements like:
- Live overlay mapping
- Numerous bugfixes
- Tutorial\*
- Key remapping
- Support for all known mods\*
    - Days of Ahoul
    - Pillars of Avalon (not the official promised)
    - Ashes of Avalon
    - Caves
    - Rise of Dwarf
    - The Seven Kingdoms
    - Chapter/Kapitel 7
- Updates check
- DDraw.dll selection\*
- Does compile in latest Delphi 11.3 Alexandria

<sup>*</sup>These do require that apart from of cause you have purchased the game on GOG or Steam (since the engine is open-sourced, not the resources), you will also need the latest modpack from SoAmigos forum.

## We made it happen!! ##

Siege of Avalon is now back and available on GOG.com and Steam - thanks to Game Publisher Sneg, Game Developing Studio General Arcade, this humble fork of the code and me.

And the released versions engine source code can be found here: https://github.com/SNEG-DEV/siege-of-avalon

And just to quote their readme:

“Siege of Avalon: Anthology” Remastered : Open Source is our huge “thank you” to the community that was keeping this great isometric RPG alive from 2003 till 2021 by adding constant updates and modifications to it. Our goal is to ensure that fans can develop the game further the way they envision it and use all the work and love we’ve put into it already.

Release Date: 8 April 2021

Game available on Steam: https://store.steampowered.com/app/1558990/Siege_of_Avalon_Anthology/

Game available on GOG.com: https://www.gog.com/game/siege_of_avalon_anthology

## Changing focus and what is happening here ##

My original focus was to make a DTMain1.exe plugin replaceable Siege.exe - but since the game now is back for the wider audience in a fixable form, I will pull back the changes we did on the re-release of the project - and then add more experimental things and just continue with cleaning up the code.

Maybe some of the stuff can flow back into the Steam/GOG release - if is makes sense and is wanted.

I hope also to get more time for tooling.

## How to complie ##
Install Delphi 10.3.3 or newer - the free Community Edition found at https://www.embarcadero.com/products/delphi/starter will do fine.  
Open the Siege.dpr project file and compile.

## Focus and origin ##

This is a fork from the [**gondur/Siege-of-Avalon-Open-Source**](https://github.com/gondur/Siege-of-Avalon-Open-Source) repository, which again is a fork of the original released repository moved over from Sourceforge where it was released in 2003.

The main focus of this fork: 

1. Moving the Win32 Delphi 4 source code to Delphi 10.3 or later - including its free Community Editons. **DONE** (and keep it updated to the latest Delphi release)
2. Fixing various glitches. **DONE** (until proved wrong)
3. Heavy refactoring, to make the code a joy to work with - and benefit a modern Object Pascal dialect. **ONGOING**
4. Include HD/FullHD support like already done by gondur. **DONE** (someone should improve the interface graphics I did)
5. Possibly replace DX with SDL2/other to gain crossplatform support, but since the DirectX headers are maintained for latest Delphi versions - this has lower priority.
6. Added improvements and fixes found by the community. **ONGOING**
7. Support Ashes of Avalon (AoA) and other Mods out of the box.

Also check out the gondur fork for changes, and the thread on SOAmigos (in german), where Raptor/Rucksacksepp (http://soamigos.de/wbb5/forum/index.php?thread/4458-hd-und-fullhd-version-zu-siege-of-avalon-aus-dem-source-code-mit-delphi-4/&postID=91286#post91286) originally fixed building with Delphi 4 amoung other things.

Look in the CHANGELOG for status, changes and planned changes.

Tooling and documentation might be found here but would probably be tied to their seperate tooling repositories:

[**POX Studio**](https://github.com/SteveNew/POXStudio) - POX file editor and fileformat documentation.
