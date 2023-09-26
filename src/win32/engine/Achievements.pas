unit Achievements;

interface

uses
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  Character;

type
  TAchievements = class
  private
    FAchievements: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function getIdByTitle( title: string; character: TCharacter; out ACH_ID: string ): Boolean;
  end;

implementation

uses
  System.SysUtils;

{ TAchievements }

constructor TAchievements.Create;
begin
  FAchievements := TDictionary<string, string>.Create;
  FAchievements.Add('01metavarous', 'ACH_REPORT_TO_AVAROUS');
  FAchievements.Add('01LetterReturned', 'ACH_SEALED_WITH_A_KISS');
  FAchievements.Add('Completed', 'ACH_FETCH_THE_HERBS');
  FAchievements.Add('01Bugs', 'ACH_WHATS_IN_THE_BASEMENT');
  FAchievements.Add('01FoundBrother', 'ACH_BROTHERS_IN_ARMS');
  FAchievements.Add('01ReturnedSteel', 'ACH_COLD_HARD_STEEL');
  FAchievements.Add('01PellHappy', 'ACH_PELLS_SORROW');
  FAchievements.Add('01Boots', 'ACH_CASE_THE_CHEST');
  FAchievements.Add('01happywizard', 'ACH_FETCH_THE_AMULET');
  FAchievements.Add('01recovered', 'ACH_FETCH_THE_CHALICE');
  FAchievements.Add('02estonequestcomp', 'ACH_RETRIEVE_THE_EARTHSTONE');
  //FAchievements.Add('02traitorquestcomp', 'ACH_LETTER_FROM_A_TRAITOR'); //delete
  FAchievements.Add('02pqcompleted', 'ACH_WHO_POISONED_THE_KING');
  FAchievements.Add('02ringdeliv', 'ACH_RETURN_TRACYS_RING');
  FAchievements.Add('02weldonnopass', 'ACH_CHANGING_OF_THE_GUARD');
  FAchievements.Add('02signetsent', 'ACH_TRISTANS_FRIEND');
  FAchievements.Add('02killmycorpse', 'ACH_LYDIAS_FATE');
  FAchievements.Add('02courierquestcomp', 'ACH_COURIER_WHAT_COURIER');
  FAchievements.Add('05savedLurkers', 'ACH_FREE_THE_LURKERS');
  FAchievements.Add('02killedmycorpse', 'ACH_FREE_LYDIA');
  FAchievements.Add('03clearthepass', 'ACH_LETTER_TO_THE_KING');
  FAchievements.Add('03mabondone', 'ACH_PAPER_PUSHING');
  FAchievements.Add('03amuletquestcomp', 'ACH_KNIGHT_QUEST');
  FAchievements.Add('03signetreturned|03crownreturned', 'ACH_SALVAGE_RUN');
  FAchievements.Add('Weapon Mastery|Pain Control|Fortitude', 'ACH_OSKARIS_TRAINING_TASKS');
  FAchievements.Add('03know', 'ACH_GO_BACK_TO_THE_FOREST');
  FAchievements.Add('04sawram|04sawnaga|04sawwarriors', 'ACH_SCOUT_THE_ENEMY_CAMP');
  FAchievements.Add('04staffdone', 'ACH_A_LITTLE_FAVOR_FOR_OLON');
  FAchievements.Add('04satcheldone', 'ACH_STEAL_THE_SATCHEL');
  FAchievements.Add('04EdgardSafe', 'ACH_RESCUE_EDGARD');
  FAchievements.Add('04solocomplete', 'ACH_RANGER_QUEST');
  FAchievements.Add('04learnedcamo|04learnedfocus|04learnedsw', 'ACH_SCOUTMASTERS_TRAINING_TASKS');
  FAchievements.Add('04ramdestroyed', 'ACH_DESTROY_THE_RAM');
  FAchievements.Add('04odead', 'ACH_KILL_OVORON');
  // Extra SNEG
  FAchievements.Add('01EndChapter', 'ACH_ACT1');
  FAchievements.Add('02endofch2', 'ACH_ACT2');
  FAchievements.Add('03battlewon', 'ACH_ACT3');
  FAchievements.Add('04endofch4', 'ACH_ACT4');
  FAchievements.Add('05endofch5', 'ACH_ACT5');
  FAchievements.Add('06MithrasDead', 'ACH_ACT6'); //player doesn't get title 'stop'
  //Version 1.5, new achievments
  FAchievements.Add('02killedosla', 'ACH_OSLA_SERVANT'); //Osla as traitor, 02traitorquestcomp
  FAchievements.Add('02itherdead', 'ACH_ITHER_SERVANT'); //Ither as traitor, 02traitorquestcomp
  FAchievements.Add('04LoveLetter', 'ACH_AHOUL_LOVE'); //Darg and Kallden quest, wagons travelling ->supply camp
  FAchievements.Add('05ApprenticeIcharas', 'ACH_ICHARAS'); //Title 05ApprenticeIcharas need to be added to pehlic.cnv, ->Icharas promotion
  FAchievements.Add('05epromotedpc', 'ACH_WIZARD_QUEST'); //Become a wizard
  FAchievements.Add('05AGFree', 'ACH_ASTRAL_GUARDIAN_FREE'); //Astralguardian free
  FAchievements.Add('6firepotion', 'ACH_FIREGULLET'); //Firegullet for Gentza
  FAchievements.Add('master of mystic arts|energy control|mind control', 'ACH_WIZARD_MASTER'); //Wizard, last training
  FAchievements.Add('master of the shadows|transcendent will|phantom movements', 'ACH_SHADOW_MASTER'); //Ranger, last training
  FAchievements.Add('Combat Mastery', 'ACH_KNIGHT_OF_LIGHT'); //Knight, last training
  FAchievements.Add('06foundphelappr', 'ACH_PHELICS_NEW_APPRENTICE'); //Send Arion to Phelic
  FAchievements.Add('06rsgiven', 'ACH_MEMORIAL_SWORD'); //Receive Roth's sword
  FAchievements.Add('06Bonniesafe', 'ACH_SAVE_YOUR_LOVE'); //Meet Bonnie in 6village02.lvl
  FAchievements.Add('06smreturn', 'ACH_SCOUTMASTER_RETURN'); //Meet the Scoutmaster in 6village05.lvl
  FAchievements.Add('06howareyougentlemen', 'ACH_LITTLE_COWARD'); //Meet Holt in 6southgatesub1.lvl
  FAchievements.Add('06Hart', 'ACH_HARDMODE'); //Beat the game in Hardmode
end;

destructor TAchievements.Destroy;
begin
  FAchievements.Free;
  inherited;
end;

function TAchievements.getIdByTitle( title: string; character: TCharacter; out ACH_ID: string ): Boolean;
var
  key: string;
  i: integer;
  complete, found: Boolean;
  arr : TArray<string>;
begin
  ACH_ID := '';
  for key in FAchievements.Keys do
  begin
    arr := key.Split(['|']);
    complete := True;
    found := False;
    for i := Low(arr) to High(arr) do
    begin
      if SameText(title, arr[i]) then
      begin
        found := true;
        Break
      end;
    end;
    // validate key
    if found then
    begin
      for i := Low(arr) to High(arr) do
      begin
        if not character.TitleExists(arr[i]) then // include wearables !?
        begin
          complete := False;
          Break;
        end;
      end;
      if complete then
      begin
        FAchievements.TryGetValue( key, ACH_ID);
        Break;
      end;
    end;
  end;
  Result := ACH_ID<>'';
end;

end.
