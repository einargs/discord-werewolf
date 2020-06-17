# To-Do List

## Working on
- [ ] Implementing checks for protection
  - [ ] Should attackers learn whether the target died or was protected? no
- [ ] Implementing bodyguard and guardian angel
- [ ] Implementing removal of protected modifier from all players at beginning
      of night.

## General
- [ ] Expand the declareVictory function to also list the players on the winning
      team.
- [x] List who died during the night.
- [ ] Implement a system that allows for the werewolf coroutine to tell the
      executor to start a timer and the executor to pass an event saying a
      timer has triggered.
- [ ] After a game ends, print information about each player and their role
      and their roles.
  - [ ] At the start of the game, record the roles of the players so that at the
        end of the game, e.g., the doppelganger is listed as the doppelganger
        and not whatever role they became.
  - [ ] Post a text file containing the log of the events.
- [ ] Allow werewolf team members to be minions
- [ ] Allow people to pause/unpause the game
- [ ] Accusations and voting
  - [ ] Have a time period where you can't vote to accuse without it being
        unanimous
  - [ ] For an accsusation after the time period, there will be a bot-controlled
        time limit (10 seconds) during which someone else can second. If someone
        seconds the accusation, it proceeds to a full vote. During the wait for
        a second, no other actions can be taken.
  - [ ] If a vote has a majority in favor of lynching the target, the target is
        lynched.
  - [ ] If you accuse someone on a night and your accusation fails, you cannot
        accuse again.
- [ ] Protection (from bodyguards and guardian angels)
  - [ ] Protects from:
    - [ ] werewolf
    - [ ] lynching
    - [ ] huntress
    - [ ] hunter
    - [ ] targeted by revealer
    - [ ] shot by gunner
    - [ ] mad scientist explosion
    - [ ] cupid-link
    - [ ] harlot sleeping with werewolf team member
  - [ ] Does not protect from:
    - [ ] revealer backlash
    - [ ] gunner backlash

## Meta Commands
- [ ] help
  - [ ] help for roles
    - [ ] command to get your current role's info
    - [ ] command to get a list of roles
    - [ ] command to get info about an arbitrary role (will include commands)
  - [ ] help for commands
    - [ ] command for getting the list of commands you can perform and what
          they do
    - [ ] command for getting info about an arbitrary command.
- [ ] pause/unpause to start/stop the timer
- [ ] start the game

## Generic Actions
- [ ] Accuse someone
- [ ] Second an accusation
- [ ] Vote on an accusation

## Roles
- [ ] Werewolf
    - [x] Kill ability
    - [x] Wins if kills all villagers
    - [ ] Discover ability (`!w prowl`)
- [x] Werecub
    - [x] If dies to lynching, werewolf can kill twice
    - [x] If werewolf dies, becomes werewolf
- [ ] Werekitten
    - [x] seen as villager
    - [ ] gunner always misses
- [ ] Spellcaster
    - [x] hexed player cannot use an action
    - [ ] can hex players
      - [x] At night
      - [ ] At day
- [ ] Tough Wolf
    - [ ] Gunner always misses while toughness shield in place
    - [ ] Protected from lynching by toughness shield
- [x] Traitor
    - [x] Becomes the first werewolf team role to die.
- [ ] Warlock
    - [x] Cursed players are seen as being on the werewolf team
    - [ ] Can curse players
      - [x] At night
      - [ ] At day
    - [x] Can go on the first night
- [ ] Doctor
    - [ ] Can revive players
    - [ ] If revives self, loses doctor ability
- [x] Seer
    - [x] Can see target's role
- [ ] Bodyguard
    - [ ] Can guard a player
    - [ ] Dies if the protectee is attacked
    - [ ] Can choose not to guard anyone
    - [ ] Cannot guard self (no point)
- [ ] Guardian Angel
    - [ ] Can protect a player day+night
    - [ ] Cannot protect same player twice in a row
    - [ ] Can guard self
    - [ ] Can choose not to guard anyone
- [x] Huntress
    - [x] Can kill a player once
- [ ] Harlot
    - [ ] Can hide with a player at night (but not first night)
    - [ ] If hiding, is protected from attacks by:
      - [ ] werewolf
      - [ ] hunter
      - [ ] huntress
    - [ ] If hides with a werewolf team member, they die (unless protected)
- [x] Hunter
    - [x] Can take (optional) revenge on a player after dying
- [x] Mentalist
    - [x] Can compare two players
- [ ] Mad Scientist
    - [ ] On death explodes two adjacent players
    - [ ] Add a command to check what players are adjacent
- [ ] Cupid
    - [x] Kills a linked player if the other linked player dies 
    - [ ] Can link players at the start of a round.
- [x] Mystic
    - [x] Can discover how many werewolf players there are
- [x] Prophet
    - [x] Can discover if a role is in play
- [x] Revealer
    - [x] Can use reveal action
- [x] Lycan
    - [x] If killed by the werewolf, is turned instead
    - [x] When turned, is on the werewolf team.
- [ ] Mason
    - [x] Is informed of other masons
    - [ ] reveal command implemented
- [ ] Gunner
    - [ ] Can shoot during the day
    - [ ] Can shoot at night
- [ ] Prince
    - [ ] Protected from lynching once
- [ ] Doppelganger
    - [x] Assumes target's role on target's death
    - [ ] Can choose a target at start of game
- [x] Monster
    - [x] Immune to werewolf attack
    - [x] Wins if alive when another team wins
- [x] Turncoat
    - [x] Selects a team at start of game
    - [x] Can switch teams
- [x] Villager
