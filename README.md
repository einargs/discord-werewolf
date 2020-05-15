# Werewolf
This is currently under development and is nowhere near ready to use.

# Brainstorming
- I think I'm going to need a log based system for people's actions at night.
  Since all night actions are going to be processed in a strict order, I want
  to set it up so that you can send your command at the start of the night and
  have it put into a backlog. Then when the bot gets to your role, it can check
  the backlog to see if you've already sent the action it needs. If you have,
  it just grabs it; otherwise it prompts you for it and waits for you.
