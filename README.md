# Dungeon-Generation
This was a slow and dumb dungeon generation I wrote in R that focused on using pictures
The idea would be that whatever game engine you are using can read the picture and create dungeons off that
Different colors would represent different things in the environments. 
	Black - Wall
	White - Open
	Red - Spawn
	Green - Powerup
	Blue - Enemy Spawn
	Pink - Boss 
	etc

The project was a success minus one or two issues. 
The way it works is it creates a picture size nxn, and selects an area from the middle third to create the inital room. 
The room and hallway diminsions are specified as (maybe parameters or hardcoded, whatever works)
The growth rate is specified (maybe there's a hardcoded value for maximum number of rooms?)
The idea is that from a room, it looks in all directions and sees if it passes some check to make a room that way, 
	with success it check for overlap (still some kinks to work out)
	with failure it tries another direction if another direction passed

The current errors are in the hallways not completely lining up (easy to fix)
Rooms overlap sometimes a little 
	maybe make hard walls around each room too? 

In neat.png you can see two distinct clusters of rooms, which should never happen given everything is grown from the same spawn room
	No idea how this happened! It's a mystery to everyone!
