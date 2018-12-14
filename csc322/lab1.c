#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//struct definitions

//10 creatures may be in a room, if another tries to enter the room, it is denied access with a "Room full message"
//each creature has access to a look command, which will decrible the room that it is in 
//Animals(1) want the room clean(0), NPCs(2) want the rooms to be dirty(2)
//the pc has repect, starting at 40
	//if the pc's respect drops down below 0, the pc will lose
	//if the pc's repsect rises above 80 the pc will win
//only the pc can clean or dirty a room that they are in, animals cannot dirty a room while the pc is in it, 
	//but the pc can make npcs or animals clean the room while it is in the room
//if a room is cleaned or dirtied and the creature cannot stay in the room due to the state of the room, it must move to
	//an adjcent one
	//if the room it goes to next does not fit its condition, it performs the action that would make it livable for the creature
	//if the creature cannot go into any of the adjacent rooms because they are all full, the creature leaves the simulation
		//when this happens, all creatures in the room will 'growl' or 'grumble', in addition to their reactions from
		//the state of the room change
		//note that the process happens animal by animal so that if one animal enters the room in order to leave, 
		//making all adjacent rooms full, and another animal will try to leave, that the first animal no longer
		//is in the room to growl
//the pc can also make any creature leave the room in the direction of the pc's choosing. Again if the creature does not like the state of the room it moves to, it will clean or dirty the room.
//animals can growl and lickface, but they only do it in the room that the pc is in 
//NPCs can smile and grumble
//if a pc performs an action and a creature must leave, they perform the action before they leave.
//Anytime the pc recieves a smile or lickface, its respect value increases by 1 
//if the player makes another creature clean or dirty the room, this creature reacts 3 times, but every other creature in the room will only react as it would otherwise.
typedef struct
{
	int type, room;
}Creature;

typedef struct 
{
	int state, creatureCount;	
	int neighbors[4];
	int creaturesInRoom[10];	
	//we should allocate the amount of maximum creatures per room, which is taken in at the begining of the program
}Room;

void initializeRoom(Room * r)
{
	r->creatureCount = 0;
	for(int i=0;i<10;i++)
	{
		r->creaturesInRoom[i] = -1;
		if(i < 4)
		{
			r->neighbors[i] = -1;
		}
	}
}

//function prototypes
void processInput(char *);	
int changeRoom(int, int);
void changePCRespect(int);
void creatureMustLeave(int, int, int, int);
void help();

//global pointers for rooms and creatures
Room * rooms;
Creature * creatures;
int pcLoc;//this is the location of pc in the creatures array  
int pcRespect;

int main()
{	
	pcRespect = 40;
	pcLoc = 0;
	int numRooms = 0;
	int numCreatures = 0;
	scanf("%d", &numRooms);
	rooms = malloc(sizeof(Room) * numRooms);
	for(int i=0;i<numRooms;i++)
	{
		initializeRoom(&rooms[i]);
		//printf("Room created at pos: %d\n", i);
		scanf("%d", &rooms[i].state);
		//printf("state: %d\n", rooms[i].state);
		scanf("%d", &rooms[i].neighbors[0]);
		//printf("north: %d\n", rooms[i].neighbors[0]);
		scanf("%d", &rooms[i].neighbors[1]);
		//printf("south: %d\n", rooms[i].neighbors[1]);
		scanf("%d", &rooms[i].neighbors[2]);
		//printf("east: %d\n", rooms[i].neighbors[2]);
		scanf("%d", &rooms[i].neighbors[3]);
		//printf("west: %d\n", rooms[i].neighbors[3]);	
	}	
	scanf("%d", &numCreatures);	
	printf("There are %d creatures\n", numCreatures);
	creatures = malloc(sizeof(Creature)*numCreatures);	
	int type, location;
	for(int i=0;i<numCreatures;i++)
	{
		//type of the creature, location in the array
		scanf("%d", &type);
		//printf("type of creature for creature %d: %d\n", i, type);
		scanf("%d", &location);
		//printf("location of creature %d: %d\n", i, location);
		creatures[i].type = type;
		creatures[i].room = location;
		rooms[location].creaturesInRoom[rooms[location].creatureCount] = i;
		rooms[location].creatureCount++;
		//setting the location of the PC from input
		if(type == 0)
		{
			pcLoc = i;
		}
   	}
	printf("\n\n\n Instructions: \n\n");		
	help();	
	printf("\n\n");
	//declaring a variable for use of user input
	char usrInput[100];
	do
	{
		printf("please enter usr input: ");
		scanf("%s", usrInput);
		printf("input of: %s being processed\n", usrInput);
		processInput(usrInput);
	}while(strcmp(usrInput, "exit") != 0);
	printf("thank you for playing!\n");
	//free all memory usage
	free(rooms);
	free(creatures);
	return 0;
}

void processInput(char * input)
{
	//check if it is a number first
	//the max number of creatures is 100, so we just have to check the first 3 positions
	char nums[10] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
	char num[5];
	int numCreature = -1;
	int inputNum = 0;
	int colonPos = -1;
	int changeDir = -1;
	//nested loop to process if there is a number 
	for(int i=0;i<4;i++)         
	{
	  if(input[i] == ':')
	  {
	 	 //printf("colon at: %d", i);
 		 colonPos = i; 
		 num[i] = '\0'; 
		 break;
	  } 
    	 for(int j=0;j<10;j++)
      	 {
       	   if(input[i] == nums[j])
       	   {
       	      num[i] = nums[j];   
           }
     	 }	
	}
         inputNum = (int) strtol(num, (char **)NULL, 10);

	//set the location of the character we are processing the command for, if colonPos = -1, its pc, otherwise it is input num
	if(colonPos != -1)//if the colon was found
	{
		char temp[100];
		//we need the substring of the command without the number that we just parsed
		strncpy(temp, &input[colonPos+1], (100-((sizeof(char)*(colonPos+1)))));
		//temp[sizeof(temp)-sizeof(char)] = '\0';
		input = temp;
	}
	else
	{
		inputNum = pcLoc;
	}
	
	//now to execute the parsed command
	if(strcmp(input, "look") == 0)
	{
		char * state;
		if(rooms[creatures[inputNum].room].state == 0)
		{
			state = "clean";
		}
		else if(rooms[creatures[inputNum].room].state == 1)
		{
			state = "half-dirty";
		}
		else
		{	
			state = "dirty";
		}
		printf("Room %d, %s, Neighbors ", creatures[inputNum].room , state);
		
		char dir[4][10] = {{"north\0"},{"south\0"},{"east\0"},{"west\0"}};
		for(int i=0;i<4;i++)
		{
			if(rooms[creatures[inputNum].room].neighbors[i] != -1)
			{
				printf("%d to the %s, ", rooms[creatures[inputNum].room].neighbors[i], dir[i]);
			}
		}
		printf("contains: \n");
		char * type; 
		for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)
		{
			//creature types: 0-Pc 1-animal 2-npc
			if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 0)
			{
				printf("PC\n");
			}
			else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 1)
			{	
				printf("animal %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i]);
			}		
			else
			{
				printf("NPC %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i]);
			}
		}
		printf("\n\n");
	}
	//states are 0=clean 1=half-dirty 2=dirty
	else if(strcmp(input, "clean") == 0)
	{		
		//if the pc makes a creature clean the room, aka if there is an input num, the pc is affected
			//three times more by that creatures reaction
		if(rooms[creatures[inputNum].room].state == 1)
		{	
			rooms[creatures[inputNum].room].state--;		
			//now the stae of the room is clean, all creatures should react accordingly
				//all NPCs must leave
			int j=0;
			int change = -1;
			//printf("pc room: %d creature room: %d\n", creatures[pcLoc].room, creatures[inputNum].room);
			for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)
			{
				if(creatures[pcLoc].room == creatures[inputNum].room)
				{
					//printf("in the same room\n");
					//printf("type of creature: %d\n", creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 2);

					if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 2)
					{
						//printf("it is an NPC\n");
						//printf("pc at loc: %d and inputNum: %d\n", pcLoc, inputNum);
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(-3);
							printf("NPC %d grumbles alot at you, you lose 3 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}
						else
						{
							changePCRespect(-1);
							printf("NPC %d grumbles at you, you lose 1 repsect, total repsect is now: %d								\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						}
						change = rooms[creatures[inputNum].room].creaturesInRoom[i];
						while(j < 3)
						{
							if(changeRoom(j, change) == 0)//if room could not be changed	
							{
								j++;//try another room
							}		
							else
							{
								break;
							}
						}
						//printf("j: %d\n\n", j);
						if(j == 3)//there is a creature that cannot move to another room and cannot stay 
						{
							printf("no room could be found for the creature to leave!!\n\n");
							creatureMustLeave(inputNum, i, change, rooms[creatures[inputNum].room].state);
						}  	
						else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 1)	
						{	
                                 			printf("it is an animal\n");
							if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
							{
								changePCRespect(3);
								printf("Animal %d licks your face 3 times, you earn 3 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
							}
							else
							{
								changePCRespect(1);
								printf("Animal %d licks your face, you earn 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
							}
						
						}	
					}

				}	
			}
		}
		else if(rooms[creatures[inputNum].room].state == 2)
		{
			rooms[creatures[inputNum].room].state--;
			//Npc will grumble
			if(creatures[pcLoc].room == creatures[inputNum].room)
			{
				for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)
				{
					if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 2)
					{
						printf("it is an npc\n");
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(-3);
							printf("NPC %d grumbles at you alot, you lose 3 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}	
						else
						{
							changePCRespect(-1);
							printf("NPC %d grumbles at you, you lose 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						} 
					} 	
					else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 1)	
					{
						printf("it is an animal\n");
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(3);
							printf("Animal %d licks your face 3 times, you earn 3 repsect, total repsect is now: %d\n", 
								rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}
						else
						{
							changePCRespect(1);
							printf("Animal %d licks your face, you earn 1 repsect, total repsect is now: %d\n", 
								rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						}
					}
				}	
			}	
		}
	}
	else if(strcmp(input, "dirty") == 0)
	{
		if(rooms[creatures[inputNum].room].state == 0)
		{
			rooms[creatures[inputNum].room].state++;
			if(creatures[pcLoc].room == creatures[inputNum].room)
			{
				for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)
				{
					if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 1)
					{
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(-3);
							printf("Animal %d growls at you, you lose 3 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}
						else
						{
							changePCRespect(-1);
							printf("Animal %d growls at you, you lose 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						}	  	
					}	
					else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 2)	
					{
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(3);
							printf("NPC %d smiles alot, you earn 3 repsect, total repsect is now: %d\n", 
								rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}
						else
						{
							changePCRespect(1);
							printf("NPC %d smiles, you earn 1 repsect, total repsect is now: %d\n", 
								rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						}
						
					}
				}	
			
			}
		}
		else if(rooms[creatures[inputNum].room].state == 1)
		{
			rooms[creatures[inputNum].room].state++;
			//now the state of the room is dirty, all creatures should react accordingly
			int j=0;
			int change = -1;
			for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)
			{
				if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 1) 
				{ 
					if(creatures[pcLoc].room == creatures[inputNum].room)
					{
						if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
						{
							changePCRespect(-3);
							printf("Animal %d growls at you, you lose 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
						}
						else
						{
							changePCRespect(-1);
							printf("Animal %d growls at you, you lose 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
						}
					}  
					change = rooms[creatures[inputNum].room].creaturesInRoom[i];
					printf("animal %d must leave the room now because it is too dirty\n", change);
					while(j < 3)
					{
						if(changeRoom(j, change) == 0)//if room could not be changed		
						{
							j++;//try another room
						}		
						else
						{
							break;
						}
					}
					if(j == 3)//there is a creature that cannot move to another room and cannot stay 
					{
						printf("no room could be found for the creature to leave!!\n\n");
						creatureMustLeave(inputNum, i, change, rooms[creatures[inputNum].room].state);
					}
				}	
				else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[i]].type == 2)	
				{
					if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
					{
						changePCRespect(3);
						printf("NPC %d smiles alot, you earn 3 repsect, total repsect is now: %d\n", 
							rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);  	
					}
					else
					{
						changePCRespect(1);
						printf("NPC %d smiles, you earn 1 repsect, total repsect is now: %d\n", 
							rooms[creatures[inputNum].room].creaturesInRoom[i], pcRespect);
					}		
			    }	  	
			  }
		   }
	    }
	
	
	//if the creature cannot move to the room because it is full, it will perform a negative action
	//if the room is not in the state that it likes, it will perform, clean or dirty 
	//move the creature from one room to the other
		//first check if the room exists, then if it is full
	else if(strcmp(input, "north") == 0)
	{
		changeDir = 0;
	}
	else if(strcmp(input, "south") == 0)
	{
		changeDir = 1;
	}
	else if(strcmp(input, "east") == 0)
	{
		changeDir = 2;
	}
	else if(strcmp(input, "west") == 0)
	{
		changeDir = 3;
	}
	//now to move the creature
	if(changeDir != -1)
	{
		changeRoom(changeDir, inputNum);
	}
}

void changePCRespect(int x)
{
	pcRespect += x;
	if(pcRespect > 80)
	{ printf("You win and are in praise from all of the creatures of the house!\n");
		exit(0);
	}
	else if(pcRespect < 0)
	{	
		printf("You have made all of the creatures upset and have lost the game!\n");
		exit(0);
	}
}

int changeRoom(int changeDir, int inputNum)
{	
	//printf("inputNum recieved: %d\n", inputNum);
	int returnStatus = 0;	
	//printf("changing rooms\n");
	if(rooms[creatures[inputNum].room].neighbors[changeDir] != -1)//if the room exists
	{
		//printf("The room exists\n");
		if(rooms[rooms[creatures[inputNum].room].neighbors[changeDir]].creatureCount < 10)//if the room is not full
		{	
			returnStatus = 1;
			//printf("The room is not full\n");
			printf("\nCreature: %d moving from room: %d to room: %d\n", inputNum, creatures[inputNum].room,
								 rooms[creatures[inputNum].room].neighbors[changeDir]);
			//move the creature to that room
			rooms[creatures[inputNum].room].creatureCount--;	
			for(int i=0;i<rooms[creatures[inputNum].room].creatureCount;i++)	
			{
				if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[i])
				{
					rooms[creatures[inputNum].room].creaturesInRoom[i] = -1;
					//now move all other positions to the right
					for(int j=i;j<10;j++)
					{
						if(rooms[creatures[inputNum].room].creaturesInRoom[j] != -1)
						{
	 						rooms[creatures[inputNum].room].creaturesInRoom[j-1] = 
									rooms[creatures[inputNum].room].creaturesInRoom[j];  
							rooms[creatures[inputNum].room].creaturesInRoom[j] = -1;
						}
					}
				}
			}
			creatures[inputNum].room = rooms[creatures[inputNum].room].neighbors[changeDir];
			rooms[creatures[inputNum].room].creatureCount++;
			rooms[creatures[inputNum].room].creaturesInRoom[rooms[creatures[inputNum].room].creatureCount-1] = inputNum;
			//check if that creature can live in the state of the new room	
			if(creatures[inputNum].type == 1)//1=animal, want clean=0
			{
				if(rooms[creatures[inputNum].room].state == 2)
				{
					rooms[creatures[inputNum].room].state--;
				}
			}
			else if(creatures[inputNum].type == 2)//2=NPC, want dirty=2
			{
				if(rooms[creatures[inputNum].room].state == 0)
				{
					rooms[creatures[inputNum].room].state++; 
				}
			}	
		     }
		     else
		     {
			 printf("That room is full\n");
		     }
		}
		else
		{
			//meaning the creature could not go to the room, a negative action will happen
			printf("That room does not exist\n");
		}
	
	return returnStatus;
}

void creatureMustLeave(int inputNum, int i, int change, int newRoomState)//I is the index of which the creature is in the creature index array int the room, change is its index in the creatures arrray
{
	printf("creature %d must leave the room because of the state, and there is no room it can go to, so now it must leave\n", change);
	//remove creature from the creatures global array 
	for(int j = change;j<(sizeof(creatures)/sizeof(creatures[0]))-1;j++)//-1 because accesing index j+1
	{
		creatures[j] = creatures[j+1];
	}
	//remove the creature from the creatures index array in the room
	rooms[creatures[change].room].creatureCount--;
	for(int j = i; j < 9;j++)
	{
		rooms[creatures[inputNum].room].creaturesInRoom[j] = 
			rooms[creatures[inputNum].room].creaturesInRoom[j+1];
	}
	//now all creatures in the room will react negatively (still only if the pc is in the room
	if(creatures[pcLoc].room == creatures[inputNum].room)
	{
		for(int j=0;j<10;j++)
		{
			if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[j]].type == 1)
			{	
				if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[j])
				{
					changePCRespect(3);
					printf("Animal %d licks your face 3 times, you earn 3 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[j], pcRespect);  	
				}
				else
				{
					changePCRespect(1);
					printf("Animal %d licks your face, you earn 1 repsect, total repsect is now: %d\n", rooms[creatures[inputNum].room].creaturesInRoom[j], pcRespect);
				}
			}
			else if(creatures[rooms[creatures[inputNum].room].creaturesInRoom[j]].type == 2)
			{	
				if(inputNum == rooms[creatures[inputNum].room].creaturesInRoom[j])
				{
					changePCRespect(3);
					printf("NPC %d smiles alot, you earn 3 repsect, total repsect is now: %d\n", 
						rooms[creatures[inputNum].room].creaturesInRoom[j], pcRespect);  	
				}
				else
				{
					changePCRespect(1);
					printf("NPC %d smiles, you earn 1 repsect, total repsect is now: %d\n", 
						rooms[creatures[inputNum].room].creaturesInRoom[j], pcRespect);
				}
			}
		}
	}
}

void help()
{
     	printf("Commands can only contain one or two words. Two words are split by \":\" (wit    hout the quotes). White spaces of any kind are forbidden.\n");
     printf("One-word commands apply to the PC, two-word commands apply to other creatures\n");
     printf("To move the PC, type (without the quotes) \"[direction]\" where [direction] is north, south, east or west. If the PC cannot move into the selected direction (e.g. because there is no room in this direction or because the room there is full), a corresponding error message will be given and the PC will not move.\n");
     printf("To move a creature, type (without the quotes) \"[Creature name]:[direction]\".If the creature cannot go there (for the same reasons as the PC), a corresponding message will be given, and the creature will react correspondingly (e.g., and NPC will grumble if it is asked to move to a full room, etc. See project description.)\n");
     printf("To make the PC clean or dirty the current room, type \"clean\" or \"dirty\".\n");
     printf("To make the PC look around the current room, type \"look\".\n");
     printf("To make a creature clean or dirty the current room, type \"[Creature name]:clean\" or \"[Creature name]:dirty\"\n.");
     printf("To make a creature that is in the current room look around the room, type \"[Creature name]:look\". Error, if creature is not in the room.\n");
     printf("To see this menu, type \"Help\" (not case-sensitive)\n");
     printf("To exit the game, type \"Exit\" (not case-sensitive)\n");
}
