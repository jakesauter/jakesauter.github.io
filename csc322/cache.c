#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

//global variable to store the amount of clock cycles past 
int currentCycle;
//global variable to store the amount of misses the cache accumulates
int totalMisses;
//global variable to store the amount of hits the cache accumulates
int totalHits;

typedef struct
{
  int lastCycleAccessed;
  int timesAccessed;
  int valid;
  char * tag;
  //we don't actually need to store the data but we would do so here   
}line;

typedef struct
{
  int numLines;
  line * lines;
}set;

typedef struct 
{
  //for replacement policy . . .
    // 1. LFU
    // 2. LRU
  //for type of cache . . .
    //1. Direct Mapped Cache 
    //2. Set Associative Cache
    //3. Fully Associative Cache
  int numSets;
  int type;
  int rp;
  set * sets;
}cache;

//the purpose of this function is to initalize the cache with proper amounts of sets and lines, 
  //and to initialize the sets and lines as well, preparing them for memory address loads and accesses
cache initializeCache(int setCount, int cacheLinesPerSet, int bytesPerLine, int bitsInAddress, int rp)
{
  cache  c;
  //initialize the sets
  c.sets = malloc((sizeof(set)*setCount));
  //initialize the lines of the sets 
  for(int i=0;i<setCount;i++)
  {
    c.sets[i].numLines = cacheLinesPerSet;
    c.sets[i].lines = malloc(sizeof(line)*cacheLinesPerSet);
    for(int j=0;j<cacheLinesPerSet;j++)
    {
      c.sets[i].lines[j].valid = 0;
      c.sets[i].lines[j].lastCycleAccessed = 0; 
      c.sets[i].lines[j].timesAccessed = 0;
      c.sets[i].lines[j].tag = malloc(sizeof(char)*bitsInAddress);
    }
  }
  printf("cache made with %d sets and %d lines per set\n", setCount, cacheLinesPerSet);
  c.rp = rp;
  return c;
}

//the purpose of this function is to convert a hex string to a binary string 
char * parseAddress(char* a)
{
  char * newA = malloc(sizeof(a)*4);
  for(int i=0;i<strlen(a);i++)
  {
    switch(a[i])
    {
      case '0': strcat(newA, "0000");
               break;
      case '1': strcat(newA, "0001");
               break;
      case '2': strcat(newA, "0010");
               break;
      case '3': strcat(newA, "0011");
               break;
      case '4': strcat(newA, "0100");
               break;
      case '5': strcat(newA, "0101");
               break;
      case '6': strcat(newA, "0110");
               break;
      case '7': strcat(newA, "0111");
               break;
      case '8': strcat(newA, "1000");
               break;
      case '9': strcat(newA, "1001");
               break;
      case 'a': strcat(newA, "1010");
               break;
      case 'b': strcat(newA, "1011");
               break;
      case 'c': strcat(newA, "1100");
               break;
      case 'd': strcat(newA, "1101");
               break;
      case 'e': strcat(newA, "1110");
               break;
      case 'f': strcat(newA, "1111");
               break;
    }
  }
  return newA;
}

//the purpose of this function is to search the cache for the line, and if 
  //the line is not found it is replaced with the specified replacement policy
  // and the correct line is loaded to a line of the cache
void fetchLine(cache c, int setIndex, char * tag, int h, int p, char * pa)
{ 
  int miss = 1;
  char * retrievedTag = malloc(sizeof(char) * 100);
  for(int i=0;i<c.sets[setIndex].numLines;i++)
  {
    if(c.sets[setIndex].lines[i].valid == 1)//if the line is valid
    {
      //get the tag of the retrieved cache line and check if it matches
      retrievedTag = c.sets[setIndex].lines[i].tag;
      if(strcmp(retrievedTag, tag) == 0)
      {
        printf("%s H\n", pa);
        //update the access count and last time accessed of the cache line
        c.sets[setIndex].lines[i].timesAccessed++;
        c.sets[setIndex].lines[i].lastCycleAccessed = currentCycle;
        //add the hit time to the current cycle time
        currentCycle+=h;
        //add to the total number of hits
        totalHits++;
        miss = 0;
      }
    }
  }
  //if there is a miss first we check if there is an empty line in the set
    //if there is not an empty line in the set, then we follow our replacement policy 
  if(miss)
  {
    printf("%s M\n", pa); 
    //add the miss penalty to the current cycle count
    currentCycle += p;
    //add the hit penalty for the next cache layer
    currentCycle += h;
    //add to the total number of misses
    totalMisses++;
    int replaceIndex = -1;
    if(c.type == 1)
    {
      //make sure replaceIndex = 0, in direct map caches there is only 
        // one line per set
      replaceIndex = 0;
    }
    else if(c.type == 2 || c.type == 3)
    { 
      //try to find an empty line in the set first
      for(int i=0;i<c.sets[setIndex].numLines;i++)
      {
        if(c.sets[setIndex].lines[i].valid != 1)
        {
          replaceIndex = i;
          break;
        }
      }
      if(replaceIndex == -1)
      {
        if(c.rp == 1)//lfu
        {
          //printf("lfu\n");
          //find the least frequently accessed cache line in the set
          int minIndex = 0;
          int minValue = c.sets[setIndex].lines[minIndex].timesAccessed;
          for(int i=1;i<c.sets[setIndex].numLines;i++)
          {
   	    if(c.sets[setIndex].lines[i].timesAccessed < minValue)
            {
              minIndex = i;
              minValue = c.sets[setIndex].lines[minIndex].timesAccessed;
            }
          }
          replaceIndex = minIndex;
        }
        else if(c.rp == 2)//lru
        {
          //printf("lru\n");
          //find the line with the lowest last clock cycle reference time
          int minIndex = 0;
          int minValue = c.sets[setIndex].lines[minIndex].lastCycleAccessed;
          for(int i=1;i<c.sets[setIndex].numLines;i++)
          { 
            if(c.sets[setIndex].lines[i].lastCycleAccessed < minValue)
            {
              minIndex = i; 
              minValue = c.sets[setIndex].lines[minIndex].lastCycleAccessed;
            }
          }
          replaceIndex = minIndex;
        }
      }
     }
    //now replace with the index found in one of three possible methods
    c.sets[setIndex].lines[replaceIndex].tag = tag;
    c.sets[setIndex].lines[replaceIndex].valid = 1;
    c.sets[setIndex].lines[replaceIndex].timesAccessed = 1;
    c.sets[setIndex].lines[replaceIndex].lastCycleAccessed = currentCycle;
  }
}


//this function is responsible for parsing the address into its set index and tag
  // then call the fetchLine function in the correct way according to the cache type
void fetch(char * addr, cache c, int b, int s, int t, int h, int p, char * pa)
{
  int miss = 0;
  char * temp, *tag;
  tag = malloc(sizeof(char)*(b+s+t));
  strncpy(tag, addr, t);
  //printf("tag num string: %s", tag);
  int tagNum = (int)strtol(tag, NULL, 2);
  temp = malloc(sizeof(char)*(b+s+t));
  strncpy(temp, (addr+t), s);
  //printf("set index string: %s\n", temp);
  int setIndex = (int)strtol(temp , NULL, 2);
  //now we have the tag, and set of the address we are looking for, 
    //so we can fetch our cache line!
  printf("set index: %d, tag: %s\n", setIndex, tag);
  if(c.type == 1)//direct map cache
  {
    fetchLine(c, setIndex, tag, h, p, pa);
  }
  else if(c.type == 2)//set-associative cache
  {
    fetchLine(c, setIndex, tag, h, p, pa);
  }
  else if(c.type == 3)//fully-associative cache
  {
    //set index is 0 because there is only one set in a fully-associative cache
    fetchLine(c, 0, tag, h, p, pa);
  }
}

/* a little about caches . . .
* # of sets = S
* # of cache lines per set = E
* # of bytes per line = B 
* # of bits in an address = m
* # of tag bits: t = m - ( b + s )
* # of clock cycles in a hit: h
* # of clock cycles in a miss: m  */

int main()
{
  currentCycle = 0;
  totalMisses = 0;
  int S, E, B, m, h, p, rp;
  char a[3];  
  S=E=B=m=h=p=0;
  scanf("%d%d%d%d", &S, &E, &B, &m);
  //printf("S: %d, E: %d, B: %d, m: %d\n", S, E, B, m);
  scanf("%s", a);
  if(strcmp("LFU", a) == 0)
  {
    //implement the least frequently used replacement policy for the block found 
      // in a lower level cache 
    //printf("lfu\n");
    rp = 1;
  }
  else if(strcmp("LRU", a) == 0)
  {
    //implement the least resently used replacement policy for the block found 
      // in a lower level cache 
    //printf("lru\n");
    rp = 2;
  }
  scanf("%d%d", &h, &p);
  //printf("h: %d, p: %d\n", h, p);
  cache c = initializeCache(S, E, B, m, rp);
  // set the type of cache
  if(E == 1)
  {
    //one line per set is a Direct Map Cache
    c.type = 1;
  }
  else if(S == 1)
  {
    //when the number of lines matches the numbe of sets you have a fully associative cache
    c.type = 3;
  }
  else
  {
   //when the number of sets is K, it is a k-way associative cache
   c.type = 2;
   printf("SAC\n");
  }
  char* printaddr = malloc(sizeof(char)*10);
  char* addr = malloc(sizeof(char)*10);
  scanf("%s", printaddr);
  //prepairing to retrieve addresses
  int t, s, b;
  b = log(B)/log(2);
  s = log(S)/log(2);
  t = m - ( b + s );
  while(strcmp(printaddr, "-1") != 0)
  {
    //printf("fetching %s . . . \n", printaddr);
    addr = parseAddress(printaddr);
    //printf("changed to: %s \n", addr );
    fetch(addr, c, b, s, t, h, p, printaddr); 
    scanf("%s", printaddr);
  }
  //now we have to print the miss rate followed by the total cycle count 
  printf("%d %d", (int)(((double)totalMisses/(totalMisses+totalHits))*100), currentCycle);
  return 0;
}

//function graveyard
/*
//helper functions for fetching
void directMapFetch(cache c, int setIndex, char * tag, int h, int p, char* pa)
{
  int miss = 0;
  char * retrievedTag = malloc(sizeof(char) * 100);
  if(c.sets[setIndex].lines[0].valid == 1)//if the line is valid
  {
    //get the tag of the retrieved cache line and check if it matches
    retrievedTag = c.sets[setIndex].lines[0].tag;
    //printf("retrieved tag: %s, tag: %s\n", retrievedTag, tag);
    if(strcmp(retrievedTag, tag) == 0)
    {
      printf("%s H\n", pa);
      //increase the number of hits on the line and update the last access time
        //just for consistency
      c.sets[setIndex].lines[0].timesAccessed++;
      c.sets[setIndex].lines[0].lastCycleAccessed = currentCycle;
      //add the hit time to the current cycle time
      currentCycle+=h;
    }
    else
    {
      miss = 1;
    }
  }
  else
  {
    miss = 1; 
  }
  if(miss)
  { 
    printf("%s M\n", pa); 
    //add the miss penalty to the current cycle count
    currentCycle += p;
    //replace the line that the miss was on 
    c.sets[setIndex].lines[0].tag = tag;
    c.sets[setIndex].lines[0].valid = 1;
    c.sets[setIndex].lines[0].timesAccessed = 1;
    c.sets[setIndex].lines[0].lastCycleAccessed = currentCycle;
  }
}

//there is no point to this function, same as above with setIndex = 0
void fullyAssociativeFetch(cache c, char * tag, int h, int p, char * pa)
{
}*/


