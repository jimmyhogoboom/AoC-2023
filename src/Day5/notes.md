seeds: 79 14 55 13

These are the seeds that must be planted, the start point

seed-to-soil map:
50 98 2
52 50 48

line 1:
DestinationRangeStart: 50
SourceRangeStart: 98
RangeLength: 2

Source range starts at 98, has 2 values: [98, 99]
Destination range starts at 50, has 2 values: [50, 51]
So SeedNumber 98 -> SoilNumber 50
   SeedNumber 99 -> SoilNumber 51

line 2:
DestinationRangeStart: 52
SourceRangeStart: 50
RangeLength: 48

Source range starts at 50, has 48 values: [50..97]
Destination range starts at 52, has 48 values: [52..99]
So SeedNumber 50 -> SoilNumber 52
   SeedNumber 51 -> SoilNumber 53
   SeedNumber 52 -> SoilNumber 54
   SeedNumber 53 -> SoilNumber 55
   ...


Anything not mapped have matching source/destination numbers. 
So SeedNumber 10 -> SoilNumber 10

So the entire SeedNumber -> SoilNumber map is like:
seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51

--

find the lowest location number that corresponds to any of the initial seeds

So in the exampe:

    Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
    Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
    Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
    Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.

So the lowest location number, the answer, is 35
