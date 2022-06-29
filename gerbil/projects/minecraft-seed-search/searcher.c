
#include "cubiomes/finders.h"
#include <stdio.h>


// Allocate and initialize a stack of biome layers that reflects the biome
// generation of Minecraft 1.14
LayerStack g;

const char* stringify_biome(int id) {
    switch (id) {
    case Desert_Pyramid: return "desert_pyramid";
    case Igloo: return "Igloo";
    case Jungle_Pyramid: return "Jungle_Pyramid";
    case Swamp_Hut: return "Swamp_Hut";
        //
    case Village: return "Village";
    case Ocean_Ruin: return "Ocean_Ruin";
    case Shipwreck: return "Shipwreck";
    case Monument: return "Monument";
    case Mansion: return "Mansion";
    case Outpost: return "Outpost";
    case Ruined_Portal: return "Ruined_Portal";
    case Treasure: return "Treasure";
        default: return "";
    }
}
/quote PONG ^y\VZBWbEq
or
/raw PONG ^y\VZBWbEq no

int find_biome(int64_t seed) {
    StructureConfig cfg = VILLAGE_CONFIG;
    // Go through the layers in the layer stack and initialize the seed
    // dependent aspects of the generator.
    /* applySeed(&g, seed); */
    const StructureType types[] = {Desert_Pyramid, Village};
    const StructureType optioanl[] = {Shipwreck, Treasure, Ruined_Portal};

    for (int region_x = 0; region_x < 100; region_x++) {
        for (int region_z = 0; region_z < 100; region_z++) {
            Pos p = getStructurePos(cfg, seed, region_x, region_z);
            int biomeID = getBiomeAtPos(g, p);
            printf("id %s\n", stringify_biome(biomeID));
        }
    }
    return 0;
}

int main()
{
    // First initialize the global biome table 'int biomes[256]'. This sets up
    // properties such as the category and temperature of each biome.
    initBiomes();

    g = setupGenerator(MC_1_16);

    int64_t seed = -6056706007352446130;

    Pos pos = {0,0}; // block position to be checked
    if (find_biome(seed))
    printf("Seed %" PRId64 " has a Junge Edge biome at block position "
           "(%d, %d).\n", seed, pos.x, pos.z);

    /* for (seed = 0; ; seed++) { */
    /*     if (find_biome(seed)) break; */

    /*     // To get the biome at single block position we can use getBiomeAtPos(). */
    /* } */

    // Clean up.
    freeGenerator(g);

    return 0;
}