#include "sound.h"

//Hashmap soundmap;

DLLEXPORT int initialize(int device, int freq) {
    debugprintf("[BASS] Initializing library...\n");
    BASS_Init(device, freq, 0, 0, NULL);
    //initHashmap(&soundmap, 10);
    return 0;
}


/*
DLLEXPORT DWORD playSample(const char* filename) {
    Hash hash = makeHash(filename);
    Node* np = hashmapGet(&soundmap, hash);
    DWORD samplehandle;
    if (np == NULL) {
        samplehandle = BASS_SampleLoad(filename);
        Node new; // tee osoitin ja varaa muisti mallocilla?
        initNode(&new, hash, handle);
        hashmapInsert(&map, &new);
    }

    DWORD ch = BASS_GetChannels(samplehandle);
    return handle;
}
*/

DLLEXPORT DWORD playStream(const char* filename, float volume, int loop) {
    DWORD channel = BASS_StreamCreateFile(FALSE, filename, 0, 0, 0);
    debugprintf("[BASS] Playing %s, handle=%u\n", filename, channel);
    BASS_ChannelSetAttribute(channel, BASS_ATTRIB_VOL, volume);
    if (loop > 0)
        BASS_ChannelFlags(channel, BASS_SAMPLE_LOOP, BASS_SAMPLE_LOOP);
    //BASS_SetVolume(1.0f);
    if(!BASS_ChannelPlay(channel, FALSE)) {
        debugprintf("[BASS] Error while playing. Error code: %d\n", BASS_ErrorGetCode());
    }
    return channel;
}

DLLEXPORT int channelStop(DWORD channel) {
    BASS_ChannelStop(channel);
    return 0;
}

DLLEXPORT void cleanup() {
    BASS_Free();
}
