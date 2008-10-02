
@interface UDFLockPickOperation : NSOperation {
}
+ (UDFLockPickOperation*) lockPickOperationWithDiscKey:(NSData*)_discKey playerKey:(NSData*)_playerKey delegate:(id)_delegate;
@end

@interface NSObject (UDFLockPickOperationDelegate)
- (void) lockPickOperation:(UDFLockPickOperation*)operation foundKey:(NSData*)key;
@end

@interface UDFDiscInformationControlBlock : NSObject {
    off_t offset;
    uint32_t length;
    BOOL isFolder;
    uint16_t version;
    id replacedByObject;
}
+ (id) informationControlBlockWithOffset:(off_t)_offset length:(uint32_t)_length isFolder:(BOOL)_isFolder version:(uint16_t)_version;
@property (readonly) off_t offset;
@property (readonly) uint32_t length;
@property (assign) id replacedByObject;
@property (readonly) BOOL isFolder;
@property (readonly) uint16_t version;
@end

@interface _UDFPlayerKeyLockPickOperation : UDFLockPickOperation {
    id delegate;
    NSData* discKey;
    NSData* playerKey;
}
- (id) initWithDiscKey:(NSData*)_discKey playerKey:(NSData*)_playerKey delegate:(id)_delegate;
@end

@interface UDFSelfDecryptingData : NSData {
    NSMutableData* data;
    NSData* titleKey;
}
+ (id) dataWithMutableData:(NSMutableData*)data titleKey:(NSData*)titleKey;
- (id) initWithMutableData:(NSMutableData*)data titleKey:(NSData*)titleKey;
@end

@interface UDFDiscExtent : NSObject {
    off_t offset;
    uint32_t length;
    uint64_t lengthInBytes;
}
+ (id) extentWithOffset:(off_t)_offset length:(uint32_t)_length lengthInBytes:(uint64_t)_lengthInBytes;
@property (readonly) off_t offset;
@property (readonly) uint32_t length;
@property (readonly) uint64_t lengthInBytes;
@end

@interface UDFReader (Internal)
- (id) objectAtPath:(NSString*)path;
- (UDFDiscExtent*) readExtentFromInformationControlBlock:(UDFDiscInformationControlBlock*)icb;
- (id) resolveObjectFromInformationControlBlock:(UDFDiscInformationControlBlock*)icb atPath:(NSString*)path;
- (NSData*) readRawSectorAt:(off_t)offset error:(NSError**)error;
- (NSData*) readRawSectors:(uint32_t)count at:(off_t)offset error:(NSError**)error;
- (NSData*) readLogicalSectorAt:(off_t)offset error:(NSError**)error;
- (NSData*) readLogicalSectors:(uint32_t)count at:(off_t)offset error:(NSError**)error;
- (NSData*) determineTitleKeyForLogicalOffset:(off_t)offset;
- (void) lockPickOperation:(UDFLockPickOperation*)lpi foundKey:(NSData*)key;
@end

@interface UDFFileHandle (Internal)
- (id) initWithReader:(UDFReader*)reader path:(NSString*)path;
@end

static BOOL extractAndCheckVolumeDescriptor(NSData* sector, uint32_t* location, uint32_t* length);
static void reportPhysicalFormatInfo(int fd, int *numberOfLayers, off_t* blockCount);
static int checkCopyProtectionAndRegionMask(int fd, int* regionMask);
static int reportAGID(int fd, int agid);
static void sendChallenge(int fd, int agid, uint8_t *challenge);
static BOOL invalidateAGID(int fd, int agid);
static void reportKey1(int fd, int agid, uint8_t* key);
static void reportChallenge(int fd, int agid, uint8_t* challenge);
static void sendKey2(int fd, int agid, uint8_t* key);
static void readDiscKey(int fd, int agid, uint8_t *key);
static int reportAuthStatusFlag(int fd, int agid);
static void readTitleKey(int fd, int agid, int pos, uint8_t *key);
static void determineBusKey(int fd, int agid, uint8_t* busKey);

#define DVD_KEY_SIZE        5
#define DVD_CHALLENGE_SIZE  DVD_KEY_SIZE * 2
#define DVD_DISCKEY_SIZE    2048

typedef uint8_t dvd_key_t[DVD_KEY_SIZE];
typedef uint8_t dvd_challenge_t[DVD_CHALLENGE_SIZE];
typedef uint8_t dvd_disckey_t[DVD_DISCKEY_SIZE];

extern void CSS_key1(int variant, const dvd_challenge_t challenge, dvd_key_t key);
extern void CSS_key2(int variant, const dvd_challenge_t challenge, dvd_key_t key);
extern void CSS_busKey(int variant, const dvd_challenge_t challenge, dvd_key_t key);

extern void CSS_decryptBlock(const dvd_key_t key, uint8_t *sector);
extern void CSS_decryptKey(uint8_t invert, const dvd_key_t key, const dvd_key_t encryptedKey, dvd_key_t decryptedKey);

