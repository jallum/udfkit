#import "UDFKit.h"
#import "UDFKit+Private.h"
#import <IOKit/storage/IOMediaBSDClient.h>
#import <IOKit/storage/IODVDMediaBSDClient.h>
#import <IOKit/storage/IODVDTypes.h>

#ifndef DEBUG
#define NSLog(...)
#endif


NSString* const UDFReaderErrorException = @"UDFReaderErrorException";
NSString* const UDFReaderAuthenticationException = @"UDFReaderAuthenticationException";
NSString* const UDFReaderUnableToRecoverKeyException = @"UDFReaderUnableToRecoverKeyException";

static const dvd_key_t player_keys[] = {
    { 0x01, 0xaf, 0xe3, 0x12, 0x80 },
    { 0x12, 0x11, 0xca, 0x04, 0x3b },
    { 0x14, 0x0c, 0x9e, 0xd0, 0x09 },
    { 0x14, 0x71, 0x35, 0xba, 0xe2 },
    { 0x1a, 0xa4, 0x33, 0x21, 0xa6 },
    { 0x26, 0xec, 0xc4, 0xa7, 0x4e },
    { 0x2c, 0xb2, 0xc1, 0x09, 0xee },
    { 0x2f, 0x25, 0x9e, 0x96, 0xdd },
    { 0x33, 0x2f, 0x49, 0x6c, 0xe0 },
    { 0x35, 0x5b, 0xc1, 0x31, 0x0f },
    { 0x36, 0x67, 0xb2, 0xe3, 0x85 },
    { 0x39, 0x3d, 0xf1, 0xf1, 0xbd },
    { 0x3b, 0x31, 0x34, 0x0d, 0x91 },
    { 0x45, 0xed, 0x28, 0xeb, 0xd3 },
    { 0x48, 0xb7, 0x6c, 0xce, 0x69 },
    { 0x4b, 0x65, 0x0d, 0xc1, 0xee },
    { 0x4c, 0xbb, 0xf5, 0x5b, 0x23 },
    { 0x51, 0x67, 0x67, 0xc5, 0xe0 },
    { 0x53, 0x94, 0xe1, 0x75, 0xbf },
    { 0x57, 0x2c, 0x8b, 0x31, 0xae },
    { 0x63, 0xdb, 0x4c, 0x5b, 0x4a },
    { 0x7b, 0x1e, 0x5e, 0x2b, 0x57 },
    { 0x85, 0xf3, 0x85, 0xa0, 0xe0 },
    { 0xab, 0x1e, 0xe7, 0x7b, 0x72 },
    { 0xab, 0x36, 0xe3, 0xeb, 0x76 },
    { 0xb1, 0xb8, 0xf9, 0x38, 0x03 },
    { 0xb8, 0x5d, 0xd8, 0x53, 0xbd },
    { 0xbf, 0x92, 0xc3, 0xb0, 0xe2 },
    { 0xcf, 0x1a, 0xb2, 0xf8, 0x0a },
    { 0xec, 0xa0, 0xcf, 0xb3, 0xff },
    { 0xfc, 0x95, 0xa9, 0x87, 0x35 }
};

#pragma mark -
@implementation UDFReader
@synthesize regionMask;
@synthesize numberOfLayers;
@synthesize rawBlockCount;
@synthesize closed;

+ (id) UDFReaderWithBSDName:(NSString*)_bsdName
{
    return [[[UDFReader alloc] initWithBSDName:_bsdName] autorelease];
}

- (id) initWithBSDName:(NSString*)_bsdName
{
    NSAssert(_bsdName, @"Shouldn't be nil");
    if (self = [super init]) {
        closed = YES;
        @try {
            if (NSNotFound == [_bsdName rangeOfString:@"rdisk"].location) {
                pathToDevice = [NSString stringWithFormat:@"/dev/r%@", _bsdName];
            } else {
                pathToDevice = [NSString stringWithFormat:@"/dev/%@", _bsdName];
            }
            [pathToDevice retain];

            fileDescriptor = open([pathToDevice fileSystemRepresentation], O_RDONLY, 0);
            if (fileDescriptor < 0) {
                [NSException raise:UDFReaderErrorException format:@"%s", strerror(errno)];
            }

            cache = [[NSMutableDictionary alloc] init];
            titleKeysByPath = [[NSMutableDictionary alloc] init];

            NSException* thrownException = nil;
            int tries = 3;
            do {
                @try {
                    /*  Read a sector to reset things.
                     */
                    uint8_t buffer[0x800];
                    pread(fileDescriptor, buffer, sizeof(buffer), 0);
                    
                    if (isProtected = checkCopyProtectionAndRegionMask(fileDescriptor, &regionMask)) {
                        /*  Authenticate ourselves with the drive, and retrieve an 
                         *  encrypted disc-key.  
                         */
                        dvd_key_t busKey;
                        dvd_disckey_t encryptedDiscKeyBytes;
                        BOOL authenticated = NO;
                        int agid = 0;
                        for (int i = 0; !authenticated && i < 10; i++) {
                            @try {
                                agid = reportAGID(fileDescriptor, i);
                                determineBusKey(fileDescriptor, agid, busKey);
                                readDiscKey(fileDescriptor, agid, encryptedDiscKeyBytes);
                                if (1 == reportAuthStatusFlag(fileDescriptor, i)) {
                                    authenticated = YES;
                                }
                            } @catch (id exception) {
                                invalidateAGID(fileDescriptor, i);
                            }
                        }
                        if (!authenticated) {
                            [NSException raise:UDFReaderAuthenticationException format:@"Unable to authenticate drive."];
                        }

                        /*  Mash up the encrypted disc-key with the bus-key.  
                         */
                        for (int i = 0; i < DVD_DISCKEY_SIZE; i++) {
                            encryptedDiscKeyBytes[i] ^= busKey[DVD_KEY_SIZE - 1 - (i % DVD_KEY_SIZE)];
                        }
                        encryptedDiscKey = [NSData dataWithBytes:encryptedDiscKeyBytes length:DVD_DISCKEY_SIZE];
                    }
                    
                    /*  Have the drive report to us it's physical characteristics.
                     *  This may fail-out with an exception if the drive has
                     *  been idle for a long time.
                     */
                    reportPhysicalFormatInfo(fileDescriptor, &numberOfLayers, &rawBlockCount);
                } @catch (id exception) {
                    thrownException = exception;
                    continue;
                }
                break;
            } while (--tries > 0);
            
            if (tries == 0) {
                if (thrownException) {
                    [thrownException raise];
                } else {
                    [NSException raise:UDFReaderErrorException format:@"%s(%d)", __FILE__, __LINE__];
                }
            }

            /*  If the drive has given us an encrypted disc key, then we'll 
             *  start by executing all of our player keys against it in 
             *  parallel.
             */
            if (encryptedDiscKey) {
                lockPicks = [[NSOperationQueue alloc] init];
                for (int i = 0; i < sizeof(player_keys) / sizeof(dvd_key_t) && !discKey; i++) {
                    [lockPicks addOperation:[UDFLockPickOperation lockPickOperationWithDiscKey:encryptedDiscKey playerKey:[NSData dataWithBytes:player_keys[i] length:sizeof(player_keys[i])] delegate:self]];
                }
            }
            
            uint16_t speed = kDVDSpeedMax;
            if (0 != ioctl(fileDescriptor, DKIOCDVDSETSPEED, &speed)) {
                NSLog(@"Info: Unable to set the drive's target speed.");
            }
            
            /*  Attempt to read in the Anchor Volume Descriptor Pointer (ADVP) from
             *  a standard location (sector 256), and failing that, at a backup 
             *  location.  If we succeed, we'll learn the location of the Main 
             *  Volume Descriptor Sequence (MVDS) and it's backup copy.  We'll 
             *  proceed to read through the MVDS until we discover the location of 
             *  Partition-0.  If we come across the volume descriptor along the 
             *  way, we make a note of it.
             */
            BOOL foundPartition = NO;
            NSError* error = nil;
            NSData* sector = nil;
            for (NSNumber* offset in [NSArray arrayWithObjects:[NSNumber numberWithLong:0x0100], [NSNumber numberWithLong:rawBlockCount - 1], [NSNumber numberWithLong:rawBlockCount - 1 - 0x100], [NSNumber numberWithLong:0x0200], nil]) {
                sector = [self readRawSectorAt:[offset integerValue] error:&error];
                if (sector && !error) {
                    const uint8_t* bytes = sector.bytes;
                    if (0x02 != OSReadLittleInt16(bytes, 0x00)) {
                        continue;
                    }
                    
                    struct extent {
                        uint32_t offset;
                        uint32_t length;
                    } extent[0x02];
                    struct extent* pextent = extent;
                    
                    pextent->length = OSReadLittleInt32(bytes, 0x18);
                    pextent->offset = OSReadLittleInt32(bytes, 0x1C);
                    pextent->length >>= 11;
                    pextent++;
                    pextent->length = OSReadLittleInt32(bytes, 0x10);
                    pextent->offset = OSReadLittleInt32(bytes, 0x14);
                    pextent->length >>= 11;
                    
                    BOOL foundVolumeDescriptor = NO;
                    do {
                        for (uint32_t offset = pextent->offset, limit = pextent->offset + pextent->length; offset < limit && (!foundVolumeDescriptor || !foundPartition); offset++) {
                            sector = [self readRawSectorAt:offset error:&error];
                            if (sector && !error) {
                                bytes = [sector bytes];
                                int tag = OSReadLittleInt16(bytes, 0x00);
                                if (!foundPartition && tag == 0x05) {  
                                    /* Partition Descriptor (PD) */
                                    volume.offset = OSReadLittleInt32(bytes, 188);
                                    volume.length = OSReadLittleInt32(bytes, 192);
                                    volume.flags = OSReadLittleInt16(bytes, 20);
                                    foundPartition = YES;
                                } else if (!foundVolumeDescriptor && tag == 0x06) {
                                    /* Logical Volume Descriptor (LVD) */
                                    if (0x0800 != OSReadLittleInt32(bytes, 212)) {
                                        /* sector size is other than expected */
                                        continue;
                                    }
                                    if (0x06 != OSReadLittleInt32(bytes, 264)) {
                                        /* should be 6 */
                                        continue;
                                    }
                                    if (0x01 != OSReadLittleInt32(bytes, 268)) {
                                        /* should be 1 */
                                        continue;
                                    }
                                    volume.descriptor = [[NSString alloc] initWithCString:(void*)(bytes + 85) encoding:(bytes[84] == 8 ? NSUTF8StringEncoding : NSUTF16LittleEndianStringEncoding)];
                                    foundVolumeDescriptor = YES;
                                } else if (tag == 0x08) {
                                    break;
                                }
                            }
                        }
                    } while (--pextent >= extent && !foundPartition);
                    if (foundPartition) {
                        break;
                    }
                }
            }
            if (!foundPartition) {
                [NSException raise:UDFReaderErrorException format:@"Unable to locate partition."];
            } else if (error) {
                [NSException raise:UDFReaderErrorException format:@"%s", strerror([error code])];
            }

            /*  Scan for a File Set Descriptor (FSD), then locate the Root 
             *  Directory.
             */
            BOOL foundFileSetDescriptor = NO;
            for (uint32_t offset = 0; offset < volume.length; offset++) {
                sector = [self readLogicalSectorAt:offset error:&error];
                if (sector && !error) {
                    const uint8_t* bytes = [sector bytes];
                    int tag = OSReadLittleInt16(bytes, 0x00);
                    if (tag == 0x0100) {
                        /* File Set Descriptor */
                        uint32_t length = OSReadLittleInt32(bytes, 400 + 0x00);
                        uint32_t offset = OSReadLittleInt32(bytes, 400 + 0x04) & 0x3FFFFFFF;
                        if (offset >= volume.length || (length & 0xFFFFFE00) != length || (offset + (length >> 11)) >= volume.length) {
                            break;
                        } else if (0x00 != OSReadLittleInt16(bytes, 400 + 0x08)) {
                            break;
                        }
                        [cache setObject:[UDFDiscInformationControlBlock informationControlBlockWithOffset:offset length:length >> 11 isFolder:YES version:0] forKey:@""];
                        foundFileSetDescriptor = YES;
                        break;
                    } else if (tag == 0x08) {
                        break;
                    }
                }
            }
            if (!foundFileSetDescriptor) {
                [NSException raise:UDFReaderErrorException format:@"Unable to locate root folder."];
            } else if (error) {
                [NSException raise:UDFReaderErrorException format:@"%s", strerror([error code])];
            }
        } @catch (id exception) {
            [self close];
            [exception raise];
        }
        closed = NO;
    }
    return self;
}

- (void) dealloc
{
    [self close];
    [pathToDevice release], pathToDevice = nil;
    [lockPicks release], lockPicks = nil;
    [cache release], cache = nil;
    [titleKeysByPath release], titleKeysByPath = nil;
    [volume.descriptor release], volume.descriptor = nil;
    [discKey release], discKey = nil;
    [super dealloc];
}

- (void) close
{
    if (!closed) {
        closed = YES;
        [lockPicks cancelAllOperations];
        [lockPicks waitUntilAllOperationsAreFinished];
        if (fileDescriptor >= 0) {
            close(fileDescriptor);
        }
    }
}

- (NSString*) volumeDescription
{
    NSAssert(!closed, @"This UDFReader is closed.");
    return [[volume.descriptor retain] autorelease];
}

- (NSData*) discKey
{
    NSAssert(!closed, @"This UDFReader is closed.");
    if (encryptedDiscKey) {
        if (!discKey) {
            [lockPicks waitUntilAllOperationsAreFinished];
        }
        if (!discKey) {
            [NSException raise:UDFReaderErrorException format:@"Unable to determine disc-key."];
        }
        return [[discKey retain] autorelease];
    } else {
        return nil;
    }
}

- (BOOL) fileExistsAtPath:(NSString*)path
{
    NSAssert(!closed, @"This UDFReader is closed.");
    NSAssert(path, @"Shouldn't be nil");
    return [self fileExistsAtPath:path isDirectory:nil];
}

- (BOOL) fileExistsAtPath:(NSString*)path isDirectory:(BOOL*)isDirectory
{
    NSAssert(!closed, @"This UDFReader is closed.");
    id object;
    if (object = [self objectAtPath:path]) {
        if (isDirectory) {
            *isDirectory = [object isKindOfClass:[NSDictionary class]] || ([object isKindOfClass:[UDFDiscInformationControlBlock class]] && [object isFolder]);
        }
        return YES;
    } else {
        return NO;
    }
}

- (NSArray*) directoryContentsAtPath:(NSString*)path
{
    NSAssert(!closed, @"This UDFReader is closed.");
    id object;
    if (object = [self objectAtPath:path]) {
        if ([object isKindOfClass:[UDFDiscInformationControlBlock class]]) {
            if ([object isFolder]) {
                object = [self resolveObjectFromInformationControlBlock:object atPath:path];
            } else {
                return nil;
            }
        }
        return [[object allKeys] sortedArrayUsingSelector:@selector(caseInsensitiveCompare:)];
    } else {
        return nil;
    }
}

- (UDFFileHandle*) fileHandleForReadingAtPath:(NSString*)path
{
    NSAssert(!closed, @"This UDFReader is closed.");
    return [[[UDFFileHandle alloc] initWithReader:self path:path] autorelease];
}

@end


#pragma mark -
@implementation UDFReader (Internal)

- (id) objectAtPath:(NSString*)path
{
    NSAssert(path, @"Shouldn't be nil");
    if (NSNotFound != [path rangeOfString:@"/" options:NSAnchoredSearch].location) {
        path = [path substringFromIndex:1];
    } else {
        [NSException raise:UDFReaderErrorException format:@"Paths must begin with a \"/\""];
    }
    if (NSNotFound != [path rangeOfString:@"/" options:NSAnchoredSearch | NSBackwardsSearch].location && [path length] > 0) {
        path = [path substringToIndex:[path length] - 1];
    }
    id object;
    BOOL mustResolve;
    @synchronized (cache) {
        if (mustResolve = (nil == (object = [cache objectForKey:path]))) {
            object = [cache objectForKey:@""];
        }
    }
    if (mustResolve) {
        NSString* partialPath = @"";
        static NSCharacterSet* separator = nil;
        if (!separator) {
            @synchronized ([UDFReader class]) {
                if (!separator) {
                    separator = [[NSCharacterSet characterSetWithCharactersInString:@"/"] retain];
                }
            }
        }
        for (NSString* name in [path componentsSeparatedByCharactersInSet:separator]) {
            if ([object isKindOfClass:[UDFDiscInformationControlBlock class]]) {
                if (![object isFolder]) {
                    object = nil;
                    break;
                }
                object = [self resolveObjectFromInformationControlBlock:object atPath:partialPath];
            }
            @synchronized (object) {
                object = [object objectForKey:name];
            }
            if (!object) {
                break;
            }
            partialPath = [partialPath stringByAppendingPathComponent:name];
        }
        @synchronized (cache) {
            if (object != [cache objectForKey:path]) {
                [cache setObject:object forKey:path];
            }
        }
    }
    return object;
}

- (UDFDiscExtent*) readExtentFromInformationControlBlock:(UDFDiscInformationControlBlock*)icb
{
    NSError* error;
    NSData* data = [self readLogicalSectors:[icb length] at:[icb offset] error:&error];
    if (!data || error) {
        [NSException raise:UDFReaderErrorException format:@"%s", strerror(errno)];
    }
    for (uint8_t* b = (void*)[data bytes], *l = b + [data length]; b < l; b += 2048) {
        uint16_t tag = OSReadLittleInt16(b, 0);
        if (8 == tag) {
            break;
        } else if (261 == tag) {
            if ((0x04 == b[27]) != [icb isFolder]) {
                [NSException raise:UDFReaderErrorException format:@"ICB at block %d is not a %@.",  [icb offset], [icb isFolder] ? @"folder" : @"file"];
            }
            uint16_t flags = OSReadLittleInt16(b, 34);
            uint64_t fileLength = OSReadLittleInt64(b, 56);
            uint32_t ea = OSReadLittleInt32(b, 168);
            uint32_t ad = OSReadLittleInt32(b, 172);
            for (uint8_t* p = b + 176 + ea, *pl = p + ad, f = flags & 0x07; p < pl;) {
                if (0 == f) {
                    uint64_t length = OSReadLittleInt32(p, 0x00);
                    uint32_t lengthInSectors = (length >> 11) + ((length & 0x07FF) > 0);
                    uint32_t offset = OSReadLittleInt32(p, 0x04);
                    if (fileLength != length) {
                        [NSException raise:UDFReaderErrorException format:@"ICB at block %d contains an invalid extent.",  [icb offset]];
                    } 
                    return [UDFDiscExtent extentWithOffset:offset length:lengthInSectors lengthInBytes:length];
                } else {
                    [NSException raise:UDFReaderErrorException format:@"ICB at block %d contains an extent type that is not expected.",  [icb offset]];
                }
            }
        }

    }
    return (id)[NSNull null];
}

- (id) resolveObjectFromInformationControlBlock:(UDFDiscInformationControlBlock*)icb atPath:(NSString*)path
{
    id object;
    if (nil == (object = [icb replacedByObject])) {
        @synchronized (icb) {
            if (nil == (object = [icb replacedByObject])) {
                UDFDiscExtent* extent = [self readExtentFromInformationControlBlock:icb];

                if ([icb isFolder]) {
                    NSError* error;
                    NSData* folderData = [self readLogicalSectors:[extent length] at:[extent offset] error:&error];
                    if (!folderData) {
                        [NSException raise:UDFReaderErrorException format:@"Unable to read folder data."];
                    } else if (error) {
                        [NSException raise:UDFReaderErrorException format:@"%s", strerror([error code])];
                    }
                    folderData = [folderData subdataWithRange:NSMakeRange(0, [extent lengthInBytes])];
      
                    object = [NSMutableDictionary dictionary];
                    for (uint8_t* p = (void*)[folderData bytes], *pl = p + [folderData length]; p < pl; ) {
                        uint16_t tag = OSReadLittleInt16(p, 0);
                        if (!tag) {
                            break;
                        } else if (257 == tag) {
                            BOOL isFolder = (p[18] & 0x02) > 0;
                            uint8_t L_FI = p[19];
                            uint16_t version = OSReadLittleInt16(p, 16);
                            uint32_t length = OSReadLittleInt32(p, 20 + 0x00);
                            uint32_t offset = OSReadLittleInt32(p, 20 + 0x04) & 0x3FFFFFFF;
                            if (offset >= volume.length || (length & 0xFFFFFE00) != length || (offset + (length >> 11)) >= volume.length) {
                                [NSException raise:UDFReaderErrorException format:@"File-Information structure is corrupt.",  [icb offset]];
                            }
                            uint16_t L_IU = OSReadLittleInt16(p, 36);
                            if (L_FI) {
                                NSString* fileName = [[[NSString alloc] initWithData:[NSData dataWithBytes:(void*)(p + 38 + L_IU + 1) length:L_FI-1] encoding:(p[38 + L_IU] == 8 ? NSUTF8StringEncoding : NSUTF16LittleEndianStringEncoding)] autorelease];
                                UDFDiscInformationControlBlock* prevICB = [object objectForKey:fileName];
                                if (!prevICB || version > prevICB.version) {
                                    [object setObject:[UDFDiscInformationControlBlock informationControlBlockWithOffset:offset length:length >> 11 isFolder:isFolder version:version] forKey:fileName];
                                }
                            }
                            p += 4 * ((38 + L_FI + L_IU + 3) / 4);
                        } else {
                            [NSException raise:UDFReaderErrorException format:@"Unable interpret folder data."];
                        }
                    }
                } else {
                    object = extent;
                }

                [icb setReplacedByObject:object];
                @synchronized (cache) {
                    [cache setObject:object forKey:path];
                }
            }
        }
    }
    return object;
}

- (NSData*) readRawSectorAt:(off_t)offset error:(NSError**)error
{
    return [self readRawSectors:1 at:offset error:error];
}

- (NSData*) readRawSectors:(uint32_t)count at:(off_t)offset error:(NSError**)error
{
    uint32_t length = 0x0800 * count;
    uint8_t* bytes = malloc(length);
    if (!bytes) {
        return nil;
    }
    NSError* errorToReturn = nil;
    off_t offsetInBytes = ((off_t)offset) << 11;
    int result = pread(fileDescriptor, bytes, length, offsetInBytes);
    if (result != length) {
        errorToReturn = [NSError errorWithDomain:NSPOSIXErrorDomain code:errno userInfo:nil];
        perror(NULL);
    }
    if (error) {
        *error = errorToReturn;
    }
    if (errorToReturn) {
        free(bytes);
        return nil;
    } else {
        return [NSMutableData dataWithBytesNoCopy:bytes length:length];
    }
}

- (NSData*) readLogicalSectorAt:(off_t)offset error:(NSError**)error
{
    NSAssert((offset >= 0) && (offset < volume.length), @"readLogicalSectorAt:error:"); 
    return [self readRawSectorAt:offset + volume.offset error:error];
}

- (NSData*) readLogicalSectors:(uint32_t)count at:(off_t)offset error:(NSError**)error
{
    if (!((offset >= 0) && (offset < volume.length) && ((offset+count) <= volume.length))) {
        NSLog(@"52");
    }
    //NSAssert((offset >= 0) && (offset < volume.length) && ((offset+count) <= volume.length), @"readLogicalSectors:at:error:"); 
    return [self readRawSectors:count at:offset + volume.offset error:error];
}

- (NSData*) determineTitleKeyForLogicalOffset:(off_t)offset
{
    off_t rawDeviceOffset = volume.offset + offset;
    NSData* key = nil;
    if (isProtected) {
        /*  Ask the drive for an encrypted title-key for the file's
         *  raw device offset.
         */
        dvd_key_t busKey;
        dvd_key_t encryptedTitleKey;
        BOOL authenticated = NO;
        int agid = 0;
        int tries = 3;
        do {
            for (int i = 0; !authenticated && i < 10; i++) {
                @try {
                    agid = reportAGID(fileDescriptor, i);
                    determineBusKey(fileDescriptor, agid, busKey);
                    readTitleKey(fileDescriptor, agid, rawDeviceOffset, encryptedTitleKey);
                    if (1 == reportAuthStatusFlag(fileDescriptor, i)) {
                        authenticated = YES;
                    }
                } @catch (id exception) {
                    invalidateAGID(fileDescriptor, i);
                }
            }
        } while (--tries > 0 && !authenticated);
        
        if (tries == 0 || !authenticated) {
            dvd_key_t busKey;
            dvd_disckey_t encryptedDiscKeyBytes;
            BOOL authenticated = NO;
            int agid = 0;
            for (int i = 0; !authenticated && i < 10; i++) {
                @try {
                    agid = reportAGID(fileDescriptor, i);
                    determineBusKey(fileDescriptor, agid, busKey);
                    readDiscKey(fileDescriptor, agid, encryptedDiscKeyBytes);
                    if (1 == reportAuthStatusFlag(fileDescriptor, i)) {
                        authenticated = YES;
                    }
                } @catch (id exception) {
                    invalidateAGID(fileDescriptor, i);
                }
            }
            return nil;
        }
        
        /*  Mash up the encrypted title-key with the bus-key.  
         */
        for (int i = 0 ; i < DVD_KEY_SIZE; i++) {
            encryptedTitleKey[i] ^= busKey[DVD_KEY_SIZE - 1 - (i % DVD_KEY_SIZE)];
        }
        
        if ((0 != encryptedTitleKey[0])
            || (0 != encryptedTitleKey[1])
            || (0 != encryptedTitleKey[2])
            || (0 != encryptedTitleKey[3])
            || (0 != encryptedTitleKey[4])
        ) {
            /*  Decrypt the title-key using the disc-key.
             */
            NSMutableData* titleKey = [NSMutableData dataWithLength:DVD_KEY_SIZE];
            CSS_decryptKey(0xFF, [[self discKey] bytes], encryptedTitleKey, [titleKey mutableBytes]);
            key = titleKey;
        }
    }
    return key;
}

- (NSData*) determineTitleKeyForLogicalOffset:(off_t)offset usingData:(NSData*)data
{
    const uint8_t* bytes = [data bytes];
    for (const uint8_t* p = bytes, *pl = p + [data length]; p < pl; p += 2048) {
        uint8_t y = 0x14 + (p[13] & 0x07);
        if (((p[y] >> 4) & 0x03) == 1) {
            dvd_key_t encryptedTitleKey;
            if (CSS_exploitPattern(p, encryptedTitleKey)) {
                return [NSData dataWithBytes:encryptedTitleKey length:sizeof(encryptedTitleKey)];
            }
        }
    }
    return nil;
}

- (void) lockPickOperation:(UDFLockPickOperation*)lpi foundKey:(NSData*)key
{
    if (!discKey) {
        @synchronized (self) {
            if (!discKey) {
                discKey = [key retain];
                [lockPicks cancelAllOperations];
#ifdef DEBUG 
                const uint8_t* k = [key bytes];
                NSLog(@"%@ -- Found Key: %02x:%02x:%02x:%02x:%02x", self, k[0], k[1], k[2], k[3], k[4]);
#endif
            }
        }
    }
}

@end


#pragma mark -
@implementation UDFFileHandle

@synthesize titleKey;
@synthesize path;

- (id) initWithReader:(UDFReader*)_reader path:(NSString*)_path
{
    if (self = [super init]) {
        NSAssert(!_reader.closed, @"This UDFReader is closed.");
        reader = [_reader retain];
        path = [_path retain];
        id object;
        if (object = [reader objectAtPath:path]) {
            if ([object isKindOfClass:[UDFDiscInformationControlBlock class]]) {
                if ([object isFolder]) {
                    return nil;
                } else {
                    object = [reader resolveObjectFromInformationControlBlock:object atPath:path];
                }
            }
        } else {
            return nil;
        }
        if (object != [NSNull null]) {
            extent = [object retain];
        }
        scanForEncryption = [_path hasPrefix:@"/VIDEO_TS/"] && (NSOrderedSame == [[_path pathExtension] compare:@"VOB"]);
    }
    return self;
}

- (void) dealloc
{
    [extent release], extent = nil;
    [titleKey release], titleKey = nil;
    [reader release], reader = nil;
    [path release], path = nil;
    [super dealloc];
}

- (uint64_t) size
{
    NSAssert(!reader.closed, @"The underlying UDFReader is closed.");
    return [extent lengthInBytes];
}

- (void) seekToFileOffset:(unsigned long long)offset
{
    NSAssert(!reader.closed, @"The underlying UDFReader is closed.");
    if (offset >= [extent lengthInBytes]) {
        [NSException raise:UDFReaderErrorException format:@"%s(%d)", __FILE__, __LINE__];
    }
    position = offset;
}

- (NSData*) readDataOfLength:(uint32_t)length
{
    NSAssert(!reader.closed, @"The underlying UDFReader is closed.");
    NSMutableData* data = nil;
    uint32_t offset;
    off_t firstSector;
    uint32_t sectors;
    NSError* error = nil;
    @synchronized (extent) {
        offset = position & 0x7FF;
        sectors = ((offset + length) >> 11) + (((offset + length) & 0x7FF) > 0);
        firstSector = [extent offset] + (position >> 11);
        data = (NSMutableData*)[reader readLogicalSectors:sectors at:firstSector error:&error];
        if (error) {
            data = nil;
        } else {
            position += length;
        }
    }
    if (data && (titleKey || scanForEncryption)) {
        uint8_t* const bytes = [data mutableBytes];
        BOOL hitEncryptedData = NO;
        for (uint8_t* p = bytes, *pl = p + [data length]; !titleKey && p < pl; p += 2048) {
            uint8_t y = 0x14 + (p[13] & 0x07);
            if (((p[y] >> 4) & 0x03) == 1) {
                hitEncryptedData = YES;
                uint32_t logicalOffset = firstSector + ((p - bytes) >> 11);
                if (nil == (titleKey = [reader determineTitleKeyForLogicalOffset:logicalOffset usingData:data])) {
                    if (nil == (titleKey = [reader determineTitleKeyForLogicalOffset:logicalOffset])) {
                        firstSector += sectors;
                        uint32_t limit = [extent length] - (offset + sectors);
                        while (limit > 0 && !titleKey && !error) {
                            if (sectors > limit) {
                                sectors = limit;
                            }
                            
                            NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
                            NSData* moreData = [[reader readLogicalSectors:sectors at:firstSector error:&error] retain];
                            [pool release];
                            if (!moreData) {
                                break;
                            } else if (error) {
                                [moreData release], moreData = nil;
                                break;
                            }

                            titleKey = [reader determineTitleKeyForLogicalOffset:logicalOffset usingData:moreData];
                            [moreData release], moreData = nil;

                            firstSector += sectors;
                            limit -= sectors;
                        }
                    }
                    break;
                }
            }
        }
        if (titleKey) {
            data = [UDFSelfDecryptingData dataWithMutableData:data titleKey:titleKey];
            [titleKey retain];
        } else if (hitEncryptedData) {
            [NSException raise:UDFReaderUnableToRecoverKeyException format:@"Unable to recover the title key for %@.", path];
        }
    }
    return (offset == 0 && length == [data length]) ? data : [data subdataWithRange:NSMakeRange(offset, length)];
}

- (NSData*) readDataToEndOfFile
{
    NSAssert(!reader.closed, @"The underlying UDFReader is closed.");
    NSMutableData* data = nil;
    uint32_t offset;
    uint32_t length;
    off_t firstSector;
    @synchronized (extent) {
        NSError* error;
        offset = position & 0x7FF;
        length = [extent lengthInBytes] - position;
        uint32_t sectors = ((offset + length) >> 11) + (((offset + length) & 0x7FF) > 0);
        firstSector = [extent offset] + (position >> 11);
        data = (NSMutableData*)[reader readLogicalSectors:sectors at:firstSector error:&error];
        if (error) {
            data = nil;
        }
        position += length;
    }
    if (data && (titleKey || scanForEncryption)) {
        uint8_t* const bytes = [data mutableBytes];
        for (uint8_t* p = bytes, *pl = p + [data length]; !titleKey && p < pl; p += 2048) {
            if ((p[0x14] & 0x30) && !titleKey) {
                if (nil == (titleKey = [reader determineTitleKeyForLogicalOffset:firstSector + ((p - bytes) >> 11) usingData:data])) {
                    titleKey = [reader determineTitleKeyForLogicalOffset:firstSector + ((p - bytes) >> 11)];
                    break;
                }
            }
        }
        if (titleKey) {
            data = [UDFSelfDecryptingData dataWithMutableData:data titleKey:titleKey];
            [titleKey retain];
        } else {
            [NSException raise:UDFReaderAuthenticationException format:@"Unable to authenticate drive."];
        }
    }
    return (offset == 0 && length == [data length]) ? data : [data subdataWithRange:NSMakeRange(offset, length)];
}

- (uint64_t) offsetInFile
{
    return position;
}

@end


#pragma mark -
@implementation UDFDiscInformationControlBlock
@synthesize offset;
@synthesize length;
@synthesize replacedByObject;
@synthesize isFolder;
@synthesize version;

- (id) initWithOffset:(off_t)_offset length:(uint32_t)_length isFolder:(BOOL)_isFolder version:(uint16_t)_version
{
    if (self = [super init]) {
        offset = _offset;
        length = _length;
        isFolder = _isFolder;
        version = _version;
    }
    return self;
} 

+ (id) informationControlBlockWithOffset:(off_t)_offset length:(uint32_t)_length isFolder:(BOOL)_isFolder version:(uint16_t)_version
{
    return [[[UDFDiscInformationControlBlock alloc] initWithOffset:_offset length:_length isFolder:_isFolder version:_version] autorelease];
}

- (void) dealloc
{
    [super dealloc];
}

@end


#pragma mark -
@implementation UDFDiscExtent 
@synthesize offset;
@synthesize length;
@synthesize lengthInBytes;

- (id) initWithOffset:(off_t)_offset length:(uint32_t)_length lengthInBytes:(uint32_t)_lengthInBytes
{
    if (self = [super init]) {
        offset = _offset;
        length = _length;
        lengthInBytes = _lengthInBytes;
    }
    return self;
} 

+ (id) extentWithOffset:(off_t)_offset length:(uint32_t)_length lengthInBytes:(uint64_t)_lengthInBytes
{
    return [[[UDFDiscExtent alloc] initWithOffset:_offset length:(uint32_t)_length lengthInBytes:_lengthInBytes] autorelease];
}

@end


#pragma mark -

void reportPhysicalFormatInfo(int fd, int *numberOfLayers, off_t* blockCount)
{
    dk_dvd_read_structure_t dvd;
    DVDPhysicalFormatInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDStructureFormatPhysicalFormatInfo;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    if (0 != ioctl(fd, DKIOCDVDREADSTRUCTURE, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"reportPhysicalFormatInfo: %s", strerror(errno)]; 
    }
    off_t bc = 0;
    if (0 != ioctl(fd, DKIOCGETBLOCKCOUNT, &bc) || bc == 0) {
        [NSException raise:UDFReaderErrorException format:@"reportPhysicalFormatInfo: %s", strerror(errno)]; 
    }
    *numberOfLayers = dvdbs.numberOfLayers;
    *blockCount = bc;
}

int checkCopyProtectionAndRegionMask(int fd, int* regionMask)
{
    dk_dvd_read_structure_t dvd;
    DVDCopyrightInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDStructureFormatCopyrightInfo;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    if (0 != ioctl(fd, DKIOCDVDREADSTRUCTURE, &dvd)) {
        *regionMask = 0;
        return 0;
    } else {
        *regionMask = dvdbs.regionMask;
        return dvdbs.copyrightProtectionSystemType;
    }
}

int reportAGID(int fd, int agid)
{
    dk_dvd_report_key_t dvd;
    DVDAuthenticationGrantIDInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatAGID_CSS;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    dvd.keyClass = kDVDKeyClassCSS_CPPM_CPRM;
    if (0 != ioctl(fd, DKIOCDVDREPORTKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"reportAGID: %s", strerror(errno)]; 
    }
    return dvdbs.grantID;
}

void sendChallenge(int fd, int agid, uint8_t *challenge)
{
    dk_dvd_send_key_t dvd;
    DVDChallengeKeyInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatChallengeKey;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    dvd.keyClass = kDVDKeyClassCSS_CPPM_CPRM;
    dvdbs.dataLength[1] = 0x0E;
    for (int i = 0; i < DVD_CHALLENGE_SIZE; i++) {
        dvdbs.challengeKeyValue[DVD_CHALLENGE_SIZE - i - 1] = challenge[i];
    }
    if (0 != ioctl(fd, DKIOCDVDSENDKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"sendChallenge: %s", strerror(errno)]; 
    }
}

BOOL invalidateAGID(int fd, int agid)
{
    dk_dvd_send_key_t dvd;
    DVDAuthenticationGrantIDInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatAGID_Invalidate;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    return 0 == ioctl(fd, DKIOCDVDSENDKEY, &dvd);
}

void reportKey1(int fd, int agid, uint8_t* key)
{
    dk_dvd_report_key_t dvd;
    DVDKey1Info dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatKey1;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    if (0 != ioctl(fd, DKIOCDVDREPORTKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"reportKey1: %s", strerror(errno)]; 
    }
    for (int i = 0; i < DVD_KEY_SIZE; i++) {
        key[DVD_KEY_SIZE - i - 1] = dvdbs.key1Value[i];
    }
}

void reportChallenge(int fd, int agid, uint8_t* challenge)
{
    dk_dvd_report_key_t dvd;
    DVDChallengeKeyInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatChallengeKey;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    if (0 != ioctl(fd, DKIOCDVDREPORTKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"reportChallenge: %s", strerror(errno)]; 
    }
    for (int i = 0; i < DVD_CHALLENGE_SIZE; i++) {
        challenge[DVD_CHALLENGE_SIZE - i - 1] = dvdbs.challengeKeyValue[i];
    }
}

void sendKey2(int fd, int agid, uint8_t* key)
{
    dk_dvd_send_key_t dvd;
    DVDKey2Info dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatKey2;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    dvd.keyClass = kDVDKeyClassCSS_CPPM_CPRM;
    dvdbs.dataLength[1] = 0x0A;
    for (int i = 0; i < DVD_KEY_SIZE; i++) {
        dvdbs.key2Value[DVD_KEY_SIZE - i - 1] = key[i];
    }
    if (0 != ioctl(fd, DKIOCDVDSENDKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"sendKey2: %s", strerror(errno)]; 
    }
}

void readDiscKey(int fd, int agid, uint8_t *key)
{
    dk_dvd_read_structure_t dvd;
    DVDDiscKeyInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDStructureFormatDiscKeyInfo;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    if (0 != ioctl(fd, DKIOCDVDREADSTRUCTURE, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"readDiskKey: %s", strerror(errno)]; 
    }
    memcpy(key, dvdbs.discKeyStructures, DVD_DISCKEY_SIZE);
}

int reportAuthStatusFlag(int fd, int agid)
{
    dk_dvd_report_key_t dvd;
    DVDAuthenticationSuccessFlagInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatASF;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.grantID = agid;
    if (0 != ioctl(fd, DKIOCDVDREPORTKEY, &dvd)) {
        return 0;
    } else {
        return dvdbs.successFlag;
    }
}

void readTitleKey(int fd, int agid, daddr_t rawDeviceOffset, uint8_t *key)
{
    dk_dvd_report_key_t dvd;
    DVDTitleKeyInfo dvdbs; 
    memset(&dvd, 0, sizeof(dvd));
    memset(&dvdbs, 0, sizeof(dvdbs));
    dvd.format = kDVDKeyFormatTitleKey;
    dvd.buffer = &dvdbs; 
    dvd.bufferLength = sizeof(dvdbs);
    dvd.address = rawDeviceOffset;
    dvd.grantID = agid;
    dvd.keyClass = kDVDKeyClassCSS_CPPM_CPRM;
    if (0 != ioctl(fd, DKIOCDVDREPORTKEY, &dvd)) {
        [NSException raise:UDFReaderErrorException format:@"readTitleKey: %s", strerror(errno)]; 
    }
    memcpy(key, dvdbs.titleKeyValue, DVD_KEY_SIZE);
}

void determineBusKey(int fd, int agid, uint8_t* busKey) 
{
    uint8_t p_challenge[DVD_CHALLENGE_SIZE];
    for (int i = 0; i < DVD_CHALLENGE_SIZE; i++) {
        p_challenge[i] = random();
    }
    sendChallenge(fd, agid, p_challenge);

    dvd_key_t key1;
    reportKey1(fd, agid, key1);

    int8_t variant = -1;
    for (int i = 0; i < 32; i++) {
        dvd_key_t p_key_check;
        CSS_key1(i, p_challenge, p_key_check);
        if (0 == memcmp(p_key_check, key1, DVD_KEY_SIZE)) {
            variant = i;
            break;
        }
    }
    if (variant == -1) {
        [NSException raise:UDFReaderErrorException format:@"determineBusKey: Unable to determine key"];
    }
    reportChallenge(fd, agid, p_challenge);

    dvd_key_t key2;
    CSS_key2(variant, p_challenge, key2);
    sendKey2(fd, agid, key2);

    memcpy(p_challenge, key1, DVD_KEY_SIZE);
    memcpy(p_challenge + DVD_KEY_SIZE, key2, DVD_KEY_SIZE);
    CSS_busKey(variant, p_challenge, busKey);
}
