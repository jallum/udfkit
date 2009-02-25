#import <Foundation/Foundation.h>

@class UDFFileHandle;

@interface UDFReader: NSObject {
    BOOL closed;
    NSString* pathToDevice;
    int fileDescriptor;
    int numberOfLayers;
    off_t rawBlockCount;
    BOOL isProtected;
    int regionMask;
    NSOperationQueue* lockPicks;
    NSData* encryptedDiscKey;
    NSData* discKey;
    struct {
        uint16_t flags;
        NSString* descriptor;
        off_t offset;
        uint32_t length;
    } volume;
    NSMutableDictionary* cache;
    NSMutableDictionary* titleKeysByPath;
}

+ (id) UDFReaderWithBSDName:(NSString*)_bsdName;

- (id) initWithBSDName:(NSString*)_bsdName;

@property (readonly) NSString* volumeDescription;
@property (readonly) int regionMask;
@property (readonly) NSData* discKey;
@property (readonly) int numberOfLayers;
@property (readonly) off_t rawBlockCount;
@property (readonly) BOOL closed;

- (void) close;
- (BOOL) fileExistsAtPath:(NSString*)path;
- (BOOL) fileExistsAtPath:(NSString*)path isDirectory:(BOOL*)isDirectory;
- (NSArray*) directoryContentsAtPath:(NSString*)path;
- (UDFFileHandle*) fileHandleForReadingAtPath:(NSString*)path;

@end

@class UDFDiscExtent;

@interface UDFFileHandle : NSObject {
    UDFReader* reader;
    NSString* path;
    NSData* titleKey;
    UDFDiscExtent* extent;
    uint64_t position;
    BOOL scanForEncryption;
}

@property (readonly) NSData* titleKey;
@property (readonly) NSString* path;
@property (readonly) uint64_t size;

- (NSData*) readDataOfLength:(uint32_t)length;
- (NSData*) readDataToEndOfFile;
- (void) seekToFileOffset:(unsigned long long)offset;
- (uint64_t) offsetInFile;

@end

extern NSString* const UDFReaderAuthenticationException;