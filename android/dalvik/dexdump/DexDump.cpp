/*
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * The "dexdump" tool is intended to mimic "objdump".  When possible, use
 * similar command-line arguments.
 *
 * TODO: rework the "plain" output format to be more regexp-friendly
 *
 * Differences between XML output and the "current.xml" file:
 * - classes in same package are not all grouped together; generally speaking
 *   nothing is sorted
 * - no "deprecated" on fields and methods
 * - no "value" on fields
 * - no parameter names
 * - no generic signatures on parameters, e.g. type="java.lang.Class&lt;?&gt;"
 * - class shows declared fields and methods; does not show inherited fields
 */
/* modified to output pilar by -Kui Luo   email:kuiluo@ksu.edu*/

#include "libdex/DexFile.h"
#include "libdex/CmdUtils.h"
#include "libdex/DexCatch.h"
#include "libdex/DexClass.h"
#include "libdex/DexDebugInfo.h"
#include "libdex/DexOpcodes.h"
#include "libdex/DexProto.h"
#include "libdex/InstrUtils.h"
#include "libdex/SysUtil.h"

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include <assert.h>

static const char* gProgName = "dexdump";
static char* packageName =NULL;
enum OutputFormat {
    OUTPUT_PLAIN = 0,               /* default */
    OUTPUT_XML,                     /* fancy */
};

/* command-line options */
struct Options {
    bool checksumOnly;
    bool disassemble;
    bool showFileHeaders;
    bool showSectionHeaders;
    bool ignoreBadChecksum;
    bool dumpRegisterMaps;
    OutputFormat outputFormat;
    const char* tempFileName;
    bool exportsOnly;
    bool verbose;
};
//******************* kui's modification begins  *******************
struct Op31t {
    int insnIdx;
    const DecodedInstruction* pDecInsn;
    void init(int insnIdx,const DecodedInstruction* pDecInsn){
      this->insnIdx=insnIdx;
      this->pDecInsn=pDecInsn;
    }
};
//******************* kui's modification ends  *******************
struct Options gOptions;

/* basic info about a field or method */
struct FieldMethodInfo {
    const char* classDescriptor;
    const char* name;
    const char* signature;
};

/*
 * Get 2 little-endian bytes.
 */
static inline u2 get2LE(unsigned char const* pSrc)
{
    return pSrc[0] | (pSrc[1] << 8);
}

/*
 * Get 4 little-endian bytes.
 */
static inline u4 get4LE(unsigned char const* pSrc)
{
    return pSrc[0] | (pSrc[1] << 8) | (pSrc[2] << 16) | (pSrc[3] << 24);
}

/*
 * Converts a single-character primitive type into its human-readable
 * equivalent.
 */
static const char* primitiveTypeLabel(char typeChar)
{
    switch (typeChar) {
    case 'B':   return "byte";
    case 'C':   return "char";
    case 'D':   return "double";
    case 'F':   return "float";
    case 'I':   return "int";
    case 'J':   return "long";
    case 'S':   return "short";
    case 'V':   return "void";
    case 'Z':   return "boolean";
    default:
                return "UNKNOWN";
    }
}

/*
 * Converts a type descriptor to human-readable "dotted" form.  For
 * example, "Ljava/lang/String;" becomes "java.lang.String", and
 * "[I" becomes "int[]".  Also converts '$' to '.', which means this
 * form can't be converted back to a descriptor.
 */
static char* descriptorToDot(const char* str)
{
    int targetLen = strlen(str);
    int offset = 0;
    int arrayDepth = 0;
    char* newStr;

    /* strip leading [s; will be added to end */
    while (targetLen > 1 && str[offset] == '[') {
        offset++;
        targetLen--;
    }
    arrayDepth = offset;

    if (targetLen == 1) {
        /* primitive type */
        str = primitiveTypeLabel(str[offset]);
        offset = 0;
        targetLen = strlen(str);
    } else {
        /* account for leading 'L' and trailing ';' */
        if (targetLen >= 2 && str[offset] == 'L' &&
            str[offset+targetLen-1] == ';')
        {
            targetLen -= 2;
            offset++;
        }
    }
    //******************* kui's modification begins  *******************
    newStr = (char*)malloc(targetLen +20 + arrayDepth * 2 +1);

    /* copy class name over */

    int i;
        int counter=0;
                 for (i = 0; i < targetLen; i++) {
                     char ch = str[offset + i];
                     if (ch == '/') {
                       newStr[i+counter]=':';
                       counter++;
                       newStr[i+counter]=':';
                     }
                     else newStr[i+counter]=ch;
                 }

    /* add the appropriate number of brackets for arrays */
    while (arrayDepth-- > 0) {
        newStr[i+counter] = '[';
        i++;
        newStr[i+counter] = ']';
        i++;
    }
    newStr[i+counter] = '\0';
    assert(i == targetLen + arrayDepth * 2);
    //******************* kui's modification ends  *******************
    return newStr;
}

//get only the package name
static char* getPackageName(const char* str)
{
  const char* lastSlash;
        char* newStr=(char*)malloc(100);
        char* cp;

      /* reduce to just the class name, trimming trailing ';' */
      lastSlash = strrchr(str, '/');
      if (lastSlash == NULL)
          lastSlash = str + 1;        /* start past 'L' */
      else
          lastSlash++;                /* start past '/' */
      //newStr = strncat(newStr,str,strlen(str)-strlen(lastSlash));
       for(int i=0;i<(int)strlen(str)-(int)strlen(lastSlash);i++)
       {
         newStr[i]=str[i];
       }
       newStr[strlen(str)-strlen(lastSlash)-1]=';';
       newStr[strlen(str)-strlen(lastSlash)]='\0';
      return descriptorToDot(newStr);
}
/*
 * Converts the class name portion of a type descriptor to human-readable
 * "dotted" form.
 *
 * Returns a newly-allocated string.
 */
static char* descriptorClassToDot(const char* str)
{
    const char* lastSlash;
    char* newStr;
    char* cp;

    /* reduce to just the class name, trimming trailing ';' */
    lastSlash = strrchr(str, '/');
    if (lastSlash == NULL)
        lastSlash = str + 1;        /* start past 'L' */
    else
        lastSlash++;                /* start past '/' */

    newStr = strdup(lastSlash);
    newStr[strlen(lastSlash)-1] = '\0';
    //******************* kui's modification begins *******************
   // for (cp = newStr; *cp != '\0'; cp++) {
     //   if (*cp == '$')
     //       *cp = '.';
   // }
    //******************* kui's modification ends  *******************
    return newStr;
}

/*
 * Returns a quoted string representing the boolean value.
 */
static const char* quotedBool(bool val)
{
    if (val)
        return "\"true\"";
    else
        return "\"false\"";
}

static const char* quotedVisibility(u4 accessFlags)
{
    if ((accessFlags & ACC_PUBLIC) != 0)
        return "\"public\"";
    else if ((accessFlags & ACC_PROTECTED) != 0)
        return "\"protected\"";
    else if ((accessFlags & ACC_PRIVATE) != 0)
        return "\"private\"";
    else
        return "\"package\"";
}

/*
 * Count the number of '1' bits in a word.
 */
static int countOnes(u4 val)
{
    int count = 0;

    val = val - ((val >> 1) & 0x55555555);
    val = (val & 0x33333333) + ((val >> 2) & 0x33333333);
    count = (((val + (val >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;

    return count;
}

/*
 * Flag for use with createAccessFlagStr().
 */
enum AccessFor {
    kAccessForClass = 0, kAccessForMethod = 1, kAccessForField = 2,
    kAccessForMAX
};

/*
 * Create a new string with human-readable access flags.
 *
 * In the base language the access_flags fields are type u2; in Dalvik
 * they're u4.
 */
static char* createAccessFlagStr(u4 flags, AccessFor forWhat)
{
#define NUM_FLAGS   18
    static const char* kAccessStrings[kAccessForMAX][NUM_FLAGS] = {
        {
            /* class, inner class */
            "PUBLIC",           /* 0x0001 */
            "PRIVATE",          /* 0x0002 */
            "PROTECTED",        /* 0x0004 */
            "STATIC",           /* 0x0008 */
            "FINAL",            /* 0x0010 */
            "?",                /* 0x0020 */
            "?",                /* 0x0040 */
            "?",                /* 0x0080 */
            "?",                /* 0x0100 */
            "INTERFACE",        /* 0x0200 */
            "ABSTRACT",         /* 0x0400 */
            "?",                /* 0x0800 */
            "SYNTHETIC",        /* 0x1000 */
            "ANNOTATION",       /* 0x2000 */
            "ENUM",             /* 0x4000 */
            "?",                /* 0x8000 */
            "VERIFIED",         /* 0x10000 */
            "OPTIMIZED",        /* 0x20000 */
        },
        {
            /* method */
            "PUBLIC",           /* 0x0001 */
            "PRIVATE",          /* 0x0002 */
            "PROTECTED",        /* 0x0004 */
            "STATIC",           /* 0x0008 */
            "FINAL",            /* 0x0010 */
            "SYNCHRONIZED",     /* 0x0020 */
            "BRIDGE",           /* 0x0040 */
            "VARARGS",          /* 0x0080 */
            "NATIVE",           /* 0x0100 */
            "?",                /* 0x0200 */
            "ABSTRACT",         /* 0x0400 */
            "STRICT",           /* 0x0800 */
            "SYNTHETIC",        /* 0x1000 */
            "?",                /* 0x2000 */
            "?",                /* 0x4000 */
            "MIRANDA",          /* 0x8000 */
            "CONSTRUCTOR",      /* 0x10000 */
            "DECLARED_SYNCHRONIZED", /* 0x20000 */
        },
        {
            /* field */
            "PUBLIC",           /* 0x0001 */
            "PRIVATE",          /* 0x0002 */
            "PROTECTED",        /* 0x0004 */
            "STATIC",           /* 0x0008 */
            "FINAL",            /* 0x0010 */
            "?",                /* 0x0020 */
            "VOLATILE",         /* 0x0040 */
            "TRANSIENT",        /* 0x0080 */
            "?",                /* 0x0100 */
            "?",                /* 0x0200 */
            "?",                /* 0x0400 */
            "?",                /* 0x0800 */
            "SYNTHETIC",        /* 0x1000 */
            "?",                /* 0x2000 */
            "ENUM",             /* 0x4000 */
            "?",                /* 0x8000 */
            "?",                /* 0x10000 */
            "?",                /* 0x20000 */
        },
    };
    const int kLongest = 21;        /* strlen of longest string above */
    int i, count;
    char* str;
    char* cp;

    /*
     * Allocate enough storage to hold the expected number of strings,
     * plus a space between each.  We over-allocate, using the longest
     * string above as the base metric.
     */
    count = countOnes(flags);
    cp = str = (char*) malloc(count * (kLongest+1) +1);
    for (i = 0; i < NUM_FLAGS; i++) {
        if (flags & 0x01) {
            const char* accessStr = kAccessStrings[forWhat][i];
            int len = strlen(accessStr);
            if (cp != str)
                {
              *cp++ = '_';
                }

            memcpy(cp, accessStr, len);
            cp += len;
        }
        flags >>= 1;
    }
    *cp = '\0';

    return str;
}


/*
 * Copy character data from "data" to "out", converting non-ASCII values
 * to printf format chars or an ASCII filler ('.' or '?').
 *
 * The output buffer must be able to hold (2*len)+1 bytes.  The result is
 * NUL-terminated.
 */
static void asciify(char* out, const unsigned char* data, size_t len)
{
    while (len--) {
        if (*data < 0x20) {
            /* could do more here, but we don't need them yet */
            switch (*data) {
            case '\0':
                *out++ = '\\';
                *out++ = '0';
                break;
            case '\n':
                *out++ = '\\';
                *out++ = 'n';
                break;
            default:
                *out++ = '.';
                break;
            }
        } else if (*data >= 0x80) {
            *out++ = '?';
        } else {
            *out++ = *data;
        }
        data++;
    }
    *out = '\0';
}

/*
 * Dump the file header.
 */
void dumpFileHeader(const DexFile* pDexFile)
{
    const DexOptHeader* pOptHeader = pDexFile->pOptHeader;
    const DexHeader* pHeader = pDexFile->pHeader;
    char sanitized[sizeof(pHeader->magic)*2 +1];

    assert(sizeof(pHeader->magic) == sizeof(pOptHeader->magic));

    if (pOptHeader != NULL) {
        printf("Optimized DEX file header:\n");

        asciify(sanitized, pOptHeader->magic, sizeof(pOptHeader->magic));
        printf("magic               : '%s'\n", sanitized);
        printf("dex_offset          : %d (0x%06x)\n",
            pOptHeader->dexOffset, pOptHeader->dexOffset);
        printf("dex_length          : %d\n", pOptHeader->dexLength);
        printf("deps_offset         : %d (0x%06x)\n",
            pOptHeader->depsOffset, pOptHeader->depsOffset);
        printf("deps_length         : %d\n", pOptHeader->depsLength);
        printf("opt_offset          : %d (0x%06x)\n",
            pOptHeader->optOffset, pOptHeader->optOffset);
        printf("opt_length          : %d\n", pOptHeader->optLength);
        printf("flags               : %08x\n", pOptHeader->flags);
        printf("checksum            : %08x\n", pOptHeader->checksum);
        printf("\n");
    }

    printf("DEX file header:\n");
    asciify(sanitized, pHeader->magic, sizeof(pHeader->magic));
    printf("magic               : '%s'\n", sanitized);
    printf("checksum            : %08x\n", pHeader->checksum);
    printf("signature           : %02x%02x...%02x%02x\n",
        pHeader->signature[0], pHeader->signature[1],
        pHeader->signature[kSHA1DigestLen-2],
        pHeader->signature[kSHA1DigestLen-1]);
    printf("file_size           : %d\n", pHeader->fileSize);
    printf("header_size         : %d\n", pHeader->headerSize);
    printf("link_size           : %d\n", pHeader->linkSize);
    printf("link_off            : %d (0x%06x)\n",
        pHeader->linkOff, pHeader->linkOff);
    printf("string_ids_size     : %d\n", pHeader->stringIdsSize);
    printf("string_ids_off      : %d (0x%06x)\n",
        pHeader->stringIdsOff, pHeader->stringIdsOff);
    printf("type_ids_size       : %d\n", pHeader->typeIdsSize);
    printf("type_ids_off        : %d (0x%06x)\n",
        pHeader->typeIdsOff, pHeader->typeIdsOff);
    printf("field_ids_size      : %d\n", pHeader->fieldIdsSize);
    printf("field_ids_off       : %d (0x%06x)\n",
        pHeader->fieldIdsOff, pHeader->fieldIdsOff);
    printf("method_ids_size     : %d\n", pHeader->methodIdsSize);
    printf("method_ids_off      : %d (0x%06x)\n",
        pHeader->methodIdsOff, pHeader->methodIdsOff);
    printf("class_defs_size     : %d\n", pHeader->classDefsSize);
    printf("class_defs_off      : %d (0x%06x)\n",
        pHeader->classDefsOff, pHeader->classDefsOff);
    printf("data_size           : %d\n", pHeader->dataSize);
    printf("data_off            : %d (0x%06x)\n",
        pHeader->dataOff, pHeader->dataOff);
    printf("\n");
}

/*
 * Dump the "table of contents" for the opt area.
 */
void dumpOptDirectory(const DexFile* pDexFile)
{
    const DexOptHeader* pOptHeader = pDexFile->pOptHeader;
    if (pOptHeader == NULL)
        return;

    printf("OPT section contents:\n");

    const u4* pOpt = (const u4*) ((u1*) pOptHeader + pOptHeader->optOffset);

    if (*pOpt == 0) {
        printf("(1.0 format, only class lookup table is present)\n\n");
        return;
    }

    /*
     * The "opt" section is in "chunk" format: a 32-bit identifier, a 32-bit
     * length, then the data.  Chunks start on 64-bit boundaries.
     */
    while (*pOpt != kDexChunkEnd) {
        const char* verboseStr;

        u4 size = *(pOpt+1);

        switch (*pOpt) {
        case kDexChunkClassLookup:
            verboseStr = "class lookup hash table";
            break;
        case kDexChunkRegisterMaps:
            verboseStr = "register maps";
            break;
        default:
            verboseStr = "(unknown chunk type)";
            break;
        }

        printf("Chunk %08x (%c%c%c%c) - %s (%d bytes)\n", *pOpt,
            *pOpt >> 24, (char)(*pOpt >> 16), (char)(*pOpt >> 8), (char)*pOpt,
            verboseStr, size);

        size = (size + 8 + 7) & ~7;
        pOpt += size / sizeof(u4);
    }
    printf("\n");
}

/*
 * Dump a class_def_item.
 */
void dumpClassDef(DexFile* pDexFile, int idx)
{
    const DexClassDef* pClassDef;
    const u1* pEncodedData;
    DexClassData* pClassData;

    pClassDef = dexGetClassDef(pDexFile, idx);
    pEncodedData = dexGetClassData(pDexFile, pClassDef);
    pClassData = dexReadAndVerifyClassData(&pEncodedData, NULL);

    if (pClassData == NULL) {
        fprintf(stderr, "Trouble reading class data\n");
        return;
    }

    printf("Class #%d header:\n", idx);
    printf("class_idx           : %d\n", pClassDef->classIdx);
    printf("access_flags        : %d (0x%04x)\n",
        pClassDef->accessFlags, pClassDef->accessFlags);
    printf("superclass_idx      : %d\n", pClassDef->superclassIdx);
    printf("interfaces_off      : %d (0x%06x)\n",
        pClassDef->interfacesOff, pClassDef->interfacesOff);
    printf("source_file_idx     : %d\n", pClassDef->sourceFileIdx);
    printf("annotations_off     : %d (0x%06x)\n",
        pClassDef->annotationsOff, pClassDef->annotationsOff);
    printf("class_data_off      : %d (0x%06x)\n",
        pClassDef->classDataOff, pClassDef->classDataOff);
    printf("static_fields_size  : %d\n", pClassData->header.staticFieldsSize);
    printf("instance_fields_size: %d\n",
            pClassData->header.instanceFieldsSize);
    printf("direct_methods_size : %d\n", pClassData->header.directMethodsSize);
    printf("virtual_methods_size: %d\n",
            pClassData->header.virtualMethodsSize);
    printf("\n");

    free(pClassData);
}

/*
 * Dump an interface that a class declares to implement.
 */
void dumpInterface(const DexFile* pDexFile, const DexTypeItem* pTypeItem,
    int i, int flag)
{
    const char* interfaceName =
        dexStringByTypeIdx(pDexFile, pTypeItem->typeIdx);
    //******************* kui's modification begins  *******************
    if (gOptions.outputFormat == OUTPUT_PLAIN) {
      if(flag==1)
        printf(", [|%s|]",descriptorToDot(interfaceName));
      else if(flag==2)
        printf(" [|%s|],",descriptorToDot(interfaceName));
      else if(flag==0)
        printf(" [|%s|]",descriptorToDot(interfaceName));
        //******************* kui's modification ends  *******************
    } else {
        char* dotted = descriptorToDot(interfaceName);
        printf("<implements name=\"%s\">\n</implements>\n", dotted);
        free(dotted);
    }
}

/*
 * Dump the catches table associated with the code.
 */
void dumpCatches(DexFile* pDexFile, const DexCode* pCode)
{
    u4 triesSize = pCode->triesSize;
    //******************* kui's modification begins  *******************
    if (triesSize == 0) {
        //printf("      @catches:(none)\n");
        return;
    }

  //  printf("      catches       : %d\n", triesSize);

    const DexTry* pTries = dexGetTries(pCode);
    u4 i;

    for (i = 0; i < triesSize; i++) {
        const DexTry* pTry = &pTries[i];
        u4 start = pTry->startAddr;
        u4 end = start + pTry->insnCount;
        DexCatchIterator iterator;

       // printf("        0x%04x - 0x%04x\n", start, end);

        dexCatchIteratorInit(&iterator, pCode, pTry->handlerOff);

        for (;;) {
            DexCatchHandler* handler = dexCatchIteratorNext(&iterator);
            const char* descriptor;

            if (handler == NULL) {
                break;
            }
            const u2* insns = pCode->insns;
            descriptor = (handler->typeIdx == kDexNoIndex) ? "any" :
                descriptorToDot(dexStringByTypeIdx(pDexFile, handler->typeIdx));
            printf("  catch  %s @[L%06x..L%06x] goto L%06x;\n", descriptor,((u1*)insns - pDexFile->baseAddr) +start*2,((u1*)insns - pDexFile->baseAddr) +end*2,((u1*)insns - pDexFile->baseAddr) +(handler->address)*2);
        }
    }
    //******************* kui's modification ends  *******************
}

static int dumpPositionsCb(void *cnxt, u4 address, u4 lineNum)
{
    printf("        0x%04x line=%d\n", address, lineNum);
    return 0;
}

/*
 * Dump the positions list.
 */
void dumpPositions(DexFile* pDexFile, const DexCode* pCode,
        const DexMethod *pDexMethod)
{

    printf("      positions     : \n");
    const DexMethodId *pMethodId
            = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
    const char *classDescriptor
            = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);

    dexDecodeDebugInfo(pDexFile, pCode, classDescriptor, pMethodId->protoIdx,
            pDexMethod->accessFlags, dumpPositionsCb, NULL, NULL);
}

static void dumpLocalsCb(void *cnxt, u2 reg, u4 startAddress,
        u4 endAddress, const char *name, const char *descriptor,
        const char *signature)
{
  //******************* kui's modification begins  *******************
	printf("        [|%s|] [|%s|] @reg %d @scope (L%04x,L%04x) ;\n",
      descriptorToDot(descriptor), name,reg, startAddress,  +endAddress );
    //******************* kui's modification ends  *******************
}

/*
 * Dump the locals list.
 */

void dumpLocals(DexFile* pDexFile, const DexCode* pCode,
        const DexMethod *pDexMethod)
{

	printf("      local placehoder ;\n");
    const DexMethodId *pMethodId
            = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
    const char *classDescriptor
            = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);

    dexDecodeDebugInfo(pDexFile, pCode, classDescriptor, pMethodId->protoIdx,
            pDexMethod->accessFlags, NULL, dumpLocalsCb, NULL);
    printf("      \n");
}

/*
 * Get information about a method.
 */
bool getMethodInfo(DexFile* pDexFile, u4 methodIdx, FieldMethodInfo* pMethInfo)
{
    const DexMethodId* pMethodId;

    if (methodIdx >= pDexFile->pHeader->methodIdsSize)
        return false;

    pMethodId = dexGetMethodId(pDexFile, methodIdx);
    pMethInfo->name = dexStringById(pDexFile, pMethodId->nameIdx);
    pMethInfo->signature = dexCopyDescriptorFromMethodId(pDexFile, pMethodId);

    pMethInfo->classDescriptor =
            dexStringByTypeIdx(pDexFile, pMethodId->classIdx);
    return true;
}

/*
 * Get information about a field.
 */
bool getFieldInfo(DexFile* pDexFile, u4 fieldIdx, FieldMethodInfo* pFieldInfo)
{
    const DexFieldId* pFieldId;

    if (fieldIdx >= pDexFile->pHeader->fieldIdsSize)
        return false;

    pFieldId = dexGetFieldId(pDexFile, fieldIdx);
    pFieldInfo->name = dexStringById(pDexFile, pFieldId->nameIdx);
    pFieldInfo->signature = dexStringByTypeIdx(pDexFile, pFieldId->typeIdx);
    pFieldInfo->classDescriptor =
        dexStringByTypeIdx(pDexFile, pFieldId->classIdx);
    return true;
}


/*
 * Look up a class' descriptor.
 */
const char* getClassDescriptor(DexFile* pDexFile, u4 classIdx)
{
    return dexStringByTypeIdx(pDexFile, classIdx);
}

/*
 * Helper for dumpInstruction(), which builds the string
 * representation for the index in the given instruction. This will
 * first try to use the given buffer, but if the result won't fit,
 * then this will allocate a new buffer to hold the result. A pointer
 * to the buffer which holds the full result is always returned, and
 * this can be compared with the one passed in, to see if the result
 * needs to be free()d.
 */
static char* indexString(DexFile* pDexFile,
    const DecodedInstruction* pDecInsn, char* buf, size_t bufSize)
{
    int outSize;
    u4 index;
    u4 width;

    /* TODO: Make the index *always* be in field B, to simplify this code. */
    switch (dexGetFormatFromOpcode(pDecInsn->opcode)) {
    case kFmt20bc:
    case kFmt21c:
    case kFmt35c:
    case kFmt35ms:
    case kFmt3rc:
    case kFmt3rms:
    case kFmt35mi:
    case kFmt3rmi:
        index = pDecInsn->vB;
        width = 4;
        break;
    case kFmt31c:
    case kFmt40sc:
    case kFmt41c:
    case kFmt5rc:
        index = pDecInsn->vB;
        width = 8;
        break;
    case kFmt22c:
    case kFmt22cs:
        index = pDecInsn->vC;
        width = 4;
        break;
    case kFmt52c:
        index = pDecInsn->vC;
        width = 8;
        break;
    default:
        index = 0;
        width = 4;
        break;
    }
    //******************* kui's modification begins  *******************
    switch (pDecInsn->indexType) {
    case kIndexUnknown:
        /*
         * This function shouldn't ever get called for this type, but do
         * something sensible here, just to help with debugging.
         */
        outSize = snprintf(buf, bufSize, "<unknown-index>");
        break;
    case kIndexNone:
        /*
         * This function shouldn't ever get called for this type, but do
         * something sensible here, just to help with debugging.
         */
        outSize = snprintf(buf, bufSize, "<no-index>");
        break;
    case kIndexVaries:
        /*
         * This one should never show up in a dexdump, so no need to try
         * to get fancy here.
         */
        outSize = snprintf(buf, bufSize, "<index-varies> // thing@%0*x",
                width, index);
        break;
    case kIndexTypeRef:
        outSize = snprintf(buf, bufSize, "%s",
            descriptorToDot(getClassDescriptor(pDexFile, index)));
        break;
    case kIndexStringRef:
        outSize = snprintf(buf, bufSize, "\"%s\"",
                dexStringById(pDexFile, index));
        break;
    case kIndexMethodRef:
        {
            FieldMethodInfo methInfo;
            if (getMethodInfo(pDexFile, index, &methInfo)) {
              {
                outSize = snprintf(buf, bufSize, "[|%s|]", methInfo.name);
              }
                    //,descriptorToDot(methInfo.classDescriptor));
            } else {
                outSize = snprintf(buf, bufSize, "<method?> // method@%0*x",
                        width, index);
            }
        }
        break;
    case kIndexFieldRef:
        {
            FieldMethodInfo fieldInfo;
            if (getFieldInfo(pDexFile, index, &fieldInfo)) {
              switch(pDecInsn->opcode) {
              case 0x60:
              case 0x61:
              case 0x62:
              case 0x63:
              case 0x64:
              case 0x65:
              case 0x66:
              case 0x67:
              case 0x68:
              case 0x69:
              case 0x6a:
              case 0x6b:
              case 0x6c:
              case 0x6d:
                outSize = snprintf(buf, bufSize, "[|%s|].[|%s|] ",
                    descriptorToDot(fieldInfo.classDescriptor), fieldInfo.name);
                                        //descriptorToDot(fieldInfo.signature));
                break;
              default:
                outSize = snprintf(buf, bufSize, ".[|%s|] @type [|%s|] ",
                    fieldInfo.name,descriptorToDot( fieldInfo.classDescriptor));
                        //descriptorToDot(fieldInfo.signature));
              }
            } else {
                outSize = snprintf(buf, bufSize, "<field?> // field@%0*x",
                        width, index);
            }
        }
        break;
    case kIndexInlineMethod:
        outSize = snprintf(buf, bufSize, "[%0*x] // inline #%0*x",
                width, index, width, index);
        break;
    case kIndexVtableOffset:
        outSize = snprintf(buf, bufSize, "[%0*x] // vtable #%0*x",
                width, index, width, index);
        break;
    case kIndexFieldOffset:
        outSize = snprintf(buf, bufSize, "[obj+%0*x]", width, index);
        break;
    default:
        outSize = snprintf(buf, bufSize, "<?>");
        break;
    }

    //******************* kui's modification ends  *******************
    if (outSize >= (int) bufSize) {
        /*
         * The buffer wasn't big enough; allocate and retry. Note:
         * snprintf() doesn't count the '\0' as part of its returned
         * size, so we add explicit space for it here.
         */
        outSize++;
        buf = (char*)malloc(outSize);
        if (buf == NULL) {
            return NULL;
        }
        return indexString(pDexFile, pDecInsn, buf, outSize);
    } else {
        return buf;
    }
}

/*
 * Dump a single instruction.
 */
void dumpInstruction(DexFile* pDexFile, const DexCode* pCode, int insnIdx,
    int insnWidth, const DecodedInstruction* pDecInsn,struct Op31t list[],int &s31t,int &l31t)
{
    char indexBufChars[200];
    char stringBufChars[200];
    char *indexBuf = indexBufChars;
    char *stringBuf=stringBufChars;
    const u2* insns = pCode->insns;
    int i;
    //******************* kui's modification begins  *******************
    //printf("        #L%04x ", insnIdx);
      printf("        #L%06x  ", ((u1*)insns - pDexFile->baseAddr) + insnIdx*2);
    if (pDecInsn->opcode == OP_NOP) {
            u2 instr = get2LE((const u1*) &insns[insnIdx]);
            if (instr == kPackedSwitchSignature) {
              printf("switch v%d\n", list[l31t].pDecInsn->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+2];
              int minValue=(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24);
              for (i = 4; i < insnWidth; i+=2,minValue++) {
                  const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                     printf("                 | %d => goto L%06x\n", minValue,
                    		 ((u1*)insns - pDexFile->baseAddr) +(((bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24))+list[l31t].insnIdx)*2);
                                        }
              printf("                 | => goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(list[l31t].insnIdx+3)*2);
              ++l31t;
            } else if (instr == kSparseSwitchSignature) {
              printf("switch v%d\n", list[l31t].pDecInsn->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+1];
              int size=(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8);
              int counter=0;
              for (i = 2; counter < size; i+=2,++counter) {
                   const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                    printf("                 | %d => goto L%06x\n",
                    (bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24),
                    ((u1*)insns - pDexFile->baseAddr) + (((bytePtr[size*4] & 0xFF) |((bytePtr[size*4+1] & 0xFF) << 8) |((bytePtr[size*4+2] & 0xFF) << 16) |(bytePtr[size*4+3] << 24))+list[l31t].insnIdx)*2);
                                                      }
                     printf("                 | => goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(list[l31t].insnIdx+3)*2);
                     ++l31t;
            } else if (instr == kArrayDataSignature) {
              printf("v%d:=`[", list[l31t].pDecInsn->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+1];
              int length=(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8);
              switch (length){
              case 2:
                for (i = 4; i < insnWidth; i++) {
                 const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                 if(i==insnWidth-1)
                   printf("%d ",(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8));
                 else
                 printf("%d, ",(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8));
                    }
                break;
              case 4:
                for (i = 4; i < insnWidth; i+=2) {
               const u1* bytePtr = (const u1*) &insns[insnIdx+i];
               if(i>=insnWidth-2)
               printf("%d ",(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24));
               else printf("%d, ",(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24));
                  }
                break;
               case 8:
                 for (i = 4; i < insnWidth; i+=4) {
                   const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                   if(i>=insnWidth-4)
                   printf("%lldL "  ,(bytePtr[0]   & 0xFFL) |
                                   ((bytePtr[1] & 0xFFL) << 8) |
                                   ((bytePtr[2] & 0xFFL) << 16) |
                                   ((bytePtr[3]  & 0xFFL) << 24) |
                                   ((bytePtr[4]  & 0xFFLL) << 32) |
                                   ((bytePtr[5] & 0xFFLL) << 40) |
                                   ((bytePtr[6] & 0xFFLL) << 48) |
                                   (((long long)bytePtr[7]) << 56));
                   else
                      printf("%lldL, "  ,(bytePtr[0]   & 0xFFL) |
                       ((bytePtr[1] & 0xFFL) << 8) |
                       ((bytePtr[2] & 0xFFL) << 16) |
                       ((bytePtr[3]  & 0xFFL) << 24) |
                       ((bytePtr[4]  & 0xFFLL) << 32) |
                       ((bytePtr[5] & 0xFFLL) << 40) |
                       ((bytePtr[6] & 0xFFLL) << 48) |
                       (((long long)bytePtr[7]) << 56));
                 }
                 break;
                 default:
                   printf("fill-array-data length error");
                   break;
              }
              printf("];\n");
              printf("               goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(list[l31t].insnIdx+3)*2);
              ++l31t;
            }
        }else{

    if (pDecInsn->indexType != kIndexNone) {
            indexBuf = indexString(pDexFile, pDecInsn,
                    indexBufChars, sizeof(indexBufChars));
        }

        switch(pDecInsn->opcode){
        case 0x00:
          break;
          //move
        case 0x01:
        case 0x02:
        case 0x03:
          printf("v%d:=v%d;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x04:
        case 0x05:
        case 0x06:
          printf("v%d:=v%d  @type wide;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x07:
        case 0x08:
        case 0x09:
          printf("v%d:=v%d  @type object;", pDecInsn->vA, pDecInsn->vB);
          break;
          //move-result
        case 0x0a:
          printf("v%d:=temp;", pDecInsn->vA);
          break;
        case 0x0b:
          printf("v%d:=temp  @type wide;", pDecInsn->vA);
          break;
        case 0x0c:
          printf("v%d:=temp  @type object;", pDecInsn->vA);
          break;
        case 0x0d:
        	printf("v%d:=Exception  @type object;", pDecInsn->vA);
          break;
          //return
        case 0x0e:
          printf("return @void ;");
          break;
        case 0x0f:
          printf("return v%d;", pDecInsn->vA );
          break;
        case 0x10:
          printf("return v%d  @type wide;", pDecInsn->vA );
          break;
        case 0x11:
          printf("return v%d  @type object;", pDecInsn->vA );
          break;
          //const
        case 0x12:
          printf("v%d:=%d  @length 4;", pDecInsn->vA,(s4)pDecInsn->vB );
          break;
        case 0x13:
          printf("v%d:=%d  @length 16;", pDecInsn->vA,(s4)pDecInsn->vB );
          break;
        case 0x14:
        {/* this is often, but not always, a float */
           union {
              float f;
              u4 i;
                  } conv;
           conv.i = pDecInsn->vB;
           printf(" v%d:=%f;",pDecInsn->vA, conv.f);
         }
          break;
        case 0x15:
          {s4 value = pDecInsn->vB << 16;
                      printf("v%d:=%d  @length high16;",pDecInsn->vA, value);
          }
          break;
        case 0x16:
           printf("v%d:=%d  @length wide16;", pDecInsn->vA,(s4)pDecInsn->vB );
           break;
        case 0x17:
        {/* this is often, but not always, a float */
         union {
             float f;
             u4 i;
                } conv;
             conv.i = pDecInsn->vB;
           printf(" v%d:=%f  @length wide32;",pDecInsn->vA, conv.f);
        }
          break;

        case 0x18:
        {/* this is often, but not always, a double */
            union {
              double d;
              u8 j;
                } conv2;
          conv2.j = pDecInsn->vB_wide;
          printf("v%d:=%fL  @length wide;",pDecInsn->vA, conv2.d);
        }
          break;
        case 0x19:
          {s8 value1 = ((s8) pDecInsn->vB) << 48;
           printf("v%d:=%lldL  @length wide_high16;",pDecInsn->vA, value1);
          }
           break;
        case 0x1a:
        case 0x1b:
        {
          // find escape characters
         const char *Str = dexStringById(pDexFile, pDecInsn->vB);
         char newStr [(int)strlen(Str)+100];
         int m=0,n=0;

        while( m<=(int)strlen(Str)){
        	if(Str[m]=='\n'||Str[m]=='\r') ++m;
        	else if(Str[m]=='\\'||Str[m]=='"')
        		 {
        		 newStr[n++]='\\';
        		 newStr[n++]=Str[m++];
        		 }
        	 else newStr[n++]=Str[m++];
         }
         //newStr[n]='\0';
         printf("v%d:=\"%s\";", pDecInsn->vA,newStr);
        }
          break;
        case 0x1c:
          printf("v%d:=[|%s|];", pDecInsn->vA,descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB)));
          break;
          //monitor
        case 0x1d:
          printf("(@monitorenter v%d)", pDecInsn->vA);
          break;
        case 0x1e:
          printf("(@monitorexit v%d)", pDecInsn->vA);
          break;
          //check cast
        case 0x1f:
          printf("v%d:=([|%s|])v%d;", pDecInsn->vA,descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB)), pDecInsn->vA);
           break;
           //instance of
        case 0x20:
          printf("v%d:=instanceof(v%d, [|%s|]);",pDecInsn->vA,pDecInsn->vB,descriptorToDot(indexBuf));
          break;
          //array lenth
        case 0x21:
          printf("v%d:=v%d.length;",pDecInsn->vA,pDecInsn->vB);
           break;
           // new
        case 0x22:
          printf("v%d:=new [|%s|];",pDecInsn->vA,indexBuf);
          break;
        case 0x23:
        {
          char newIndex[200];
          int flag=0;
          for(int m=0,n=0;n<(int)strlen(indexBuf);){
            if(indexBuf[n]=='['&&indexBuf[n+1]==']'&&flag==0)
            {
              newIndex[m]=indexBuf[n];
              m++;n++;
              char buffer[5];
              sprintf(buffer, "v%d",pDecInsn->vB);
              for(int i=0;i<(int)strlen(buffer);i++,m++)
                newIndex[m]=buffer[i];
              flag=1;
            }
            else {
              newIndex[m]=indexBuf[n];
              m++;n++;
            }
            newIndex[m]='\0';
          }
          printf("v%d:=new %s ;",pDecInsn->vA,newIndex);
        }
          break;
          //fill array
        case 0x24:
        {
          printf("temp:=(%s)`[", indexBuf);
          for (i = 0; i < (int) pDecInsn->vA; i++) {
              if (i == 0)
                printf("v%d", pDecInsn->arg[i]);
              else
                printf(", v%d", pDecInsn->arg[i]);
                    }
          printf("];");
        }
        break;
        case 0x25:
        {
          printf("temp:=(%s)`[", indexBuf);
          for (i = 0; i < (int) pDecInsn->vA; i++) {
              if (i == 0)
                printf("v%d", pDecInsn->vC + i);
              else
                printf(", v%d", pDecInsn->vC + i);
                      }
                printf("];");
         }
          break;
        case 0x26:
        { printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2);
          list[s31t].init(insnIdx,pDecInsn);
         ++s31t;
          }
          break;
          //throw
        case 0x27:
          printf("throw v%d;",pDecInsn->vA);
          break;
          // goto
        case 0x28:
        case 0x29:
        case 0x2a:
        {
            s4 targ = (s4) pDecInsn->vA;
            printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
        }
          break;
        case 0x2b:
        case 0x2c:
        { printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2);
        list[s31t].init(insnIdx,pDecInsn);
          ++s31t;
        }
          break;
          // cmp
        case 0x2d:
        case 0x2f:
          printf("v%d:=cmpl(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x2e:
        case 0x30:
          printf("v%d:=cmpg(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x31:
          printf("v%d:=cmp(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
          // if
        case 0x32:
           {
              s4 targ = (s4) pDecInsn->vC;
              printf("if v%d==v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
           }
          break;
        case 0x33:
           {
              s4 targ = (s4) pDecInsn->vC;
              printf("if v%d!=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
           }
           break;
        case 0x34:
        {
                      s4 targ = (s4) pDecInsn->vC;
                      printf("if v%d<v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                   }
                  break;
        case 0x35:
        {
                      s4 targ = (s4) pDecInsn->vC;
                      printf("if v%d>=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                   }
                  break;
        case 0x36:
        {
                      s4 targ = (s4) pDecInsn->vC;
                      printf("if v%d>v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                   }
                  break;
        case 0x37:
        {
                      s4 targ = (s4) pDecInsn->vC;
                      printf("if v%d<=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                   }
                  break;
        case 0x38:
        {
            s4 targ = (s4) pDecInsn->vB;
            printf("if v%d==0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
        }
          break;
        case 0x39:
        {
                    s4 targ = (s4) pDecInsn->vB;
                    printf("if v%d!=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                }
                  break;
        case 0x3a:
        {
                    s4 targ = (s4) pDecInsn->vB;
                    printf("if v%d<0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                }
                  break;
        case 0x3b:
        {
                    s4 targ = (s4) pDecInsn->vB;
                    printf("if v%d>=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                }
                  break;
        case 0x3c:
        {
                    s4 targ = (s4) pDecInsn->vB;
                    printf("if v%d>0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
                }
                  break;
        case 0x3d:
        {
           s4 targ = (s4) pDecInsn->vB;
            printf("if v%d<=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
         }
           break;
          //aput/aget
        case 0x44:
          printf("v%d:=v%d[v%d];",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x45:
          printf("v%d:=v%d[v%d]  @wide;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x46:
          printf("v%d:=v%d[v%d]  @object;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x47:
          printf("v%d:=v%d[v%d]  @boolean;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x48:
          printf("v%d:=v%d[v%d]  @byte;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x49:
          printf("v%d:=v%d[v%d]  @char;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x4a:
          printf("v%d:=v%d[v%d]  @short;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
          break;
        case 0x4b:
          printf("v%d[v%d]:=v%d;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x4c:
          printf("v%d[v%d]:=v%d  @wide;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x4d:
          printf("v%d[v%d]:=v%d  @object;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x4e:
          printf("v%d[v%d]:=v%d  @boolean;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x4f:
          printf("v%d[v%d]:=v%d  @byte;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x50:
          printf("v%d[v%d]:=v%d  @char;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
        case 0x51:
          printf("v%d[v%d]:=v%d  @short;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
          break;
          //iget
        case 0x52:
          printf("v%d:=v%d%s;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x53:
          printf("v%d:=v%d%s  @wide;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x54:
          printf("v%d:=v%d%s  @object;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x55:
          printf("v%d:=v%d%s  @boolean;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x56:
          printf("v%d:=v%d%s  @byte;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x57:
          printf("v%d:=v%d%s  @char;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
        case 0x58:
          printf("v%d:=v%d%s  @short;", pDecInsn->vA, pDecInsn->vB, indexBuf);
          break;
          //iput
        case 0x59:
          printf("v%d%s :=v%d;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5a:
          printf("v%d%s :=v%d @wide;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5b:
          printf("v%d%s :=v%d @object;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5c:
          printf("v%d%s :=v%d @boolean;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5d:
          printf("v%d%s :=v%d @byte;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5e:
          printf("v%d%s :=v%d @char;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
        case 0x5f:
          printf("v%d%s :=v%d @short;", pDecInsn->vB,indexBuf,pDecInsn->vA);
          break;
          //sget
        case 0x60:
          printf("v%d:=%s;", pDecInsn->vA, indexBuf);
          break;
        case 0x61:
          printf("v%d:=%s  @wide;", pDecInsn->vA, indexBuf);
          break;
        case 0x62:
          printf("v%d:=%s  @object;", pDecInsn->vA, indexBuf);
          break;
        case 0x63:
          printf("v%d:=%s  @boolean;", pDecInsn->vA, indexBuf);
          break;
        case 0x64:
          printf("v%d:=%s  @byte;", pDecInsn->vA, indexBuf);
          break;
        case 0x65:
          printf("v%d:=%s  @char;", pDecInsn->vA, indexBuf);
          break;
        case 0x66:
          printf("v%d:=%s  @short;", pDecInsn->vA, indexBuf);
          break;
          //sput
        case 0x67:
          printf("%s:=v%d;", indexBuf, pDecInsn->vA);
          break;
        case 0x68:
          printf("%s:=v%d  @wide;", indexBuf, pDecInsn->vA);
          break;
        case 0x69:
          printf("%s:=v%d  @object;", indexBuf, pDecInsn->vA);
          break;
        case 0x6a:
          printf("%s:=v%d  @boolean;", indexBuf, pDecInsn->vA);
          break;
        case 0x6b:
          printf("%s:=v%d  @byte;", indexBuf, pDecInsn->vA);
          break;
        case 0x6c:
          printf("%s:=v%d  @char;", indexBuf, pDecInsn->vA);
          break;
        case 0x6d:
          printf("%s:=v%d  @short;", indexBuf, pDecInsn->vA);
          break;
          //invoke
        case 0x6e:
        {
           FieldMethodInfo methInfo;
           if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {
          printf("call temp:= %s(", indexBuf);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
             if (i == 0)
               printf("v%d", pDecInsn->arg[i]);
            else
               printf(", v%d", pDecInsn->arg[i]);
                    }
           printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type virtual;",methInfo.classDescriptor, methInfo.name,
                   methInfo.signature,descriptorToDot(methInfo.classDescriptor));
         }
        }
           break;
        case 0x6f:
        {
          FieldMethodInfo methInfo;
                     if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {
                    printf("call temp:= %s(", indexBuf);
                      for (i = 0; i < (int) pDecInsn->vA; i++) {
                       if (i == 0)
                         printf("v%d", pDecInsn->arg[i]);
                      else
                         printf(", v%d", pDecInsn->arg[i]);
                              }
                      printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type super;",methInfo.classDescriptor, methInfo.name,
                                         methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                   }
        }
                   break;
        case 0x70:
        {
          FieldMethodInfo methInfo;
                     if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {
                    printf("call temp:= %s(", indexBuf);
                      for (i = 0; i < (int) pDecInsn->vA; i++) {
                       if (i == 0)
                         printf("v%d", pDecInsn->arg[i]);
                      else
                         printf(", v%d", pDecInsn->arg[i]);
                              }
                      printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type direct;",methInfo.classDescriptor, methInfo.name,
                                         methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                   }
                 }
                   break;
        case 0x71:
        {
          FieldMethodInfo methInfo;
                     if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {
                    printf("call temp:= %s(", indexBuf);
                      for (i = 0; i < (int) pDecInsn->vA; i++) {
                       if (i == 0)
                         printf("v%d", pDecInsn->arg[i]);
                      else
                         printf(", v%d", pDecInsn->arg[i]);
                              }
                      printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type static;",methInfo.classDescriptor, methInfo.name,
                                         methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                   }
                 }
                   break;
        case 0x72:
        {
          FieldMethodInfo methInfo;
                     if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {
                    printf("call temp:= %s(", indexBuf);
                      for (i = 0; i < (int) pDecInsn->vA; i++) {
                       if (i == 0)
                         printf("v%d", pDecInsn->arg[i]);
                      else
                         printf(", v%d", pDecInsn->arg[i]);
                              }
                      printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type interface;",methInfo.classDescriptor, methInfo.name,
                                         methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                   }
                 }
                   break;
        case 0x74:
        {
          FieldMethodInfo methInfo;
          if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {

          printf("call temp:= %s(", indexBuf);
        for (i = 0; i < (int) pDecInsn->vA; i++) {
            if (i == 0)
                printf("v%d", pDecInsn->vC + i);
            else
                printf(", v%d", pDecInsn->vC + i);
        }
        printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type virtual;",methInfo.classDescriptor, methInfo.name,
                           methInfo.signature,descriptorToDot(methInfo.classDescriptor));
          }
         }break;
        case 0x75:
        {
          FieldMethodInfo methInfo;
                    if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {

                    printf("call temp:= %s(", indexBuf);
                  for (i = 0; i < (int) pDecInsn->vA; i++) {
                      if (i == 0)
                          printf("v%d", pDecInsn->vC + i);
                      else
                          printf(", v%d", pDecInsn->vC + i);
                  }
                  printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type super;",methInfo.classDescriptor, methInfo.name,
                                     methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                    }
                 }break;
        case 0x76:
        {
          FieldMethodInfo methInfo;
                    if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {

                    printf("call temp:= %s(", indexBuf);
                  for (i = 0; i < (int) pDecInsn->vA; i++) {
                      if (i == 0)
                          printf("v%d", pDecInsn->vC + i);
                      else
                          printf(", v%d", pDecInsn->vC + i);
                  }
                  printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type direct;",methInfo.classDescriptor, methInfo.name,
                                     methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                    }
                 }break;
        case 0x77:
        {
          FieldMethodInfo methInfo;
                    if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {

                    printf("call temp:= %s(", indexBuf);
                  for (i = 0; i < (int) pDecInsn->vA; i++) {
                      if (i == 0)
                          printf("v%d", pDecInsn->vC + i);
                      else
                          printf(", v%d", pDecInsn->vC + i);
                  }
                  printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type static;",methInfo.classDescriptor, methInfo.name,
                                     methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                    }
                 }break;
        case 0x78:
        {
          FieldMethodInfo methInfo;
                    if (getMethodInfo(pDexFile, pDecInsn->vB, &methInfo)) {

                    printf("call temp:= %s(", indexBuf);
                  for (i = 0; i < (int) pDecInsn->vA; i++) {
                      if (i == 0)
                          printf("v%d", pDecInsn->vC + i);
                      else
                          printf(", v%d", pDecInsn->vC + i);
                  }
                  printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type interface;",methInfo.classDescriptor, methInfo.name,
                                     methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                    }
                 }break;
          //unop
        case 0x7b:
          printf("v%d:=-v%d  @type int;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x7c:
          printf("v%d:=~v%d  @type int;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x7d:
          printf("v%d:=-v%d  @type long;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x7e:
          printf("v%d:=~v%d  @type long;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x7f:
          printf("v%d:=-v%d  @type float;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x80:
          printf("v%d:=-v%d  @type double;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x81:
          printf("v%d:=(long)v%d  @type i2l;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x82:
          printf("v%d:=(float)v%d  @type i2f;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x83:
          printf("v%d:=(double)v%d  @type i2d;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x84:
          printf("v%d:=(int)v%d  @type l2i;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x85:
          printf("v%d:=(float)v%d  @type l2f;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x86:
          printf("v%d:=(double)v%d  @type l2d;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x87:
          printf("v%d:=(int)v%d  @type f2i;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x88:
          printf("v%d:=(long)v%d  @type f2l;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x89:
          printf("v%d:=(double)v%d  @type f2d;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x8a:
          printf("v%d:=(int)v%d  @type d2i;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x8b:
          printf("v%d:=(long)v%d  @type d2l;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x8c:
          printf("v%d:=(float)v%d  @type d2f;", pDecInsn->vA, pDecInsn->vB);
           break;
        case 0x8d:
          printf("v%d:=(byte)v%d  @type i2b;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x8e:
          printf("v%d:=(char)v%d  @type i2c;", pDecInsn->vA, pDecInsn->vB);
          break;
        case 0x8f:
          printf("v%d:=(short)v%d  @type i2s;", pDecInsn->vA, pDecInsn->vB);
          break;
          //binop
        case 0x90:
          printf("v%d:=v%d+v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x91:
          printf("v%d:=v%d-v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x92:
          printf("v%d:=v%d*v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x93:
          printf("v%d:=v%d/v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x94:
          printf("v%d:=v%d%%v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x95:
          printf("v%d:=v%d^&v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x96:
          printf("v%d:=v%d^|v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x97:
          printf("v%d:=v%d^~v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x98:
          printf("v%d:=v%d^<v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x99:
          printf("v%d:=v%d^>v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
        case 0x9a:
          printf("v%d:=v%d^>>v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
          break;
                case 0x9b:
                  printf("v%d:=v%d+v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0x9c:
                  printf("v%d:=v%d-v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0x9d:
                  printf("v%d:=v%d*v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0x9e:
                  printf("v%d:=v%d/v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0x9f:
                  printf("v%d:=v%d%%v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa0:
                  printf("v%d:=v%d^&v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa1:
                  printf("v%d:=v%d^|v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa2:
                  printf("v%d:=v%d^~v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa3:
                  printf("v%d:=v%d^<v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa4:
                  printf("v%d:=v%d^>v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                case 0xa5:
                  printf("v%d:=v%d^>>v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                  break;
                  case 0xa6:
                    printf("v%d:=v%d+v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                    break;
                  case 0xa7:
                    printf("v%d:=v%d-v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                    break;
                  case 0xa8:
                    printf("v%d:=v%d*v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                    break;
                  case 0xa9:
                    printf("v%d:=v%d/v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                    break;
                  case 0xaa:
                    printf("v%d:=v%d%%v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                    break;
                    case 0xab:
                      printf("v%d:=v%d+v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                      break;
                    case 0xac:
                      printf("v%d:=v%d-v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                      break;
                    case 0xad:
                      printf("v%d:=v%d*v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                      break;
                    case 0xae:
                      printf("v%d:=v%d/v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                      break;
                    case 0xaf:
                      printf("v%d:=v%d%%v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
                      break;
                  //binop/2addr
                            case 0xb0:
                              printf("v%d:=v%d+v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb1:
                              printf("v%d:=v%d-v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb2:
                              printf("v%d:=v%d*v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb3:
                              printf("v%d:=v%d/v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb4:
                              printf("v%d:=v%d%%v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb5:
                              printf("v%d:=v%d^&v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb6:
                              printf("v%d:=v%d^|v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb7:
                              printf("v%d:=v%d^~v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb8:
                              printf("v%d:=v%d^<v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xb9:
                              printf("v%d:=v%d^>v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                            case 0xba:
                              printf("v%d:=v%d^>>v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                              break;
                                    case 0xbb:
                                      printf("v%d:=v%d+v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xbc:
                                      printf("v%d:=v%d-v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xbd:
                                      printf("v%d:=v%d*v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xbe:
                                      printf("v%d:=v%d/v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xbf:
                                      printf("v%d:=v%d%%v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc0:
                                      printf("v%d:=v%d^&v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc1:
                                      printf("v%d:=v%d^|v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc2:
                                      printf("v%d:=v%d^~v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc3:
                                      printf("v%d:=v%d^<v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc4:
                                      printf("v%d:=v%d^>v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                    case 0xc5:
                                      printf("v%d:=v%d^>>v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                      break;
                                      case 0xc6:
                                        printf("v%d:=v%d+v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                        break;
                                      case 0xc7:
                                        printf("v%d:=v%d-v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                        break;
                                      case 0xc8:
                                        printf("v%d:=v%d*v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                        break;
                                      case 0xc9:
                                        printf("v%d:=v%d/v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                        break;
                                      case 0xca:
                                        printf("v%d:=v%d%%v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                        break;
                                        case 0xcb:
                                          printf("v%d:=v%d+v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                          break;
                                        case 0xcc:
                                          printf("v%d:=v%d-v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                          break;
                                        case 0xcd:
                                          printf("v%d:=v%d*v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                          break;
                                        case 0xce:
                                          printf("v%d:=v%d/v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                          break;
                                        case 0xcf:
                                          printf("v%d:=v%d%%v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
                                          break;
         //binop/lit16
        case 0xd0:
          printf("v%d:=v%d+%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd1:
          printf("v%d:=v%d-%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd2:
          printf("v%d:=v%d*%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd3:
          printf("v%d:=v%d/%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd4:
          printf("v%d:=v%d%%%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd5:
          printf("v%d:=v%d^&%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd6:
          printf("v%d:=v%d^|%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
        case 0xd7:
          printf("v%d:=v%d^~%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
          break;
          //binop/lit8
                case 0xd8:
                  printf("v%d:=v%d+%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xd9:
                  printf("v%d:=v%d-%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xda:
                  printf("v%d:=v%d*%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdb:
                  printf("v%d:=v%d/%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdc:
                  printf("v%d:=v%d%%%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdd:
                  printf("v%d:=v%d^&%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xde:
                  printf("v%d:=v%d^|%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdf:
                  printf("v%d:=v%d^~%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe0:
                  printf("v%d:=v%d^<%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe1:
                  printf("v%d:=v%d^>%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe2:
                  printf("v%d:=v%d^>>%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
        default:
            printf(" ???");
            break;
        }

   /* if (pDecInsn->opcode == OP_NOP) {
        u2 instr = get2LE((const u1*) &insns[insnIdx]);
        if (instr == kPackedSwitchSignature) {
            printf("|%04x: packed-switch-data (%d units)",
                insnIdx, insnWidth);
        } else if (instr == kSparseSwitchSignature) {
            printf("|%04x: sparse-switch-data (%d units)",
                insnIdx, insnWidth);
        } else if (instr == kArrayDataSignature) {
            printf("|%04x: array-data (%d units)",
                insnIdx, insnWidth);
        } else {
            printf("|%04x: nop // spacer", insnIdx);
        }
    } else {
        printf("|%04x: %s", insnIdx, dexGetOpcodeName(pDecInsn->opcode));
    }

    if (pDecInsn->indexType != kIndexNone) {
        indexBuf = indexString(pDexFile, pDecInsn,
                indexBufChars, sizeof(indexBufChars));
    }

    switch (dexGetFormatFromOpcode(pDecInsn->opcode)) {
    case kFmt10x:        // op
        break;
    case kFmt12x:        // op vA, vB
        printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB);
        break;
    case kFmt11n:        // op vA, #+B
        printf(" v%d, #int %d // #%x",
            pDecInsn->vA, (s4)pDecInsn->vB, (u1)pDecInsn->vB);
        break;
    case kFmt11x:        // op vAA
        printf(" v%d", pDecInsn->vA);
        break;
    case kFmt10t:        // op +AA
    case kFmt20t:        // op +AAAA
        {
            s4 targ = (s4) pDecInsn->vA;
            printf(" %04x // %c%04x",
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                (targ < 0) ? -targ : targ);
        }
        break;
    case kFmt22x:        // op vAA, vBBBB
        printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB);
        break;
    case kFmt21t:        // op vAA, +BBBB
        {
            s4 targ = (s4) pDecInsn->vB;
            printf(" v%d, %04x // %c%04x", pDecInsn->vA,
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                (targ < 0) ? -targ : targ);
        }
        break;
    case kFmt21s:        // op vAA, #+BBBB
        printf(" v%d, #int %d // #%x",
            pDecInsn->vA, (s4)pDecInsn->vB, (u2)pDecInsn->vB);
        break;
    case kFmt21h:        // op vAA, #+BBBB0000[00000000]
        // The printed format varies a bit based on the actual opcode.
        if (pDecInsn->opcode == OP_CONST_HIGH16) {
            s4 value = pDecInsn->vB << 16;
            printf(" v%d, #int %d // #%x",
                pDecInsn->vA, value, (u2)pDecInsn->vB);
        } else {
            s8 value = ((s8) pDecInsn->vB) << 48;
            printf(" v%d, #long %lld // #%x",
                pDecInsn->vA, value, (u2)pDecInsn->vB);
        }
        break;
    case kFmt21c:        // op vAA, thing@BBBB
    case kFmt31c:        // op vAA, thing@BBBBBBBB
    case kFmt41c:        // exop vAAAA, thing@BBBBBBBB
        printf(" v%d, %s", pDecInsn->vA, indexBuf);
        break;
    case kFmt23x:        // op vAA, vBB, vCC
    case kFmt33x:        // exop vAA, vBB, vCCCC
        printf(" v%d, v%d, v%d", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
        break;
    case kFmt22b:        // op vAA, vBB, #+CC
        printf(" v%d, v%d, #int %d // #%02x",
            pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC, (u1)pDecInsn->vC);
        break;
    case kFmt22t:        // op vA, vB, +CCCC
        {
            s4 targ = (s4) pDecInsn->vC;
            printf(" v%d, v%d, %04x // %c%04x", pDecInsn->vA, pDecInsn->vB,
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                (targ < 0) ? -targ : targ);
        }
        break;
    case kFmt22s:        // op vA, vB, #+CCCC
    case kFmt32s:        // exop vAA, vBB, #+CCCC
        printf(" v%d, v%d, #int %d // #%04x",
            pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC, (u2)pDecInsn->vC);
        break;
    case kFmt22c:        // op vA, vB, thing@CCCC
    case kFmt22cs:       // [opt] op vA, vB, field offset CCCC
    case kFmt52c:        // exop vAAAA, vBBBB, thing@CCCCCCCC
        printf(" v%d, v%d, %s", pDecInsn->vA, pDecInsn->vB, indexBuf);
        break;
    case kFmt30t:
        printf(" #%08x", pDecInsn->vA);
        break;
    case kFmt31i:        // op vAA, #+BBBBBBBB
        { */
            /* this is often, but not always, a float */
          /*  union {
                float f;
                u4 i;
            } conv;
            conv.i = pDecInsn->vB;
            printf(" v%d, #float %f // #%08x",
                pDecInsn->vA, conv.f, pDecInsn->vB);
        }
        break;
    case kFmt31t:       // op vAA, offset +BBBBBBBB
        printf(" v%d, %08x // +%08x",
            pDecInsn->vA, insnIdx + pDecInsn->vB, pDecInsn->vB);
        break;
    case kFmt32x:        // op vAAAA, vBBBB
        printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB);
        break;
    case kFmt35c:        // op {vC, vD, vE, vF, vG}, thing@BBBB
    case kFmt35ms:       // [opt] invoke-virtual+super
    case kFmt35mi:       // [opt] inline invoke
        {
            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->arg[i]);
                else
                    printf(", v%d", pDecInsn->arg[i]);
            }
            printf("}, %s", indexBuf);
        }
        break;
    case kFmt3rc:        // op {vCCCC .. v(CCCC+AA-1)}, thing@BBBB
    case kFmt3rms:       // [opt] invoke-virtual+super/range
    case kFmt3rmi:       // [opt] execute-inline/range
    case kFmt5rc:        // exop {vCCCC .. v(CCCC+AAAA-1)}, meth@BBBBBBBB
        { */
            /*
             * This doesn't match the "dx" output when some of the args are
             * 64-bit values -- dx only shows the first register.
             */
     /*       fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->vC + i);
                else
                    printf(", v%d", pDecInsn->vC + i);
            }
            printf("}, %s", indexBuf);
        }
        break;
    case kFmt51l:        // op vAA, #+BBBBBBBBBBBBBBBB
        {  */
            /* this is often, but not always, a double */
         /*   union {
                double d;
                u8 j;
            } conv;
            conv.j = pDecInsn->vB_wide;
            printf(" v%d, #double %f // #%016llx",
                pDecInsn->vA, conv.d, pDecInsn->vB_wide);
        }
        break;
    case kFmt00x:        // unknown op or breakpoint
        break;
    default:
        printf(" ???");
        break;
    } */
        }
    putchar('\n');

    if (indexBuf != indexBufChars) {
        free(indexBuf);
    }
    if (stringBuf != stringBufChars) {
            free(stringBuf);
        };
    //******************* kui's modification ends  *******************
}

/*
 * Dump a bytecode disassembly.
 */
void dumpBytecodes(DexFile* pDexFile, const DexMethod* pDexMethod)
{
    const DexCode* pCode = dexGetCode(pDexFile, pDexMethod);
    const u2* insns;
    int insnIdx;
    FieldMethodInfo methInfo;
    int startAddr;
    char* className = NULL;

    assert(pCode->insnsSize > 0);
    insns = pCode->insns;

    getMethodInfo(pDexFile, pDexMethod->methodIdx, &methInfo);
    startAddr = ((u1*)pCode - pDexFile->baseAddr);
    className = descriptorToDot(methInfo.classDescriptor);
    //******************* kui's modification begins  *******************
   // printf("%06x:                                        |[%06x] %s.%s:%s\n",
    //    startAddr, startAddr,
     //   className, methInfo.name, methInfo.signature);
    struct Op31t list[20];
    int s31t=0;
    int l31t=0;
    //******************* kui's modification ends  *******************
    insnIdx = 0;
    while (insnIdx < (int) pCode->insnsSize) {
        int insnWidth;
        DecodedInstruction decInsn;
        u2 instr;

        /*
         * Note: This code parallels the function
         * dexGetWidthFromInstruction() in InstrUtils.c, but this version
         * can deal with data in either endianness.
         *
         * TODO: Figure out if this really matters, and possibly change
         * this to just use dexGetWidthFromInstruction().
         */
        instr = get2LE((const u1*)insns);
        if (instr == kPackedSwitchSignature) {
            insnWidth = 4 + get2LE((const u1*)(insns+1)) * 2;
        } else if (instr == kSparseSwitchSignature) {
            insnWidth = 2 + get2LE((const u1*)(insns+1)) * 4;
        } else if (instr == kArrayDataSignature) {
            int width = get2LE((const u1*)(insns+1));
            int size = get2LE((const u1*)(insns+2)) |
                       (get2LE((const u1*)(insns+3))<<16);
            // The plus 1 is to round up for odd size and width.
            insnWidth = 4 + ((size * width) + 1) / 2;
        } else {
            Opcode opcode = dexOpcodeFromCodeUnit(instr);
            insnWidth = dexGetWidthFromOpcode(opcode);
            if (insnWidth == 0) {
                fprintf(stderr,
                    "GLITCH: zero-width instruction at idx=0x%04x\n", insnIdx);
                break;
            }
        }
        //******************* kui's modification begins  *******************
        dexDecodeInstruction(insns, &decInsn);
        dumpInstruction(pDexFile, pCode, insnIdx, insnWidth, &decInsn,list,s31t,l31t);
        //******************* kui's modification ends  *******************
        insns += insnWidth;
        insnIdx += insnWidth;
    }

    free(className);
}

/*
 * Dump a "code" struct.
 */
void dumpCode(DexFile* pDexFile, const DexMethod* pDexMethod)
{
    const DexCode* pCode = dexGetCode(pDexFile, pDexMethod);
    //******************* kui's modification begins  *******************
   // printf("      @registers     : %d\n", pCode->registersSize);
   // printf("      ins           : %d\n", pCode->insSize);
  //  printf("      outs          : %d\n", pCode->outsSize);
   // printf("      insns size    : %d 16-bit code units\n", pCode->insnsSize);
    dumpLocals(pDexFile, pCode, pDexMethod);
    if (gOptions.disassemble)
        dumpBytecodes(pDexFile, pDexMethod);
    dumpCatches(pDexFile, pCode);
    /* both of these are encoded in debug info */
   // dumpPositions(pDexFile, pCode, pDexMethod);

    //******************* kui's modification ends  *******************
}

/*
 * Dump a method.
 */
void dumpMethod(DexFile* pDexFile, const DexMethod* pDexMethod, int i, char* owner)
{
    const DexMethodId* pMethodId;
    const char* backDescriptor;
    const char* name;
    char* typeDescriptor = NULL;
    char* accessStr = NULL;

    if (gOptions.exportsOnly &&
        (pDexMethod->accessFlags & (ACC_PUBLIC | ACC_PROTECTED)) == 0)
    {
        return;
    }

    pMethodId = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
    name = dexStringById(pDexFile, pMethodId->nameIdx);
    typeDescriptor = dexCopyDescriptorFromMethodId(pDexFile, pMethodId);

    backDescriptor = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);

    accessStr = createAccessFlagStr(pDexMethod->accessFlags,
                    kAccessForMethod);

    //******************* kui's modification begins  *******************
    if (gOptions.outputFormat == OUTPUT_PLAIN) {
       /* printf("    #%d              : (in %s)\n", i, backDescriptor);
        printf("      name          : '%s'\n", name);
        printf("      type          : '%s'\n", typeDescriptor);
        printf("      access        : 0x%04x (%s)\n",
            pDexMethod->accessFlags, accessStr);

        if (pDexMethod->codeOff == 0) {
            printf("      code          : (none)\n");
        } else {
            printf("      code          -\n");
            dumpCode(pDexFile, pDexMethod);
        }

        if (gOptions.disassemble)
            putchar('\n'); */
            int startReg=0;
        if( pDexMethod->codeOff != 0)
        { const DexCode* pCode = dexGetCode(pDexFile, pDexMethod);
         startReg =pCode->registersSize-pCode->insSize;
         if((pDexMethod->accessFlags & ACC_STATIC) ==0)startReg++;
        }
            // find return type
            const char* returnType = strrchr(typeDescriptor, ')');
                   if (returnType == NULL) {
                     fprintf(stderr, "bad method type descriptor '%s'\n",typeDescriptor);
                            goto bail;
                        }
                     char* rtype = descriptorToDot(returnType+1);
                        /*
                         * Parameters.
                         */
                     if (typeDescriptor[0] != '(') {
                      fprintf(stderr, "ERROR: bad descriptor '%s'\n", typeDescriptor);
                         goto bail;
                       }

                   char tmpBuf[strlen(typeDescriptor)+1];      /* more than big enough */
                   const char* base = typeDescriptor+1;
                   const char* preTraverse = typeDescriptor+1;
                   char* para=(char*)malloc(sizeof(char)*1000);
                   strcpy(para,"");
                   // print the parameters with index

                   while (*base != ')') {
                     char* cp = tmpBuf;
                     int flag=0;
                     while (*base == '[')
                       *cp++ = *base++;

                     if (*base == 'L') {
                      /* copy through ';' */
                      do {
                        *cp = *base++;
                        } while (*cp++ != ';');
                                    }
                     else {
                           /* primitive char, copy it */
                            if (strchr("ZBCSIFJD", *base) == NULL) {
                           fprintf(stderr, "ERROR: bad method signature '%s'\n", base);
                            goto bail;
                               }
                             if(*base=='D'||*base=='J')
                               flag=1;
                             *cp++ = *base++;
                             }
                            /* null terminate and display */
                           *cp++ = '\0';

                             char* tmp = descriptorToDot(tmpBuf);
                           // printf("<parameter name=\"arg%d\" type=\"%s\">\n</parameter>\n",
                            // argNum++, tmp);
                             {
                              strcat(para,tmp);
                               char buffer[5];
                               sprintf(buffer, " v%d",startReg++);
                               strcat(para,buffer);
                               if(*base !=')') strcat(para,",");
                             }
                             if(flag==1)startReg++;
                              free(tmp);
                   }
              //if(strcmp(name,"<init>")==0)  printf("    procedure %s [|init|] (%s) @%s {\n", rtype,para,accessStr);
             // else if(strcmp(name,"<clinit>")==0)  printf("    procedure %s [|clinit|] (%s) @%s {\n", rtype,para,accessStr);
              printf("    procedure [|%s|] [|%s|] ([|%s|]) @owner [|%s|] @signature [|%s.%s:%s|] @Access %s {\n",
            		                 rtype, name,para,owner,backDescriptor,name,typeDescriptor,accessStr);
             free(rtype);
             free(para);
              //printf("      name          : '%s'\n", name);
            //  printf("      type          : '%s'\n", typeDescriptor);

              if (pDexMethod->codeOff == 0) {
                 printf("      #");
              } else {
                 // printf("      @code          -\n");
                  dumpCode(pDexFile, pDexMethod);
              }

              if (gOptions.disassemble)
                  putchar('\n');
              printf("   }\n");


              //******************* kui's modification ends  *******************
    } else if (gOptions.outputFormat == OUTPUT_XML) {
        bool constructor = (name[0] == '<');

        if (constructor) {
            char* tmp;

            tmp = descriptorClassToDot(backDescriptor);
            printf("<constructor name=\"%s\"\n", tmp);
            free(tmp);

            tmp = descriptorToDot(backDescriptor);
            printf(" type=\"%s\"\n", tmp);
            free(tmp);
        } else {
            printf("<method name=\"%s\"\n", name);

            const char* returnType = strrchr(typeDescriptor, ')');
            if (returnType == NULL) {
                fprintf(stderr, "bad method type descriptor '%s'\n",
                    typeDescriptor);
                goto bail;
            }

            char* tmp = descriptorToDot(returnType+1);
            printf(" return=\"%s\"\n", tmp);
            free(tmp);

            printf(" abstract=%s\n",
                quotedBool((pDexMethod->accessFlags & ACC_ABSTRACT) != 0));
            printf(" native=%s\n",
                quotedBool((pDexMethod->accessFlags & ACC_NATIVE) != 0));

            bool isSync =
                (pDexMethod->accessFlags & ACC_SYNCHRONIZED) != 0 ||
                (pDexMethod->accessFlags & ACC_DECLARED_SYNCHRONIZED) != 0;
            printf(" synchronized=%s\n", quotedBool(isSync));
        }

        printf(" static=%s\n",
            quotedBool((pDexMethod->accessFlags & ACC_STATIC) != 0));
        printf(" final=%s\n",
            quotedBool((pDexMethod->accessFlags & ACC_FINAL) != 0));
        // "deprecated=" not knowable w/o parsing annotations

        printf(" visibility=%s\n",
            quotedVisibility(pDexMethod->accessFlags));

        printf(">\n");

        /*
         * Parameters.
         */
        if (typeDescriptor[0] != '(') {
            fprintf(stderr, "ERROR: bad descriptor '%s'\n", typeDescriptor);
            goto bail;
        }

        char tmpBuf[strlen(typeDescriptor)+1];      /* more than big enough */
        int argNum = 0;

        const char* base = typeDescriptor+1;

        while (*base != ')') {
            char* cp = tmpBuf;

            while (*base == '[')
                *cp++ = *base++;

            if (*base == 'L') {
                /* copy through ';' */
                do {
                    *cp = *base++;
                } while (*cp++ != ';');
            } else {
                /* primitive char, copy it */
                if (strchr("ZBCSIFJD", *base) == NULL) {
                    fprintf(stderr, "ERROR: bad method signature '%s'\n", base);
                    goto bail;
                }
                *cp++ = *base++;
            }

            /* null terminate and display */
            *cp++ = '\0';

            char* tmp = descriptorToDot(tmpBuf);
            printf("<parameter name=\"arg%d\" type=\"%s\">\n</parameter>\n",
                argNum++, tmp);
            free(tmp);
        }

        if (constructor)
            printf("</constructor>\n");
        else
            printf("</method>\n");
    }

bail:
    free(typeDescriptor);
    free(accessStr);
}

/*
 * Dump a static (class) field.
 */
void dumpSField(const DexFile* pDexFile, const DexField* pSField, int i,bool flag)
{
    const DexFieldId* pFieldId;
    const char* backDescriptor;
    const char* name;
    const char* typeDescriptor;
    char* accessStr;

    if (gOptions.exportsOnly &&
        (pSField->accessFlags & (ACC_PUBLIC | ACC_PROTECTED)) == 0)
    {
        return;
    }

    pFieldId = dexGetFieldId(pDexFile, pSField->fieldIdx);
    name = dexStringById(pDexFile, pFieldId->nameIdx);
    typeDescriptor = dexStringByTypeIdx(pDexFile, pFieldId->typeIdx);
    backDescriptor = dexStringByTypeIdx(pDexFile, pFieldId->classIdx);

    accessStr = createAccessFlagStr(pSField->accessFlags, kAccessForField);
    //******************* kui's modification begins  *******************
    if (gOptions.outputFormat == OUTPUT_PLAIN) {
      /* printf("    #%d              : (in %s)\n", i, backDescriptor);
              printf("      name          : '%s'\n", name);
              printf("      type          : '%s'\n", typeDescriptor);
              printf("      access        : 0x%04x (%s)\n",
                  pSField->accessFlags, accessStr); */
                    if(flag)printf("      global [|%s|] @@[|%s|]", descriptorToDot(typeDescriptor),name);
                    else printf("      [|%s|] [|%s|]",descriptorToDot(typeDescriptor),name);
                    printf("    @AccessFlag %s;\n",accessStr);

            //******************* kui's modification ends  *******************
    } else if (gOptions.outputFormat == OUTPUT_XML) {
        char* tmp;

        printf("<field name=\"%s\"\n", name);

        tmp = descriptorToDot(typeDescriptor);
        printf(" type=\"%s\"\n", tmp);
        free(tmp);

        printf(" transient=%s\n",
            quotedBool((pSField->accessFlags & ACC_TRANSIENT) != 0));
        printf(" volatile=%s\n",
            quotedBool((pSField->accessFlags & ACC_VOLATILE) != 0));
        // "value=" not knowable w/o parsing annotations
        printf(" static=%s\n",
            quotedBool((pSField->accessFlags & ACC_STATIC) != 0));
        printf(" final=%s\n",
            quotedBool((pSField->accessFlags & ACC_FINAL) != 0));
        // "deprecated=" not knowable w/o parsing annotations
        printf(" visibility=%s\n",
            quotedVisibility(pSField->accessFlags));
        printf(">\n</field>\n");
    }

    free(accessStr);
}

/*
 * Dump an instance field.
 */
void dumpIField(const DexFile* pDexFile, const DexField* pIField, int i)
{
    dumpSField(pDexFile, pIField, i,false);
}

/*
 * Dump the class.
 *
 * Note "idx" is a DexClassDef index, not a DexTypeId index.
 *
 * If "*pLastPackage" is NULL or does not match the current class' package,
 * the value will be replaced with a newly-allocated string.
 */
void dumpClass(DexFile* pDexFile, int idx, char** pLastPackage)
{
    const DexTypeList* pInterfaces;
    const DexClassDef* pClassDef;
    DexClassData* pClassData = NULL;
    const u1* pEncodedData;
    const char* fileName;
    const char* classDescriptor;
    const char* superclassDescriptor;
    char* accessStr = NULL;

    int i;

    pClassDef = dexGetClassDef(pDexFile, idx);

    if (gOptions.exportsOnly && (pClassDef->accessFlags & ACC_PUBLIC) == 0) {
        //printf("<!-- omitting non-public class %s -->\n",
        //    classDescriptor);
        goto bail;
    }

    pEncodedData = dexGetClassData(pDexFile, pClassDef);
    pClassData = dexReadAndVerifyClassData(&pEncodedData, NULL);

    if (pClassData == NULL) {
        printf("Trouble reading class data (#%d)\n", idx);
        goto bail;
    }

    classDescriptor = dexStringByTypeIdx(pDexFile, pClassDef->classIdx);

    /*
     * For the XML output, show the package name.  Ideally we'd gather
     * up the classes, sort them, and dump them alphabetically so the
     * package name wouldn't jump around, but that's not a great plan
     * for something that needs to run on the device.
     */
    if (!(classDescriptor[0] == 'L' &&
          classDescriptor[strlen(classDescriptor)-1] == ';'))
    {
        /* arrays and primitives should not be defined explicitly */
        fprintf(stderr, "Malformed class name '%s'\n", classDescriptor);
        /* keep going? */
    } else if (gOptions.outputFormat == OUTPUT_XML) {
        char* mangle;
        char* lastSlash;
        char* cp;

        mangle = strdup(classDescriptor + 1);
        mangle[strlen(mangle)-1] = '\0';

        /* reduce to just the package name */
        lastSlash = strrchr(mangle, '/');
        if (lastSlash != NULL) {
            *lastSlash = '\0';
        } else {
            *mangle = '\0';
        }

        for (cp = mangle; *cp != '\0'; cp++) {
            if (*cp == '/')
                *cp = '.';
        }

        if (*pLastPackage == NULL || strcmp(mangle, *pLastPackage) != 0) {
            /* start of a new package */
            if (*pLastPackage != NULL)
                printf("</package>\n");
            printf("<package name=\"%s\"\n>\n", mangle);
            free(*pLastPackage);
            *pLastPackage = mangle;
        } else {
            free(mangle);
        }
    }

    accessStr = createAccessFlagStr(pClassDef->accessFlags, kAccessForClass);

    if (pClassDef->superclassIdx == kDexNoIndex) {
        superclassDescriptor = NULL;
    } else {
        superclassDescriptor =
            dexStringByTypeIdx(pDexFile, pClassDef->superclassIdx);
    }
    //******************* kui's modification begins  *******************
    if (gOptions.outputFormat == OUTPUT_PLAIN) {
      /* printf("Class #%d            -\n", idx);
              printf("  Class descriptor  : '%s'\n", classDescriptor);
              printf("  Access flags      : 0x%04x (%s)\n",
                  pClassDef->accessFlags, accessStr);

              if (superclassDescriptor != NULL)
                  printf("  Superclass        : '%s'\n", superclassDescriptor);

              printf("  Interfaces        -\n"); */

          if(packageName==NULL||strcmp(packageName,getPackageName(classDescriptor))!=0)
          {
            printf("package [|%s|] ;\n", getPackageName(classDescriptor));
            packageName=getPackageName(classDescriptor);
          }
             pInterfaces = dexGetInterfacesList(pDexFile, pClassDef);
            if (superclassDescriptor != NULL && strcmp(superclassDescriptor,"Ljava/lang/Object;")!= 0)
              {
              printf("record [|%s|] ", descriptorToDot(classDescriptor));
              if(!((pClassDef->accessFlags)&ACC_INTERFACE)==0)  printf(" @type interface");
              else printf(" @type class");
              printf(" @AccessFlag %s  extends [|%s|]",accessStr,descriptorToDot(superclassDescriptor));
              if (pInterfaces != NULL) {
                for (i = 0; i < (int) pInterfaces->size; i++)
                	dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,1);

                 }
              }
            else{
              printf("record [|%s|] ", descriptorToDot(classDescriptor));
              if(!((pClassDef->accessFlags)&ACC_INTERFACE)==0)  printf(" @type interface");
                          else printf(" @type class");
                          printf(" @AccessFlag %s ",accessStr);
              if (pInterfaces != NULL) {
            	  printf("extends ");
                  for (i = 0; i < (int) pInterfaces->size-1; i++)
                  dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,2);
                  dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,0);
             }
             }

            printf(" {\n");
            for (i = 0; i < (int) pClassData->header.instanceFieldsSize; i++) {
                dumpIField(pDexFile, &pClassData->instanceFields[i], i);
            }
            printf("   }\n");
                    //******************* kui's modification ends  *******************
    } else {
        char* tmp;

        tmp = descriptorClassToDot(classDescriptor);
        printf("<class name=\"%s\"\n", tmp);
        free(tmp);

        if (superclassDescriptor != NULL) {
            tmp = descriptorToDot(superclassDescriptor);
            printf(" extends=\"%s\"\n", tmp);
            free(tmp);
        }
        printf(" abstract=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_ABSTRACT) != 0));
        printf(" static=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_STATIC) != 0));
        printf(" final=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_FINAL) != 0));
        // "deprecated=" not knowable w/o parsing annotations
        printf(" visibility=%s\n",
            quotedVisibility(pClassDef->accessFlags));
        printf(">\n");
    }


    //******************* kui's modification begins  *******************
       // if (gOptions.outputFormat == OUTPUT_PLAIN)
         // if((int) pClassData->header.instanceFieldsSize!=0||(int) pClassData->header.staticFieldsSize!=0)
           //   printf("  local\n");
        for (i = 0; i < (int) pClassData->header.staticFieldsSize; i++) {
            dumpSField(pDexFile, &pClassData->staticFields[i], i,true);
        }

       // if (gOptions.outputFormat == OUTPUT_PLAIN)
         //   printf("  Instance fields   -\n");


        //if (gOptions.outputFormat == OUTPUT_PLAIN)
            //printf("  Direct methods    -\n");
        for (i = 0; i < (int) pClassData->header.directMethodsSize; i++) {
            dumpMethod(pDexFile, &pClassData->directMethods[i], i,descriptorToDot(classDescriptor));
        }

       // if (gOptions.outputFormat == OUTPUT_PLAIN)
          //  printf("  Virtual methods   -\n");
        for (i = 0; i < (int) pClassData->header.virtualMethodsSize; i++) {
            dumpMethod(pDexFile, &pClassData->virtualMethods[i], i,descriptorToDot(classDescriptor));
        }
        //******************* kui's modification ends  *******************

    // TODO: Annotations.

    if (pClassDef->sourceFileIdx != kDexNoIndex)
        fileName = dexStringById(pDexFile, pClassDef->sourceFileIdx);
    else
        fileName = "unknown";

    //******************* kui's modification begins  *******************
        if (gOptions.outputFormat == OUTPUT_PLAIN) {
           // printf("  @source_file_idx:(%s)\n",fileName);
        }
        //******************* kui's modification ends  *******************

    if (gOptions.outputFormat == OUTPUT_XML) {
        printf("</class>\n");
    }

bail:
    free(pClassData);
    free(accessStr);
}


/*
 * Advance "ptr" to ensure 32-bit alignment.
 */
static inline const u1* align32(const u1* ptr)
{
    return (u1*) (((int) ptr + 3) & ~0x03);
}


/*
 * Dump a map in the "differential" format.
 *
 * TODO: show a hex dump of the compressed data.  (We can show the
 * uncompressed data if we move the compression code to libdex; otherwise
 * it's too complex to merit a fast & fragile implementation here.)
 */
void dumpDifferentialCompressedMap(const u1** pData)
{
    const u1* data = *pData;
    const u1* dataStart = data -1;      // format byte already removed
    u1 regWidth;
    u2 numEntries;

    /* standard header */
    regWidth = *data++;
    numEntries = *data++;
    numEntries |= (*data++) << 8;

    /* compressed data begins with the compressed data length */
    int compressedLen = readUnsignedLeb128(&data);
    int addrWidth = 1;
    if ((*data & 0x80) != 0)
        addrWidth++;

    int origLen = 4 + (addrWidth + regWidth) * numEntries;
    int compLen = (data - dataStart) + compressedLen;

    printf("        (differential compression %d -> %d [%d -> %d])\n",
        origLen, compLen,
        (addrWidth + regWidth) * numEntries, compressedLen);

    /* skip past end of entry */
    data += compressedLen;

    *pData = data;
}

/*
 * Dump register map contents of the current method.
 *
 * "*pData" should point to the start of the register map data.  Advances
 * "*pData" to the start of the next map.
 */
void dumpMethodMap(DexFile* pDexFile, const DexMethod* pDexMethod, int idx,
    const u1** pData)
{
    const u1* data = *pData;
    const DexMethodId* pMethodId;
    const char* name;
    int offset = data - (u1*) pDexFile->pOptHeader;

    pMethodId = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
    name = dexStringById(pDexFile, pMethodId->nameIdx);
    printf("      #%d: 0x%08x %s\n", idx, offset, name);

    u1 format;
    int addrWidth;

    format = *data++;
    if (format == 1) {              /* kRegMapFormatNone */
        /* no map */
        printf("        (no map)\n");
        addrWidth = 0;
    } else if (format == 2) {       /* kRegMapFormatCompact8 */
        addrWidth = 1;
    } else if (format == 3) {       /* kRegMapFormatCompact16 */
        addrWidth = 2;
    } else if (format == 4) {       /* kRegMapFormatDifferential */
        dumpDifferentialCompressedMap(&data);
        goto bail;
    } else {
        printf("        (unknown format %d!)\n", format);
        /* don't know how to skip data; failure will cascade to end of class */
        goto bail;
    }

    if (addrWidth > 0) {
        u1 regWidth;
        u2 numEntries;
        int idx, addr, byte;

        regWidth = *data++;
        numEntries = *data++;
        numEntries |= (*data++) << 8;

        for (idx = 0; idx < numEntries; idx++) {
            addr = *data++;
            if (addrWidth > 1)
                addr |= (*data++) << 8;

            printf("        %4x:", addr);
            for (byte = 0; byte < regWidth; byte++) {
                printf(" %02x", *data++);
            }
            printf("\n");
        }
    }

bail:
    //if (addrWidth >= 0)
    //    *pData = align32(data);
    *pData = data;
}

/*
 * Dump the contents of the register map area.
 *
 * These are only present in optimized DEX files, and the structure is
 * not really exposed to other parts of the VM itself.  We're going to
 * dig through them here, but this is pretty fragile.  DO NOT rely on
 * this or derive other code from it.
 */
void dumpRegisterMaps(DexFile* pDexFile)
{
    const u1* pClassPool = (const u1*)pDexFile->pRegisterMapPool;
    const u4* classOffsets;
    const u1* ptr;
    u4 numClasses;
    int baseFileOffset = (u1*) pClassPool - (u1*) pDexFile->pOptHeader;
    int idx;

    if (pClassPool == NULL) {
        printf("No register maps found\n");
        return;
    }

    ptr = pClassPool;
    numClasses = get4LE(ptr);
    ptr += sizeof(u4);
    classOffsets = (const u4*) ptr;

    printf("RMAP begins at offset 0x%07x\n", baseFileOffset);
    printf("Maps for %d classes\n", numClasses);
    for (idx = 0; idx < (int) numClasses; idx++) {
        const DexClassDef* pClassDef;
        const char* classDescriptor;

        pClassDef = dexGetClassDef(pDexFile, idx);
        classDescriptor = dexStringByTypeIdx(pDexFile, pClassDef->classIdx);

        printf("%4d: +%d (0x%08x) %s\n", idx, classOffsets[idx],
            baseFileOffset + classOffsets[idx], classDescriptor);

        if (classOffsets[idx] == 0)
            continue;

        /*
         * What follows is a series of RegisterMap entries, one for every
         * direct method, then one for every virtual method.
         */
        DexClassData* pClassData;
        const u1* pEncodedData;
        const u1* data = (u1*) pClassPool + classOffsets[idx];
        u2 methodCount;
        int i;

        pEncodedData = dexGetClassData(pDexFile, pClassDef);
        pClassData = dexReadAndVerifyClassData(&pEncodedData, NULL);
        if (pClassData == NULL) {
            fprintf(stderr, "Trouble reading class data\n");
            continue;
        }

        methodCount = *data++;
        methodCount |= (*data++) << 8;
        data += 2;      /* two pad bytes follow methodCount */
        if (methodCount != pClassData->header.directMethodsSize
                            + pClassData->header.virtualMethodsSize)
        {
            printf("NOTE: method count discrepancy (%d != %d + %d)\n",
                methodCount, pClassData->header.directMethodsSize,
                pClassData->header.virtualMethodsSize);
            /* this is bad, but keep going anyway */
        }

        printf("    direct methods: %d\n",
            pClassData->header.directMethodsSize);
        for (i = 0; i < (int) pClassData->header.directMethodsSize; i++) {
            dumpMethodMap(pDexFile, &pClassData->directMethods[i], i, &data);
        }

        printf("    virtual methods: %d\n",
            pClassData->header.virtualMethodsSize);
        for (i = 0; i < (int) pClassData->header.virtualMethodsSize; i++) {
            dumpMethodMap(pDexFile, &pClassData->virtualMethods[i], i, &data);
        }

        free(pClassData);
    }
}

/*
 * Dump the requested sections of the file.
 */
void processDexFile(const char* fileName, DexFile* pDexFile)
{
    char* package = NULL;
    int i;

    if (gOptions.verbose) {
       // printf("Opened '%s', DEX version '%.3s'\n", fileName,
        //    pDexFile->pHeader->magic +4);
    }

    if (gOptions.dumpRegisterMaps) {
        dumpRegisterMaps(pDexFile);
        return;
    }

    if (gOptions.showFileHeaders) {
        dumpFileHeader(pDexFile);
        dumpOptDirectory(pDexFile);
    }

    if (gOptions.outputFormat == OUTPUT_XML)
        printf("<api>\n");

    for (i = 0; i < (int) pDexFile->pHeader->classDefsSize; i++) {
        if (gOptions.showSectionHeaders)
            dumpClassDef(pDexFile, i);

        dumpClass(pDexFile, i, &package);
    }

    /* free the last one allocated */
    if (package != NULL) {
        printf("</package>\n");
        free(package);
    }

    if (gOptions.outputFormat == OUTPUT_XML)
        printf("</api>\n");
}


/*
 * Process one file.
 */
int process(const char* fileName)
{
    DexFile* pDexFile = NULL;
    MemMapping map;
    bool mapped = false;
    int result = -1;

    if (gOptions.verbose)
       // printf("Processing '%s'...\n", fileName);

    if (dexOpenAndMap(fileName, gOptions.tempFileName, &map, false) != 0) {
        return result;
    }
    mapped = true;

    int flags = kDexParseVerifyChecksum;
    if (gOptions.ignoreBadChecksum)
        flags |= kDexParseContinueOnError;

    pDexFile = dexFileParse((u1*)map.addr, map.length, flags);
    if (pDexFile == NULL) {
        fprintf(stderr, "ERROR: DEX parse failed\n");
        goto bail;
    }

    if (gOptions.checksumOnly) {
        printf("Checksum verified\n");
    } else {
        processDexFile(fileName, pDexFile);
    }

    result = 0;

bail:
    if (mapped)
        sysReleaseShmem(&map);
    if (pDexFile != NULL)
        dexFileFree(pDexFile);
    return result;
}


/*
 * Show usage.
 */
void usage(void)
{
    fprintf(stderr, "Copyright (C) 2007 The Android Open Source Project\n\n");
    fprintf(stderr,
        "%s: [-c] [-d] [-f] [-h] [-i] [-l layout] [-m] [-t tempfile] dexfile...\n",
        gProgName);
    fprintf(stderr, "\n");
    fprintf(stderr, " -c : verify checksum and exit\n");
    fprintf(stderr, " -d : disassemble code sections\n");
    fprintf(stderr, " -f : display summary information from file header\n");
    fprintf(stderr, " -h : display file header details\n");
    fprintf(stderr, " -i : ignore checksum failures\n");
    fprintf(stderr, " -l : output layout, either 'plain' or 'xml'\n");
    fprintf(stderr, " -m : dump register maps (and nothing else)\n");
    fprintf(stderr, " -t : temp file name (defaults to /sdcard/dex-temp-*)\n");
}

/*
 * Parse args.
 *
 * I'm not using getopt_long() because we may not have it in libc.
 */
int main(int argc, char* const argv[])
{
    bool wantUsage = false;
    int ic;

    memset(&gOptions, 0, sizeof(gOptions));
    gOptions.verbose = true;

    while (1) {
        ic = getopt(argc, argv, "cdfhil:mt:");
        if (ic < 0)
            break;

        switch (ic) {
        case 'c':       // verify the checksum then exit
            gOptions.checksumOnly = true;
            break;
        case 'd':       // disassemble Dalvik instructions
            gOptions.disassemble = true;
            break;
        case 'f':       // dump outer file header
            gOptions.showFileHeaders = true;
            break;
        case 'h':       // dump section headers, i.e. all meta-data
            gOptions.showSectionHeaders = true;
            break;
        case 'i':       // continue even if checksum is bad
            gOptions.ignoreBadChecksum = true;
            break;
        case 'l':       // layout
            if (strcmp(optarg, "plain") == 0) {
                gOptions.outputFormat = OUTPUT_PLAIN;
            } else if (strcmp(optarg, "xml") == 0) {
                gOptions.outputFormat = OUTPUT_XML;
                gOptions.verbose = false;
                gOptions.exportsOnly = true;
            } else {
                wantUsage = true;
            }
            break;
        case 'm':       // dump register maps only
            gOptions.dumpRegisterMaps = true;
            break;
        case 't':       // temp file, used when opening compressed Jar
            gOptions.tempFileName = optarg;
            break;
        default:
            wantUsage = true;
            break;
        }
    }

    if (optind == argc) {
        fprintf(stderr, "%s: no file specified\n", gProgName);
        wantUsage = true;
    }

    if (gOptions.checksumOnly && gOptions.ignoreBadChecksum) {
        fprintf(stderr, "Can't specify both -c and -i\n");
        wantUsage = true;
    }

    if (wantUsage) {
        usage();
        return 2;
    }

    int result = 0;
    while (optind < argc) {
        result |= process(argv[optind++]);
    }

    return (result != 0);
}
