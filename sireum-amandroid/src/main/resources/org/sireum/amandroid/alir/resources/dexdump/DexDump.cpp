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
/* further modified by Sankardas Roy (email:sroy@ksu.edu) and Fengguo Wei */

#include "libdex/DexFile.h"
#include "libdex/CmdUtils.h"
#include "libdex/DexCatch.h"
#include "libdex/DexClass.h"
#include "libdex/DexDebugInfo.h"
#include "libdex/DexOpcodes.h"
#include "libdex/DexProto.h"
#include "libdex/InstrUtils.h"
#include "libdex/SysUtil.h"

#include "pilar.h" // sankar adds this header file
#include "pStash.h" // sankar adds this which defines a container class

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include <assert.h>


// #include<map>
 // using namespace std;

static const char* gProgName = "dexdump";
static char* packageName =NULL;
enum OutputFormat {
    OUTPUT_PLAIN = 0,               /* default */
    OUTPUT_XML,                     /* fancy */
};


u4 lastAddress = 0; // sankar adds : this would store the last address of the current method
typedef struct locVarInfo locVarInf; // sankar adds

PStash* locVarList = NULL; // sankar adds this list which holds local variables' info for the current method
char* pilarRootDir; // the root directory which contains all pilar
char* currentDir; // this will hold the current package's directory name; // sankar adds
char* currentFile; // this will hold the current class name; // sankar adds
FILE* pFp; // a file pointer; at any point, it corresponds to the current "class" of dex;
FILE* topFp; // represents a special file named top.txt in the root directory; In addition to other info, this can hold some error information, if any.

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
    newStr = (char*)malloc(targetLen +20 + arrayDepth * 2 +1); // source of memory leak ?

    /* copy class name over */

    int i;
        int counter=0;
                 for (i = 0; i < targetLen; i++) {
                     char ch = str[offset + i];
                     if (ch == '/') {
                       newStr[i+counter]='.';
                       //counter++;
                       //newStr[i+counter]=':';
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
    // printf("class descriptor is \n %s \n while pilar format is \n %s \n", newStr, toPilar(newStr)); // *********** this print is only for testing
    //******************* kui's modification ends  *******************
    return newStr;
}



/* sankar adds arraySym: This func checks the array depth of a descriptor and returns correct number of the array symbols.  For
 * "example, if descriptor starts with [I" then it returns "[]".
 */
static char* arraySym(const char* str)
{
    int targetLen = strlen(str);
    int offset = 0;
    int arrayDepth = 0;
    char* newStr;

    /* strip leading [s;*/
    while (targetLen > 1 && str[offset] == '[') {
        offset++;
        targetLen--;
    }
    arrayDepth = offset;

    newStr = (char*)malloc(2*arrayDepth + 1); // source of memory leak as this space is never freed;

    int i = 0;

    /* add the appropriate number of brackets for arrays */
    while (arrayDepth-- > 0) {
        newStr[i] = '[';
        i++;
        newStr[i] = ']';
        i++;
    }
   
    newStr[i] = '\0';
    assert(i == 2*arrayDepth);
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
        // fprintf(pFp, "Optimized DEX file header:\n"); // sankar

        asciify(sanitized, pOptHeader->magic, sizeof(pOptHeader->magic));
        printf("magic               : '%s'\n", sanitized);
        // fprintf(pFp, "magic               : '%s'\n", sanitized); // sankar

        printf("dex_offset          : %d (0x%06x)\n",
            pOptHeader->dexOffset, pOptHeader->dexOffset);
        // fprintf(pFp, "dex_offset          : %d (0x%06x)\n",
           // pOptHeader->dexOffset, pOptHeader->dexOffset);  // sankar

        printf("dex_length          : %d\n", pOptHeader->dexLength);
        // fprintf(pFp, "dex_length          : %d\n", pOptHeader->dexLength); // sankar

        printf("deps_offset         : %d (0x%06x)\n",
            pOptHeader->depsOffset, pOptHeader->depsOffset);
        // fprintf(pFp, "deps_offset         : %d (0x%06x)\n",
           //  pOptHeader->depsOffset, pOptHeader->depsOffset); // sankar

        printf("deps_length         : %d\n", pOptHeader->depsLength);
        // fprintf(pFp, "deps_length         : %d\n", pOptHeader->depsLength); // sankar

        printf("opt_offset          : %d (0x%06x)\n",
            pOptHeader->optOffset, pOptHeader->optOffset);
        // fprintf(pFp, "opt_offset          : %d (0x%06x)\n",
           // pOptHeader->optOffset, pOptHeader->optOffset); // sankar

        printf("opt_length          : %d\n", pOptHeader->optLength);
        // fprintf(pFp, "opt_length          : %d\n", pOptHeader->optLength); // sankar

        printf("flags               : %08x\n", pOptHeader->flags);
        // fprintf(pFp, "flags               : %08x\n", pOptHeader->flags); // sankar

        printf("checksum            : %08x\n", pOptHeader->checksum);
        // fprintf(pFp, "checksum            : %08x\n", pOptHeader->checksum); // sankar
        printf("\n");
        // fprintf(pFp, "\n"); // sankar
    }

    printf("DEX file header:\n");
    // fprintf(pFp, "DEX file header:\n"); // sankar

    asciify(sanitized, pHeader->magic, sizeof(pHeader->magic));
    printf("magic               : '%s'\n", sanitized);
    // fprintf(pFp, "magic               : '%s'\n", sanitized); // sankar

    printf("checksum            : %08x\n", pHeader->checksum);
    // fprintf(pFp, "checksum            : %08x\n", pHeader->checksum); // sankar

    printf("signature           : %02x%02x...%02x%02x\n",
        pHeader->signature[0], pHeader->signature[1],
        pHeader->signature[kSHA1DigestLen-2],
        pHeader->signature[kSHA1DigestLen-1]);
    // fprintf(pFp, "signature           : %02x%02x...%02x%02x\n",
       // pHeader->signature[0], pHeader->signature[1],
       // pHeader->signature[kSHA1DigestLen-2],
       // pHeader->signature[kSHA1DigestLen-1]); // sankar

    printf("file_size           : %d\n", pHeader->fileSize);
    // fprintf(pFp, "file_size           : %d\n", pHeader->fileSize); // sankar

    printf("header_size         : %d\n", pHeader->headerSize);
    // fprintf(pFp, "header_size         : %d\n", pHeader->headerSize); // sankar

    printf("link_size           : %d\n", pHeader->linkSize);
    // fprintf(pFp, "link_size           : %d\n", pHeader->linkSize); // sankar

    printf("link_off            : %d (0x%06x)\n",
        pHeader->linkOff, pHeader->linkOff);
    // fprintf(pFp, "link_off            : %d (0x%06x)\n",
       // pHeader->linkOff, pHeader->linkOff); // sankar

    printf("string_ids_size     : %d\n", pHeader->stringIdsSize);
    // fprintf(pFp, "string_ids_size     : %d\n", pHeader->stringIdsSize); // sankar

    printf("string_ids_off      : %d (0x%06x)\n",
        pHeader->stringIdsOff, pHeader->stringIdsOff);
    // fprintf(pFp, "string_ids_off      : %d (0x%06x)\n",
       //  pHeader->stringIdsOff, pHeader->stringIdsOff); // sankar

    printf("type_ids_size       : %d\n", pHeader->typeIdsSize);
    // fprintf(pFp, "type_ids_size       : %d\n", pHeader->typeIdsSize); // sankar

    printf("type_ids_off        : %d (0x%06x)\n",
        pHeader->typeIdsOff, pHeader->typeIdsOff);
    // fprintf(pFp, "type_ids_off        : %d (0x%06x)\n",
       // pHeader->typeIdsOff, pHeader->typeIdsOff); // sankar

    printf("proto_ids_size       : %d\n", pHeader->protoIdsSize);
    printf("proto_ids_off        : %d (0x%06x)\n",
    		pHeader->protoIdsOff, pHeader->protoIdsOff);

    printf("field_ids_size      : %d\n", pHeader->fieldIdsSize);
    // fprintf(pFp, "field_ids_size      : %d\n", pHeader->fieldIdsSize); // sankar

    printf("field_ids_off       : %d (0x%06x)\n",
        pHeader->fieldIdsOff, pHeader->fieldIdsOff);
    // fprintf(pFp, "field_ids_off       : %d (0x%06x)\n",
       // pHeader->fieldIdsOff, pHeader->fieldIdsOff); // sankar

    printf("method_ids_size     : %d\n", pHeader->methodIdsSize);
    // fprintf(pFp, "method_ids_size     : %d\n", pHeader->methodIdsSize); // sankar

    printf("method_ids_off      : %d (0x%06x)\n",
        pHeader->methodIdsOff, pHeader->methodIdsOff);
    // fprintf(pFp, "method_ids_off      : %d (0x%06x)\n",
       // pHeader->methodIdsOff, pHeader->methodIdsOff); // sankar

    printf("class_defs_size     : %d\n", pHeader->classDefsSize);
    // fprintf(pFp, "class_defs_size     : %d\n", pHeader->classDefsSize); // sankar

    printf("class_defs_off      : %d (0x%06x)\n",
        pHeader->classDefsOff, pHeader->classDefsOff);
    // fprintf(pFp, "class_defs_off      : %d (0x%06x)\n",
       // pHeader->classDefsOff, pHeader->classDefsOff); // sankar

    printf("data_size           : %d\n", pHeader->dataSize);
    // fprintf(pFp, "data_size           : %d\n", pHeader->dataSize); // sankar

    printf("data_off            : %d (0x%06x)\n",
        pHeader->dataOff, pHeader->dataOff);
    // fprintf(pFp, "data_off            : %d (0x%06x)\n",
       // pHeader->dataOff, pHeader->dataOff);
    printf("\n");
    //fprintf(pFp, "\n"); // sankar
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
    // fprintf(pFp, "OPT section contents:\n"); // sankar

    const u4* pOpt = (const u4*) ((u1*) pOptHeader + pOptHeader->optOffset);

    if (*pOpt == 0) {
        printf("(1.0 format, only class lookup table is present)\n\n");
       // fprintf(pFp, "(1.0 format, only class lookup table is present)\n\n");
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

       // fprintf(pFp, "Chunk %08x (%c%c%c%c) - %s (%d bytes)\n", *pOpt,
           // *pOpt >> 24, (char)(*pOpt >> 16), (char)(*pOpt >> 8), (char)*pOpt,
           // verboseStr, size); // sankar

        size = (size + 8 + 7) & ~7;
        pOpt += size / sizeof(u4);
    }
    printf("\n");
   // fprintf(pFp, "\n");
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
   // fprintf(pFp, "Class #%d header:\n", idx); // sankar

    printf("class_idx           : %d\n", pClassDef->classIdx);
  //  fprintf(pFp, "class_idx           : %d\n", pClassDef->classIdx); // sankar

    printf("access_flags        : %d (0x%04x)\n",
        pClassDef->accessFlags, pClassDef->accessFlags);
  //  fprintf(pFp, "access_flags        : %d (0x%04x)\n",
    //    pClassDef->accessFlags, pClassDef->accessFlags); // sankar

    printf("superclass_idx      : %d\n", pClassDef->superclassIdx);
  //  fprintf(pFp, "superclass_idx      : %d\n", pClassDef->superclassIdx); // sankar

    printf("interfaces_off      : %d (0x%06x)\n",
        pClassDef->interfacesOff, pClassDef->interfacesOff);
  //  fprintf(pFp, "interfaces_off      : %d (0x%06x)\n",
      //  pClassDef->interfacesOff, pClassDef->interfacesOff); // sankar

    printf("source_file_idx     : %d\n", pClassDef->sourceFileIdx);
  //  fprintf(pFp, "source_file_idx     : %d\n", pClassDef->sourceFileIdx); // sankar

    printf("annotations_off     : %d (0x%06x)\n",
        pClassDef->annotationsOff, pClassDef->annotationsOff);
  //  fprintf(pFp, "annotations_off     : %d (0x%06x)\n",
    //    pClassDef->annotationsOff, pClassDef->annotationsOff); // sankar

    printf("class_data_off      : %d (0x%06x)\n",
        pClassDef->classDataOff, pClassDef->classDataOff);
   // fprintf(pFp, "class_data_off      : %d (0x%06x)\n",
     //   pClassDef->classDataOff, pClassDef->classDataOff); // sankar

    printf("static_fields_size  : %d\n", pClassData->header.staticFieldsSize);
  //  fprintf(pFp, "static_fields_size  : %d\n", pClassData->header.staticFieldsSize); // sankar

    printf("instance_fields_size: %d\n",
            pClassData->header.instanceFieldsSize);
  //  fprintf(pFp, "instance_fields_size: %d\n",
    //        pClassData->header.instanceFieldsSize); // sankar

    printf("direct_methods_size : %d\n", pClassData->header.directMethodsSize);

  //  fprintf(pFp, "direct_methods_size : %d\n", pClassData->header.directMethodsSize); // sankar

    printf("virtual_methods_size: %d\n",
            pClassData->header.virtualMethodsSize);

  //  fprintf(pFp, "virtual_methods_size: %d\n",
    //        pClassData->header.virtualMethodsSize); // sankar

    printf("\n");

  //  fprintf(pFp, "\n"); // sankar

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
        { 
			printf(", %s",descriptorToDot(interfaceName));
			fprintf(pFp, ", %s", toPilar(descriptorToDot(interfaceName)));
			
		}
      else if(flag==2)
        { 
			printf(" %s,",descriptorToDot(interfaceName));
			fprintf(pFp, " %s,",toPilar(descriptorToDot(interfaceName)));
			
		}
      else if(flag==0)
        { 
			printf(" %s",descriptorToDot(interfaceName));
			fprintf(pFp, " %s",toPilar(descriptorToDot(interfaceName)));
			
		}
        //******************* kui's modification ends  *******************
    } else {
        char* dotted = descriptorToDot(interfaceName);
        printf("<implements name=\"%s\">\n</implements>\n", dotted);
        fprintf(pFp, "<implements name=\"%s\">\n</implements>\n", dotted);
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
        printf("      @catches:(none)\n"); // not in pilar
        return;
    }

    printf("      catches       : %d\n", triesSize); // not in pilar

    const DexTry* pTries = dexGetTries(pCode);
    u4 i;

    for (i = 0; i < triesSize; i++) {
        const DexTry* pTry = &pTries[i];
        u4 start = pTry->startAddr;
        u4 end = start + pTry->insnCount;
        DexCatchIterator iterator;

        printf("        0x%04x - 0x%04x\n", start, end); // not in pilar

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
            printf("  catch  %s @[L%06x..L%06x] goto L%06x;\n", descriptor,((u1*)insns - pDexFile->baseAddr) +start*2,((u1*)insns - pDexFile->baseAddr) +end*2,((u1*)insns - pDexFile->baseAddr) +(handler->address)*2); // need to change back

           /*********** sankar starts ***********/
            if(end > lastAddress)  
			{
				end = lastAddress; // casting ok?
			}
		   //  else
		   //    printf("end = %d , lastAdd = %d  \n", end, lastAddress);
          /********** sankar ends **********/

            fprintf(pFp,"  catch  %s @[L%06x..L%06x] goto L%06x;\n", toPilar(descriptor),((u1*)insns - pDexFile->baseAddr) +start*2,((u1*)insns - pDexFile->baseAddr) +end*2,((u1*)insns - pDexFile->baseAddr) +(handler->address)*2); // sankar

            
			// printf("insns = %06x , pDexFile->baseAddr = %06x , start*2 =  %06x , end*2 = %06x \n", (u1*)insns , pDexFile->baseAddr , start*2 , end*2);
        }
    }
    //******************* kui's modification ends  *******************
}

static int dumpPositionsCb(void *cnxt, u4 address, u4 lineNum)
{
    printf("        0x%04x line=%d\n", address, lineNum); // not in pilar
    return 0;
}

/*
 * Dump the positions list.
 */
void dumpPositions(DexFile* pDexFile, const DexCode* pCode,
        const DexMethod *pDexMethod)
{

    printf("      positions     : \n"); // not in pilar
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

   locVarInf* temp; // sankar adds temp

   printf("        0x%04x - 0x%04x reg=%d %s %s %s\n",
            startAddress, endAddress, reg, name, descriptor,
			            signature);
  /************* sankar starts ************/

   temp = new locVarInf(descriptorToDot(descriptor), name, reg, startAddress, endAddress); 
   //printf("***** %d \n", temp->reg);

   assert(locVarList);
   locVarList->add((void*)temp);
  /****************** sankar ends ***********/

  //******************* kui's modification begins : sankar moved the following to dumpLocals()  *******************
	// fprintf(pFp, "        [|%s|] [|%s|] @reg %d @scope (L%04x,L%04x) ;\n",
      // descriptorToDot(descriptor), name,reg, startAddress,  +endAddress );
    //******************* kui's modification ends  *******************
}



/*
 * Dump the locals list.
 */

void dumpLocals(DexFile* pDexFile, const DexCode* pCode,
        const DexMethod *pDexMethod, char* tailRegs)
{
	/**** moved the following block to dumpMethod() ***

    if(locVarList) 
		delete locVarList; // sankar adds: check some possible memory leak here

    locVarList = new PStash; // sankar adds

    ****/

	printf("      locals :\n");
	fprintf(pFp,"      temp ;\n"); // sankar adds: previously it was "local temp" instead of just "temp"

	/****** moved the following block to dumpMethod() ****
    const DexMethodId *pMethodId
            = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
    const char *classDescriptor
            = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);

    dexDecodeDebugInfo(pDexFile, pCode, classDescriptor, pMethodId->protoIdx,
            pDexMethod->accessFlags, NULL, dumpLocalsCb, NULL);
    ****/

   /*********** sankar starts ***********/



   if(locVarList)
       {
		 for(int i=0; i < locVarList->count(); i++)
		  {	 
		    void* temp1 = (*locVarList)[i];
		    if(temp1) 
			   {
				 locVarInf* t1 = (locVarInf*) temp1;
                 if(t1->descriptor && t1->name && !t1->paramFlag)
				 {
				    // fprintf(pFp, "        %s v%d @varname %s @scope ((L%04x,L%04x)",
				                     // toPilar(t1->descriptor), t1->reg, toPilar(t1->name), t1->startAddress,  t1->endAddress );

                    int conflictCount = 0;

                    for(int j=i+1; j < locVarList->count(); j++)
				      {
				        void* temp2 = (*locVarList)[j];
				        if(temp2)
				          {
				            locVarInf* t2 = (locVarInf*) temp2;
					        if(t2->descriptor && t2->name && !t2->paramFlag  && t1->name && !strcmp(t2->name, t1->name))
					          { 
						        conflictCount++;

			                    // fprintf(pFp, ", (L%04x,L%04x)", t2->startAddress,  t2->endAddress);
				                delete t2; // is it safe?
						        locVarList->remove(j); // possible memory leak?

                                // char* tName;
					        	// char extra[6]; //*******  assume that number of conflicts is less than 10^4 *******
					        	// snprintf(extra, 6, ".%d", conflictCount);
					        	// tName = new char[strlen(t2->name) + strlen(extra) + 2]; // i think even +1 would have been suffi
						        // strcpy(tName, t2->name);
						        // strcat(tName, extra);
						        // free(t2->name);
					        	// t2->name= tName;
                             }
				          }
				      }
                 
				    // fprintf(pFp, ");"); // note that ")" closes the scopes which have been already listed for the current local variable
                  }
                 
				 // fprintf(pFp, "\n");  
				 // delete t1; // do it later
				 // locVarList->remove(i); // do it later

               }
		  }
	   }

   int numOfRegForLocals = pCode->registersSize - pCode->insSize;


   for(int j=0; j < numOfRegForLocals; j++)
   {
   /*
	 bool presenseFlag = false;  

     if(locVarList)
        {
		   for(int i=0; i < locVarList->count(); i++)
		     {	 
		        void* temp1 = (*locVarList)[i];
		        if(temp1) 
			      {
				    locVarInf* t1 = (locVarInf*) temp1;
                    if(t1 && t1->reg == j)
				      {
                         presenseFlag = true;

                      }

                  }
	    	  }
	     }
    */
	   //if(!presenseFlag)  // commenting out -- now we want to print each regForLocals here	 
	   fprintf(pFp, "        v%d;\n", j); // sankar: not in dexdump; in pilar printing an extra register used as a local but which does not have any var name
       //fprintf(pFp, "\n"); // sankar: not in dexdump. NOTE that there is possible error in above print; some registers can be already part of a long local
                 
   }

   fprintf(pFp, "%s", tailRegs); // printing tail registers of Long and Double parameters
   if(tailRegs) 
	   free(tailRegs);
// deleting locVarList

     if(locVarList)
        {
		   for(int i=0; i < locVarList->count(); i++)
		     {	 
		        void* temp1 = (*locVarList)[i];
		        if(temp1) 
			      {
				    locVarInf* t1 = (locVarInf*) temp1;
                    if(t1)
				      {
                 
			    	     delete t1; // is it safe?
				         locVarList->remove(i); // possible memory leak?
					  }

                  }
	    	  }
	     }

   /************* sankar ends *************/

   fprintf(pFp, "      \n"); // sankar: not in dexdump
   
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
    // case kFmt40sc:  // sankar: this case is no longer defined 
    // case kFmt41c:   // sankar: this case is no longer defined
    // case kFmt5rc:   // sankar: this case is no longer defined
        index = pDecInsn->vB;
        width = 8;
        break;
    case kFmt22c:
    case kFmt22cs:
        index = pDecInsn->vC;
        width = 4;
        break;
                       // sankar: the following case i.e. kFmt52c is no longer defined, so commenting out the following block.
    /* case kFmt52c:
        index = pDecInsn->vC;
        width = 8;
        break; */    
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
                outSize = snprintf(buf, bufSize, "%s", toPilar(methInfo.name));
                free((void *) methInfo.signature);
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
                outSize = snprintf(buf, bufSize, "`@@%s.%s` ",                   // @@ identifies global/static variables in pilar
                    descriptorToDot(fieldInfo.classDescriptor), fieldInfo.name);
                                        //descriptorToDot(fieldInfo.signature));
                break;
              default:
                outSize = snprintf(buf, bufSize, ".`%s.%s` ",
                    descriptorToDot( fieldInfo.classDescriptor), fieldInfo.name);
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
 * Sankar copied this function from Android4.1.original.source.DexDump.cpp and then modified it to output pilar following some of Kui's previous modification
 */
void dumpInstruction(DexFile* pDexFile, const DexCode* pCode, int insnIdx,
    int insnWidth, const DecodedInstruction* pDecInsn, PStash &list, int &s31t, int &l31t) 
    // sankar adds 3 extra args (list,int &s31t,int &l31t) to process switch-data-table and fill-array-data-table  
{
    char indexBufChars[200];
    char *indexBuf = indexBufChars;
    const u2* insns = pCode->insns;
    int i;

    lastAddress = insnIdx; // sankar adds

   // printf("insns = %06x , pDexFile->baseAddr = %06x , insnIdx*2 =  %06x \n", (u1*)insns , pDexFile->baseAddr , insnIdx*2 );

    printf("%06x:", ((u1*)insns - pDexFile->baseAddr) + insnIdx*2);
    fprintf(pFp,"#L%06x.   ", ((u1*)insns - pDexFile->baseAddr) + insnIdx*2); // sankar adds "L" at the left
 
   //***** sankar: the following block simply dumps the hex bytes of the whole instruction in dexdump but not in pilar ***** 

    for (i = 0; i < 8; i++) {
        if (i < insnWidth) {
            if (i == 7) {
                printf(" ... ");
            } else {
                // print 16-bit value in little-endian order 
                const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                printf(" %02x%02x", bytePtr[0], bytePtr[1]);
            }
        } else {
            fputs("     ", stdout);
        }
    }

  // ****** sankar: the above block ends *******

    if (pDecInsn->opcode == OP_NOP) {
        u2 instr = get2LE((const u1*) &insns[insnIdx]);


        if (instr == kPackedSwitchSignature) {

           printf("|%04x: packed-switch-data (%d units)",   insnIdx, insnWidth); // not in pilar

           // ******* sankar starts ********

		     struct Op31t* temp = (struct Op31t*)list[l31t];
		     assert(temp);

             // extra check to test if the appearance of switch  statements was in the same sequence as the appearance of the corresponding data-tables in the code later
          
              assert(insnIdx == (temp->insnIdx + temp->vB)); // if passes, then it is in ok sequence
               
		       //  printf("\n test; currInsnIdx = %04x, l31t = %d, storedInsnIdx = %04x, stored->vA= %d, and stored->vB = %04x \n", insnIdx, l31t, list[l31t].insnIdx, list[l31t].vA, list[l31t].vB);

		      // extra check ends

              fprintf(pFp, "switch  v%d\n", temp->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+2];
              int minValue=(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24);
              for (i = 4; i < insnWidth; i+=2,minValue++) {
                  const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                     fprintf(pFp, "                 | %d => goto L%06x\n", minValue,
                    		 ((u1*)insns - pDexFile->baseAddr) +(((bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24))+ temp->insnIdx)*2);
               }
              fprintf(pFp, "                 | else => goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(temp->insnIdx+3)*2);
              ++l31t;
           // ********* sankar ends *********

        } else if (instr == kSparseSwitchSignature) {

            printf("|%04x: sparse-switch-data (%d units)",  insnIdx, insnWidth); // not in pilar

            // ******** sankar starts ********
		     struct Op31t* temp = (struct Op31t*)list[l31t];
		     assert(temp);

             // extra check to test if the appearance of switch statements was in the same sequence as the appearance of the corresponding data-tables in the code later
          
              assert(insnIdx == (temp->insnIdx + temp->vB)); // if passes, then it is in ok sequence
               
		       //  printf("\n test; currInsnIdx = %04x, l31t = %d, storedInsnIdx = %04x, stored->vA= %d, and stored->vB = %04x \n", insnIdx, l31t, list[l31t].insnIdx, list[l31t].vA, list[l31t].vB);

		      // extra check ends

              fprintf(pFp, "switch v%d\n", temp->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+1];
              int size=(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8);
              int counter=0;
              for (i = 2; counter < size; i+=2,++counter) {
                   const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                    fprintf(pFp, "                 | %d => goto L%06x\n",
                    (bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24),
                    ((u1*)insns - pDexFile->baseAddr) + (((bytePtr[size*4] & 0xFF) |((bytePtr[size*4+1] & 0xFF) << 8) |((bytePtr[size*4+2] & 0xFF) << 16) |(bytePtr[size*4+3] << 24))+ temp->insnIdx)*2);
              }
                     fprintf(pFp, "                 | else => goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(temp->insnIdx+3)*2);
                     ++l31t;
           // *********** sankar ends **************

        } else if (instr == kArrayDataSignature) {

          printf("|%04x: array-data (%d units)", insnIdx, insnWidth); // not in pilar

          // ********** sankar starts ******************
		     struct Op31t* temp = (struct Op31t*)list[l31t];
		     assert(temp);

             // extra check to test if the appearance of fill-array statements was in the same sequence as the appearance of the corresponding data-tables in the code later
          
              assert(insnIdx == (temp->insnIdx + temp->vB)); // if passes, then it is in ok sequence
               
		       //  printf("\n test; currInsnIdx = %04x, l31t = %d, storedInsnIdx = %04x, stored->vA= %d, and stored->vB = %04x \n", insnIdx, l31t, list[l31t].insnIdx, list[l31t].vA, list[l31t].vB);

		      // extra check ends

              fprintf(pFp, "v%d:= new `byte`[", temp->vA);
              const u1* bytePtr = (const u1*) &insns[insnIdx+1];
              int length=(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8);
              switch (length){
              case 2:
                for (i = 4; i < insnWidth; i++) {
                 const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                 if(i==insnWidth-1)
                   fprintf(pFp, "%d ",(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8));
                 else
                 fprintf(pFp, "%d, ",(bytePtr[0] & 0xFF) | ((bytePtr[1] & 0xFF) << 8));
                    }
                break;
              case 4:
                for (i = 4; i < insnWidth; i+=2) {
               const u1* bytePtr = (const u1*) &insns[insnIdx+i];
               if(i>=insnWidth-2)
               fprintf(pFp, "%d ",(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24));
               else fprintf(pFp, "%d, ",(bytePtr[0] & 0xFF) |((bytePtr[1] & 0xFF) << 8) |((bytePtr[2] & 0xFF) << 16) |(bytePtr[3] << 24));
                  }
                break;
               case 8:
                 for (i = 4; i < insnWidth; i+=4) {
                   const u1* bytePtr = (const u1*) &insns[insnIdx+i];
                   if(i>=insnWidth-4)
                   fprintf(pFp, "%lldL"  ,(bytePtr[0]   & 0xFFL) |
                                   ((bytePtr[1] & 0xFFL) << 8) |
                                   ((bytePtr[2] & 0xFFL) << 16) |
                                   ((bytePtr[3]  & 0xFFL) << 24) |
                                   ((bytePtr[4]  & 0xFFLL) << 32) |
                                   ((bytePtr[5] & 0xFFLL) << 40) |
                                   ((bytePtr[6] & 0xFFLL) << 48) |
                                   (((long long)bytePtr[7]) << 56));
                   else
                      fprintf(pFp, "%lldL, "  ,(bytePtr[0]   & 0xFFL) |
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
                   //fprintf(pFp, "");
                   break;
              }
              fprintf(pFp, "];\n");
              fprintf(pFp, "#L%06x.   goto L%06x;", ((u1*)insns - pDexFile->baseAddr) + insnIdx*2 + 1, ((u1*)insns - pDexFile->baseAddr) +(temp->insnIdx+3)*2);
              ++l31t;
           // ************* sankar ends ***********

        } else {  // sankar will check if this little one-line "else block" is necessary
            printf("|%04x: nop // spacer", insnIdx);
            // fprintf(pFp, "|%04x: nop // spacer", insnIdx); // fengguo was getting error if not uncommented
        }
    } 
	
	else {   // sankar includes the remaining of this method (a LOT of lines) as part of this else block

       printf("|%04x: %s", insnIdx, dexGetOpcodeName(pDecInsn->opcode)); // not in pilar
    

    if (pDecInsn->indexType != kIndexNone) {
        indexBuf = indexString(pDexFile, pDecInsn,
                indexBufChars, sizeof(indexBufChars));
    }

    switch (dexGetFormatFromOpcode(pDecInsn->opcode)) {
    case kFmt10x:        // op
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
			
             case 0x00: // NOP
			   break;

			 case 0x0e:
			  outReturnVoid();
			  break;

             default:
			  fprintf(pFp, " 10x????");
			  break;
			
			}
		/***** sankar ends ***/
        break;
    case kFmt12x:        // op vA, vB
        printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
             case 0x01: // move
			  outMove(pDecInsn->vA, pDecInsn->vB);
              break;
			 case 0x04: // move-wide
              outMoveWide(pDecInsn->vA, pDecInsn->vB); 
              break;
			 case 0x07: // move-object
			  outMoveObject(pDecInsn->vA, pDecInsn->vB); 
              break;
			 case 0x21: // array-length
			  outArrayLen(pDecInsn->vA,pDecInsn->vB); 
			  break;

         //uninary opCodes follow
             case 0x7b:
               outUnopNegInt(pDecInsn->vA, pDecInsn->vB); 
               break;
             case 0x7c:
              outUnopNotInt(pDecInsn->vA, pDecInsn->vB); 
              break;
             case 0x7d:
              outUnopNegLong(pDecInsn->vA, pDecInsn->vB); 
              break;
             case 0x7e:
              outUnopNotLong(pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=~v%d  @type long;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x7f:
              outUnopNegFloat(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=-v%d  @type float;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x80:
              outUnopNegDouble(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=-v%d  @type double;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x81:
              outUnopInt2Long(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(long)v%d  @type i2l;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x82:
              outUnopInt2Float(pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=(float)v%d  @type i2f;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x83:
              outUnopInt2Double(pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=(double)v%d  @type i2d;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x84:
              outUnopLong2Int(pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=(int)v%d  @type l2i;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x85:
              outUnopLong2Float(pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=(float)v%d  @type l2f;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x86:
              outUnopLong2Double(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(double)v%d  @type l2d;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x87:
              outUnopFloat2Int(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(int)v%d  @type f2i;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x88:
              outUnopFloat2Long(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(long)v%d  @type f2l;", pDecInsn->vA, pDecInsn->vB);
              break;
             case 0x89:
              outUnopFloat2Double(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(double)v%d  @type f2d;", pDecInsn->vA, pDecInsn->vB);
              break;
            case 0x8a:
              outUnopDouble2Int(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(int)v%d  @type d2i;", pDecInsn->vA, pDecInsn->vB);
              break;
            case 0x8b:
             outUnopDouble2Long(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(long)v%d  @type d2l;", pDecInsn->vA, pDecInsn->vB);
             break;
            case 0x8c:
             outUnopDouble2Float(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(float)v%d  @type d2f;", pDecInsn->vA, pDecInsn->vB);
             break;
            case 0x8d:
             outUnopInt2Byte(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(byte)v%d  @type i2b;", pDecInsn->vA, pDecInsn->vB);
             break;
            case 0x8e:
             outUnopInt2Char(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(char)v%d  @type i2c;", pDecInsn->vA, pDecInsn->vB);
             break;
            case 0x8f:
             outUnopInt2short(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=(short)v%d  @type i2s;", pDecInsn->vA, pDecInsn->vB);
             break;

        //binary /2addr opCodes follow
            case 0xb0:
             outAddInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d+v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb1:
             outSubInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d-v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb2:
             outMulInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d*v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb3:
             outDivInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d/v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb4:
             outRemInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d%%v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb5:
             outAndInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^&v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb6:
             outOrInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^|v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb7:
             outXorInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^~v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb8:
             outShlInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^<v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xb9:
             outShrInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^>v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xba:
             outUshrInt2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^>>v%d  @type int;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xbb:
             outAddLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d+v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xbc:
             outSubLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d-v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xbd:
             outMulLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d*v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xbe:
             outDivLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d/v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xbf:
             outRemLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d%%v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc0:
             outAndLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^&v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc1:
             outOrLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^|v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc2:
             outXorLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^~v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc3:
             outShlLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^<v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc4:
             outShrLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^>v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc5:
             outUshrLong2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d^>>v%d  @type long;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc6:
             outAddFloat2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d+v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc7:
             outSubFloat2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d-v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc8:
             outMulFloat2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d*v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xc9:
             outDivFloat2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d/v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xca:
             outRemFloat2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d%%v%d  @type float;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xcb:
             outAddDouble2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d+v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xcc:
             outSubDouble2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d-v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xcd:
             outMulDouble2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d*v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xce:
             outDivDouble2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d/v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;
            case 0xcf:
             outRemDouble2addr(pDecInsn->vA, pDecInsn->vA, pDecInsn->vB); //  printf("v%d:=v%d%%v%d  @type double;", pDecInsn->vA, pDecInsn->vA, pDecInsn->vB);
             break;


             default:
			  fprintf(pFp, " 12x????");
			  break;
              }
	    /**** sankar ends *****/

        break;
    case kFmt11n:        // op vA, #+B
        printf(" v%d, #int %d // #%x",
           pDecInsn->vA, (s4)pDecInsn->vB, (u1)pDecInsn->vB); // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x12:
              outConst4(pDecInsn->vA, (s4)pDecInsn->vB); // printf("v%d:=%d  @length 4;", pDecInsn->vA,(s4)pDecInsn->vB );
              break;

             default:
			  fprintf(pFp," 11n????");
			  break;
              }
	    /**** sankar ends *****/

        break;
    case kFmt11x:        // op vAA
        printf(" v%d", pDecInsn->vA); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
          case 0x0a:
           outMoveResult(pDecInsn->vA); // printf("v%d:=temp;", pDecInsn->vA);
           break;
         case 0x0b:
           outMoveResultWide(pDecInsn->vA); // printf("v%d:=temp  @type wide;", pDecInsn->vA);
           break;
         case 0x0c:
           outMoveResultObject(pDecInsn->vA); // printf("v%d:=temp  @type object;", pDecInsn->vA);
           break;
         case 0x0d:
          outMoveExc(pDecInsn->vA); // printf("v%d:=Exception  @type object;", pDecInsn->vA);
          break;


        case 0x0f:
          outReturn(pDecInsn->vA); // printf("return v%d;", pDecInsn->vA );
          break;
        case 0x10:
          outReturnWide(pDecInsn->vA); // printf("return v%d  @type wide;", pDecInsn->vA );
          break;
        case 0x11:
          outReturnObj(pDecInsn->vA); // printf("return v%d  @type object;", pDecInsn->vA );
          break;


        //next 2 opcodes are about monitor
        case 0x1d:
          outMonitorEnter(pDecInsn->vA); // printf("(@monitorenter v%d)", pDecInsn->vA);
          break;
        case 0x1e:
          outMonitorExit(pDecInsn->vA); // printf("(@monitorexit v%d)", pDecInsn->vA);
          break;


          //throw
        case 0x27:
          outThrow(pDecInsn->vA); //printf("throw v%d;",pDecInsn->vA);
          break;

         default:
		  fprintf(pFp, " 11x????");
		  break;
          }
	    /**** sankar ends *****/

        break;
    case kFmt10t:        // op +AA

         {
             s4 targ = (s4) pDecInsn->vA;
             printf(" %04x // %c%04x",
	               insnIdx + targ,
	                 (targ < 0) ? '-' : '+',
	                (targ < 0) ? -targ : targ); // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
             case 0x28:
              outGoto(((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;

             default:
			  fprintf(pFp, " 10t????");
			  break;
              }
	    /**** sankar ends *****/

	     }
	     break;  



    case kFmt20t:        // op +AAAA

        {
            s4 targ = (s4) pDecInsn->vA;
            printf(" %04x // %c%04x",
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                (targ < 0) ? -targ : targ);  // not in pilar
	
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
             case 0x29:
               outGoto(((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;

             default:
			  fprintf(pFp, " 20t????");
			  break;
              }
	    /**** sankar ends *****/
       }
        break;
    case kFmt22x:        // op vAA, vBBBB
         printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
              case 0x02:
               outMove(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d;", pDecInsn->vA, pDecInsn->vB);
               break;

              case 0x05:
               outMoveWide(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d  @type wide;", pDecInsn->vA, pDecInsn->vB);
               break;

              case 0x08:
               outMoveObject(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d  @type object;", pDecInsn->vA, pDecInsn->vB);
               break;

             default:
			  fprintf(pFp, " 22x????");
			  break;
              }
	    /**** sankar ends *****/

        break;
    case kFmt21t:        // op vAA, +BBBB
 
        {
            s4 targ = (s4) pDecInsn->vB;
            printf(" v%d, %04x // %c%04x", pDecInsn->vA,
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                (targ < 0) ? -targ : targ); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
              case 0x38:
               outIfEqz(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d==0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
              case 0x39:
               outIfNez(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d!=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
              case 0x3a:
               outIfLtz(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d<0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
              case 0x3b:
               outIfGez(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d>=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
              case 0x3c:
               outIfGtz(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d>0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
              case 0x3d:
               outIfLez(pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d<=0 then goto L%06x;", pDecInsn->vA,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
               break;
         

             default:
			  fprintf(pFp, " 21t????");
			  break;
              }
	    /**** sankar ends *****/
       }
        break;

    case kFmt21s:        // op vAA, #+BBBB
        printf(" v%d, #int %d // #%x",
            pDecInsn->vA, (s4)pDecInsn->vB, (u2)pDecInsn->vB); // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x13:
              outConst16(pDecInsn->vA, (s4)pDecInsn->vB); // printf("v%d:=%d  @length 16;", pDecInsn->vA,(s4)pDecInsn->vB );
              break;

             case 0x16:
              outConstWide16(pDecInsn->vA,(s4)pDecInsn->vB ); // printf("v%d:=%d  @length wide16;", pDecInsn->vA,(s4)pDecInsn->vB );
              break;

             default:
			  fprintf(pFp, " 21s????");
			  break;
              }
	    /**** sankar ends *****/

        break;

    case kFmt21h:        // op vAA, #+BBBB0000[00000000]
      
	  // The printed format varies a bit based on the actual opcode.
        if (pDecInsn->opcode == OP_CONST_HIGH16) {
            s4 value = pDecInsn->vB << 16;
            printf(" v%d, #int %d // #%x",
              pDecInsn->vA, value, (u2)pDecInsn->vB);  // not in pilar
        } else {
            s8 value = ((s8) pDecInsn->vB) << 48;
            printf(" v%d, #long %lld // #%x",
                pDecInsn->vA, value, (u2)pDecInsn->vB); // not in pilar
        }


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

                case 0x15:
                 {
				    s4 value = pDecInsn->vB << 16;
                    outConstHigh16(pDecInsn->vA, value);        // printf("v%d:=%d  @length high16;",pDecInsn->vA, value);
                    break;
				 }

                case 0x19:           
		         {
					 s8 value1 = ((s8) pDecInsn->vB) << 48;
                     outConstWideHigh16(pDecInsn->vA, value1); // printf("v%d:=%lldL  @length wide_high16;",pDecInsn->vA, value1);
                     break;
				 }

                default:
			     fprintf(pFp, " 21h????");
			     break;
              }
	    /**** sankar ends *****/

        break;

    case kFmt21c:        // op vAA, thing@BBBB
        printf(" v%d, %s", pDecInsn->vA, indexBuf); // not in pilar

       /**** sankar starts ****/


        switch(pDecInsn->opcode){
             case 0x1a:

             {
               // find escape characters
               const char *Str = dexStringById(pDexFile, pDecInsn->vB);
               char newStr [(int)strlen(Str)*2+100];
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
               outConstString(pDecInsn->vA, newStr); // printf("v%d:=\"%s\";", pDecInsn->vA,newStr);
             }
             break;

             case 0x1c:
              outConstClass(pDecInsn->vA, descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB))); // printf("v%d:=[|%s|];", pDecInsn->vA,descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB)));
              break;

             case 0x1f:
              outCheckCast(pDecInsn->vA,descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB)), pDecInsn->vA); // printf("v%d:=([|%s|])v%d;", pDecInsn->vA,descriptorToDot(getClassDescriptor(pDexFile, pDecInsn->vB)), pDecInsn->vA);
              break;


             case 0x22:
              outNewIns(pDecInsn->vA, indexBuf); // printf("v%d:=new %s;",pDecInsn->vA, indexBuf);
              break;

          //sget
             case 0x60:
              outSget(pDecInsn->vA, indexBuf); // printf("v%d:=%s;", pDecInsn->vA, indexBuf);
              break;
             case 0x61:
              outSgetWide(pDecInsn->vA, indexBuf); // printf("v%d:=%s  @wide;", pDecInsn->vA, indexBuf);
              break;
             case 0x62:
              outSgetObject(pDecInsn->vA, indexBuf); // printf("v%d:=%s  @object;", pDecInsn->vA, indexBuf);
              break;
             case 0x63:
              outSgetBool(pDecInsn->vA, indexBuf); // printf("v%d:=%s  @boolean;", pDecInsn->vA, indexBuf);
              break;
             case 0x64:
              outSgetByte(pDecInsn->vA, indexBuf); //printf("v%d:=%s  @byte;", pDecInsn->vA, indexBuf);
              break;
             case 0x65:
              outSgetChar(pDecInsn->vA, indexBuf); // printf("v%d:=%s  @char;", pDecInsn->vA, indexBuf);
              break;
             case 0x66:
              outSgetShort(pDecInsn->vA, indexBuf); // printf("v%d:=%s  @short;", pDecInsn->vA, indexBuf);
              break;
         //sput
             case 0x67:
              outSput(indexBuf, pDecInsn->vA); // printf("%s:=v%d;", indexBuf, pDecInsn->vA);
              break;
             case 0x68:
              outSputWide(indexBuf, pDecInsn->vA); //  printf("%s:=v%d  @wide;", indexBuf, pDecInsn->vA);
              break;
             case 0x69:
              outSputObject(indexBuf, pDecInsn->vA); //  printf("%s:=v%d  @object;", indexBuf, pDecInsn->vA);
              break;
             case 0x6a:
              outSputBool(indexBuf, pDecInsn->vA); //  printf("%s:=v%d  @boolean;", indexBuf, pDecInsn->vA);
              break;
             case 0x6b:
              outSputByte(indexBuf, pDecInsn->vA); //  printf("%s:=v%d  @byte;", indexBuf, pDecInsn->vA);
              break;
             case 0x6c:
              outSputChar(indexBuf, pDecInsn->vA); // printf("%s:=v%d  @char;", indexBuf, pDecInsn->vA);
              break;
             case 0x6d:
              outSputShort(indexBuf, pDecInsn->vA); // printf("%s:=v%d  @short;", indexBuf, pDecInsn->vA);
              break;

             default:
			  fprintf(pFp, " 21c????");
			  break;
          }
	    /**** sankar ends *****/
		break;



    case kFmt31c:        // op vAA, thing@BBBBBBBB
        printf(" v%d, %s", pDecInsn->vA, indexBuf);  // not in pilar
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
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
               outConstString(pDecInsn->vA, newStr); // printf("v%d:=\"%s\";", pDecInsn->vA,newStr);
             }
             break;

             default:
			  fprintf(pFp, " 31c????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt23x:        // op vAA, vBB, vCC
        printf(" v%d, v%d, v%d", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
          // cmp
             case 0x2d:
             case 0x2f:
               outCmpl(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=cmpl(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
               break;
             case 0x2e:
             case 0x30:
               outCmpg(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=cmpg(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
               break;
             case 0x31:
               outCmp(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=cmp(v%d,v%d);",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
               break;

          //aput/aget
             case 0x44:
              outAget(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=v%d[v%d];",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x45:
              outAgetWide(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=v%d[v%d]  @wide;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x46:
              outAgetObject(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=v%d[v%d]  @object;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x47:
              outAgetBool(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=v%d[v%d]  @boolean;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x48:
              outAgetByte(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);  // printf("v%d:=v%d[v%d]  @byte;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x49:
              outAgetChar(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); // printf("v%d:=v%d[v%d]  @char;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x4a:
              outAgetShort(pDecInsn->vA,pDecInsn->vB,pDecInsn->vC); //  printf("v%d:=v%d[v%d]  @short;",pDecInsn->vA,pDecInsn->vB,pDecInsn->vC);
              break;
             case 0x4b:
              outAput(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x4c:
              outAputWide(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @wide;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x4d:
              outAputObject(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @object;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x4e:
              outAputBool(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @boolean;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x4f:
              outAputByte(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @byte;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x50:
              outAputChar(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @char;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
             case 0x51:
              outAputShort(pDecInsn->vB,pDecInsn->vC,pDecInsn->vA); // printf("v%d[v%d]:=v%d  @short;",pDecInsn->vB,pDecInsn->vC,pDecInsn->vA);
              break;
        
          //binop
             case 0x90:
              outAddInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d+v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x91:
              outSubInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d-v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x92:
              outMulInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d*v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x93:
              outDivInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d/v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x94:
              outRemInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d%%v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x95:
              outAndInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^&v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x96:
              outOrInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^|v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x97:
              outXorInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^~v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x98:
              outShlInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^<v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x99:
              outShrInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^>v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9a:
              outUshrInt(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d^>>v%d  @type int;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9b:
              outAddLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d+v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9c:
              outSubLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d-v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9d:
              outMulLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d*v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9e:
              outDivLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d/v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0x9f:
              outRemLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d%%v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa0:
              outAndLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^&v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa1:
              outOrLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^|v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa2:
              outXorLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^~v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa3:
              outShlLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^<v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa4:
              outShrLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^>v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa5:
              outUshrLong(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d^>>v%d  @type long;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa6:
              outAddFloat(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //  printf("v%d:=v%d+v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa7:
              outSubFloat(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //   printf("v%d:=v%d-v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa8:
              outMulFloat(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //   printf("v%d:=v%d*v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xa9:
              outDivFloat(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //   printf("v%d:=v%d/v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xaa:
              outRemFloat(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); //   printf("v%d:=v%d%%v%d  @type float;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xab:
               outAddDouble(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d+v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
               break;
             case 0xac:
              outSubDouble(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d-v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xad:
              outMulDouble(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d*v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xae:
              outDivDouble(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d/v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;
             case 0xaf:
              outRemDouble(pDecInsn->vA, pDecInsn->vB, pDecInsn->vC); // printf("v%d:=v%d%%v%d  @type double;", pDecInsn->vA, pDecInsn->vB, pDecInsn->vC);
              break;

             default:
			  fprintf(pFp, " 23x????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt22b:        // op vAA, vBB, #+CC
         printf(" v%d, v%d, #int %d // #%02x",
            pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC, (u1)pDecInsn->vC); // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

          //binop/lit8
                case 0xd8:
                  outAddLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d+%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xd9:
                 outSubLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d-%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xda:
                 outMulLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d*%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdb:
                 outDivLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d/%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdc:
                 outRemLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d%%%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdd:
                 outAndLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^&%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xde:
                 outOrLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^|%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xdf:
                 outXorLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^~%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe0:
                 outShlLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^<%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe1:
                 outShrLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^>%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;
                case 0xe2:
                 outUshrLit8(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); //  printf("v%d:=v%d^>>%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
                  break;

                default:
			     fprintf(pFp, " 22b????");
			     break;
            }
	    /**** sankar ends *****/

        break;
    case kFmt22t:        // op vA, vB, +CCCC

        {
            s4 targ = (s4) pDecInsn->vC;
            printf(" v%d, v%d, %04x // %c%04x", pDecInsn->vA, pDecInsn->vB,
                insnIdx + targ,
                (targ < 0) ? '-' : '+',
                   (targ < 0) ? -targ : targ); // not in pilar
        


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
         // if
            case 0x32:
              outIfEq(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);   // printf("if v%d==v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;
            case 0x33:
              outIfNq(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d!=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;
            case 0x34:
              outIfLt(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d<v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;
            case 0x35:
              outIfGe(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d>=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;
            case 0x36:
              outIfGt(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("if v%d>v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              break;
            case 0x37:
             outIfLe(pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);  // printf("if v%d<=v%d then goto L%06x;", pDecInsn->vA, pDecInsn->vB,((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
             break;
       

             default:
			  fprintf(pFp, " 22t????");
			  break;
         }

      }
	    /**** sankar ends *****/

        break;

    case kFmt22s:        // op vA, vB, #+CCCC

         printf(" v%d, v%d, #int %d // #%04x",
             pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC, (u2)pDecInsn->vC); // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
         //binop/lit16
           case 0xd0:
            outAddLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d+%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd1:
            outSubLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d-%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd2:
            outMulLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d*%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd3:
            outDivLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d/%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd4:
            outRemLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d%%%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd5:
            outAndLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d^&%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd6:
            outOrLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d^|%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
           case 0xd7:
            outXorLit16(pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC); // printf("v%d:=v%d^~%d;",pDecInsn->vA, pDecInsn->vB, (s4)pDecInsn->vC);
            break;
      

             default:
			  fprintf(pFp, " 22s????");
			  break;
        }
	    /**** sankar ends *****/

        break;

    case kFmt22c:        // op vA, vB, thing@CCCC

    	 printf(" v%d, v%d, %s", pDecInsn->vA, pDecInsn->vB, indexBuf); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             //instance of
             case 0x20:
              outInstanceOf(pDecInsn->vA,pDecInsn->vB,descriptorToDot(indexBuf)); // printf("v%d:=instanceof(v%d, [|%s|]);",pDecInsn->vA,pDecInsn->vB,descriptorToDot(indexBuf));
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
               outNewArray(pDecInsn->vA, newIndex); // printf("v%d:=new [|%s|] ;",pDecInsn->vA,newIndex);
             }
              break;

         //iget
             case 0x52:
              outIget(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s;", pDecInsn->vA, pDecInsn->vB, indexBuf);
              break;
             case 0x53:
              outIgetWide(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @wide;", pDecInsn->vA, pDecInsn->vB, indexBuf);
              break;
             case 0x54:
              outIgetObject(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @object;", pDecInsn->vA, pDecInsn->vB, indexBuf);
              break;
             case 0x55:
              outIgetBool(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @boolean;", pDecInsn->vA, pDecInsn->vB, indexBuf);
              break;
             case 0x56:
               outIgetByte(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @byte;", pDecInsn->vA, pDecInsn->vB, indexBuf);
               break;
             case 0x57:
               outIgetChar(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @char;", pDecInsn->vA, pDecInsn->vB, indexBuf);
               break;
             case 0x58:
              outIgetShort(pDecInsn->vA, pDecInsn->vB, indexBuf); //  printf("v%d:=v%d%s  @short;", pDecInsn->vA, pDecInsn->vB, indexBuf);
              break;
		  
           //iput
              case 0x59:
                outIput(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d;", pDecInsn->vB,indexBuf,pDecInsn->vA);
                break;
              case 0x5a:
               outIputWide(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @wide;", pDecInsn->vB,indexBuf,pDecInsn->vA);
               break;
              case 0x5b:
              outIputObject(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @object;", pDecInsn->vB,indexBuf,pDecInsn->vA);
              break;
              case 0x5c:
                outIputBool(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @boolean;", pDecInsn->vB,indexBuf,pDecInsn->vA);
                break;
              case 0x5d:
               outIputByte(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @byte;", pDecInsn->vB,indexBuf,pDecInsn->vA);
               break;
             case 0x5e:
              outIputChar(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @char;", pDecInsn->vB,indexBuf,pDecInsn->vA);
              break;
             case 0x5f:
              outIputShort(pDecInsn->vB,indexBuf,pDecInsn->vA); // printf("v%d%s :=v%d @short;", pDecInsn->vB,indexBuf,pDecInsn->vA);
              break;
      

             default:
			  fprintf(pFp, " 22c????");
			  break;
        }
	    /**** sankar ends *****/
        break;



    case kFmt22cs:       // [opt] op vA, vB, field offset CCCC
         printf(" v%d, v%d, %s", pDecInsn->vA, pDecInsn->vB, indexBuf); // not in pilar
	     /**** sankar starts ****/
         {
                /* now processing indexBuf which looks like "[obj+003b]" ; we want to convert z to "`003b`" */
                 char* newStr = (char*)malloc(sizeof(indexBuf));
                 int index1 = 0;
                 int index2 = 0;
                 assert(sizeof(newStr) > 10);
                 strcpy(newStr,"`");
                 index1 = 1;
                 index2 = 5;
                 while(indexBuf[index2] != ']' && index1 < 7){
                     newStr[index1] = indexBuf[index2];
                     index1++;
                     index2++;
                     }
                 newStr[index1]='\0';
                 strcat(newStr, "`");
                 strcpy(indexBuf, newStr);
				 free(newStr);
               /* indexBuf processing ends; note the free(newStr) at the end */ 
         }
	     switch(pDecInsn->opcode){
    
           // ***** sankar starts *********

            //iget
             case 0xf2:
              outIgetQuick(pDecInsn->vA, pDecInsn->vB, indexBuf); 
              break;
             case 0xf3:
              outIgetWideQuick(pDecInsn->vA, pDecInsn->vB, indexBuf); 
              break;
             case 0xf4:
              outIgetObjectQuick(pDecInsn->vA, pDecInsn->vB, indexBuf); 
              break;

            //iput
              case 0xf5:
                outIputQuick(pDecInsn->vB,indexBuf,pDecInsn->vA); 
                break;
              case 0xf6:
               outIputWideQuick(pDecInsn->vB,indexBuf,pDecInsn->vA); 
               break;
              case 0xf7:
               outIputObjectQuick(pDecInsn->vB,indexBuf,pDecInsn->vA); 
               break;
         
             default:
			  fprintf(pFp, " 22cs????");
			  break;
              }
	    /**** sankar ends *****/
        break;

    case kFmt30t:
        printf(" #%08x", pDecInsn->vA); // not in pilar
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){
             case 0x2a:
              {
               s4 targ = (s4) pDecInsn->vA;
               outGoto(((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2); // printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + targ)*2);
              }
              break;
   

             default:
			  fprintf(pFp, " 30t????");
			  break;
              }
	    /**** sankar ends *****/
        break;

    case kFmt31i:        // op vAA, #+BBBBBBBB

        {
            /* this is often, but not always, a float */
            union {
                float f;
                u4 i;
            } conv;
            conv.i = pDecInsn->vB;

            printf(" v%d, #float %f // #%08x",
                pDecInsn->vA, conv.f, pDecInsn->vB); // not in pilar
        


	      /**** sankar starts ****/
	     switch(pDecInsn->opcode){

              case 0x14:
            	  // we saw original dexdump is using conv.f which gives wrong result for 0x14
               outConst32(pDecInsn->vA, conv.i); // printf(" v%d:=%f;",pDecInsn->vA, conv.f);
               break;
    

             case 0x17:
              outConstWide32(pDecInsn->vA, conv.f); // printf(" v%d:=%f  @length wide32;",pDecInsn->vA, conv.f);
              break;

             default:
			  fprintf(pFp, " 31i????");
			  break;
          }

	    /**** sankar ends *****/
       }
        break;

    case kFmt31t:       // op vAA, offset +BBBBBBBB

        printf(" v%d, %08x // +%08x",
            pDecInsn->vA, insnIdx + pDecInsn->vB, pDecInsn->vB); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x26:
              { 
		        outFillArrData(((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2); // printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2);
                void* temp = (void*) new struct Op31t(insnIdx, pDecInsn);   // ***** note that we ignore deleting this later; possible little leakage ******  
		        list.add(temp); // ********** do we need to do something to address this extra code in a separte place? *******
                ++s31t; // represents the number of elements in list which is same as list.count()
              }
               break;
    
              case 0x2b:
              case 0x2c:
               { 
		    	outSwitch(((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2); // printf("goto L%06x;",((u1*)insns - pDexFile->baseAddr) +(insnIdx + pDecInsn->vB)*2);
				// printf("at sw statment: ((u1*)insns - pDexFile->baseAddr) = %04x , insnIdx = %04x , pDecInsn->vB = %04x , ((u1*)insns - pDexFile->baseAddr) + (insnIdx + pDecInsn->vB)*2 = %04x", ((u1*)insns - pDexFile->baseAddr), insnIdx, pDecInsn->vB, ((u1*)insns - pDexFile->baseAddr) + (insnIdx + pDecInsn->vB)*2);
                
                void* temp = (void*) new struct Op31t(insnIdx, pDecInsn); // ***** note that we ignore deleting this later; possible little leakage ******  
		        list.add(temp); // ********** do we need to do something to address this extra code in a separte place? *******
                ++s31t; // represents the number of elements in list which is same as list.count()
               }
               break;
      
             default:
			  fprintf(pFp, " 31t????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt32x:        // op vAAAA, vBBBB

        printf(" v%d, v%d", pDecInsn->vA, pDecInsn->vB); // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x03:
              outMove(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d;", pDecInsn->vA, pDecInsn->vB);
              break;


             case 0x06:
               outMoveWide(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d  @type wide;", pDecInsn->vA, pDecInsn->vB);
               break;

             case 0x09:
               outMoveObject(pDecInsn->vA, pDecInsn->vB); // printf("v%d:=v%d  @type object;", pDecInsn->vA, pDecInsn->vB);
               break;

             default:
			  fprintf(pFp, " 32x????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt35c:        // op {vC, vD, vE, vF, vG}, thing@BBBB

        {
            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->arg[i]);
                else
                    printf(", v%d", pDecInsn->arg[i]);
            }
            printf("}, %s", indexBuf); // not in pilar
        }

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x24:
		      outFilledNewArray(indexBuf, pDecInsn->vA, pDecInsn->arg); // this is a multi-line macro ****** double check for error
              break;
             case 0x6e:
		      outInvokeVirtual(pDexFile, pDecInsn, indexBuf); 
              break;
            case 0x6f:
		     outInvokeSuper(pDexFile, pDecInsn, indexBuf); 
             break;
            case 0x70:
		     outInvokeDirect(pDexFile, pDecInsn, indexBuf); 
             break;
           case 0x71:
		    outInvokeStatic(pDexFile, pDecInsn, indexBuf);
            break;
           case 0x72:
            /*
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
                } */
	    	  outInvokeInterface(pDexFile, pDecInsn, indexBuf);
              break;

             case 0xf0:
			  outInvokeObjectInitRange(pDexFile, pDecInsn, indexBuf); 
              break;

             default:
			  fprintf(pFp, " 35c????");
			  break;
              }
	    /**** sankar ends *****/
        break;

    case kFmt35ms:       // [opt] invoke-virtual+super

        {
            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->arg[i]);
                else
                    printf(", v%d", pDecInsn->arg[i]);
            }
            printf("}, %s", indexBuf); // not in pilar
        }

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

            case 0xf8: // invoke-virtual-quick
            /*
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
                      printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type virtual-quick;",methInfo.classDescriptor, methInfo.name,
                                         methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                   }
                } */
	    	  outInvokeVirtualQuick(pDexFile, pDecInsn, indexBuf);
              break;

            case 0xfa: // invoke-super-quick
	    	  outInvokeSuperQuick(pDexFile, pDecInsn, indexBuf);
              break;
        

             default:
			  fprintf(pFp, " 35ms????");
			  break;
         }
	    /**** sankar ends *****/
        break;

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
        }  // not in pilar

	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

            // sankar starts

             case 0xee: // execute-inline
              outExecuteInline(pDexFile, pDecInsn, indexBuf);
              break;


             default:
			  fprintf(pFp, " 35mi????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt3rc:        // op {vCCCC .. v(CCCC+AA-1)}, thing@BBBB

        {
            // * This doesn't match the "dx" output when some of the args are
            // * 64-bit values -- dx only shows the first register.

            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->vC + i);
                else
                    printf(", v%d", pDecInsn->vC + i);
            }
            printf("}, %s", indexBuf);
        }  // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x25:
     	    	/*
                {
                 printf("temp:=(%s)`[", indexBuf);
                 for (i = 0; i < (int) pDecInsn->vA; i++) {
                  if (i == 0)
                   printf("v%d", pDecInsn->vC + i);
                  else
                   printf(", v%d", pDecInsn->vC + i);
                      }
                  printf("];");
                 } */

		       outFilledNewArrRange(indexBuf, pDecInsn->vA, pDecInsn->vC); // this is a multi-line macro ****** double check for error
               break;
    


             case 0x74:
               /*
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
               }*/
	          outInvokeVirtualRange(pDexFile, pDecInsn, indexBuf);	 
		      break;
             case 0x75:
		      outInvokeSuperRange(pDexFile, pDecInsn, indexBuf);
		      break;
             case 0x76:
		      outInvokeDirectRange(pDexFile, pDecInsn, indexBuf);
	          break;
             case 0x77:
	      	  outInvokeStaticRange(pDexFile, pDecInsn, indexBuf);
		      break;
             case 0x78:
		      outInvokeInterfaceRange(pDexFile, pDecInsn, indexBuf);
		      break;

             default:
			  fprintf(pFp, " 3rc????");
			  break;
         }
	    /**** sankar ends *****/
        break;

    case kFmt3rms:       // [opt] invoke-virtual+super/range

        {
            
             // * This doesn't match the "dx" output when some of the args are
             // * 64-bit values -- dx only shows the first register.
             
            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->vC + i);
                else
                    printf(", v%d", pDecInsn->vC + i);
            }
            printf("}, %s", indexBuf);
        }  // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             // invoke-virtual-quick/range

             case 0xf9:
               /*
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
                        printf(") @signature [|%s.%s:%s|] @classDescriptor [|%s|] @type virtual-quick;",methInfo.classDescriptor, methInfo.name,
                              methInfo.signature,descriptorToDot(methInfo.classDescriptor));
                 }
               }*/
	          outInvokeVirtualQuickRange(pDexFile, pDecInsn, indexBuf);	 
		      break;
              
			 case 0xfb:
	          outInvokeSuperQuickRange(pDexFile, pDecInsn, indexBuf);	 
			  break;

             default:
			  fprintf(pFp, " 3rms????");
			  break;
          }
	    /**** sankar ends *****/
        break;

    case kFmt3rmi:       // [opt] execute-inline/range
	 
        {
            
             // * This doesn't match the "dx" output when some of the args are
             // * 64-bit values -- dx only shows the first register.
             
            fputs(" {", stdout);
            for (i = 0; i < (int) pDecInsn->vA; i++) {
                if (i == 0)
                    printf("v%d", pDecInsn->vC + i);
                else
                    printf(", v%d", pDecInsn->vC + i);
            }
            printf("}, %s", indexBuf);
        } // not in pilar


	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){


             case 0xef: // execute-inline-range
              outExecuteInlineRange(pDexFile, pDecInsn, indexBuf);
              break;
             
             default:
			  fprintf(pFp, " 3rmi????");
			  break;
          }
	    /**** sankar ends *****/
        break;

    case kFmt51l:        // op vAA, #+BBBBBBBBBBBBBBBB

        {
            /* this is often, but not always, a double */
            union {
                double d;
                u8 j;
            } conv;
            conv.j = pDecInsn->vB_wide;

            printf(" v%d, #double %f // #%016llx",
                pDecInsn->vA, conv.d, pDecInsn->vB_wide); // not in pilar
        
	    /**** sankar starts ****/
	    switch(pDecInsn->opcode){

             case 0x18:
              outConstWide(pDecInsn->vA, conv.d); // printf("v%d:=%fL  @length wide;",pDecInsn->vA, conv2.d);
              break;
       
             default:
			  fprintf(pFp, " 51l????");
			  break;
          }
	    }
	    /**** sankar ends *****/
        break;

    case kFmt00x:        // unknown op or breakpoint
        break;

    default:
        printf(" ???");
        fprintf(pFp, " ???"); // sankar
        break;
    }

   }
   putchar('\n');
   fprintf(pFp, "\n"); // sankar

   if (indexBuf != indexBufChars) {
        free(indexBuf);
   }
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

    printf("%06x:                                        |[%06x] %s.%s:%s\n",
        startAddr, startAddr,
        className, methInfo.name, methInfo.signature); // not in pilar

    free((void *) methInfo.signature);

    //******************* kui's modification begins  *******************
    PStash list; // sankar adds this; // Kui did "struct Op31t list[20];"
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
        dumpInstruction(pDexFile, pCode, insnIdx, insnWidth, &decInsn, list,s31t,l31t); // sankar adds 3 args 
        //******************* kui's modification ends  *******************
        insns += insnWidth;
        insnIdx += insnWidth;
    }

    free(className);
}

/*
 * Dump a "code" struct.
 */
void dumpCode(DexFile* pDexFile, const DexMethod* pDexMethod, char* tailRegs)
{
    const DexCode* pCode = dexGetCode(pDexFile, pDexMethod);
    printf("      @registers     : %d\n", pCode->registersSize);
    printf("      ins           : %d\n", pCode->insSize);
    printf("      outs          : %d\n", pCode->outsSize);
    printf("      insns size    : %d 16-bit code units\n", pCode->insnsSize);
    //******************* kui's modification begins  *******************
    dumpLocals(pDexFile, pCode, pDexMethod, tailRegs);
    if (gOptions.disassemble)
        dumpBytecodes(pDexFile, pDexMethod);
    dumpCatches(pDexFile, pCode);
    /* both of these are encoded in debug info */
    dumpPositions(pDexFile, pCode, pDexMethod);

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

    if (gOptions.outputFormat == OUTPUT_PLAIN) {
        printf("    #%d              : (in %s)\n", i, backDescriptor);
        printf("      name          : '%s'\n", name);
        printf("      type          : '%s'\n", typeDescriptor);
        printf("      access        : 0x%04x (%s)\n",
            pDexMethod->accessFlags, accessStr);


   /********** sankar starts : moving the following block of code from the previous dumpLocals() function; this block just populates locVarList but does not print ***/

                if(locVarList) 
	            	delete locVarList; // sankar adds: check some possible memory leak here

                locVarList = new PStash; // sankar adds

                if( pDexMethod->codeOff != 0)
                { 
					const DexCode* pCode = dexGetCode(pDexFile, pDexMethod);


                    const DexMethodId *pMethodId
                        = dexGetMethodId(pDexFile, pDexMethod->methodIdx);
                    const char *classDescriptor
                        = dexStringByTypeIdx(pDexFile, pMethodId->classIdx);

                    dexDecodeDebugInfo(pDexFile, pCode, classDescriptor, pMethodId->protoIdx,
                        pDexMethod->accessFlags, NULL, dumpLocalsCb, NULL);
				}
  /************* sankar ends *******/

    /******************* kui's modification begins  *******************
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
                   char* para=(char*)malloc(sizeof(char)*5000);  // sankar increased the size from 1000 to 5000
                   char* paraThis=(char*)malloc(sizeof(char)*1000);  // sankar adds
                   char* paraTotal=(char*)malloc(sizeof(char)*6000);  // sankar adds
				   char* tailRegisters = (char*)malloc(sizeof(char)*5000); // sankar adds. this is the collection of tail registers of long or double parameters
                   strcpy(para,"");
				   strcpy(paraThis,""); // initializing with null string
				   strcpy(paraTotal,"");
				   strcpy(tailRegisters, "");

				   int thisFlag = 0; // if 1 then this method has "this" as the first param as well as a local variable
				   int paramCount = 0; // this count does not include "this"

                   // print the parameters with index

                   // sankar adds: print "this" register as the first param for non-static method
                   if(locVarList)
                     {
		                for(int i=0; i < locVarList->count(); i++)
		                   {	 
		                      void* temp1 = (*locVarList)[i];
		                      if(temp1) 
			                   {
			                     	locVarInf* t1 = (locVarInf*) temp1;
                                    if(t1->descriptor && t1->name && !strcmp(t1->name, "this"))
									{
				                       // fprintf(pFp, "        [|%s|] [|%s|] [|v%d|] @scope (L%04x,L%04x);",
				                          // t1->descriptor, t1->name, t1->reg, t1->startAddress,  t1->endAddress );

				                       strcpy(paraThis,""); // initializing with null string
                                       // strcat(paraThis, "[|");
									   strcat(paraThis, toPilar(t1->descriptor));
									   // strcat(paraThis, "|]");
									   char regNamebuff[20];
									   sprintf(regNamebuff, " v%d", startReg);
									   strcat(paraThis, regNamebuff);
									   strcat(paraThis, " @type `this`");
									   thisFlag = 1;
									   t1->paramFlag = true; // the corresponding element of locVarList is set as a parameter so that we can identify it in dumpLocals() 
									   // note there can be more than one "this" in locVarList; we break out after the 1st one. we assume the 1st one has the first scope.
									   break;
									}
							   }
						   }
					 }
                  
				  // sometimes populating locVarList in localsCb() method is not working, so no locVar e.g. "this" in locVarList. Below we forcefully add "this" to paraThis in that case
                  
				  if((thisFlag == 0) && (pDexMethod->accessFlags & (ACC_STATIC | ACC_ABSTRACT | ACC_NATIVE)) ==0) // i.e. non-static method but still "this" is not added
                     { 
				      strcpy(paraThis,""); // initializing with null string
				      strcat(paraThis, toPilar(descriptorToDot(backDescriptor)));
					  char regNamebuff[20];
					  sprintf(regNamebuff, " v%d", startReg);
					  strcat(paraThis, regNamebuff);
					  strcat(paraThis, " @type `this`");
					  thisFlag = 1;
                     }
                  // now print other params 
                   if((pDexMethod->accessFlags & ACC_STATIC) ==0)
                       startReg++;
                   while (*base != ')') {
                     char* cp = tmpBuf;    
                     int flag=0;
					 int objectParamFlag = 0; // sankar introduces this flag to add annotation "@ type object" for each object parameter
                     while (*base == '[')
                       *cp++ = *base++;

                     if (*base == 'L') {

					   objectParamFlag = 1; // the current parameter is an object 	 
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

                   // identifying the local variables which are also parameters

                   if(locVarList)
                     {
		                for(int i=0; i < locVarList->count(); i++)
		                   {	 
		                      void* temp1 = (*locVarList)[i];
		                      if(temp1) 
			                   {
			                     	locVarInf* t1 = (locVarInf*) temp1;

                                    if(t1->descriptor && (t1->reg == startReg)) // startReg is the current variable's register's index
									{
				                        // fprintf(pFp, "        [|%s|] [|%s|] [|v%d|] @scope (L%04x,L%04x); tmp=%s ",
				                            // t1->descriptor, t1->name, t1->reg, t1->startAddress,  t1->endAddress, tmp );

									   t1->paramFlag = true; // the corresponding element of locVarList is set as a parameter so that we can identify it in dumpLocals()
									}
							   }
						   }
					 }

			     // param identification done

                     {  
						paramCount++; 
						// strcat(para, "[|"); // sankar added 
                        strcat(para,toPilar(tmp));
						// strcat(para, "|]"); // sankar added
                        char buffer[20];  // sankar increased the size 
                        sprintf(buffer, " v%d", startReg++);
                        strcat(para,buffer);
                        if(objectParamFlag==1)
							strcat(para, " @type `object`");

                        if(*base !=')') strcat(para,", ");
                     }
                     if(flag==1){
						 char buffer[20];
						 sprintf(buffer, "       v%d;\n", startReg);
						 strcat(tailRegisters,buffer);
						 startReg++;
					 }
                     free(tmp);
                 }

              //if(strcmp(name,"<init>")==0)  printf("    procedure %s [|init|] (%s) @%s {\n", rtype,para,accessStr);
             // else if(strcmp(name,"<clinit>")==0)  printf("    procedure %s [|clinit|] (%s) @%s {\n", rtype,para,accessStr);

             if(thisFlag == 1 && paramCount > 0)
			 {
			   	 strcat(paraThis, ", ");
		     }

			 strcat(paraTotal, paraThis);
			 strcat(paraTotal, para);

              fprintf(pFp, "    procedure %s `%s.%s` (%s) @owner %s @signature `%s.%s:%s` @Access `%s` {\n",
			            toPilar(rtype), owner, name, paraTotal, toPilar(owner), backDescriptor, name, typeDescriptor, accessStr); // not in dexdump
             free(rtype);
             free(para);
			 free(paraThis);
			 free(paraTotal);
              //printf("      name          : '%s'\n", name);
            //  printf("      type          : '%s'\n", typeDescriptor);

              if (pDexMethod->codeOff == 0) {
				  printf("      code          : (none)\n");
                 fprintf(pFp, "      # return;"); // sankar changed from "#" to "#return;" because some abstract interface method body was containing only {#} which was causing symbol resolution error 
              } else {
                  printf("      code          -\n");
                  dumpCode(pDexFile, pDexMethod, tailRegisters);
              }

              if (gOptions.disassemble)
                  printf("\n");  
                  fprintf(pFp, "\n"); // sankar  
              fprintf(pFp, "   }\n"); // not in dexdump


              //******************* kui's modification ends  *******************
    } else if (gOptions.outputFormat == OUTPUT_XML) { // seems to be not supported for pilar 
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
void dumpSField(const DexFile* pDexFile, const DexField* pSField, int i,bool flag, char* className) // sankar adds the 5th argument
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
              printf("    #%d              : (in %s)\n", i, backDescriptor);
              printf("      name          : '%s'\n", name);
              printf("      type          : '%s'\n", typeDescriptor);
              printf("      access        : 0x%04x (%s)\n",
                  pSField->accessFlags, accessStr); 
                    if(flag)fprintf(pFp, "      global %s `@@%s.%s`", toPilar(descriptorToDot(typeDescriptor)), className, name); // sankar adds className
                    else fprintf(pFp, "      %s `%s.%s`",toPilar(descriptorToDot(typeDescriptor)), className, name); // sankar adds className
                    fprintf(pFp, "    @AccessFlag %s;\n",accessStr);

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
void dumpIField(const DexFile* pDexFile, const DexField* pIField, int i, char* className) // sankar adds the 4th argument
{
    dumpSField(pDexFile, pIField, i,false, className); // sankar adds the 5th argument "className"
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

    char* currentFile; // sankar adds
    char* tmpFile; // fengguo adds
    char* currentClassName; // sankar adds
    char* classDesc; // sankar adds
    char* currentPath; // sankar adds
    int fd;
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
        fprintf(topFp, "Trouble reading class data (#%d)\n", idx); // sankar
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
                { 
					printf("</package>\n");
					fprintf(pFp, "</package>\n"); // sankar

				}
            printf("<package name=\"%s\"\n>\n", mangle);
            fprintf(pFp, "<package name=\"%s\"\n>\n", mangle); // sankar
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
    //******************* kui's and sankar's modification begins  *******************

    // starting creation of a file which will contain the current class's pilar
     classDesc = descriptorToDot(classDescriptor);
     currentDir = replaceChar(dirName(classDesc), '.', '/');
     currentPath = (char*)malloc(strlen(pilarRootDir) + strlen(currentDir) + 2);
     strcpy(currentPath, pilarRootDir);
     strcat(currentPath, "/");
     strcat(currentPath, currentDir);
     currentClassName = className(classDesc);
     mkdirp(currentPath); // creating the directory if it does not already exists
     currentFile = (char*)malloc(strlen(currentPath) + strlen(currentClassName) + strlen(PILAR_EXT) + 2); // double check for the length miscalculation here
     strcpy(currentFile, currentPath);
     if(strcmp(currentDir, "") != 0)
    	 strcat(currentFile, "/");
     strcat(currentFile, currentClassName);
     strcat(currentFile, PILAR_EXT);
     while((fd = open(currentFile, O_CREAT | O_WRONLY | O_EXCL, S_IRUSR | S_IWUSR)) < 0){
		 if(errno == EEXIST){
			 close(fd);
			 tmpFile = (char*)malloc(strlen(currentFile) + strlen(PILAR_EXT) + 1);
			 strcpy(tmpFile, currentFile);
			 strcat(tmpFile, PILAR_EXT);
			 free(currentFile);
			 currentFile = tmpFile;
		 } else {
			 break;
		 }
     }
     close(fd);
     pFp = fopen(currentFile, "w"); // creating a file
     if (!pFp) {
	   fprintf(stderr, " \n could not open the pilar file %s \n", currentFile);
	 }

     // creation of the file is done

    if (gOptions.outputFormat == OUTPUT_PLAIN) {
		printf("Class #%d            -\n", idx);
		printf("  Class descriptor  : '%s'\n", classDescriptor);
		printf("  Access flags      : 0x%04x (%s)\n",
		  pClassDef->accessFlags, accessStr);

		if (superclassDescriptor != NULL)
		  printf("  Superclass        : '%s'\n", superclassDescriptor);

		printf("  Interfaces        -\n"); // above 5 prints not in pilar

		if(packageName==NULL||strcmp(packageName,getPackageName(classDescriptor))!=0) {
		  // fprintf(pFp, "package [|%s|] ;\n", getPackageName(classDescriptor)); // not in dexdump
		  packageName=getPackageName(classDescriptor);
		}
		pInterfaces = dexGetInterfacesList(pDexFile, pClassDef);
		if (superclassDescriptor != NULL && strcmp(superclassDescriptor,"Ljava/lang/Object;")!= 0) {
		  fprintf(pFp, "record %s ", toPilar(descriptorToDot(classDescriptor))); // not in dexdump
		  if(!((pClassDef->accessFlags)&ACC_INTERFACE)==0)  fprintf(pFp, " @type interface");
		  else fprintf(pFp, " @type class"); // not in dexdump
		  fprintf(pFp, " @AccessFlag %s  extends %s",accessStr,toPilar(descriptorToDot(superclassDescriptor))); // not in dexdump
		  if (pInterfaces != NULL) {
			for (i = 0; i < (int) pInterfaces->size; i++)
				dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,1);

		  }
		}
		else{
		  fprintf(pFp, "record %s ", toPilar(descriptorToDot(classDescriptor))); // not in dexdump
		  if(!((pClassDef->accessFlags)&ACC_INTERFACE)==0)  fprintf(pFp, " @type interface"); // not in dexdump
					  else fprintf(pFp, " @type class"); // not in dexdump
					  fprintf(pFp, " @AccessFlag %s ",accessStr); // not in dexdump
		  if (pInterfaces != NULL) {
			  fprintf(pFp, "extends "); // not in dexdump
			  for (i = 0; i < (int) pInterfaces->size-1; i++)
			  dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,2);
			  dumpInterface(pDexFile, dexGetTypeItem(pInterfaces, i), i,0);
		 }
             }

            fprintf(pFp, " {\n"); // not in dexdump
            for (i = 0; i < (int) pClassData->header.instanceFieldsSize; i++) {
                dumpIField(pDexFile, &pClassData->instanceFields[i], i, descriptorToDot(classDescriptor)); // sankar adds the 4th argument
            }
            fprintf(pFp, "   }\n"); // not in dexdump
                    //******************* kui's modification ends  *******************
    } else {
        char* tmp;

        tmp = descriptorClassToDot(classDescriptor);
        printf("<class name=\"%s\"\n", tmp);
        fprintf(pFp, "<class name=\"%s\"\n", tmp); // sankar
        free(tmp);

        if (superclassDescriptor != NULL) {
            tmp = descriptorToDot(superclassDescriptor);
            printf(" extends=\"%s\"\n", tmp);
            fprintf(pFp," extends=\"%s\"\n", tmp); // sankar
            free(tmp);
        }
        printf(" abstract=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_ABSTRACT) != 0));
        fprintf(pFp, " abstract=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_ABSTRACT) != 0));
        printf(" static=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_STATIC) != 0));
        fprintf(pFp, " static=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_STATIC) != 0));
        printf(" final=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_FINAL) != 0));
        fprintf(pFp, " final=%s\n",
            quotedBool((pClassDef->accessFlags & ACC_FINAL) != 0));
        // "deprecated=" not knowable w/o parsing annotations
        printf(" visibility=%s\n",
            quotedVisibility(pClassDef->accessFlags));
        fprintf(pFp, " visibility=%s\n",
            quotedVisibility(pClassDef->accessFlags));
        printf(">\n");
        fprintf(pFp, ">\n");
    }


    //******************* kui's modification begins  *******************
        if (gOptions.outputFormat == OUTPUT_PLAIN)
          if((int) pClassData->header.instanceFieldsSize!=0||(int) pClassData->header.staticFieldsSize!=0)
              printf("  local\n"); // not in pilar
        for (i = 0; i < (int) pClassData->header.staticFieldsSize; i++) {
            dumpSField(pDexFile, &pClassData->staticFields[i], i,true, descriptorToDot(classDescriptor)); // sankar adds the 5th argument
        }

        if (gOptions.outputFormat == OUTPUT_PLAIN)
            printf("  Instance fields   -\n"); // not in pilar


        if (gOptions.outputFormat == OUTPUT_PLAIN)
            printf("  Direct methods    -\n"); // not in pilar
        for (i = 0; i < (int) pClassData->header.directMethodsSize; i++) {
            dumpMethod(pDexFile, &pClassData->directMethods[i], i,descriptorToDot(classDescriptor));
        }

        if (gOptions.outputFormat == OUTPUT_PLAIN)
            printf("  Virtual methods   -\n"); // not in pilar
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
            printf("  @source_file_idx:(%s)\n",fileName);
        }
        //******************* kui's modification ends  *******************

    if (gOptions.outputFormat == OUTPUT_XML) {
        printf("</class>\n");
        fprintf(pFp, "</class>\n"); // sankar
    }

    if(pFp)
    	fclose(pFp); // closing the current class's pilar file. // sankar adds.

bail:
    free(pClassData);
    free(accessStr);
}


/*
 * Advance "ptr" to ensure 32-bit alignment.
 */
static inline const u1* align32(const u1* ptr)
{
    return (u1*) (((uintptr_t) ptr + 3) & ~0x03); // sankar : note that it was "(int)" casting instead of "(uintptr_t)" in version 4.0
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

    fprintf(pFp,"        (differential compression %d -> %d [%d -> %d])\n",
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
    fprintf(topFp, "      #%d: 0x%08x %s\n", idx, offset, name);

    u1 format;
    int addrWidth;

    format = *data++;
    if (format == 1) {              /* kRegMapFormatNone */
        /* no map */
        printf("        (no map)\n");
        fprintf(topFp, "        (no map)\n");
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
        fprintf(topFp, "        (unknown format %d!)\n", format);
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
            fprintf(topFp, "        %4x:", addr);
            for (byte = 0; byte < regWidth; byte++) {
                printf(" %02x", *data++);
                fprintf(topFp, " %02x", *data++);
            }
            printf("\n");
            fprintf(topFp, "\n");
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
        fprintf(topFp, "No register maps found\n");
        return;
    }

    ptr = pClassPool;
    numClasses = get4LE(ptr);
    ptr += sizeof(u4);
    classOffsets = (const u4*) ptr;

    printf("RMAP begins at offset 0x%07x\n", baseFileOffset);
    fprintf(topFp, "RMAP begins at offset 0x%07x\n", baseFileOffset);
    printf("Maps for %d classes\n", numClasses);
    fprintf(topFp, "Maps for %d classes\n", numClasses);
    for (idx = 0; idx < (int) numClasses; idx++) {
        const DexClassDef* pClassDef;
        const char* classDescriptor;

        pClassDef = dexGetClassDef(pDexFile, idx);
        classDescriptor = dexStringByTypeIdx(pDexFile, pClassDef->classIdx);

        printf("%4d: +%d (0x%08x) %s\n", idx, classOffsets[idx],
            baseFileOffset + classOffsets[idx], classDescriptor);
        fprintf(topFp, "%4d: +%d (0x%08x) %s\n", idx, classOffsets[idx],
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
            fprintf(topFp, "NOTE: method count discrepancy (%d != %d + %d)\n",
                methodCount, pClassData->header.directMethodsSize,
                pClassData->header.virtualMethodsSize);
            /* this is bad, but keep going anyway */
        }

        printf("    direct methods: %d\n",
            pClassData->header.directMethodsSize);
        fprintf(topFp, "    direct methods: %d\n",
            pClassData->header.directMethodsSize);
        for (i = 0; i < (int) pClassData->header.directMethodsSize; i++) {
            dumpMethodMap(pDexFile, &pClassData->directMethods[i], i, &data);
        }

        printf("    virtual methods: %d\n",
            pClassData->header.virtualMethodsSize);
        fprintf(topFp, "    virtual methods: %d\n",
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
        fprintf(topFp, "</package>\n");
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
        fprintf(topFp, "Checksum verified\n"); // sankar
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
    fprintf(stderr, "Copyright (C) 2007 The Android Open Source Project\nSankar and Fengguo modified it!\n\n");
    fprintf(stderr,
        "%s: [-c] [-d] [-f] [-h] [-i] [-l layout] [-m] [-p] [-t tempfile] [-o outputfile] filename[.apk|.dex|.odex]...\n",
        gProgName);
    fprintf(stderr, "\n");
    fprintf(stderr, " -c : verify checksum and exit\n");
    fprintf(stderr, " -d : disassemble code sections\n");
    fprintf(stderr, " -f : display summary information from file header\n");
    fprintf(stderr, " -h : display file header details\n");
    fprintf(stderr, " -i : ignore checksum failures\n");
    fprintf(stderr, " -l : output layout, either 'plain' or 'xml'\n");
    fprintf(stderr, " -m : dump register maps (and nothing else)\n");
    fprintf(stderr, " -p : also produce pilar output in a file, name.pilar \n");  // sankar adds
    fprintf(stderr, " -t : temp file name (defaults to /sdcard/dex-temp-*)\n");
    fprintf(stderr, " -o : output file name (defaults to /currentpath/filename)\n");	// fengguo adds
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

    char* filename; // given (input) file name; // sankar adds
	bool pilar = false; // becomes "true" if -p option is used; // sankar adds
	char* newFile;

    memset(&gOptions, 0, sizeof(gOptions));
    gOptions.verbose = true;

    while (1) {
        ic = getopt(argc, argv, "cdfhil:mpt:o:");  // sankar adds p fengguo adds o
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
        case 'p':       // sankar adds this case
		    pilar = true;
            break;
        case 't':       // temp file, used when opening compressed Jar
            gOptions.tempFileName = optarg;
            break;
        case 'o':		// fengguo adds this case
        	pilarRootDir = optarg;
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

        if(pilar) {  // sankar adds this if clause
           filename = strdup(argv[optind]); // is strdup safe? // sankar adds this line for pilar
           if(!pilarRootDir)
        	   pilarRootDir = pilarDirName(filename);
	          // cut .ext (.dex or .apk) from the input file name (x.ext), and get "x" as the pilar-containing-root-directory; // sankar adds;

	       mkdirp(pilarRootDir); // creating the root directory which contains pilar

	       newFile = (char*)malloc(strlen(pilarRootDir) + strlen("/top.txt") + 1);
	       strcpy(newFile, pilarRootDir);
	       strcat(newFile, "/top.txt");

		   topFp = fopen(newFile, "w");
		      // creating a file named top.txt to hold some info, which can possibly be empty.
		      // Maybe, we do not need to create this "top.txt"; In that case I will delete this code later
		   if(!topFp)
			{
			  fprintf(stderr, " \n could not open the pilar file \n");
			}
        }

        result |= process(argv[optind++]);
        if(topFp)
          fclose(topFp); // sankar adds
    }

    return (result != 0);
}
