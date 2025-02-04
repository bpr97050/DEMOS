#ifndef _ndir_h_
#   define _ndir_h_

#ifndef _sys_localopts_h_
#   include <sys/localopts.h>
#endif
#ifndef _sys_types_h_
#   include <sys/types.h>
#endif

/*
 * A directory consists of some number of blocks of DIRBLKSIZ
 * bytes, where DIRBLKSIZ is chosen such that it can be transferred
 * to disk in a single atomic operation (e.g. 512 bytes on most machines).
 *
 * Each DIRBLKSIZ byte block contains some number of directory entry
 * structures, which are of variable length.  Each directory entry has
 * a struct direct at the front of it, containing its inode number,
 * the length of the entry, and the length of the name contained in
 * the entry.  These are followed by the name padded to a 4 byte boundary
 * with null bytes.  All names are guaranteed null terminated.
 * The maximum length of a name in a directory is MAXNAMLEN.
 *
 * The macro DIRSIZ(dp) gives the amount of space required to represent
 * a directory entry.  Free space in a directory is represented by
 * entries which have dp->d_reclen >= DIRSIZ(dp).  All DIRBLKSIZ bytes
 * in a directory block are claimed by the directory entries.  This
 * usually results in the last entry in a directory having a large
 * dp->d_reclen.  When entries are deleted from a directory, the
 * space is returned to the previous entry in the same directory
 * block by increasing its dp->d_reclen.  If the first entry of
 * a directory block is free, then its dp->d_ino is set to 0.
 * Entries other than the first in a directory do not normally have
 * dp->d_ino set to 0.
 */

#define DIRBLKSIZ       512
#define MAXNAMLEN       255

struct  direct {
#ifndef IPK_DIRECTORY
	u_long   d_ino;                 /* inode number of entry */
#else
	ino_t    d_ino  :14;            /* u_long было здесь */
	unsigned d_class:2;
#define DIR_OLD    0
#define DIR_IFDIR  1
#define DIR_IFLNK  2
#define DIR_IFREG  3
#endif
	u_short d_reclen;               /* length of this record */
	u_short d_namlen;               /* length of string in d_name */
	char    d_name[MAXNAMLEN + 1];  /* name must be no longer than this */
};

/*
 * The DIRSIZ macro gives the minimum record length which will hold
 * the directory entry.  This requires the amount of space in struct direct
 * without the d_name field, plus enough space for the name with a terminating
 * null byte (dp->d_namlen+1), rounded up to a 4 byte boundary.
 */
#ifdef  DIRSIZ
#undef  DIRSIZ
#endif

#define DIRSIZ(dp) \
    ((sizeof (struct direct) - (MAXNAMLEN+1)) + (((dp)->d_namlen+1 + 3) &~ 3))

/*
 * Definitions for library routines operating on directories.
 */
typedef struct _dirdesc {
	int     dd_fd;
	long    dd_loc;
	long    dd_size;
	char    dd_buf[DIRBLKSIZ];
} DIR;
#ifndef NULL
#define NULL 0
#endif
extern  DIR *opendir();
extern  struct direct *readdir();
extern  long telldir();
extern  void seekdir();
#define rewinddir(dirp) seekdir((dirp), (long)0)
extern  void closedir();
#endif _ndir_h_
