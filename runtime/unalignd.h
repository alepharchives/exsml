/* To read 16 bit and 32 bit words when they are not aligned */

#ifndef _unaligned_
#define _unaligned_


#ifdef HAVE_ALIGNED_ACCESS_REQUIRED

#define s16(p) (int) ((((schar *) (p))[1] << 8) + ((unsigned char *) (p))[0])
#define u16(p) (unsigned int) ((((unsigned char *) (p))[1] << 8) \
               + ((unsigned char *) (p))[0])
#define s32(p) (int32_t) ((((schar *) (p))[3] << 24) \
               + (((unsigned char *) (p))[2] << 16) \
               + (((unsigned char *) (p))[1] << 8) \
               + ((unsigned char *) (p))[0])
#define u32(p) (uint32_t) ((((unsigned char *) (p))[3] << 24) \
               + (((unsigned char *) (p))[2] << 16) \
               + (((unsigned char *) (p))[1] << 8) \
               + ((unsigned char *) (p))[0])

#else

#define s16(p) (* (short *) (p))
#define u16(p) (* (unsigned short *) (p))
#define s32(p) (* (int32_t *) (p))
#define u32(p) (* (uint32_t *) (p))

#endif


#endif /* _unaligned_ */
