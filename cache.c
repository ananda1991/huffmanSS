/* Ananda Biswas */
/* cache.c - cache module routines */

/* SimpleScalar(TM) Tool Suite
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 * All Rights Reserved. 
 * 
 * THIS IS A LEGAL DOCUMENT, BY USING SIMPLESCALAR,
 * YOU ARE AGREEING TO THESE TERMS AND CONDITIONS.
 * 
 * No portion of this work may be used by any commercial entity, or for any
 * commercial purpose, without the prior, written permission of SimpleScalar,
 * LLC (info@simplescalar.com). Nonprofit and noncommercial use is permitted
 * as described below.
 * 
 * 1. SimpleScalar is provided AS IS, with no warranty of any kind, express
 * or implied. The user of the program accepts full responsibility for the
 * application of the program and the use of any results.
 * 
 * 2. Nonprofit and noncommercial use is encouraged. SimpleScalar may be
 * downloaded, compiled, executed, copied, and modified solely for nonprofit,
 * educational, noncommercial research, and noncommercial scholarship
 * purposes provided that this notice in its entirety accompanies all copies.
 * Copies of the modified software can be delivered to persons who use it
 * solely for nonprofit, educational, noncommercial research, and
 * noncommercial scholarship purposes provided that this notice in its
 * entirety accompanies all copies.
 * 
 * 3. ALL COMMERCIAL USE, AND ALL USE BY FOR PROFIT ENTITIES, IS EXPRESSLY
 * PROHIBITED WITHOUT A LICENSE FROM SIMPLESCALAR, LLC (info@simplescalar.com).
 * 
 * 4. No nonprofit user may place any restrictions on the use of this software,
 * including as modified by the user, by any other authorized user.
 * 
 * 5. Noncommercial and nonprofit users may distribute copies of SimpleScalar
 * in compiled or executable form as set forth in Section 2, provided that
 * either: (A) it is accompanied by the corresponding machine-readable source
 * code, or (B) it is accompanied by a written offer, with no time limit, to
 * give anyone a machine-readable copy of the corresponding source code in
 * return for reimbursement of the cost of distribution. This written offer
 * must permit verbatim duplication by anyone, or (C) it is distributed by
 * someone who received only the executable form, and is accompanied by a
 * copy of the written offer of source code.
 * 
 * 6. SimpleScalar was developed by Todd M. Austin, Ph.D. The tool suite is
 * currently maintained by SimpleScalar LLC (info@simplescalar.com). US Mail:
 * 2395 Timbercrest Court, Ann Arbor, MI 48105.
 * 
 * Copyright (C) 1994-2003 by Todd M. Austin, Ph.D. and SimpleScalar, LLC.
 */


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <string.h>
#include "host.h"
#include "misc.h"
#include "machine.h"
#include "cache.h"
// BRANCH TEST

/* cache access macros */
#define CACHE_TAG(cp, addr)	((addr) >> (cp)->tag_shift)
#define CACHE_SET(cp, addr)	(((addr) >> (cp)->set_shift) & (cp)->set_mask)
#define CACHE_BLK(cp, addr)	((addr) & (cp)->blk_mask)
#define CACHE_TAGSET(cp, addr)	((addr) & (cp)->tagset_mask)

/* extract/reconstruct a block address */
#define CACHE_BADDR(cp, addr)	((addr) & ~(cp)->blk_mask)
#define CACHE_MK_BADDR(cp, tag, set)					\
  (((tag) << (cp)->tag_shift)|((set) << (cp)->set_shift))

/* index an array of cache blocks, non-trivial due to variable length blocks */
#define CACHE_BINDEX(cp, blks, i)					\
  ((struct cache_blk_t *)(((char *)(blks)) +				\
			  (i)*(sizeof(struct cache_blk_t) +		\
			       ((cp)->balloc				\
				? (cp)->bsize*sizeof(byte_t) : 0))))

/* cache data block accessor, type parameterized */
#define __CACHE_ACCESS(type, data, bofs)				\
  (*((type *)(((char *)data) + (bofs))))

/* cache data block accessors, by type */
#define CACHE_DOUBLE(data, bofs)  __CACHE_ACCESS(double, data, bofs)
#define CACHE_FLOAT(data, bofs)	  __CACHE_ACCESS(float, data, bofs)
#define CACHE_WORD(data, bofs)	  __CACHE_ACCESS(unsigned int, data, bofs)
#define CACHE_HALF(data, bofs)	  __CACHE_ACCESS(unsigned short, data, bofs)
#define CACHE_BYTE(data, bofs)	  __CACHE_ACCESS(unsigned char, data, bofs)

/* cache block hashing macros, this macro is used to index into a cache
   set hash table (to find the correct block on N in an N-way cache), the
   cache set index function is CACHE_SET, defined above */
#define CACHE_HASH(cp, key)						\
  (((key >> 24) ^ (key >> 16) ^ (key >> 8) ^ key) & ((cp)->hsize-1))

/* copy data out of a cache block to buffer indicated by argument pointer p */
#define CACHE_BCOPY(cmd, blk, bofs, p, nbytes)	\
  if (cmd == Read)							\
    {									\
      switch (nbytes) {							\
      case 1:								\
	*((byte_t *)p) = CACHE_BYTE(&blk->data[0], bofs); break;	\
      case 2:								\
	*((half_t *)p) = CACHE_HALF(&blk->data[0], bofs); break;	\
      case 4:								\
	*((word_t *)p) = CACHE_WORD(&blk->data[0], bofs); break;	\
      default:								\
	{ /* >= 8, power of two, fits in block */			\
	  int words = nbytes >> 2;					\
	  while (words-- > 0)						\
	    {								\
	      *((word_t *)p) = CACHE_WORD(&blk->data[0], bofs);	\
	      p += 4; bofs += 4;					\
	    }\
	}\
      }\
    }\
  else /* cmd == Write */						\
    {									\
      switch (nbytes) {							\
      case 1:								\
	CACHE_BYTE(&blk->data[0], bofs) = *((byte_t *)p); break;	\
      case 2:								\
        CACHE_HALF(&blk->data[0], bofs) = *((half_t *)p); break;	\
      case 4:								\
	CACHE_WORD(&blk->data[0], bofs) = *((word_t *)p); break;	\
      default:								\
	{ /* >= 8, power of two, fits in block */			\
	  int words = nbytes >> 2;					\
	  while (words-- > 0)						\
	    {								\
	      CACHE_WORD(&blk->data[0], bofs) = *((word_t *)p);		\
	      p += 4; bofs += 4;					\
	    }\
	}\
    }\
  }

/* bound sqword_t/dfloat_t to positive int */
#define BOUND_POS(N)		((int)(MIN(MAX(0, (N)), 2147483647)))

/* frequency threshold for TrailS */
#define T1MINFREQ 1000
#define T2MINFREQ 100
#define T3MINFREQ 10

/* unlink BLK from the hash table bucket chain in SET */
static void
unlink_htab_ent(struct cache_t *cp,		/* cache to update */
		struct cache_set_t *set,	/* set containing bkt chain */
		struct cache_blk_t *blk)	/* block to unlink */
{
  struct cache_blk_t *prev, *ent;
  int index = CACHE_HASH(cp, blk->tag);

  /* locate the block in the hash table bucket chain */
  for (prev=NULL,ent=set->hash[index];
       ent;
       prev=ent,ent=ent->hash_next)
    {
      if (ent == blk)
	break;
    }
  assert(ent);

  /* unlink the block from the hash table bucket chain */
  if (!prev)
    {
      /* head of hash bucket list */
      set->hash[index] = ent->hash_next;
    }
  else
    {
      /* middle or end of hash bucket list */
      prev->hash_next = ent->hash_next;
    }
  ent->hash_next = NULL;
}

/* insert BLK onto the head of the hash table bucket chain in SET */
static void
link_htab_ent(struct cache_t *cp,		/* cache to update */
	      struct cache_set_t *set,		/* set containing bkt chain */
	      struct cache_blk_t *blk)		/* block to insert */
{
  int index = CACHE_HASH(cp, blk->tag);

  /* insert block onto the head of the bucket chain */
  blk->hash_next = set->hash[index];
  set->hash[index] = blk;
}

/* where to insert a block onto the ordered way chain */
enum list_loc_t { Head, Tail };

/* insert BLK into the order way chain in SET at location WHERE */
static void
update_way_list(struct cache_set_t *set,	/* set contained way chain */
		struct cache_blk_t *blk,	/* block to insert */
		enum list_loc_t where)		/* insert location */
{
  /* unlink entry from the way list */
  if (!blk->way_prev && !blk->way_next)
    {
      /* only one entry in list (direct-mapped), no action */
      assert(set->way_head == blk && set->way_tail == blk);
      /* Head/Tail order already */
      return;
    }
  /* else, more than one element in the list */
  else if (!blk->way_prev)
    {
      assert(set->way_head == blk && set->way_tail != blk);
      if (where == Head)
	{
	  /* already there */
	  return;
	}
      /* else, move to tail */
      set->way_head = blk->way_next;
      blk->way_next->way_prev = NULL;
    }
  else if (!blk->way_next)
    {
      /* end of list (and not front of list) */
      assert(set->way_head != blk && set->way_tail == blk);
      if (where == Tail)
	{
	  /* already there */
	  return;
	}
      set->way_tail = blk->way_prev;
      blk->way_prev->way_next = NULL;
    }
  else
    {
      /* middle of list (and not front or end of list) */
      assert(set->way_head != blk && set->way_tail != blk);
      blk->way_prev->way_next = blk->way_next;
      blk->way_next->way_prev = blk->way_prev;
    }

  /* link BLK back into the list */
  if (where == Head)
    {
      /* link to the head of the way list */
      blk->way_next = set->way_head;
      blk->way_prev = NULL;
      set->way_head->way_prev = blk;
      set->way_head = blk;
    }
  else if (where == Tail)
    {
      /* link to the tail of the way list */
      blk->way_prev = set->way_tail;
      blk->way_next = NULL;
      set->way_tail->way_next = blk;
      set->way_tail = blk;
    }
  else
    panic("bogus WHERE designator");
}

/* create and initialize a general cache structure */
struct cache_t *			/* pointer to cache created */
cache_create(char *name,		/* name of the cache */
	     int nsets,			/* total number of sets in cache */
	     int bsize,			/* block (line) size of cache */
	     int balloc,		/* allocate data space for blocks? */
	     int usize,			/* size of user data to alloc w/blks */
	     int assoc,			/* associativity of cache */
	     enum cache_policy policy,	/* replacement policy w/in sets */
	     /* block access function, see description w/in struct cache def */
	     unsigned int (*blk_access_fn)(enum mem_cmd cmd,
					   md_addr_t baddr, int bsize,
					   struct cache_blk_t *blk,
					   tick_t now),
	     unsigned int hit_latency,	/* latency in cycles for a hit */
	     int use_trail)		/* 1 to enable trails, else 0 */
{
  struct cache_t *cp;
  struct cache_blk_t *blk;
  int i, j, bindex;

  /* check all cache parameters */
  if (nsets <= 0)
    fatal("cache size (in sets) `%d' must be non-zero", nsets);
  if ((nsets & (nsets-1)) != 0)
    fatal("cache size (in sets) `%d' is not a power of two", nsets);
  /* blocks must be at least one datum large, i.e., 8 bytes for SS */
  if (bsize < 8)
    fatal("cache block size (in bytes) `%d' must be 8 or greater", bsize);
  if ((bsize & (bsize-1)) != 0)
    fatal("cache block size (in bytes) `%d' must be a power of two", bsize);
  if (usize < 0)
    fatal("user data size (in bytes) `%d' must be a positive value", usize);
  if (assoc <= 0)
    fatal("cache associativity `%d' must be non-zero and positive", assoc);
  if ((assoc & (assoc-1)) != 0)
    fatal("cache associativity `%d' must be a power of two", assoc);
  if (!blk_access_fn)
    fatal("must specify miss/replacement functions");

  /* allocate the cache structure */
  cp = (struct cache_t *)
    calloc(1, sizeof(struct cache_t) + (nsets-1)*sizeof(struct cache_set_t));
  if (!cp)
    fatal("out of virtual memory");

  /* initialize user parameters */
  cp->name = mystrdup(name);
  cp->nsets = nsets;
  cp->bsize = bsize;
  cp->balloc = balloc;
  cp->usize = usize;
  cp->assoc = assoc;
  cp->policy = policy;
  cp->hit_latency = hit_latency;
  cp->use_trail = use_trail;

  /* miss/replacement functions */
  cp->blk_access_fn = blk_access_fn;

  /* compute derived parameters */
  cp->hsize = CACHE_HIGHLY_ASSOC(cp) ? (assoc >> 2) : 0;
  cp->blk_mask = bsize-1;
  cp->set_shift = log_base2(bsize);
  cp->set_mask = nsets-1;
  cp->tag_shift = cp->set_shift + log_base2(nsets);
  cp->tag_mask = (1 << (32 - cp->tag_shift))-1;
  cp->tagset_mask = ~cp->blk_mask;
  cp->bus_free = 0;
  cp->flat = 0;
  /* print derived parameters during debug */
  debug("%s: cp->hsize     = %d", cp->name, cp->hsize);
  debug("%s: cp->blk_mask  = 0x%08x", cp->name, cp->blk_mask);
  debug("%s: cp->set_shift = %d", cp->name, cp->set_shift);
  debug("%s: cp->set_mask  = 0x%08x", cp->name, cp->set_mask);
  debug("%s: cp->tag_shift = %d", cp->name, cp->tag_shift);
  debug("%s: cp->tag_mask  = 0x%08x", cp->name, cp->tag_mask);

  /* initialize cache stats */
  cp->hits = 0;
  char *cname = cp->name;
  if(cp->use_trail == 1 && !mystricmp(cname,"dl1") )
  {
  cp->t1_hits = 0;
  cp->t2_hits = 0;
  cp->t3_hits = 0;
  cp->t4_hits = 0;
  cp->t1_misses = 0;
  cp->t2_misses = 0;
  cp->t3_misses = 0;
  cp->t4_misses = 0;
  cp->t1_size = 64;//(nsets*assoc)/8;	/* Trail-1 is 1/8 of Cache size */
  cp->t2_size = 64;//(nsets*assoc)/8;	/* Trail-1 is 1/8 of Cache size */
  cp->t3_size = 128;//(nsets*assoc)/4;	/* Trail-1 is 1/4 of Cache size */
//  cp->trail4.size = (nsets*assoc)/2;	/* Trail-1 is 1/2 of Cache size */
  }

  cp->misses = 0;
  cp->replacements = 0;
  cp->writebacks = 0;
  cp->invalidations = 0;

  /* blow away the last block accessed */
  cp->last_tagset = 0;
  cp->last_blk = NULL;

  /* allocate data blocks */
  cp->data = (byte_t *)calloc(nsets * assoc,
			      sizeof(struct cache_blk_t) +
			      (cp->balloc ? (bsize*sizeof(byte_t)) : 0));
  if (!cp->data)
    fatal("out of virtual memory");

  /* slice up the data blocks */
  for (bindex=0,i=0; i<nsets; i++)
    {
      cp->sets[i].way_head = NULL;
      cp->sets[i].way_tail = NULL;
      /* get a hash table, if needed */
      if (cp->hsize)
	{
	  cp->sets[i].hash =
	    (struct cache_blk_t **)calloc(cp->hsize,
					  sizeof(struct cache_blk_t *));
	  if (!cp->sets[i].hash)
	    fatal("out of virtual memory");
	}
      /* NOTE: all the blocks in a set *must* be allocated contiguously,
	 otherwise, block accesses through SET->BLKS will fail (used
	 during random replacement selection) */
      cp->sets[i].blks = CACHE_BINDEX(cp, cp->data, bindex);
      
      /* link the data blocks into ordered way chain and hash table bucket
         chains, if hash table exists */
      for (j=0; j<assoc; j++)
	{
	  /* locate next cache block */
	  blk = CACHE_BINDEX(cp, cp->data, bindex);
	  bindex++;

	  /* invalidate new cache block */
	  blk->status = 0;
	  blk->tag = 0;
	  blk->ready = 0;
	  blk->user_data = (usize != 0
			    ? (byte_t *)calloc(usize, sizeof(byte_t)) : NULL);

	  /* insert cache block into set hash table */
	  if (cp->hsize)
	    link_htab_ent(cp, &cp->sets[i], blk);

	  /* insert into head of way list, order is arbitrary at this point */
	  blk->way_next = cp->sets[i].way_head;
	  blk->way_prev = NULL;
	  if (cp->sets[i].way_head)
	    cp->sets[i].way_head->way_prev = blk;
	  cp->sets[i].way_head = blk;
	  if (!cp->sets[i].way_tail)
	    cp->sets[i].way_tail = blk;
	}
    }
  printf("Created Cache %s\n",name);
  return cp;
}

/* parse policy */
enum cache_policy			/* replacement policy enum */
cache_char2policy(char c)		/* replacement policy as a char */
{
  switch (c) {
  case 'l': return LRU;
  case 'r': return Random;
  case 'f': return FIFO;
  default: fatal("bogus replacement policy, `%c'", c);
  }
}

/* print cache configuration */
void
cache_config(struct cache_t *cp,	/* cache instance */
	     FILE *stream)		/* output stream */
{
  fprintf(stream,
	  "cache: %s: %d sets, %d byte blocks, %d bytes user data/block\n",
	  cp->name, cp->nsets, cp->bsize, cp->usize);
  fprintf(stream,
	  "cache: %s: %d-way, `%s' replacement policy, write-back\n",
	  cp->name, cp->assoc,
	  cp->policy == LRU ? "LRU"
	  : cp->policy == Random ? "Random"
	  : cp->policy == FIFO ? "FIFO"
	  : (abort(), ""));
}
/* calculate cache access time from cacti model */
float cacti_delay(struct cache_t *cp, char *trail, int time, long long reads, long long writes)
{
  float access_time;
  float energy;
  float read_en;
  float write_en;  
  float acc_time;
  counter_t hits;
  printf("\n\tINSIDE CACTI DELAY FUNCTION\n");

  /*
  Cache size : 32K  Block size : 64B  Assoc : 4
  Trail1 : 4K  Trail2 : 4K  Trail3 : 8K  Trail4 : 16K
  */  
  if(!mystricmp(trail, "notrail")){
  hits = cp->t1_hits;
  read_en = 0.00713416;
  write_en = 0.00866007;
  acc_time = 0.228525;
  }
  if(!mystricmp(trail, "t1")){
  hits = cp->t1_hits;
  read_en = 0.00326818;
  write_en = 0.00338381;
  acc_time = 0.133457;
  }
  if(!mystricmp(trail, "t2")){
  hits = cp->t2_hits;
  read_en = 0.00326818;
  write_en = 0.00338381;
  acc_time = 0.133457;
  }
  if(!mystricmp(trail, "t3")){
  hits = cp->t3_hits;
  read_en = 0.00405591;
  write_en = 0.00411318;
  acc_time = 0.137565;
  }
  if(!mystricmp(trail, "t4")){
  hits = cp->t4_hits;
  read_en = 0.00504986;
  write_en = 0.0056024;
  acc_time = 0.168648;
  }
  if(time == 1){
  printf("\nTIME = 1\n");
  printf("lld\n",reads);
  access_time = (hits * acc_time);
  return access_time;}
  else{
  energy = (hits * (read_en*reads) + hits * (write_en*writes));
  return energy;}
  
  
}
/* register cache stats */
void
cache_reg_stats(struct cache_t *cp,	/* cache instance */
		struct stat_sdb_t *sdb)	/* stats database */
{
  char buf[512], buf1[512],buf2[512], *name;

  /* get a name for this cache */
  if (!cp->name || !cp->name[0])
    name = "<unknown>";
  else
    name = cp->name;

  sprintf(buf, "%s.accesses", name);
  sprintf(buf1, "%s.hits + %s.misses", name, name);
  stat_reg_formula(sdb, buf, "total number of accesses", buf1, "%12.0f");
  sprintf(buf, "%s.hits", name);
  stat_reg_counter(sdb, buf, "total number of hits", &cp->hits, 0, NULL);
  sprintf(buf, "%s.misses", name);
  stat_reg_counter(sdb, buf, "total number of misses", &cp->misses, 0, NULL);
 
  //USING TRAILS ?
  if(cp->use_trail == 1 && !mystricmp(name,"dl1"))
  {
  float access_time,access_energy;
  printf("**************************Registering Trail stats**********************\n");
  sprintf(buf, "%s.t1_accesses", name);
  sprintf(buf1, "%s.t1_hits + %s.t1_misses", name, name);
  stat_reg_formula(sdb, buf, "total number of accesses to trail-1", buf1, "%12.0f");
  sprintf(buf, "%s.t1_reads", name);
  stat_reg_counter(sdb, buf, "total number of reads in trail-1", &cp->t1_reads, 0, NULL);
  sprintf(buf, "%s.t1_writes", name);
  stat_reg_counter(sdb, buf, "total number of writes in trail-1", &cp->t1_writes, 0, NULL);
  sprintf(buf, "%s.t2_accesses", name);
  sprintf(buf1, "%s.t2_hits + %s.t2_misses", name, name);
  stat_reg_formula(sdb, buf, "total number of accesses to trail-2", buf1, "%12.0f");
  sprintf(buf, "%s.t2_reads", name);
  stat_reg_counter(sdb, buf, "total number of reads in trail-2", &cp->t2_reads, 0, NULL);
  sprintf(buf, "%s.t2_writes", name);
  stat_reg_counter(sdb, buf, "total number of writes in trail-2", &cp->t2_writes, 0, NULL);
  sprintf(buf, "%s.t3_accesses", name);
  sprintf(buf1, "%s.t3_hits + %s.t3_misses", name, name);
  stat_reg_formula(sdb, buf, "total number of accesses to trail-3", buf1, "%12.0f");
  sprintf(buf, "%s.t3_reads", name);
  stat_reg_counter(sdb, buf, "total number of reads in trail-3", &cp->t3_reads, 0, NULL);
  sprintf(buf, "%s.t3_writes", name);
  stat_reg_counter(sdb, buf, "total number of writes in trail-3", &cp->t3_writes, 0, NULL);
  sprintf(buf, "%s.t4_accesses", name);
  //int a = cp->t4_hits; int b = cp->t4_misses;  
  //sprintf(buf1, "%lld + %lld", a, b);
  sprintf(buf1, "%s.t4_hits + %s.t4_misses", name, name);
  stat_reg_formula(sdb, buf, "total number of accesses to trail-4", buf1, "%12.0f");
  sprintf(buf, "%s.t4_reads", name);
  stat_reg_counter(sdb, buf, "total number of reads in trail-4", &cp->t4_reads, 0, NULL);
  sprintf(buf, "%s.t4_writes", name);
  stat_reg_counter(sdb, buf, "total number of writes in trail-4", &cp->t4_writes, 0, NULL);
  sprintf(buf, "%s.t1_hits", name);
  stat_reg_counter(sdb, buf, "total number of hits in trail-1", &cp->t1_hits, 0, NULL);
  sprintf(buf, "%s.t2_hits", name);
  stat_reg_counter(sdb, buf, "total number of hits in trail-2", &cp->t2_hits, 0, NULL);
  sprintf(buf, "%s.t3_hits", name);
  stat_reg_counter(sdb, buf, "total number of hits in trail-3", &cp->t3_hits, 0, NULL);
  sprintf(buf, "%s.t4_hits", name);
  stat_reg_counter(sdb, buf, "total number of hits in trail-4", &cp->t4_hits, 0, NULL);
  sprintf(buf, "%s.t1_misses", name);
  stat_reg_counter(sdb, buf, "total number of misses in trail-1", &cp->t1_misses, 0, NULL);
  sprintf(buf, "%s.t2_misses", name);
  stat_reg_counter(sdb, buf, "total number of misses in trail-2", &cp->t2_misses, 0, NULL);
  sprintf(buf, "%s.t3_misses", name);
  stat_reg_counter(sdb, buf, "total number of misses in trail-3", &cp->t3_misses, 0, NULL);
  sprintf(buf, "%s.t4_misses", name);
  stat_reg_counter(sdb, buf, "total number of misses in trail-4", &cp->t4_misses, 0, NULL);
  sprintf(buf, "%s.t1_miss_rate", name);
  sprintf(buf1, "%s.t1_misses / %s.t1_accesses", name, name);
  stat_reg_formula(sdb, buf, "trail-1 miss rate (i.e., misses/ref)", buf1, NULL);
  sprintf(buf, "%s.t2_miss_rate", name);
  sprintf(buf1, "%s.t2_misses / %s.t2_accesses", name, name);
  stat_reg_formula(sdb, buf, "trail-2 miss rate (i.e., misses/ref)", buf1, NULL);
  sprintf(buf, "%s.t3_miss_rate", name);
  sprintf(buf1, "%s.t3_misses / %s.t3_accesses", name, name);
  stat_reg_formula(sdb, buf, "trail-3 miss rate (i.e., misses/ref)", buf1, NULL);
  sprintf(buf, "%s.t4_miss_rate", name);
  sprintf(buf1, "%s.t4_misses / %s.t4_accesses", name, name);
  stat_reg_formula(sdb, buf, "trail-4 miss rate (i.e., misses/ref)", buf1, NULL);

  access_time = 1;//cacti_delay(cp,"t1",1, cp->t1_reads, cp->t1_writes);
  //sprintf(buf, "%s.t1_access_time",name);
  printf("\n\nWTF%lld\n\n",cp->t1_hits);
  stat_reg_float(sdb,buf,"trail-1 access time",(void*)&access_time,2.0,NULL);
  //DEBUG
  printf("**************************Registering Trail stats complete**********************\n");
  }
  sprintf(buf, "%s.replacements", name);
  stat_reg_counter(sdb, buf, "total number of replacements",
		 &cp->replacements, 0, NULL);
  sprintf(buf, "%s.writebacks", name);
  stat_reg_counter(sdb, buf, "total number of writebacks",
		 &cp->writebacks, 0, NULL);
  sprintf(buf, "%s.invalidations", name);
  stat_reg_counter(sdb, buf, "total number of invalidations",
		 &cp->invalidations, 0, NULL);
  sprintf(buf, "%s.miss_rate", name);
  sprintf(buf1, "%s.misses / %s.accesses", name, name);
  stat_reg_formula(sdb, buf, "miss rate (i.e., misses/ref)", buf1, NULL);
  sprintf(buf, "%s.repl_rate", name);
  sprintf(buf1, "%s.replacements / %s.accesses", name, name);
  stat_reg_formula(sdb, buf, "replacement rate (i.e., repls/ref)", buf1, NULL);
  sprintf(buf, "%s.wb_rate", name);
  sprintf(buf1, "%s.writebacks / %s.accesses", name, name);
  stat_reg_formula(sdb, buf, "writeback rate (i.e., wrbks/ref)", buf1, NULL);
  sprintf(buf, "%s.inv_rate", name);
  sprintf(buf1, "%s.invalidations / %s.accesses", name, name);
  stat_reg_formula(sdb, buf, "invalidation rate (i.e., invs/ref)", buf1, NULL);
  //printf("%lld\n",cp->t1_hits);
  printf("Cache Registration Complete\n");
}

/* print cache stats */
void
cache_stats(struct cache_t *cp,		/* cache instance */
	    FILE *stream)		/* output stream */
{
  char *cname = cp->name;
  double sum = (double)(cp->hits + cp->misses);

  fprintf(stream,
	  "cache: %s: %.0f hits %.0f misses %.0f repls %.0f invalidations\n",
	  cp->name, (double)cp->hits, (double)cp->misses,
	  (double)cp->replacements, (double)cp->invalidations);
  fprintf(stream,
	  "cache: %s: miss rate=%f  repl rate=%f  invalidation rate=%f\n",
	  cp->name,
	  (double)cp->misses/sum, (double)(double)cp->replacements/sum,
	  (double)cp->invalidations/sum);
  if(cp->use_trail == 1 && !mystricmp(cname,"dl1"))
  {
#if 0
	  double t1_sum = (double)(cp->t1_hits + cp->t1_misses);
	  double t2_sum = (double)(cp->t2_hits + cp->t2_misses);
	  double t3_sum = (double)(cp->t3_hits + cp->t3_misses);
	  double t4_sum = (double)(cp->t4_hits + cp->t4_misses);
#endif	  
	  double t1_sum = (double)(cp->t1_hits + cp->t1_misses);
	  double t2_sum = (double)(cp->t2_hits + cp->t2_misses);
	  double t3_sum = (double)(cp->t3_hits + cp->t3_misses);
	  double t4_sum = (double)(cp->t4_hits + cp->t4_misses);

  fprintf(stream,
          "Trail-1 cache: %s: %.0f hits %.0f misses %.0f repls %.0f invalidations\n",
          cp->name, (double)cp->t1_hits, (double)cp->t1_misses,
          (double)cp->replacements, (double)cp->invalidations);
  fprintf(stream,
          "Trail-1 cache: %s: miss rate=%f  repl rate=%f  invalidation rate=%f\n",
          cp->name,
          (double)cp->t1_misses/t1_sum, (double)(double)cp->replacements/t1_sum,
          (double)cp->invalidations/t1_sum);
  fprintf(stream,
          "Trail-2 cache: %s: %.0f hits %.0f misses %.0f repls %.0f invalidations\n",
          cp->name, (double)cp->t2_hits, (double)cp->t2_misses,
          (double)cp->replacements, (double)cp->invalidations);
  fprintf(stream,
          "Trail-2 cache: %s: miss rate=%f  repl rate=%f  invalidation rate=%f\n",
          cp->name,
          (double)cp->t2_misses/t2_sum, (double)(double)cp->replacements/t2_sum,
          (double)cp->invalidations/t2_sum);
  fprintf(stream,
          "Trail-3 cache: %s: %.0f hits %.0f misses %.0f repls %.0f invalidations\n",
          cp->name, (double)cp->t3_hits, (double)cp->t3_misses,
          (double)cp->replacements, (double)cp->invalidations);
  fprintf(stream,
          "Trail-3 cache: %s: miss rate=%f  repl rate=%f  invalidation rate=%f\n",
          cp->name,
          (double)cp->t3_misses/t3_sum, (double)(double)cp->replacements/t3_sum,
          (double)cp->invalidations/t3_sum);
  fprintf(stream,
          "Trail-4 cache: %s: %.0f hits %.0f misses %.0f repls %.0f invalidations\n",
          cp->name, (double)cp->t4_hits, (double)cp->t4_misses,
          (double)cp->replacements, (double)cp->invalidations);
  fprintf(stream,
          "Trail-4 cache: %s: miss rate=%f  repl rate=%f  invalidation rate=%f\n",
          cp->name,
          (double)cp->t4_misses/t4_sum, (double)(double)cp->replacements/t4_sum,
          (double)cp->invalidations/t4_sum);

  }
}

/* access a cache, perform a CMD operation on cache CP at address ADDR,
   places NBYTES of data at *P, returns latency of operation if initiated
   at NOW, places pointer to block user data in *UDATA, *P is untouched if
   cache blocks are not allocated (!CP->BALLOC), UDATA should be NULL if no
   user data is attached to blocks */
unsigned int				/* latency of access in cycles */
cache_access(struct cache_t *cp,	/* cache to access */
	     enum mem_cmd cmd,		/* access type, Read or Write */
	     md_addr_t addr,		/* address of access */
	     void *vp,			/* ptr to buffer for input/output */
	     int nbytes,		/* number of bytes to access */
	     tick_t now,		/* time of access */
	     byte_t **udata,		/* for return of user data ptr */
	     md_addr_t *repl_addr)	/* for address of replaced block */
{
  byte_t *p = vp;
  md_addr_t tag = CACHE_TAG(cp, addr);
  md_addr_t set = CACHE_SET(cp, addr);
  md_addr_t bofs = CACHE_BLK(cp, addr);
  struct cache_blk_t *blk, *repl;
  int lat = 0, tlat = 0, c;
  int loop = 0;
  struct cache_blk_t *minblk;
  char *cname = cp->name;
  FILE *f, *m, *s;
  struct cache_blk_t *state; /* loop counter for cp->sets[set] state */

  const int tt1=1, tt2=1,tt3=2,tt4=2,ttm=3;
  const int td1=1,td2=1,td3=2,td4=2,tdm=3;
 
  //DEBUG CODE
  //if(cp->use_trail == 1)
  //printf("Use Trail ? %d\t Cache: %s\n", cp->use_trail, cname);
//printf("cache access %s\n",cname);

/*  if(strcmp(cname,"dl2") && strcmp(cname,"dl1") && strcmp(cname,"il1") && strcmp(cname,"il2") && strcmp(cname,"dtlb") && strcmp(cname,"itlb"))
  printf("%s cache being accessed\n",cname);
*/
  //if (!mystricmp(cname,"dl1")){
  //f = fopen("/home/ananda/simplescalar/simplesim-3.0/bench_analysis/DcacheL1_accesses.txt", "a");
  //if(!f){printf("Error opening file %s_accesses\n",cname); exit(1);}
  //fprintf(f,"%u\n",addr); fclose(f);
  //}
  /* default replacement address */
  if (repl_addr)
    *repl_addr = 0;

  /* check alignments */
  if ((nbytes & (nbytes-1)) != 0 || (addr & (nbytes-1)) != 0)
    fatal("cache: access error: bad size or alignment, addr 0x%08x", addr);

  /* access must fit in cache block */
  /* FIXME:
     ((addr + (nbytes - 1)) > ((addr & ~cp->blk_mask) + (cp->bsize - 1))) */
  if ((addr + nbytes) > ((addr & ~cp->blk_mask) + cp->bsize))
    fatal("cache: access error: access spans block, addr 0x%08x", addr);

  /* permissions are checked on cache misses */

  /* check for a fast hit: access to same block */
  if (CACHE_TAGSET(cp, addr) == cp->last_tagset)
    {
      /* hit in the same block */
      blk = cp->last_blk;
      if (!mystricmp(cname,"dl1") && set == 0){
  //            s = fopen("/home/ananda/simplescalar/simplesim-3.0/set_state.txt", "a");
  //            for(c=0,state = cp->sets[set].way_head; state; state = state->way_next)
  //            { if(c==0)fprintf(s,"\nH\n");fprintf(s,"way%d tag=%u\t",c,state->tag);c++; }        
  //            fclose(s);
              }

    }
    
  if (cp->hsize)
    {
      /* higly-associativity cache, access through the per-set hash tables */
      int hindex = CACHE_HASH(cp, tag);

      for (blk=cp->sets[set].hash[hindex];
	   blk;
	   blk=blk->hash_next)
	{
	  if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
          {
	      if (!mystricmp(cname,"dl1") && set == 0){
//	      s = fopen("/home/ananda/simplescalar/simplesim-3.0/set_state.txt", "a");
//	      for(c=0,state = cp->sets[set].way_head; state; state = state->way_next)
//	      { if(c==0)fprintf(s,"\nH\n");fprintf(s,"way%d tag=%u\t",c,state->tag);c++; }	
//	      fclose(s);
	      }
	    goto cache_hit;
	  }
	}
    }
  else
    {
      /* low-associativity cache, linear search the way list */
      for (blk=cp->sets[set].way_head;
	   blk;
	   blk=blk->way_next)
	{	
	  if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
	  {
      	    if (!mystricmp(cname,"dl1") && set == 0){
//              s = fopen("/home/ananda/simplescalar/simplesim-3.0/set_state.txt", "a");
//              for(c=0,state = cp->sets[set].way_head; state; state = state->way_next)
//              { if(c==0)fprintf(s,"\nH\n");fprintf(s,"way%d tag=%u\t",c,state->tag);c++; }        
//              fclose(s);
              }

	    goto cache_hit;
	  }
	}
    }

  /* cache block not found */

  /* **MISS** */
  cp->misses++;
  if(cp->use_trail == 1 && !mystricmp(cname,"dl1"))
  {
  //if(cmd == Read) cp->t4_reads++;
 //if(cmd == Write) cp->t4_writes++;
  //printf("Using Trail\n");
  cp->t1_misses++;
  cp->t2_misses++;
  cp->t3_misses++;
  cp->t4_misses++;
  }
  /* Report Misses to log*/
//  m = fopen("/home/ananda/simplescalar/simplesim-3.0/hit_miss_log.txt", "a");    /*create/open file for writing*/
//  if (m == NULL)
//  {printf("Error opening hit_miss_log.txt file %s\n", strerror(errno));exit(1);}
//  fprintf(m,"M\t%u\t%u\n",addr,tag);
//  fclose(m);

  /* select the appropriate block to replace, and re-link this entry to
     the appropriate place in the way list */
  switch (cp->policy) {
  case LRU:
  case FIFO:
    repl = cp->sets[set].way_tail;
/*    if(cp->use_trail == 1 && !mystricmp(cname,"dl1") && set == 0)
    { s = fopen("/home/ananda/simplescalar/simplesim-3.0/set_state.txt", "a");
      if (s == NULL)
      { printf("Error opening addr_freq.txt file %s\n", strerror(errno));exit(1);}
  
    fprintf(s,"on miss way_tail=%u\n",cp->sets[set].way_tail->tag);
    fclose(m);
    } //end use_trail == 1
  */  update_way_list(&cp->sets[set], repl, Head);
    break;
  case Random:
    {
      int bindex = myrand() & (cp->assoc - 1);
      repl = CACHE_BINDEX(cp, cp->sets[set].blks, bindex);
    /*  if(cp->use_trail == 1 && !mystricmp(cname,"dl1") && set == 0)
      { m = fopen("/home/ananda/simplescalar/simplesim-3.0/hit_miss_log.txt", "a");
        if (m == NULL)
        { printf("Error opening addr_freq.txt file %s\n", strerror(errno));exit(1);}
      fprintf(m,"M\t%u\t%u\n",addr,repl->tag);
//    fprintf(f,"%s %u %lld\n",cp->name,addr,repl->accesses);
      fclose(m);  
      }*/
    }
    break;
  default:
    panic("bogus replacement policy");
  }

  /* remove this block from the hash bucket chain, if hash exists */
  if (cp->hsize)
    unlink_htab_ent(cp, &cp->sets[set], repl);

  /* blow away the last block to hit */
  cp->last_tagset = 0;
  cp->last_blk = NULL;

  /* write back replaced block data */
  if (repl->status & CACHE_BLK_VALID)
    {
      cp->replacements++;

      if (repl_addr)
	*repl_addr = CACHE_MK_BADDR(cp, repl->tag, set);
 
      /* don't replace the block until outstanding misses are satisfied */
      lat += BOUND_POS(repl->ready - now);
 
      /* stall until the bus to next level of memory is available */
      lat += BOUND_POS(cp->bus_free - ( + lat));
 
      /* track bus resource usage */
      cp->bus_free = MAX(cp->bus_free, ( + lat)) + 1;

      if (repl->status & CACHE_BLK_DIRTY)
	{
	  /* write back the cache block */
	  cp->writebacks++;
	  lat += cp->blk_access_fn(Write,
				   CACHE_MK_BADDR(cp, repl->tag, set),
				   cp->bsize, repl, +lat);
	}
    }
  if(cp->use_trail == 1 && !mystricmp(cname,"dl1")){
  cp->flat += tt1+tt2+tt3+tt4;}
  else
  cp->flat += ttm;
  
  /* update block tags */
  repl->tag = tag;
  repl->status = CACHE_BLK_VALID;	/* dirty bit set on update */
  if (!mystricmp(cname,"dl1") && set == 0){
//              s = fopen("/home/ananda/simplescalar/simplesim-3.0/set_state.txt", "a");
//              s = fopen("/home/ananda/simplescalar/simplesim-3.0/latency.txt", "a");
//              for(c=0,state = cp->sets[0].way_head; state; state = state->way_next)
//              { if(c==0)fprintf(s,"\nR\n");fprintf(s,"way%d tag=%u\t",c,state->tag);c++; }        
                /*fprintf(s,"\nR\n");*///fprintf(s,"%d\n",lat);      
//              fclose(s);
              }

  /*DEBUG*/
  /*if(cp->use_trail == 1 && !mystricmp(cname,"dl1"))
  {
  m = fopen("/home/ananda/simplescalar/simplesim-3.0/hit_miss_log.txt", "a");
  if (m == NULL)
  {printf("Error opening hit_miss_log.txt file %s\n", strerror(errno));exit(1);}
  fprintf(m,"R\t%u\t%u\n",addr,repl->tag);
  fclose(m);
  }*/
  
  if(cp->use_trail == 1 && !mystricmp(cname,"dl1"))
  {repl->trailname = malloc(sizeof(char)*2);		/* Insert new block into T-4 */
  repl->trailname = "t2";}
  //repl->trailname[1]='1';
  //repl->trailname = "t2";  
  /* read data block */
  lat += cp->blk_access_fn(Read, CACHE_BADDR(cp, addr), cp->bsize,
			   repl, +lat);

  /* copy data out of cache block */
  if (cp->balloc)
    {
      CACHE_BCOPY(cmd, repl, bofs, p, nbytes);
    }

  /* update dirty status */
  if (cmd == Write)
    repl->status |= CACHE_BLK_DIRTY;

  /* get user block data, if requested and it exists */
  if (udata)
    *udata = repl->user_data;

  /* update block status */
  repl->ready = +lat;

  /* link this entry back into the hash table */
  if (cp->hsize)
    link_htab_ent(cp, &cp->sets[set], repl);

  /* return latency of the operation */
  return lat;


 cache_hit: /* slow hit handler */
  
  /* **HIT** */
  cp->hits++;
/*  m = fopen("/home/ananda/simplescalar/simplesim-3.0/hit_miss_log.txt", "a");
  if (m == NULL)
  {printf("Error opening hit_miss_log.txt file %s\n", strerror(errno));exit(1);}
  fprintf(m,"H\t%u\t%u\n",addr,tag);
  fclose(m);*/
  /* copy data out of cache block, if block exists */
  if (cp->balloc)
    {
      CACHE_BCOPY(cmd, blk, bofs, p, nbytes);
    }

  /* update dirty status */
  if (cmd == Write)
    blk->status |= CACHE_BLK_DIRTY;
      
  /* if LRU replacement and this is not the first element of list, reorder */
  if (blk->way_prev && cp->policy == LRU)
    {
      /* move this block to head of the way (MRU) list */
      update_way_list(&cp->sets[set], blk, Head);
    }

  /* tag is unchanged, so hash links (if they exist) are still valid */

  /* record the last block to hit */
  cp->last_tagset = CACHE_TAGSET(cp, addr);
  cp->last_blk = blk;

  /* get user block data, if requested and it exists */
  if (udata)
    *udata = blk->user_data;

/**************************Block Management starts here*************************/

  if(cp->use_trail == 1 && !mystricmp(cp->name, "dl1"))
  {
  blk->freq++ ;

  if(!mystricmp(blk->trailname, "t1")) // block belongs to t1 
  {   cp->t1_hits++; 
      cp->flat += (tt1 + td1);
      if(cmd == Read) cp->t1_reads++;
      else cp->t1_writes++;
  }
  else if(!mystricmp(blk->trailname, "t2"))        //block belongs to t2
  {
      cp->t1_misses++;
//      if(cmd == Read) cp->t1_reads++;
//      if(cmd == Write) cp->t1_writes++;
      cp->t2_hits++;
      cp->flat += (tt1 + tt2+ td2);
      if(cmd == Read) cp->t2_reads++;
      else cp->t2_writes++;
  
      // migrate to higher trail ? No swapping  
      if((blk->freq > T1MINFREQ) && (cp->t1_count < cp->t1_size)) 
      {
        blk->trailname = "t1"; //move to t1
	cp->t1_count++;	       //increment t1 count
        for(loop=0; loop < cp->nsets; loop++)
        {
	  // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t1") && minblk->freq > blk->freq)
            {              
              //set new blk as min
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T1MINFREQ) && !(cp->t1_count < cp->t1_size))
      { 
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
	    //find the min block but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t1") && minblk->freq < blk->freq)    
            {
              //swap blocks
              minblk->trailname = "t2";
              blk->trailname = "t1";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t2")
    
    else if(!mystricmp(blk->trailname, "t3"))        //block belongs to t3
    {
      cp->t1_misses++;
//      if(cmd == Read) cp->t1_reads++;
//      if(cmd == Write) cp->t1_writes++;
      cp->t2_misses++;
//      if(cmd == Read) cp->t2_reads++;
//      if(cmd == Write) cp->t2_writes++;
      cp->t3_hits++;
      cp->flat += (tt1 + tt2 + tt3 + td3);
      if(cmd == Read) cp->t3_reads++;
      else cp->t3_writes++;
  
      // migrate to higher trail ? No swapping  
      if((blk->freq > T2MINFREQ) && (cp->t2_count < cp->t2_size))                              
      {
        blk->trailname = "t2"; //move to t2
        cp->t2_count++;        //increment t2 count
        for(loop=0; loop < cp->nsets; loop++)
        {
          // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
	    // find min blk in t2  
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t2") && minblk->freq > blk->freq)
            { 
              //set new blk as min if old min blk has higher freq
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T2MINFREQ) && !(cp->t2_count < cp->t2_size)) 
      { 
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            //find the min blk in t3 but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t2") && minblk->freq < blk->freq)    
            {
              //swap blocks
              minblk->trailname = "t3";
              blk->trailname = "t2";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t3")

    else if(!mystricmp(blk->trailname, "t4"))        //block belongs to t4
    {
      cp->t1_misses++;
//      if(cmd == Read) cp->t1_reads++;
//      if(cmd == Write) cp->t1_writes++;
      cp->t2_misses++;
//      if(cmd == Read) cp->t2_reads++;
//      if(cmd == Write) cp->t2_writes++;
      cp->t3_misses++;
//      if(cmd == Read) cp->t3_reads++;
//      if(cmd == Write) cp->t3_writes++;
      cp->t4_hits++;
      cp->flat += (tt1 + tt2 + tt3 + tt4 + td4);
      if(cmd == Read) cp->t4_reads++;
      else cp->t4_writes++;

      // migrate to higher trail ? No swapping  
      if((blk->freq > T3MINFREQ) && (cp->t3_count < cp->t3_size))
      {
        blk->trailname = "t3"; //move to t3
        cp->t3_count++;        //increment t3 count
        for(loop=0; loop < cp->nsets; loop++)
        {
          // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            // find min blk in t3  
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t3") && minblk->freq > blk->freq)
            {
              //set new blk as min if old min blk has higher freq
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T3MINFREQ) && !(cp->t3_count < cp->t3_size))
      {
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            //find the min blk in t3 but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t3") && minblk->freq < blk->freq)
            {
              //swap blocks
              minblk->trailname = "t4";
              blk->trailname = "t3";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t4")

  
  }
  // End Block Management
  if(cp->use_trail != 1)
  {cp->flat += (tdm + ttm);} 

  if ( (0.1+cp->flat) <= 0.29)
  tlat = 0;
  else if(0.29 < (0.1 + cp->flat) <= 0.58)
  {tlat = 1;cp->flat = cp->flat - 0.29;}  
  else if(0.58 < (0.1 + cp->flat) <= 0.87)
  {tlat = 2;cp->flat = cp->flat - 0.58;}
  else if(0.87 < (0.1 + cp->flat) <= 1.16)
  {tlat = 3;cp->flat = cp->flat - 0.87;}  
  else if(1.16 < (0.1 + cp->flat) <= 1.45)
  {tlat = 4;cp->flat = cp->flat - 1.16;}  
 // printf("TLAT = %f\nHIT LAT=%d\n",tlat,cp->hit_latency); 
  /* return first cycle data is available to access */
  //return (int) MAX(tlat, (blk->ready - now));
  return (int) MAX(cp->hit_latency, (blk->ready - now));

 cache_fast_hit: /* fast hit handler */
  
  /* **FAST HIT** */
  cp->hits++;

  /* copy data out of cache block, if block exists */
  if (cp->balloc)
    {
      CACHE_BCOPY(cmd, blk, bofs, p, nbytes);
    }

  /* update dirty status */
  if (cmd == Write)
    blk->status |= CACHE_BLK_DIRTY;

  /* this block hit last, no change in the way list */

  /* tag is unchanged, so hash links (if they exist) are still valid */

  /* get user block data, if requested and it exists */
  if (udata)
    *udata = blk->user_data;

  /* record the last block to hit */
  cp->last_tagset = CACHE_TAGSET(cp, addr);
  cp->last_blk = blk;

/***********************************Block Management starts here************************************/

  if(cp->use_trail == 1 && !mystricmp(cp->name, "dl1"))
  {
  blk->freq++ ;

  if(!mystricmp(blk->trailname, "t1")) // block belongs to t1 
  {   cp->t1_hits++;
      cp->flat += (tt1 + td1);
      if(cmd == Read) cp->t1_reads++;
      else cp->t1_writes++;
  }
  else if(!mystricmp(blk->trailname, "t2"))        //block belongs to t2
  {
      cp->t1_misses++;
      cp->t2_hits++;
      cp->flat += (tt1 + tt2 + td1);
      if(cmd == Read) cp->t2_reads++;
      else cp->t2_writes++;  
      // migrate to higher trail ? No swapping  
      if((blk->freq > T1MINFREQ) && (cp->t1_count < cp->t1_size)) 
      {
        blk->trailname = "t1"; //move to t1
	cp->t1_count++;	       //increment t1 count
        for(loop=0; loop < cp->nsets; loop++)
        {
	  // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t1") && minblk->freq > blk->freq)
            {              
              //set new blk as min
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T1MINFREQ) && !(cp->t1_count < cp->t1_size))
      { 
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
	    //find the min block but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t1") && minblk->freq < blk->freq)    
            {
              //swap blocks
              minblk->trailname = "t2";
              blk->trailname = "t1";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t2")
    
    else if(!mystricmp(blk->trailname, "t3"))        //block belongs to t3
    {
      cp->t1_misses++;
      cp->t2_misses++;
      cp->t3_hits++;
      cp->flat += (tt1 + tt2 + tt3 + td1);
      if(cmd == Read) cp->t3_reads++;
      else cp->t3_writes++;
      // migrate to higher trail ? No swapping  
      if((blk->freq > T2MINFREQ) && (cp->t2_count < cp->t2_size))                              
      {
        blk->trailname = "t2"; //move to t2
        cp->t2_count++;        //increment t2 count
        for(loop=0; loop < cp->nsets; loop++)
        {
          // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
	    // find min blk in t2  
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t2") && minblk->freq > blk->freq)
            { 
              //set new blk as min if old min blk has higher freq
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T2MINFREQ) && !(cp->t2_count < cp->t2_size)) 
      { 
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            //find the min blk in t3 but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t2") && minblk->freq < blk->freq)    
            {
              //swap blocks
              minblk->trailname = "t3";
              blk->trailname = "t2";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t3")

    else if(!mystricmp(blk->trailname, "t4"))        //block belongs to t4
    {
      cp->t1_misses++;
      cp->t2_misses++;
      cp->t3_misses++;
      cp->t4_hits++;
      cp->flat += (tt1 + tt2 + tt3 + tt4 + td1);
      if(cmd == Read) cp->t4_reads++;
      else cp->t4_writes++;
      // migrate to higher trail ? No swapping  
      if((blk->freq > T3MINFREQ) && (cp->t3_count < cp->t3_size))
      {
        blk->trailname = "t3"; //move to t3
        cp->t3_count++;        //increment t3 count
        for(loop=0; loop < cp->nsets; loop++)
        {
          // loop through all blocks in all sets
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            // find min blk in t3  
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t3") && minblk->freq > blk->freq)
            {
              //set new blk as min if old min blk has higher freq
              minblk->min = 0;
              blk->min = 1;
            }
          }
        }
      }
      // migrate to higher trail ? Swapping required
      else if((blk->freq > T3MINFREQ) && !(cp->t3_count < cp->t3_size))
      {
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            //find the min blk in t3 but swap only if it has a lesser freq than blk
            if(minblk->min == 1 && !mystricmp(blk->trailname, "t3") && minblk->freq < blk->freq)
            {
              //swap blocks
              minblk->trailname = "t4";
              blk->trailname = "t3";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) 
    }       //end if(blk->trailname == "t4")

  }
  // End Block Management



  /*if(cp->use_trail == 1 && !mystricmp(cname,"dl1"))
  {
    //printf("blk freq = %d\ttrail = %s\n", blk->freq, blk->trailname);
    blk->freq++ ;
    //printf("blk freq = %d\ttrail = %s\n", blk->freq, blk->trailname);

    if(!mystricmp(blk->trailname, "t1")) // block belongs to t1 
    {
      cp->t1_hits++;
    }
    else if(!mystricmp(blk->trailname, "t2"))        //block belongs to t2
    {
      cp->t1_misses++;
      cp->t2_hits++;
      if((blk->freq > MINFREQ) && (cp->t1_count < cp->t1_size)) // migrate to higher trail ? No swapping 
      {
        //DEBUG
        //printf("Moving block to Trail 1");
        blk->trailname = "t1";
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            if(minblk->min == 1)
            {
              if(minblk->freq > blk->freq)
              {
                //set new blk as min
                minblk->min = 0;
                blk->min = 1;
              }
            }
          }
        }
      }
      else if((blk->freq > MINFREQ) && !(cp->t1_count < cp->t1_size)) // migrate to higher trail ? Swapping required
      { 
	//DEBUG
        //printf("Moving block to Trail 1");
        //search cache for min freq block
        for(loop=0; loop < cp->nsets; loop++)
        {
          for(minblk = cp->sets[loop].way_head; minblk; minblk = minblk->way_next)
          {
            if(minblk->min == 1)    //found
            {
              cp->t1_count++; //
              //swap blocks
              minblk->trailname = "t2";
              blk->trailname = "t1";
              //new min
              minblk->min = 0;
              blk->min = 1;
            }       //end check for min = 1
          }         //end loop through blocks in a set 
        }         //end loop ober all sets
      }       //end elseif((blk->freq > minfreq) && !(t1count<t1size)) */
//    }       //end if(blk->trailname == "t2")
//  }
  // End Block Management
  // End Block Management
  if(cp->use_trail != 1)
  {cp->flat += (tdm + ttm);}
  /* return first cycle data is available to access */
  if ( (0.1+cp->flat) <= 0.29)
  tlat = 0;
  else if(0.29 < (0.1 + cp->flat) <= 0.58)
  {tlat = 1;cp->flat = cp->flat - 0.29;}
  else if(0.58 < (0.1 + cp->flat) <= 0.87)
  {tlat = 2;cp->flat = cp->flat - 0.58;}
  else if(0.87 < (0.1 + cp->flat) <= 1.16)
  {tlat = 3;cp->flat = cp->flat - 0.87;}
  else if(1.16 < (0.1 + cp->flat) <= 1.45)
  {tlat = 4;cp->flat = cp->flat - 1.16;}
//  printf("TLAT = %f\nHIT LAT=%d\n",tlat,cp->hit_latency); 
  //return (int) MAX(tlat, (blk->ready - now));
  return (int) MAX(cp->hit_latency, (blk->ready - now));

}

/* return non-zero if block containing address ADDR is contained in cache
   CP, this interface is used primarily for debugging and asserting cache
   invariants */
int					/* non-zero if access would hit */
cache_probe(struct cache_t *cp,		/* cache instance to probe */
	    md_addr_t addr)		/* address of block to probe */
{
  md_addr_t tag = CACHE_TAG(cp, addr);
  md_addr_t set = CACHE_SET(cp, addr);
  struct cache_blk_t *blk;

  /* permissions are checked on cache misses */

  if (cp->hsize)
  {
    /* higly-associativity cache, access through the per-set hash tables */
    int hindex = CACHE_HASH(cp, tag);
    
    for (blk=cp->sets[set].hash[hindex];
	 blk;
	 blk=blk->hash_next)
    {	
      if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
	  return TRUE;
    }
  }
  else
  {
    /* low-associativity cache, linear search the way list */
    for (blk=cp->sets[set].way_head;
	 blk;
	 blk=blk->way_next)
    {
      if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
	  return TRUE;
    }
  }
  
  /* cache block not found */
  return FALSE;
}

/* flush the entire cache, returns latency of the operation */
unsigned int				/* latency of the flush operation */
cache_flush(struct cache_t *cp,		/* cache instance to flush */
	    tick_t now)			/* time of cache flush */
{
  int i, lat = cp->hit_latency; /* min latency to probe cache */
  struct cache_blk_t *blk;

  /* blow away the last block to hit */
  cp->last_tagset = 0;
  cp->last_blk = NULL;

  /* no way list updates required because all blocks are being invalidated */
  for (i=0; i<cp->nsets; i++)
    {
      for (blk=cp->sets[i].way_head; blk; blk=blk->way_next)
	{
	  if (blk->status & CACHE_BLK_VALID)
	    {
	      cp->invalidations++;
	      blk->status &= ~CACHE_BLK_VALID;

	      if (blk->status & CACHE_BLK_DIRTY)
		{
		  /* write back the invalidated block */
          	  cp->writebacks++;
		  lat += cp->blk_access_fn(Write,
					   CACHE_MK_BADDR(cp, blk->tag, i),
					   cp->bsize, blk, now+lat);
		}
	    }
	}
    }

  /* return latency of the flush operation */
  return lat;
}

/* flush the block containing ADDR from the cache CP, returns the latency of
   the block flush operation */
unsigned int				/* latency of flush operation */
cache_flush_addr(struct cache_t *cp,	/* cache instance to flush */
		 md_addr_t addr,	/* address of block to flush */
		 tick_t now)		/* time of cache flush */
{
  md_addr_t tag = CACHE_TAG(cp, addr);
  md_addr_t set = CACHE_SET(cp, addr);
  struct cache_blk_t *blk;
  int lat = cp->hit_latency; /* min latency to probe cache */

  if (cp->hsize)
    {
      /* higly-associativity cache, access through the per-set hash tables */
      int hindex = CACHE_HASH(cp, tag);

      for (blk=cp->sets[set].hash[hindex];
	   blk;
	   blk=blk->hash_next)
	{
	  if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
	    break;
	}
    }
  else
    {
      /* low-associativity cache, linear search the way list */
      for (blk=cp->sets[set].way_head;
	   blk;
	   blk=blk->way_next)
	{
	  if (blk->tag == tag && (blk->status & CACHE_BLK_VALID))
	    break;
	}
    }

  if (blk)
    {
      cp->invalidations++;
      blk->status &= ~CACHE_BLK_VALID;

      /* blow away the last block to hit */
      cp->last_tagset = 0;
      cp->last_blk = NULL;

      if (blk->status & CACHE_BLK_DIRTY)
	{
	  /* write back the invalidated block */
          cp->writebacks++;
	  lat += cp->blk_access_fn(Write,
				   CACHE_MK_BADDR(cp, blk->tag, set),
				   cp->bsize, blk, now+lat);
	}
      /* move this block to tail of the way (LRU) list */
      update_way_list(&cp->sets[set], blk, Tail);
    }

  /* return latency of the operation */
  return lat;
}
