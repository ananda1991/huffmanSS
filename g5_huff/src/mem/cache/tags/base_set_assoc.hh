/*
 * Copyright (c) 2012-2014,2017 ARM Limited
 * All rights reserved.
 *
 * The license below extends only to copyright in the software and shall
 * not be construed as granting a license to any other intellectual
 * property including but not limited to intellectual property relating
 * to a hardware implementation of the functionality of the software
 * licensed hereunder.  You may use the software subject to the license
 * terms below provided that you ensure that this notice is replicated
 * unmodified and in its entirety in all distributions of the software,
 * modified or unmodified, in source code or in binary form.
 *
 * Copyright (c) 2003-2005,2014 The Regents of The University of Michigan
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Erik Hallnor
 */

/**
 * @file
 * Declaration of a base set associative tag store.
 */

#ifndef __MEM_CACHE_TAGS_BASE_SET_ASSOC_HH__
#define __MEM_CACHE_TAGS_BASE_SET_ASSOC_HH__

#include <functional>
#include <string>
#include <vector>

#include "base/logging.hh"
#include "base/types.hh"
#include "mem/cache/base.hh"
#include "mem/cache/cache_blk.hh"
#include "mem/cache/replacement_policies/base.hh"
#include "mem/cache/replacement_policies/replaceable_entry.hh"
#include "mem/cache/tags/base.hh"
#include "mem/cache/tags/indexing_policies/base.hh"
#include "mem/packet.hh"
#include "params/BaseSetAssoc.hh"

/**
 * A basic cache tag store.
 * @sa  \ref gem5MemorySystem "gem5 Memory System"
 *
 * The BaseSetAssoc placement policy divides the cache into s sets of w
 * cache lines (ways).
 */
#ifndef TRAIL_STUFF
#define TRAIL_STUFF

#define T0_FMIN 1000
#define T1_FMIN 1000
#define T2_FMIN 1000

#define T0_SIZE 200
#define T1_SIZE 200
#define T2_SIZE 200

#endif

class BaseSetAssoc : public BaseTags
{
  protected:
    /** The allocatable associativity of the cache (alloc mask). */
    unsigned allocAssoc;

    /** The cache blocks. */
    std::vector<CacheBlk> blks;

    /** Whether tags and data are accessed sequentially. */
    const bool sequentialAccess;

    /** Replacement policy */
    BaseReplacementPolicy *replacementPolicy;

  public:

    /**Trail vars */
    int t0_size = 0;
    int t1_size = 0;
    int t2_size = 0;
    CacheBlk* t0_mb = new(CacheBlk);    
    CacheBlk* t1_mb = new(CacheBlk);    
    CacheBlk* t2_mb = new(CacheBlk);    

    /** Convenience typedef. */
     typedef BaseSetAssocParams Params;

    /**
     * Construct and initialize this tag store.
     */
    BaseSetAssoc(const Params *p);

    /**
     * Destructor
     */
    virtual ~BaseSetAssoc() {};

    /**
     * Initialize blocks as CacheBlk instances.
     */
    void tagsInit() override;

    /**
     * This function updates the tags when a block is invalidated. It also
     * updates the replacement data.
     *
     * @param blk The block to invalidate.
     */
    void invalidate(CacheBlk *blk) override;

    /**
     * Access block and update replacement data. May not succeed, in which case
     * nullptr is returned. This has all the implications of a cache access and
     * should only be used as such. Returns the tag lookup latency as a side
     * effect.
     *
     * @param addr The address to find.
     * @param is_secure True if the target memory space is secure.
     * @param lat The latency of the tag lookup.
     * @return Pointer to the cache block if found.
     */
    CacheBlk* accessBlock(Addr addr, bool is_secure, Cycles &lat, int cmd) override
    {
        CacheBlk *blk = findBlock(addr, is_secure);

        // Access all tags in parallel, hence one in each way.  The data side
        // either accesses all blocks in parallel, or one block sequentially on
        // a hit.  Sequential access with a miss doesn't access data.
        tagAccesses += allocAssoc;
        if (sequentialAccess) {
            if (blk != nullptr) {
                dataAccesses += 1;
            }
        } else {
            dataAccesses += allocAssoc;
        }

        // If a cache hit
        if (blk != nullptr) {
            // Update number of references to accessed block
            blk->refCount++;

            // Update replacement data of accessed block
            replacementPolicy->touch(blk->replacementData);
	    
	    	//Trails not enabled
	    	if(blk->trail_en == false){
	        	l1d_lat += 4;
			switch(cmd){
				case 0: l1d_reads++;
					break;
				case 1: l1d_writes++;
					break;
			}
			
	    	}
	    
	    	//Block management for Trail 3
	    	else if(blk->trail_en == true && blk->trail == 3 && name()=="system.cpu.dcache.tags")
	    	{	
	    		//Update hit count on Trail
	        	t3_hits++;
	        	l1d_lat += 2;
			switch(cmd){
				case 0: t3_reads++;
					break;
				case 1: t3_writes++;
					break;
			}
				
	    		//Migrate to Trail 2 ?
           		if(blk->refCount >= T2_FMIN && t2_size < T2_SIZE)
				{
			    	//Increment Trail 2 occupancy
					t2_size++;	

					//Add block to Trail 2
					blk->trail = 2;	
					blk->lfreq = false;

					//Update Trail 2 min block
					if(t2_mb->refCount >= blk->refCount)
					{
						blk->lfreq = true;
						t2_mb->lfreq = false;
						t2_mb = blk;
					}
				}
				else if(blk->refCount >= T2_FMIN && t2_size >= T2_SIZE)
				{		
					/* If the current min block in Trail 2 has a lower refCount
					** than our blk, then swap their trails
					*/
					if(t2_mb->refCount+2 < blk->refCount)
					{
						blk->lfreq = true;
						t2_mb->lfreq = false;
						blk->trail = 2;
						t2_mb->trail = 3;
						t2_mb = blk;
					}
				}
	    	}

	    	//Block management for Trail 2
	    	else if(blk->trail_en == true && blk->trail == 2 && name()=="system.cpu.dcache.tags")
	    	{	
	    		//Update hit count on Trail
	       		t2_hits++;
	        	l1d_lat += 2;
			switch(cmd){
				case 0: t2_reads++;
				        break;
				case 1: t2_writes++;
				   	break;
			}
	    		//Migrate to Trail 1 ?
           		if(blk->refCount >= T1_FMIN && t1_size < T1_SIZE)
				{
			    	//Increment Trail 1 occupancy
					t1_size++;	

					//Add block to Trail 1
					blk->trail = 1;	
					blk->lfreq = false;

					//Update Trail 2 min block
					if(t1_mb->refCount >= blk->refCount)
					{
						blk->lfreq = true;
						t1_mb->lfreq = false;
						t1_mb = blk;
					}
				}
				else if(blk->refCount >= T1_FMIN && t1_size >= T1_SIZE)
				{		
					/* If the current min block in Trail 1 has a lower refCount
					** than our blk, then swap their trails
					*/
					if(t1_mb->refCount+2 < blk->refCount)
					{
						blk->lfreq = true;
						t1_mb->lfreq = false;
						blk->trail = 1;
						t1_mb->trail = 2;
						t1_mb = blk;
					}
				}
	    	}
	    	//Block management for Trail 1
	    	else if(blk->trail_en == true && blk->trail == 1 && name()=="system.cpu.dcache.tags")
	    	{	
	    		//Update hit count on Trail
	        	t1_hits++;
	        	l1d_lat += 2;
			switch(cmd){
				case 0: t1_reads++;
					break;
				case 1: t1_writes++;
					break;
			}
	    		//Migrate to Trail 0 ?
        	   	if(blk->refCount >= T0_FMIN && t0_size < T0_SIZE)
				{
			    	//Increment Trail 2 occupancy
					t0_size++;	

					//Add block to Trail 2
					blk->trail = 0;	
					blk->lfreq = false;

					//Update Trail 2 min block
					if(t0_mb->refCount >= blk->refCount)
					{
						blk->lfreq = true;
						t0_mb->lfreq = false;
						t0_mb = blk;
					}
				}
				else if(blk->refCount >= T0_FMIN && t0_size >= T0_SIZE)
				{		
					/* If the current min block in Trail 0 has a lower refCount
					** than our blk, then swap their trails
					*/
					if(t0_mb->refCount+2 < blk->refCount)
					{
						blk->lfreq = true;
						t0_mb->lfreq = false;
						blk->trail = 0;
						t0_mb->trail = 1;
						t0_mb = blk;
					}
				}
	    	}
	    	//Block management for Trail 0
	    	else if(blk->trail_en == true && blk->trail == 0)
	    	{	
			//Update hit count on Trail
			t0_hits++;
			l1d_lat += 1;
			switch(cmd){
				case 0: t0_reads++;
					break;
				case 1: t0_writes++;
					break;
			}
			// If blk is minBlk, check if its still the least frequent
				 
	    	}
        }
	else {
	    l1d_misses++;
	}

        // The tag lookup latency is the same for a hit or a miss
        lat = lookupLatency;

        return blk;
    }

    /**
     * Find replacement victim based on address. The list of evicted blocks
     * only contains the victim.
     *
     * @param addr Address to find a victim for.
     * @param is_secure True if the target memory space is secure.
     * @param evict_blks Cache blocks to be evicted.
     * @return Cache block to be replaced.
     */
    CacheBlk* findVictim(Addr addr, const bool is_secure,
                         std::vector<CacheBlk*>& evict_blks) const override
    {
        // Get possible entries to be victimized
        const std::vector<ReplaceableEntry*> entries =
            indexingPolicy->getPossibleEntries(addr);

        // Choose replacement victim from replacement candidates
        CacheBlk* victim = static_cast<CacheBlk*>(replacementPolicy->getVictim(
                                entries));

        // There is only one eviction for this replacement
        evict_blks.push_back(victim);

        return victim;
    }

    /**
     * Insert the new block into the cache and update replacement data.
     *
     * @param pkt Packet holding the address to update
     * @param blk The block to update.
     */
    void insertBlock(const PacketPtr pkt, CacheBlk *blk) override
    {
        // Insert block
        BaseTags::insertBlock(pkt, blk);

        // Increment tag counter
        tagsInUse++;

        // Update replacement policy
        replacementPolicy->reset(blk->replacementData);
    }

    /**
     * Limit the allocation for the cache ways.
     * @param ways The maximum number of ways available for replacement.
     */
    virtual void setWayAllocationMax(int ways) override
    {
        fatal_if(ways < 1, "Allocation limit must be greater than zero");
        allocAssoc = ways;
    }

    /**
     * Get the way allocation mask limit.
     * @return The maximum number of ways available for replacement.
     */
    virtual int getWayAllocationMax() const override
    {
        return allocAssoc;
    }

    /**
     * Regenerate the block address from the tag and indexing location.
     *
     * @param block The block.
     * @return the block address.
     */
    Addr regenerateBlkAddr(const CacheBlk* blk) const override
    {
        return indexingPolicy->regenerateAddr(blk->tag, blk);
    }

    void forEachBlk(std::function<void(CacheBlk &)> visitor) override {
        for (CacheBlk& blk : blks) {
            visitor(blk);
        }
    }

    bool anyBlk(std::function<bool(CacheBlk &)> visitor) override {
        for (CacheBlk& blk : blks) {
            if (visitor(blk)) {
                return true;
            }
        }
        return false;
    }
};

#endif //__MEM_CACHE_TAGS_BASE_SET_ASSOC_HH__
