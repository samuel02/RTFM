//*****************************************************************************
// RTFM-kernel.h
// Copyright Per Lindgren 2014
// Kernel API
//*****************************************************************************
#ifndef __RTFM_KERNEL__
#define __RTFM_KERNEL__
#include <stdint.h>
#include "RTFM_COMPILER_DEP.h"

/* map logic priorities to physical */
#define H(P) 				    ( (1<<__NVIC_PRIO_BITS)-P )

/* map and shift logic priority to physical */
#define PRIO_SHIFT(P)		    ( H(P)<<(8 -__NVIC_PRIO_BITS) )

// Compiler agnostic
#define RTFM_pend(X) 			{ NVIC_SetPendingIRQ(X);	  }
#define RTFM_clear(X) 			{ NVIC_ClearPendingIRQ(X);    }

#define RTFM_set_priority(X, P) { NVIC_SetPriority(X, H(P));  }
#define RTFM_enable_irq(X)      { NVIC_EnableIRQ(X); 	  	  }
#define RTFM_disable_irq(X)     { NVIC_DisableIRQ(X);   	  }
#define RTFM_enable() 			{ ENABLE_IRQ();				  }
#define RTFM_disable() 	    	{ DISABLE_IRQ();			  }

// Kernel locking mechanism
#define RTFM_lock(R) { 										  \
	int sc_old = GET_BASEPRI();							      \
	SET_BASEPRI_MAX(PRIO_SHIFT(R));			 				  \
	BARRIER_LOCK;

#define RTFM_unlock(R)										  \
	BARRIER_UNLOCK; 										  \
	SET_BASEPRI(sc_old); 	    							  \
}

#endif
