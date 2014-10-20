#ifdef __COMPCERT__
#include "CCOMP_LPC17xx.h"  /* CompCert modified CMSIS, stripped to the necessary */
#include "CCOMP_core_cm3.h" /* CompCert modified CMSIS, stripped to the necessary */

#define BARRIER_LOCK        { __builtin_barrier_dsb();      \
#define BARRIER_UNLOCK      { __builtin_barrier();          }

#define SET_BASEPRI_MAX(x)  { __builtin_set_basepri_max(x); }
#define SET_BASEPRI(x)  	{ __builtin_set_basepri(x); 	}
#define GET_BASEPRI()       { __builtin_get_basepri(); 		}
#define ENABLE_IRQ()        { __builtin_enable_irq();       }
#define DISABLE_IRQ()       { __builtin_disable_irq();      }

#define __FORCE_INLINE      inline

#elif __GNUC__

#include "stm32f4xx.h" /* Original CMSIS */

// Fix lacking CMSIS definition
__attribute__( ( always_inline ) ) static __INLINE void __set_BASEPRI_MAX(uint32_t value)
{
  __ASM volatile ("MSR basepri_max, %0" : : "r" (value) );
}

#define BARRIER_LOCK 		{ __ASM volatile("dsb\n" "isb\n" ::: "memory"); }
#define BARRIER_UNLOCK 		{ __ASM volatile("" ::: "memory"); }
#define GCC_BARRIER 		{ __ASM volatile("" ::: "memory"); }

#define SET_BASEPRI_MAX(x)  { __set_BASEPRI_MAX(x);	 		}
#define SET_BASEPRI(x)  	{ __set_BASEPRI(x); 			}
#define GET_BASEPRI()       ( __get_BASEPRI()				)
#define ENABLE_IRQ()        { __enable_irq();       		}
#define DISABLE_IRQ()       { __disable_irq();      		}

#define __FORCE_INLINE      __attribute__( ( always_inline ) ) __STATIC_INLINE

#endif
