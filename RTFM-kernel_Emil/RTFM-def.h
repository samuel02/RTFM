/*
 * RTFM-def.h
 *
 *  Created on: 20 oct 2014
 *      Author: Emil Fresk
 */

#ifndef RTFM_DEF_H_
#define RTFM_DEF_H_

#include <stddef.h>

/*
 * NVIC hardware addresses
 */
#define PENDSTSET         26
#define ICSR              (* ((volatile uint32_t *) 0xE000ED04UL))

#define DWT_CTRL          (* ((volatile uint32_t *) 0xE0001000UL))
#define DWT_CYCCNT        (* ((volatile uint32_t *) 0xE0001004UL))

#define T_CURR()          ( DWT_CYCCNT )
#define T_CLEAR()         {  }

#define SYSTICK_CURRENT   (* ((volatile uint32_t *) 0xE000E018UL))
#define SYSTICK_RELOAD    (* ((volatile uint32_t *) 0xE000E014UL))
#define SYSTICK_CSR       (* ((volatile uint32_t *) 0xE000E010UL))


/*
 * SysTick mask
 */
#define SYSTICK_MASK       ((1<<24)-1)


/*
 * Time specifics
 */
typedef int32_t  rt_time_t;
typedef uint32_t rt_tid_t;
typedef uint32_t rtfm_lock_t;

#endif /* RTFM_DEF_H_ */
