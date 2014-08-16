/*
 * RTFM-PT.h
 * Pthread run-time system API for RTFM-core
 * (C) 2014 Per Lindgren
 */
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>

#ifndef RTFM_PT_H_
#define RTFM_PT_H_

typedef void (*ENTRY_FUNC)(int);
void user_reset(int);

#define FALSE 				false
#define TRUE 				true

void RTFM_lock(int, int);
void RTFM_unlock(int, int);
void RTFM_pend(int, int, int);

#define RT_rand(x)      	( rand() % x )
#define RT_sleep(x) 		( sleep( x ) )
#define RT_printf(...) 		{ printf( __VA_ARGS__ ); }

//#define Task				void
//#define ISR					void

#endif /* RTFM_PT_H_ */
