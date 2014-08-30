/*
 * RTFM-PT.h
 * Pthread run-time system API for RTFM-core
 * (C) 2014 Per Lindgren
 */
#ifndef RTFM_PT_H_
#define RTFM_PT_H_
#include "RTFM-RT.h"

typedef void (*ENTRY_FUNC)(int);
void user_reset(int);
void user_idle(int);

#define FALSE 					false
#define TRUE 					true

void RTFM_lock(int, int);
void RTFM_unlock(int, int);
void RTFM_pend(RTFM_time, RTFM_time, int, int);

#endif /* RTFM_PT_H_ */
