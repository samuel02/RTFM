/*
 * RTFM-PT.h
 * Pthread run-time system API for RTFM-core
 * (C) 2014 Per Lindgren
 */


#ifndef RTFM_PT_H_
#define RTFM_PT_H_

typedef void (*ENTRY_FUNC)();

#define FALSE 		0
#define TRUE 		(~FALSE)

void RTFM_lock(int r);
void RTFM_unlock(int r);
void RTFM_pend(int t);

#endif /* RTFM_PT_H_ */
